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
001500020528     FAZORG21l  IF   E           K DISK
001600020528     fazorg01l  if   e           k disk
001700020322     Ftntbe01l  IF   E           K DISK
001800020322
001900000000     D*------------------------------------------------------------------------*
002000000000     D* SCHIERE
002100000000     D*------------------------------------------------------------------------*
002200000000     D* MESSAGGI
002300000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002400000000     D* COMANDI
002500000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002600000000     D* DESCRIZIONI OPZIONI
002700000000     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
002800000000     D* DESCRIZIONI TASTI FUNZIONALI
002900000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003000000000     D* TABELLA OPZIONI DA CARICARE A VIDEO
003100000000     D OPV             S             19    DIM(24)
003200000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
003300000000     D FUV             S             19    DIM(24)
003400000000     D* FILIALI E LORO DECODIFICHE
003500971124     D FIL             S              3  0 DIM(500)
003600971124     D DFIL            S             20    DIM(500)
003700000000     D* TIPI UNITA E LORO DECODIFICHE
003800000000     D UNI             S              2  0 DIM(100)
003900000000     D DUNI            S             25    DIM(100)
004000000000     D*------------------------------------------------------------------------*
004100000000     D* INPUT
004200000000     D*------------------------------------------------------------------------*
004300020326
004400020711     d WokUni          s              1    inz('0')
004500020711     d WokDot          s              1    inz('0')
004600020326     d wnosfl          s              1    inz('0')
004700020326     d wunmag          s                   like(unamag)
004800020326     d wuntip          s                   like(unatip)
004900020711     d SavMag          s                   Like(UnaMag)
005000020711     d SavTip          s                   Like(UnaTip)
005100020711     d SavAtb          s                   Like(DotAtb)
005200020711     d SavQta          s                   Like(DotQta)
005300020326     d depdot          s                   like(vdpdot)
005400020405     d depmag          s                   like(vdpmag)
005500020326     d kdofil          s                   like(dotfil)
005600020326
005700000000     D*----------------
005800000000     D* RIDEFINIZIONE SCHIERA -OPD-
005900000000     D*----------------
006000000000     D OPDDS           DS
006100000000     D*                -OPZIONE PRINCIPALE
006200000000     D  OPDOPZ                 1      2
006300000000     D*                -DESCRIZIONE PARZIALE
006400000000     D  OPDDE1                 3     19
006500000000     D*                -OPZIONE ALTERNATIVA
006600000000     D  OPDOP1                21     22
006700000000     D*                -FILLER
006800000000     D  OPDFI0                23     34
006900000000     D*                -FLAG OPZ. ATTIVA (S/N)
007000000000     D  OPDATT                35     35
007100000000     D*                -FILLER
007200000000     D  OPDFI1                36     80
007300000000     D*                -DESCRIZIONE COMPLETA
007400000000     D  OPDDES                 1     19
007500000000     D*                -RIDEFINIZIONE RIGA
007600000000     D  OPDRIG                 1     80
007700000000     D*----------------
007800000000     D* RIDEFINIZIONE SCHIERA -FUD-
007900000000     D*----------------
008000000000     D FUDDS           DS
008100000000     D*                -DESCRIZIONE PARZIALE
008200000000     D  FUDDE1                 1      1
008300000000     D*                -FUNZIONE
008400000000     D  FUDFUN                 2      3
008500000000     D*                -DESCRIZIONE PARZIALE
008600000000     D  FUDDE2                 4     19
008700000000     D*                -FILLER
008800000000     D  FUDFI0                20     34
008900000000     D*                -FLAG FUNZ.ATTIVA (S/N)
009000000000     D  FUDATT                35     35
009100000000     D*                -FILLER
009200000000     D  FUDFI1                36     80
009300000000     D*                -DESCRIZIONE COMPLETA
009400000000     D  FUDDES                 1     19
009500000000     D*                -RIDEFINIZIONE RIGA
009600000000     D  FUDRIG                 1     80
009700000000     D*----------------
009800000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
009900000000     D*----------------
010000000000     D DSMSG         E DS
010100000000     D*----------------
010200000000     D* PARAMETRI PER LANCIO ALTRI PGM DELLA PROCEDURA
010300000000     D*----------------
010400000000     D DSUNIT        E DS
010500000000     D*----------------
010600000000     D* DS PER GESTIONE TASTI FUNZIONALI
010700000000     D*----------------
010800000000     D DSKEY         E DS
010900000000     D DEVDS1          DS
011000000000     D  KEY                  369    369
011100000000     D*----------------
011200000000     D* ARCHITETTURA
011300000000     D*----------------
011400000000     D KPJBA         E DS
011500020709
011600020709     d ddatiute      e ds
011700020709     d azuteds       e ds                  extname(azute00f)
011800020709     d tibs34ds      e ds                  inz
011900020710      *------------------------------------------------------------------------*
012000020710     Iunana000      01
012100020710     Iundot000      02
012200020710     Iunana02       03
012300020710     Iundot02       04
012400000000     C*------------------------------------------------------------------------*
012500000000     C* MAIN LINE
012600000000     C*------------------------------------------------------------------------*
012700000000     C*
012800000000     C* SE CONTROLLI PRELIMINARI NON OK VADO A FINE PGM
012900000000     C     ERRL00        IFNE      '1'                                          --- 1 -->
013000000000     C*
013100000000     C* CARICA IL SFL DAI PARAMETRI IN INPUT AL PROGRAMMA
013200000000     C                   EXSR      POSSFL
013300000000     C*
013400000000     C* CICLO FINO A CHE:
013500000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
013600000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
013700000000     C*  -FINE PGM PER ENTER SENZA SCELTA
013800000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
013900000000     C     F12L00        OREQ      '1'
014000020404     C**!!!FINL00        OREQ      '1'
014100000000     C*
014200000000     C* AZZERA FLAG DI ERROE
014300000000     C                   MOVEL     '0'           ERRL00
014400020717      * Se Profilo non OK devo uscire dal pgm
014500020717     c                   If        *In76
014600020717     c                   eval      f03l00 = *on
014700020717     c                   EndIf
014800000000     C*
014900000000     C* EMETTE SFL A VIDEO
015000000000     C                   EXSR      OUTSFL
015100000000     C*
015200000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI -ROUTINE FUNCON
015300000000     C                   EXSR      FUNCON
015400000000     C*
015500000000     C                   END                                                    <-- 2 ---
015600000000     C*
015700000000     C                   END                                                    <-- 1 --
015800000000     C*
015900000000     C                   MOVEL     DSUNIT        KPJBU
016000000000     C                   SETON                                        LR
016100000000     C*================================================================
016200000000     C* F03GES -GEST.TASTO FUNZ. F03 ===> FINE
016300000000     C*================================================================
016400000000     C     F03GES        BEGSR
016500000000     C*
016600000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PGM 00)
016700000000     C                   MOVEL     '1'           F03L00
016800000000     C*
016900000000     C                   ENDSR
017000000000     C*================================================================
017100000000     C* F05GES -GEST.TASTO FUNZ. F05 ===> RIVISUALIZZAZIONE
017200000000     C*================================================================
017300000000     C     F05GES        BEGSR
017400000000     C*
017500000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
017600000000     C                   EXSR      POSSFL
017700000000     C*
017800000000     C                   ENDSR
017900000000     C*================================================================
018000000000     C* F06GES -GEST.TASTO FUNZ. F06 ===> INSERIMENTO
018100000000     C*================================================================
018200000000     C     F06GES        BEGSR
018300000000     C*
018400000000     C* INSERIMENTO CONSENTITO SOLO IN SEDE
018500020709     c                   If        DutLpo <> 'S'
018600000000     C                   MOVEL     '1'           ERRL00
018700000000     C                   MOVEL     MSG(3)        DSMSMS
018800000000     C                   END
018900000000     C*
019000000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
019100000000     C*
019200000000     C* LANCIO IL PGM PER L'INSERIMENTO DEL RECORD (TNSDUDR)
019300000000     C                   MOVEL     'F06'         WOPZ1             3
019400000000     C                   MOVEL     'TNSDUDR'     WPGM              8
019500000000     C                   EXSR      LANPGM
019600000000     C*
019700000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
019800000000     C                   EXSR      POSSFL
019900000000     C*
020000000000     C                   END                                                    <-- 1 ---
020100000000     C*
020200000000     C                   ENDSR
020300000000     C*================================================================
020400000000     C* F12GES -GEST.TASTO FUNZ. F12 ===> RITORNO
020500000000     C*================================================================
020600000000     C     F12GES        BEGSR
020700000000     C*
020800000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12 (A LIVELLO PGM 00)
020900000000     C                   MOVEL     '1'           F12L00
021000000000     C*
021100000000     C                   ENDSR
021200000000     C*================================================================
021300000000     C* F17GES -GEST.TASTO FUNZ. F17 ===> INIZIO DATI
021400000000     C*================================================================
021500000000     C     F17GES        BEGSR
021600000000     C*
021700000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
021800000000     C                   Z-ADD     1             SFLNBR
021900000000     C*
022000000000     C                   ENDSR
022100000000     C*================================================================
022200000000     C* F18GES -GEST.TASTO FUNZ. F18 ===> FINE DATI
022300000000     C*================================================================
022400000000     C     F18GES        BEGSR
022500000000     C*
022600000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
022700000000     C                   Z-ADD     SFLULT        SFLNBR
022800000000     C*
022900000000     C                   ENDSR
023000000000     C*================================================================
023100000000     C* F21GES -GEST.TASTO FUNZ. F21 ===> STAMPA DIFFERENZE
023200000000     C*================================================================
023300000000     C     F21GES        BEGSR
023400000000     C*
023500000000     C* LANCIA PROGRAMMA DI STAMPA DELLE UNITA
023600000000     C                   MOVEL     DSUNIT        KPJBU
023700000000     C                   CALL      'TNSDUER'
023800000000     C                   PARM                    KPJBA
023900000000     C                   MOVEL     KPJBU         DSUNIT
024000000000     C*
024100000000     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
024200000000     C     DSUF03        IFEQ      '1'
024300000000     C                   MOVEL     '1'           F03L00
024400000000     C                   MOVEL     '1'           ERRL00
024500000000     C                   ENDIF
024600000000     C*
024700000000     C                   ENDSR
024800000000     C*================================================================
024900000000     C* F22GES -GEST.TASTO FUNZ. F22 ===> STAMPE
025000000000     C*================================================================
025100000000     C     F22GES        BEGSR
025200000000     C*
025300000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
025400000000     C                   Z-ADD     80            LENGH            15 5
025500000000     C                   CALL      'QCAEXEC'
025600000000     C                   PARM                    CMD(1)
025700000000     C                   PARM                    LENGH
025800000000     C*
025900000000     C                   ENDSR
026000000000     C*================================================================
026100000000     C* F23GES -GEST.TASTO FUNZ. F23 ===> ALTRE OPZIONI
026200000000     C*================================================================
026300000000     C     F23GES        BEGSR
026400000000     C*
026500000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
026600000000     C                   EXSR      OPZGES
026700000000     C*
026800000000     C                   ENDSR
026900000000     C*================================================================
027000000000     C* F24GES -GEST.TASTO FUNZ. F24 ===> ALTRI TASTI
027100000000     C*================================================================
027200000000     C     F24GES        BEGSR
027300000000     C*
027400000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
027500000000     C                   EXSR      FUNGES
027600000000     C*
027700000000     C                   ENDSR
027800000000     C*================================================================
027900000000     C* RUPGES -GEST.TASTO FUNZ. ROLLUP ===> PAGINA SUCCESSIVA
028000000000     C*================================================================
028100000000     C     RUPGES        BEGSR
028200000000     C*
028300000000     C* CARICA IL SFL DA FILE "FISICO"
028400000000     C                   EXSR      CARSFL
028500000000     C*
028600000000     C                   ENDSR
028700000000     C*================================================================
028800000000     C* HELGES - GEST.TASTO FUNZ. HELP ===> AIUTO
028900000000     C*================================================================
029000000000     C     HELGES        BEGSR
029100000000     C*
029200000000     C                   ENDSR
029300000000     C*================================================================
029400000000     C* ENTGES - GEST.TASTO FUNZ. ENTER ===> INVIO
029500000000     C*================================================================
029600000000     C     ENTGES        BEGSR
029700000000     C*
029800000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
029900000000     C                   EXSR      NEWPOS
030000000000     C*
030100000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
030200000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
030300000000     C*
030400000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
030500000000     C     SWPOSI        IFNE      0                                            --- 2 -->
030600000000     C                   EXSR      POSSFL
030700000000     C*
030800000000     C                   ELSE                                                   <-- 2 -->
030900000000     C*
031000000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
031100000000     C     SFLULT        IFNE      0                                            --- 3 -->
031200000000     C                   EXSR      ENTATT
031300000000     C*
031400000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
031500000000     C     SWMODI        IFNE      0                                            --- 4 -->
031600000000     C                   EXSR      POSSFL
031700000000     C                   END                                                    <-- 4 ---
031800000000     C*
031900000000     C                   END                                                    <-- 3 ---
032000000000     C                   END                                                    <-- 2 ---
032100000000     C                   END                                                    <-- 1 ---
032200000000     C*
032300000000     C                   ENDSR
032400000000     C*================================================================
032500000000     C*  O01GES - GESTIONE OPZIONE 1=SCELTA
032600000000     C*================================================================
032700000000     C     O01GES        BEGSR
032800000000     C                   ENDSR
032900000000     C*================================================================
033000000000     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
033100000000     C*================================================================
033200000000     C     O02GES        BEGSR
033300000000     C*
033400000000     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA MODIFICA
033500000000     C     VIDATB        IFEQ      'A'
033600000000     C                   MOVEL     '1'           ERRL00
033700000000     C                   MOVEL     MSG(4)        DSMSMS
033800000000     C                   END
033900000000     C*
034000000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
034100000000     C*
034200000000     C* LANCIO IL PGM PER LA MODIFICA DEL DETTAGLIO DELL'ANAGRAFICA
034300000000     C                   MOVEL     'O02'         WOPZ1
034400000000     C                   MOVEL     'TNSDUDR'     WPGM
034500000000     C                   EXSR      LANPGM
034600000000     C*
034700000000     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
034800000000     C                   Z-ADD     1             SWMODI
034900000000     C*
035000000000     C                   END                                                    <-- 1 ---
035100000000     C*
035200000000     C                   ENDSR
035300000000     C*================================================================
035400000000     C*  O03GES - GESTIONE OPZIONE 3=COPIA
035500000000     C*================================================================
035600000000     C     O03GES        BEGSR
035700000000     C*
035800000000     C                   ENDSR
035900000000     C*================================================================
036000000000     C*  O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE
036100000000     C*================================================================
036200000000     C     O04GES        BEGSR
036300000000     C*
036400000000     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
036500000000     C     VIDATB        IFEQ      'A'
036600000000     C                   MOVEL     '1'           ERRL00
036700000000     C                   MOVEL     MSG(4)        DSMSMS
036800000000     C                   END
036900000000     C*
037000000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
037100000000     C*
037200000000     C* LANCIO IL PGM DI GESTIONE DELLE UNITA
037300000000     C                   MOVEL     'O04'         WOPZ1
037400000000     C                   MOVEL     'TNSDUDR'     WPGM
037500000000     C                   EXSR      LANPGM
037600000000     C*
037700000000     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
037800000000     C                   Z-ADD     1             SWMODI
037900000000     C*
038000000000     C                   END                                                    <-- 1 ---
038100000000     C*
038200000000     C                   ENDSR
038300000000     C*================================================================
038400000000     C*  O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
038500000000     C*================================================================
038600000000     C     O05GES        BEGSR
038700000000     C*
038800000000     C* LANCIO ROUTINE PER LA VISUALIZZAZIONE DEL DETTAGLIO DELLA RIGA
038900000000     C                   MOVEL     'O05'         WOPZ1
039000000000     C                   MOVEL     'TNSDUDR'     WPGM
039100000000     C                   EXSR      LANPGM
039200000000     C*
039300000000     C                   ENDSR
039400000000     C*================================================================
039500000000     C*  O06GES - GESTIONE OPZIONE 6=STAMPA
039600000000     C*================================================================
039700000000     C     O06GES        BEGSR
039800000000     C                   ENDSR
039900000000     C*================================================================
040000000000     C*  O07GES - GESTIONE OPZIONE 7=RIPRISTINO
040100000000     C*================================================================
040200000000     C     O07GES        BEGSR
040300000000     C*
040400000000     C* SE IL RECORD NON E' ANNULLATO NON CONSENTO IL RIPRISTINO
040500000000     C     VIDATB        IFNE      'A'
040600000000     C                   MOVEL     '1'           ERRL00
040700000000     C                   MOVEL     MSG(5)        DSMSMS
040800000000     C                   END
040900000000     C*
041000000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
041100000000     C*
041200000000     C* LANCIO IL PGM PER IL RIPRISTINO DI UN ANNULLAMENTO
041300000000     C                   MOVEL     'O07'         WOPZ1
041400000000     C                   MOVEL     'TNSDUDR'     WPGM
041500000000     C                   EXSR      LANPGM
041600000000     C*
041700000000     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
041800000000     C                   Z-ADD     1             SWMODI
041900000000     C*
042000000000     C                   END                                                    <-- 1 ---
042100000000     C*
042200000000     C                   ENDSR
042300000000     C*================================================================
042400000000     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
042500000000     C*================================================================
042600000000     C     NEWPOS        BEGSR
042700000000     C*
042800000000     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAM.
042900000000     C*         1= RICHIESTO POSIZIONAM.
043000000000     C                   Z-ADD     0             SWPOSI            7 0
043100000000     C*
043200000000     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
043300000000     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
043400000000     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
043500000000     C                   Z-ADD     0             SWMODI            1 0
043600000000     C*
043700000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
043800020405     c                   if        vdptip <> deptip or vdpdot <> depdot or      --- 1 -->
043900020405     c                             vdpmag <> depmag
044000000000     C*
044100000000     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI DI PARTENZA
044200000000     C                   EXSR      CHKPAR
044300000000     C*
044400000000     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
044500000000     C     ERRL00        IFEQ      '0'                                          --- 2 -->
044600000000     C                   Z-ADD     1             SWPOSI
044700000000     C                   Z-ADD     VDPTIP        DEPTIP
044800020326     c                   eval      depdot = vdpdot
044900020405     c                   eval      depmag = vdpmag
045000000000     C                   END                                                    <-- 2 ---
045100000000     C*
045200000000     C                   END                                                    <-- 1 ---
045300000000     C*
045400000000     C                   ENDSR
045500000000     C*================================================================
045600000000     C* LETTURA RECORD FILE DA CARICARE
045700000000     C*================================================================
045800000000     C     LETFIL        BEGSR
045900020711
046000020711     c                   eval      *In01 = *off
046100020711     c                   eval      *In02 = *off
046200020711     c                   eval      *In03 = *off
046300020711     c                   eval      *In04 = *off
046400000000     C*
046500000000     C     WTIPRI        IFEQ      'TIP'
046600020710     C                   READ      unana01c                               99
046700000000     C                   END
046800020405
046900020405     c                   if        wtipri = 'MAG'
047000020710     c     kanmag        reade     unana02c                               99
047100020405     c                   endif
047200020711
047300020711     c                   If        Not *In99
047400020711
047500020711     c                   If        *In01 or *In03
047600020711     c                   Eval      SavTip = UnaTip
047700020711     c                   Eval      SavMag = UnaMag
047800020711     c                   EndIf
047900020711     c                   If        *In02 or *In04
048000020711     c                   Eval      SavTip = DotTip
048100020711     c                   Eval      SavMag = DotFil
048200020711     c                   EndIf
048300020711
048400020711
048500020711     c                   EndIf
048600020405
048700000000     C*
048800000000     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
048900000000     C     *IN99         IFEQ      '1'
049000000000     C                   Z-ADD     1             SFLEOF
049100000000     C                   END
049200000000     C*
049300000000     C                   ENDSR
049400000000     C*================================================================
049500000000     C* SETFIL - SETLL DEL FILE DA CARICARE
049600000000     C*================================================================
049700000000     C     SETFIL        BEGSR
049800020711
049900020711     c                   eval      *In01 = *off
050000020711     c                   eval      *In02 = *off
050100020711     c                   eval      *In03 = *off
050200020711     c                   eval      *In04 = *off
050300000000     C*
050400000000     C* POSIZIONAMENTO DEL FILE DA CARICARE E PRIMA LETTURA
050500000000     C     WTIPRI        IFEQ      'TIP'
050600020326     c                   eval      kantip = vdptip
050700020710     c     KaNtip        SETLL     unana01c                           99        *FINE-ARCHIV
050800020710     c  N99              READ      unana01c                               99
050900000000     C                   END
051000020405
051100020405     c                   if        wtipri = 'MAG'
051200020405     c                   move      vdpmag        kanmag
051300020710     c     kanmag        setll     unana02c                           99
051400020710     c  n99kanmag        reade     unana02c                               99
051500020405     c                   endif
051600020322
051700020322     c                   if        not *in99
051800020711
051900020711     c                   If        *In01 or *In03
052000020711     c                   If        WunTip = *Zeros
052100020711     c                   Eval      WunTip = UnaTip
052200020711     c                   EndIf
052300020711     c                   If        WunMag = *Zeros
052400020711     c                   Eval      WunMag = UnaMag
052500020711     c                   EndIf
052600020711     c                   Eval      SavTip = UnaTip
052700020711     c                   Eval      SavMag = UnaMag
052800020711     c                   EndIf
052900020711
053000020711     c                   If        *In02 or *In04
053100020711     c                   If        WunTip = *Zeros
053200020711     c                   Eval      WunTip = DotTip
053300020711     c                   EndIf
053400020711     c                   If        WunMag = *Zeros
053500020711     c                   Eval      WunMag = DotFil
053600020711     c                   EndIf
053700020711     c                   Eval      SavTip = DotTip
053800020711     c                   Eval      SavMag = DotFil
053900020711     c                   EndIf
054000020711
054100020322     c                   endif
054200020326
054300000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
054400000000     C     *IN99         IFEQ      '1'                                          --- 1 -->
054500000000     C                   Z-ADD     1             SFLEOF
054600000000     C                   END                                                    <-- 1 ---
054700000000     C*
054800000000     C                   ENDSR
054900020326      *================================================================
055000020326      * CTRSFL - CONTROLLO SE RECORD DA SCRIVERE NEL SUBFILE
055100020326      *================================================================
055200020326     C     CTRSFL        BEGSR
055300020326
055400020711     c                   If        VdpDot = 'S' and
055500020711     c                             WokUni = *On and WokDot = *On
055600020711     c                   Eval      WnoSfl = *On
055700020711     c                   Endif
055800020711     c                   goto      passa
055900020326      * controllo se richieste solo unità NON in dotazione
056000020403     c                   if        (vdpdot = 'S' and vidqta <> 0) or
056100020403     c                             (vdpdot = 'S' and viddif = 0  and
056200020403     c                              vidqta = 0)
056300020326     c                   eval      wnosfl = *on
056400020326     c                   endif
056500020711     c     passa         tag
056600020326
056700020326     C                   ENDSR
056800000000     C*================================================================
056900000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
057000000000     C*================================================================
057100000000     C     IRESFL        BEGSR
057200000000     C*
057300000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
057400020322     C                   Z-ADD     wunTIP        VIDTIP
057500020322     C                   Z-ADD     wunmag        VIDFIL
057600020326
057700020322
057800020711     C                   movel     Savatb        VIDatb
057900020711     C                   Z-ADD     SavQTA        VIDQTA
058000950124     C*
058100950124     C* CALCOLA LA DIFFERENZA FRA LE QUANTITA' IN DOTAZIONE E QUELLE IN ESSERE
058200950124     C                   Z-ADD     *ZEROS        VIDDIF
058300020711     C     TOTFIL        SUB       SavQta        VIDDIF
058400000000     C* DECODIFICHE
058500000000     C                   Z-ADD     1             J
058600000000     C                   SETOFF                                       28
058700020326     C     wuntip        LOOKUP    UNI(J)                                 28
058800000000     C     *IN28         IFEQ      *ON
058900000000     C                   MOVEL     DUNI(J)       VIDTID
059000000000     C                   ELSE
059100000000     C                   MOVEL     *ALL'*'       VIDTID
059200000000     C                   ENDIF
059300000000     C*
059400000000     C                   Z-ADD     1             J
059500000000     C                   SETOFF                                       28
059600020322     C     wunmag        LOOKUP    FIL(J)                                 28
059700000000     C     *IN28         IFEQ      *ON
059800000000     C                   MOVEL     DFIL(J)       VIDFID
059900000000     C                   ELSE
060000000000     C                   MOVEL     *ALL'*'       VIDFID
060100000000     C                   ENDIF
060200000000     C*
060300000000     C                   ENDSR
060400000000     C*================================================================
060500000000     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
060600000000     C*================================================================
060700000000     C     CHKPAR        BEGSR
060800020405
060900020405      * IMPOSTO FLAG EVIDENZAZIONE TIPO DI RICERCA
061000020405     c                   eval       *in90 = *off                                x tipo
061100020405     c                   eval       *in91 = *off                                x magazzino
061200000000     C*
061300000000     C* IMPOSTO FLAG EVIDENZAZIONE ERRORI SUI PARAMETRI
061400020405     C                   SETOFF                                       80        tipo
061500020405     c                   eval       *in81 = *off                                magazzino
061600000000     C*
061700000000     C* IMPOSTO A BLANK LE DECODIFICHE
061800000000     C                   MOVEL     *BLANKS       VDPTID
061900000000     C*
062000000000     C* SE TUTTI I PARAMETRI SONO UGUALI A BLANK O ZERO, UTILIZZO CHIAVE X TIP
062100020405     c                   if        vdptip = *zeros and vdpmag = *blanks
062200000000     C                   MOVEL     'TIP'         WTIPRI            3
062300000000     C                   SETON                                        90
062400000000     C*
062500000000X   1C                   ELSE
062600000000     C*
062700000000     C                   MOVEL     *BLANKS       WTIPRI
062800000000     C* ===> ACCETTO SOLO UN PARAMETRO DI INIZIO RICERCA
062900000000     C* CONTROLLO E DECODIFICO IL TIPO UNITA
063000000000IF  2C     VDPTIP        IFNE      *ZEROS
063100000000     C                   Z-ADD     1             J
063200000000     C                   SETOFF                                           28
063300000000     C     VDPTIP        LOOKUP    UNI(J)                                 28
063400000000IF  3C     *IN28         IFEQ      *OFF
063500000000     C                   MOVEL     *ALL'*'       VDPTID
063600000000     C                   MOVEL     '1'           ERRL00
063700000000     C                   MOVEL     MSG(7)        DSMSMS
063800000000     C                   SETON                                        80
063900000000     C                   ELSE
064000000000     C                   MOVEL     DUNI(J)       VDPTID
064100000000     C                   MOVEL     'TIP'         WTIPRI
064200020405     C                   SETON                                        90
064300000000E   3C                   END
064400000000E   2C                   END
064500020405
064600020405      * Magazzino / tipo unità
064700020405     c                   if        vdpmag <> *blanks and errl00 = *off          --- 1 -->
064800020405     c                   if        wtipri <> *blanks                            --- 2 -->
064900020405     c                   eval      errl00 = *on
065000020405     c                   eval      dsmsms = msg(6)
065100020405     c                   else                                                   <-- 2 -->
065200020405      * controllo magazzino
065300020405     c                   if        vdpmag <> *blanks
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
066800020405     C*
066900020405     C* CONTROLLO SE E' STATO INSERITO '?'
067000020405     C     '?'           SCAN      VDPMAG                                 99
067100020405     C*
067200020405     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
067300020405     C     *IN99         IFEQ      '1'                                          --- 1 -->
067400020405     C                   MOVEL     *BLANKS       §COD1             3
067500020405     C                   MOVE      *BLANKS       §DES1
067600020405     C                   CALL      'TNSD20R'
067700020405     C                   PARM                    §COD1
067800020405     C                   PARM                    §TIP              1
067900020405     C                   PARM                    §DES1            25
068000020405     C                   MOVEL     §COD1         VDPMAG
068100020405     C*
068200020405     C                   END                                                    <-- 1 ---
068300020405     C*
068400020405     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
068500020405     C                   Z-ADD     1             J
068600020405     C                   SETOFF                                           28
068700020405     C                   MOVE      VDPMAG        KORFIL
068800020405     C     KORFIL        LOOKUP    FIL(J)                                 28
068900020405IF  3C     *IN28         IFEQ      *OFF
069000020405     C                   MOVEL     '1'           ERRL00
069100020405     C                   MOVEL     MSG(8)        DSMSMS
069200020405     C                   SETON                                        81
069300020405     C                   END
069400020405     C*
069500020405     C                   ENDSR
069600000000     C*================================================================
069700000000     C* LANPGM - LANCIA UN PROGRAMMA
069800000000     C*================================================================
069900000000     C     LANPGM        BEGSR
070000000000     C*
070100000000     C* IMPOSTO PARAMETRI PER IL PGM
070200000000     C                   MOVEL     '0'           DSUF03                         PREMUTO F03
070300000000     C                   MOVEL     '0'           DSUERR                         '1'=ERRORE
070400000000     C                   MOVEL     *BLANKS       DSUMSG                         MESSAGGIO
070500000000     C                   MOVE      WOPZ1         DSUOP1                         *OPZIONE LIV.PGM
070600000000     C                   Z-ADD     VIDTIP        DSUTIP                         TIPO
070700000000     C                   Z-ADD     VIDFIL        DSUMAG                         MAGAZZINO
070800000000     C*
070900000000     C* LANCIO PROGRAMMA
071000000000     C                   MOVEL     DSUNIT        KPJBU
071100000000     C                   CALL      WPGM
071200000000     C                   PARM                    KPJBA
071300000000     C                   MOVEL     KPJBU         DSUNIT
071400000000     C*
071500000000     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
071600000000     C     DSUF03        IFEQ      '1'
071700000000     C                   MOVEL     '1'           F03L00
071800000000     C                   MOVEL     '1'           ERRL00
071900000000     C                   ELSE
072000000000     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
072100000000     C                   MOVEL     DSUMSG        DSMSMS
072200000000     C                   END
072300000000     C*
072400000000     C                   ENDSR
072500000000     C*================================================================
072600000000     C* CHKFOP - CONTROLLA TIPO GESTIONE E IMPOSTA FUNZ/OPZ
072700000000     C*================================================================
072800000000     C     CHKFOP        BEGSR
072900000000     C*
073000000000     C* CONTROLLO CHE VI SIA UNA OPZIONE DI PROCEDURA VALIDA IN INPUT
073100000000     C* ALTRIMENTI FORZO INTERROGAZIONE
073200000000     C     DSUOP0        IFNE      ' 2'                                         *GESTIONE
073300000000     C     DSUOP0        ANDNE     ' 5'                                         *INTERROGAZIONE
073400000000     C                   MOVE      ' 5'          DSUOP0
073500000000     C                   END
073600000000     C*
073700000000     C* SE L'UTENTE E' ENTRATO IN GESTIONE LO ABILITO A TUTTE LE FUNZIONI
073800000000     C     DSUOP0        IFEQ      ' 2'
073900000000     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
074000000000     C                   MOVEL     'S'           OPDATT
074100000000     C                   MOVEL     OPDRIG        OPD(2)
074200000000     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
074300000000     C                   MOVEL     'S'           OPDATT
074400000000     C                   MOVEL     OPDRIG        OPD(4)
074500000000     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
074600000000     C                   MOVEL     'S'           OPDATT
074700000000     C                   MOVEL     OPDRIG        OPD(5)
074800000000     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
074900000000     C                   MOVEL     'S'           OPDATT
075000000000     C                   MOVEL     OPDRIG        OPD(7)
075100000000     C                   MOVEL     OPD(8)        OPDRIG                         *GEST.MOVIMENTI
075200000000     C                   MOVEL     'S'           OPDATT
075300000000     C                   MOVEL     OPDRIG        OPD(8)
075400000000     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
075500000000     C                   MOVEL     'S'           FUDATT
075600000000     C                   MOVEL     FUDRIG        FUD(6)
075700000000     C                   END
075800000000     C*
075900000000     C* SE IL PGM E' STATO LANCIATO IN INTERROGAZIONE ABILITO SOLO QUELLA
076000000000     C     DSUOP0        IFEQ      ' 5'
076100000000     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
076200000000     C                   MOVEL     'S'           OPDATT
076300000000     C                   MOVEL     OPDRIG        OPD(5)
076400000000     C                   MOVEL     OPD(8)        OPDRIG                         *GEST.MOVIMENTI
076500000000     C                   MOVEL     'S'           OPDATT
076600000000     C                   MOVEL     OPDRIG        OPD(8)
076700000000     C                   END
076800000000     C*
076900000000     C* LANCIO ROUTINE PER VERIFICARE SE
077000000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
077100000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
077200020709
077300020709     c     *dtaara       define    §azute        azuteds
077400020709     c     *dtaara       define    §datiute      ddatiute
077500020709
077600020709     c                   in(E)     *dtaara
077700020709     c                   if        %Error or rsut = *blanks
077800020709     c                   clear                   tibs34ds
077900020709     c                   call      'TIBS34R'
078000020709     c                   parm                    Tibs34Ds
078100020709     c                   in        *dtaara
078200020709     c                   endif
078300020717
078400020717      * Controllo il profilo utente
078500020717     c                   ExSr      CtrUte
078600000000     C*
078700000000     C* SE SONO IN FILIALE NON CONCEDO CANCELLAZIONE,INSERIMENTO,RIPRISTINO
078800020709     c                   If        DutLpo <> 'S'
078900000000     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
079000000000     C                   MOVEL     'N'           OPDATT
079100000000     C                   MOVEL     OPDRIG        OPD(4)
079200000000     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
079300000000     C                   MOVEL     'N'           OPDATT
079400000000     C                   MOVEL     OPDRIG        OPD(7)
079500000000     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
079600000000     C                   MOVEL     'N'           FUDATT
079700000000     C                   MOVEL     FUDRIG        FUD(6)
079800000000     C                   END
079900000000     C*
080000000000     C* IMPOSTO INDICE PER EMISSIONE OPZIONI A VIDEO
080100000000     C                   Z-ADD     0             OPZGRU            5 0
080200000000     C*
080300000000     C* GESTIONE OPZIONI A VIDEO
080400000000     C                   EXSR      OPZGES
080500000000     C*
080600000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
080700000000     C                   Z-ADD     0             FUNGRU            5 0
080800000000     C*
080900000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
081000000000     C                   EXSR      FUNGES
081100000000     C*
081200000000     C                   ENDSR
081300020717      *------------------------------------------------------------------------*
081400020717      *  CTRUTE - ROUTINE CONTROLLO UTENTE
081500020717      *------------------------------------------------------------------------*
081600020717     c     CtrUte        BegSr
081700020717
081800020717     c                   Eval      *In76 = *Off
081900020717
082000020717      * Controllo se il profilo è ok da tibs34
082100020717     c                   If        DutErr = 'E'
082200020717     c                   Eval      DsmSms = DutMsg
082300020717     c                   Eval      *In76 = *On
082400020717     c                   EndIf
082500020717
082600020717      * Controllo se il profilo di sede
082700020717     c                   If        DutLpo <> 'S'
082800020717     c                   Eval      DsmSms = Msg(9)
082900020717     c                   Eval      *In76 = *On
083000020717     c                   EndIf
083100020717
083200020717     c                   endSr
083300000000     C*================================================================
083400000000     C* *INZSR - ROUTINE INIZIALE
083500000000     C*================================================================
083600000000     C     *INZSR        BEGSR
083700000000     C*
083800000000     C* RICEVIMENTO PARAMETRI DALL'AZIONE (' 2'=GEST.,' 5'=INTERR.)
083900000000     C     *ENTRY        PLIST
084000000000     C                   PARM                    KPJBA
084100000000     C                   MOVEL     KPJBU         DSUNIT
084200000000     C*
084300000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
084400000000     C     XMSGPA        PLIST
084500000000     C                   PARM                    DSMSMS
084600000000     C                   PARM                    DSMSPG
084700000000     C                   PARM                    DSMSKE
084800000000     C                   PARM                    DSMSER
084900000000     C* DEFINIZIONE CAMPI
085000000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
085100000000     C     *LIKE         DEFINE    DOTTIP        KDOTIP                         *UNDOT00F
085200950125     C     *LIKE         DEFINE    UNAMAG        KANMAG                         *UNANA00F
085300950125     C     *LIKE         DEFINE    UNATIP        KANTIP
085400000000     C*
085500000000     C* CHIAVE LETTURA UNDOT01L - PARZIALE
085600000000     C     KUNDOT        KLIST
085700020322     C                   KFLD                    KDOTIP
085800020322     C                   KFLD                    KDOfil
085900950124     C*
086000020322     C* CHIAVE LETTURA UNANA09L - PARZIALE
086100950124     C     KEYANA        KLIST
086200020322     C                   KFLD                    KANTIP
086300950124     C                   KFLD                    KANMAG
086400000000     C*
086500020528     C* CHIAVE LETTURA AZORG21l/azorg01l - COMPLETA
086600000000     C     KEYORG        KLIST
086700000000     C                   KFLD                    KORFIL
086800000000     C*
086900000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
087000000000     C                   CALL      'XKEY'
087100000000     C                   PARM                    DSKEY
087200000000     C*
087300000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
087400000000     C     *LIKE         DEFINE    SFLNBR        SFLPAG
087500000000     C     *LIKE         DEFINE    SFLPAG        WFLPAG
087600000000     C     *LIKE         DEFINE    SFLNBR        SFLEOF
087700000000     C     *LIKE         DEFINE    SFLNBR        SFLERR
087800000000     C     *LIKE         DEFINE    SFLNBR        SFLULT
087900000000     C     *LIKE         DEFINE    SFLNBR        WFLULT
088000000000     C     *LIKE         DEFINE    SFLNBR        MSGULT
088100000000     C     *LIKE         DEFINE    SFLNBR        SFLRIG
088200000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
088300000000     C     *LIKE         DEFINE    VDPTIP        DEPTIP
088400000000     C*
088500000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
088600000000     C*   -NOME PGM CHIAMANTE
088700000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
088800000000     C                   MOVEL     'TNSDUCR'     DSMSPG
088900000000     C                   MOVEL     'TNSDUCR'     MSGQUE
089000000000     C                   MOVEL     *BLANKS       MSGKEY
089100000000     C                   MOVEL     *BLANKS       DSMSMS
089200000000     C                   MOVEL     *BLANKS       DSMSKE
089300000000     C                   Z-ADD     0             DSMSER
089400000000     C*
089500000000     C* INIZIALIZZO SWITCH DI LIVELLO 0 (PGM)
089600000000     C                   MOVEL     '0'           F12L00            1
089700000000     C                   MOVEL     '0'           F03L00            1
089800000000     C                   MOVEL     '0'           FINL00            1
089900000000     C                   MOVEL     '0'           ERRL00            1
090000000000     C                   MOVEL     '0'           FLFINE            1
090100950124     C                   MOVEL     '0'           $FIN2             1
090200000000     C*
090300000000     C* IMPOSTO DEPOSITI PER CONFRONTI
090400000000     C                   MOVEL     *BLANKS       DEPTIP
090500020405     c                   clear                   depdot
090600020405     c                   clear                   depmag
090700020322     c                   clear                   wuntip
090800020322     c                   clear                   wunmag
090900000000     C*
091000000000     C* IMPOSTO TIPO DI RICERCA DI DEFAULT --> 'TIP'
091100000000     C                   MOVEL     'TIP'         WTIPRI
091200000000     C                   SETON                                        90
091300000000     C*
091400000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
091500000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
091600020405     C                   Z-ADD     11            SFLPAG
091700000000     C*
091800000000     C* IMPOSTO LE OPZIONI E LE FUNZIONI A VIDEO
091900000000     C                   EXSR      CHKFOP
092000000000     C*
092100000000     C* IMPOSTO INTERROGAZIONE/GESTIONE NELLA TESTATA DELLA VIDEATA
092200000000     C     DSUOP0        IFEQ      ' 2'
092300000000     C                   MOVEL     '    GEST'    VIDING
092400000000     C                   MOVE      'IONE    '    VIDING
092500000000     C                   ELSE
092600000000     C                   MOVEL     ' INTERRO'    VIDING
092700000000     C                   MOVE      'GAZIONE '    VIDING
092800000000     C                   END
092900000000     C*
093000000000     C* CARICA ORGANIGRAMMA
093100000000     C                   EXSR      CARORG
093200000000     C*
093300000000     C* CARICA TABELLE
093400000000     C                   EXSR      CARTAB
093500000000     C*
093600000000     C                   ENDSR
093700000000     C*================================================================
093800000000     C* CARORG - CARICA ORGANIGRAMMA
093900000000     C*================================================================
094000000000     C     CARORG        BEGSR
094100000000     C*
094200000000     C                   Z-ADD     0             J                 3 0          *INDICE SCHIERA
094300000000     C                   Z-ADD     *ZEROS        KORFIL
094400020528     C     KEYORG        SETLL     AZORG21l                           99        *FINE FILE
094500020528     C  N99              READ      AZORG21l                               99
094600000000     C*
094700000000     C     *IN99         DOWEQ     *OFF
094800000000     C                   ADD       1             J
094900000000     C                   Z-ADD     ORGFIL        FIL(J)
095000000000     C                   MOVEL     ORGDES        DFIL(J)
095100000000     C*
095200020528     C                   READ      AZORG21l                               99
095300000000     C                   ENDDO
095400020528
095500020528     c     keyorg        setll     azorg01l
095600020528     c                   do        *hival
095700020528     c                   read      azorg01l
095800020528     c                   if        %eof(azorg01l)
095900020528     c                   leave
096000020528     c                   endif
096100020528     c                   z-add     1             j
096200020528     c     orgfil        lookup    fil(j)                                 99
096300020528     c                   if        not *in99
096400020528     c                   z-add     1             j
096500020528     c     *zeros        lookup    fil(j)                                 99
096600020528     c                   if        *in99
096700020528     c                   movel     orgfil        fil(j)
096800020528     c                   movel     orgdes        dfil(j)
096900020528     c                   endif
097000020528     c                   endif
097100020528     c                   enddo
097200000000     C*
097300000000     C                   ENDSR
097400000000     C*================================================================
097500000000     C* CARTAB - CARICA TABELLE
097600000000     C*================================================================
097700000000     C     CARTAB        BEGSR
097800000000     C*--------------------
097900000000     C* TIPI UNITA'
098000000000     C*--------------------
098100000000     C                   Z-ADD     0             J                 3 0          *INDICE SCHIERA
098200020322     C                   MOVEL     'UNT'         tbecod
098300020322     C     tbecod        SETLL     tntbe01l                           99        *FINE FILE
098400020322     C  N99tbecod        READE     tntbe01l                               99
098500000000     C*
098600000000     C     *IN99         DOWEQ     *OFF
098700020322     C     TBeatb        IFEQ      *BLANKS
098800000000     C                   ADD       1             J
098900020322     C                   MOVEL     TBeke1        UNI(J)
099000020322     C                   MOVEL     tbeuni        DUNI(J)
099100000000     C                   ENDIF
099200000000     C*
099300020322     C  N99tbecod        READE     tntbe01l                               99
099400000000     C                   ENDDO
099500000000     C*
099600000000     C                   ENDSR
099700000000     C*================================================================
099800000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
099900000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
100000000000     C* LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
100100000000     C* STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
100200000000     C* NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
100300000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
100400000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
100500000000     C*================================================================
100600000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA
100700000000     C*          CARICARE
100800000000     C*================================================================
100900000000     C     POSSFL        BEGSR
101000000000     C*
101100000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
101200000000     C                   EXSR      CLRSFL
101300000000     C*
101400000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
101500000000     C                   EXSR      SETFIL
101600000000     C*
101700000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
101800000000     C     SFLEOF        IFEQ      0
101900000000     C                   EXSR      CARSFL
102000000000     C                   END
102100000000     C*
102200000000     C                   ENDSR
102300000000     C*================================================================
102400000000     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
102500000000     C*================================================================
102600000000     C     ENTATT        BEGSR
102700000000     C*
102800000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
102900000000     C                   Z-ADD     0             OPZOK             1 0
103000000000     C*
103100000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
103200000000     C                   Z-ADD     0             SFLVAR            5 0
103300000000     C*
103400000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
103500000000     C                   Z-ADD     0             SFLERR
103600000000     C*
103700000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
103800000000     C                   Z-ADD     SFLULT        WFLULT
103900000000     C*
104000000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
104100000000     C                   READC     SDUCD05                                98
104200000000     C     *IN98         DOWEQ     '0'
104300000000     C*
104400000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
104500000000     C                   ADD       1             SFLVAR
104600000000     C*
104700000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
104800000000     C     OPZOK         IFEQ      0
104900000000     C*
105000000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
105100000000     C     OPZKEY        IFNE      *BLANKS
105200000000     C                   EXSR      OPZCON
105300000000     C                   END
105400000000     C*
105500000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
105600000000     C*            2. PULIZIA SCELTA IN SFL
105700000000     C     OPZOK         IFEQ      0
105800000000     C                   MOVEL     '0'           *IN41
105900000000     C                   MOVEL     *BLANKS       OPZKEY
106000000000     C                   ELSE
106100000000     C*
106200000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
106300000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
106400000000     C                   MOVEL     '1'           *IN41
106500000000     C                   Z-ADD     SFLULT        SFLERR
106600000000     C                   END
106700000000     C                   ELSE
106800000000     C*
106900000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
107000000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
107100000000     C                   MOVEL     '1'           *IN41
107200000000     C                   END                                                    OPZOK=0
107300000000     C*
107400000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
107500000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
107600000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
107700000000     C     OPZOK         IFNE      0
107800000000     C                   Z-ADD     SFLERR        SFLNBR
107900000000     C                   ELSE
108000000000     C                   Z-ADD     SFLULT        SFLNBR
108100000000     C                   END
108200000000     C*
108300000000     C* AGGIORNO RECORD SFL
108400000000     C                   UPDATE    SDUCD05
108500000000     C*
108600000000     C                   READC     SDUCD05                                98
108700000000     C                   END                                                    DO WHILE
108800000000     C*
108900000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
109000000000     C                   Z-ADD     WFLULT        SFLULT
109100000000     C*
109200000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
109300000000     C     SFLVAR        IFEQ      0
109400000000     C                   MOVEL     '1'           FINL00
109500000000     C                   END
109600000000     C*
109700000000     C                   ENDSR
109800000000     C*================================================================
109900000000     C* ELIMINAZIONE CONTENUTO SFL
110000000000     C*================================================================
110100000000     C     CLRSFL        BEGSR
110200000000     C*
110300000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
110400000000     C* (0=NON FINE FILE  1=FINE FILE)
110500000000     C                   Z-ADD     0             SFLEOF
110600000000     C*
110700000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
110800000000     C                   Z-ADD     0             SFLULT
110900000000     C*
111000000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
111100000000     C                   MOVEL     '1'           *IN40
111200000000     C                   WRITE     SDUCD06
111300000000     C                   MOVEL     '0'           *IN40
111400000000     C*
111500000000     C                   ENDSR
111600000000     C*================================================================
111700000000     C* CARSFL - CARICAMENTO SFL
111800000000     C*================================================================
111900000000     C     CARSFL        BEGSR
112000000000     C*
112100000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
112200000000     C     SFLULT        IFEQ      0
112300020322     c                   clear                   totfil
112400020322     c                   clear                   wuntip
112500020322     c                   clear                   wunmag
112600020711     c                   Clear                   SavTip
112700020711     c                   Clear                   SavMag
112800020711     c                   Clear                   SavAtb
112900020711     c                   Clear                   SavQta
113000000000     C                   EXSR      SETFIL
113100000000     C                   END
113200000000     C*
113300000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
113400000000     C                   Z-ADD     SFLULT        WFLULT
113500000000     C*
113600000000     C* CICLO:
113700000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
113800000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
113900000000     C                   Z-ADD     1             SFLRIG
114000000000     C     SFLRIG        DOWLE     SFLPAG
114100000000     C     SFLEOF        ANDEQ     0
114200020326
114300020326     c                   eval      wnosfl = *off
114400020326
114500000000     C*
114600000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
114700000000     C                   MOVEL     *BLANKS       OPZKEY
114800020326
114900020322      * emetto solo a rottura di magazzino
115000020710     c**!!!              if        unamag <> wunmag or unatip <> wuntip
115100020711
115200020711if  2c                   If        SavMag <> WunMag or SavTip <> WunTip
115300020711if  3c                   if        wunmag > *zeros
115400000000     C*
115500000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
115600000000     C                   EXSR      IRESFL
115700020326
115800020322     C                   Z-ADD     *ZEROS        TOTFIL            7 0
115900020711     c                   Clear                   SavAtb
116000020711     c                   Clear                   SavQta
116100020711
116200020711     c                   If        *In01 or *In03
116300020711     c                   eval      wunmag = unamag
116400020711     c                   eval      wuntip = unatip
116500020711     c                   EndIf
116600020711     c                   If        *In02 or *In04
116700020711     c                   eval      wunmag = dotfil
116800020711     c                   eval      wuntip = dottip
116900020711     c                   EndIf
117000020326
117100020326      * controllo se devo scrivere il record nel subfile
117200020326     c                   exsr      ctrsfl
117300020711     c                   Eval      WokUni = *Off
117400020711     c                   Eval      WokDot = *Off
117500020326
117600020711if  4c                   if        wnosfl = *off
117700000000     C*
117800000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
117900000000     C                   ADD       1             SFLULT
118000000000     C*
118100000000     C* SCRITTURA RECORD SFL
118200000000     C                   WRITE     SDUCD05
118300000000     C                   ADD       1             SFLRIG
118400020711e   4c                   endif
118500020322
118600020711     c                   If        *In01 or *In03
118700020322     C                   ADD       1             TOTFIL
118800020711     c                   Eval      WokUni = *On
118900020711     c                   EndIf
119000020711e   3c                   EndIf
119100020711     c                   If        *In02 or *In04
119200020711     C                   Eval      SavAtb = DotAtb
119300020711     C                   Eval      SavQta = DotQta
119400020711     c                   Eval      WokDot = *On
119500020711     c                   EndIf
119600020322
119700020322      * conto le unità presenti in magazzino
119800020711x   2c                   else
119900020711     c                   If        *In01 or *In03
120000020711     C                   ADD       1             TOTFIL
120100020711     c                   Eval      WokUni = *On
120200020711     c                   EndIf
120300020711     c                   If        *In02 or *In04
120400020711     C                   Eval      SavAtb = DotAtb
120500020711     C                   Eval      SavQta = DotQta
120600020711     c                   Eval      WokDot = *On
120700020711     c                   EndIf
120800020711e   2c                   EndIf
120900000000     C*
121000000000     C* LETTURA FILE DA CARICARE
121100000000     C                   EXSR      LETFIL
121200000000     C                   END                                                    DO WHILE
121300020322
121400020322      * emetto l'ultimo record del file
121500020322     c                   if        wunmag > *zeros and sfleof = 1
121600020711     c                             and not *in42
121700020322     C*
121800020322     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
121900020322     C                   EXSR      IRESFL
122000020326
122100020326      * controllo se devo scrivere il record nel subfile
122200020326     c                   exsr      ctrsfl
122300020326
122400020326     c                   if        wnosfl = *off
122500020322     C*
122600020322     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
122700020322     C                   ADD       1             SFLULT
122800020322     C*
122900020322     C* SCRITTURA RECORD SFL
123000020322     C                   WRITE     SDUCD05
123100020322     C                   ADD       1             SFLRIG
123200020326     c                   endif
123300020322     c                   endif
123400000000     C*
123500000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
123600000000     C     WFLULT        IFNE      SFLULT
123700000000     C                   Z-ADD     WFLULT        SFLNBR
123800000000     C                   ADD       1             SFLNBR
123900000000     C                   END
124000000000     C*
124100000000     C                   ENDSR
124200000000     C*================================================================
124300000000     C* EMISSIONE SUBFILE A VIDEO
124400000000     C*================================================================
124500000000     C     OUTSFL        BEGSR
124600000000     C*
124700000000     C* WRITE RECORD TESTATA VIDEO
124800000000     C                   WRITE     SDUCD01
124900000000     C*
125000000000     C* VISUALIZZAZIONE MESSAGGIO
125100000000     C                   EXSR      SNDMSG
125200000000     C*
125300000000     C* WRITE RECORD OPZIONI VIDEO
125400000000     C                   WRITE     SDUCD02
125500000000     C*
125600000000     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
125700000000     C                   WRITE     SDUCD03
125800000000     C*
125900000000     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
126000000000     C                   WRITE     SDUCD04
126100000000     C*
126200000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
126300000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
126400000000     C     SFLEOF        IFEQ      0
126500000000     C                   MOVEL     '0'           *IN42
126600000000     C                   ELSE
126700000000     C*
126800000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
126900000000     C                   MOVEL     '1'           *IN42
127000000000     C                   END
127100000000     C*
127200000000     C* WRITE RECORD TASTI FUNZIONALI
127300000000     C                   WRITE     SDUCD09
127400000000     C*
127500000000     C* CASO DI SFL VUOTO
127600000000     C     SFLULT        IFLE      0
127700000000     C*
127800000000     C*    - WRITE RECORD VUOTO
127900000000     C                   WRITE     SDUCD0A
128000000000     C                   READ      SDUCD0A                                99
128100000000     C*
128200000000     C                   ELSE
128300000000     C*
128400000000     C* CASO DI SFL CON RECORDS
128500000000     C*
128600000000     C*    - WRITE RECORD CONTROLLO SFL
128700000000     C                   WRITE     SDUCD06
128800000000     C                   READ      SDUCD06                                99
128900000000     C                   END
129000000000     C*
129100000000     C* READ RECORD PER POSIZIONAMEMTO VIDEO
129200000000     C                   READ      SDUCD03                                99
129300000000     C*
129400000000     C                   ENDSR
129500000000     C*================================================================
129600000000     C* SNDMSG - MANDA MESSAGGIO
129700000000     C*================================================================
129800000000     C     SNDMSG        BEGSR
129900000000     C*
130000000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
130100000000     C                   MOVEL     '1'           *IN40
130200000000     C                   WRITE     SDUCDMC
130300000000     C                   MOVEL     '0'           *IN40
130400000000     C*
130500000000     C* VISUALIZZAZIONE MESSAGGIO
130600000000     C                   Z-ADD     0             MSGULT
130700000000     C                   CALL      'XMSG'        XMSGPA
130800000000     C     DSMSER        IFEQ      0
130900000000     C                   MOVEL     DSMSKE        MSGKEY
131000000000     C                   END
131100000000     C                   Z-ADD     1             MSGULT
131200000000     C                   WRITE     SDUCDMS                                      SFL MESS
131300000000     C                   WRITE     SDUCDMC                                      CONTR MESS
131400000000     C*
131500000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
131600000000     C                   MOVEL     *BLANKS       DSMSMS
131700000000     C*
131800000000     C                   ENDSR
131900000000     C*================================================================
132000000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
132100000000     C*================================================================
132200000000     C     FUNCON        BEGSR
132300000000     C*
132400000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
132500000000     C                   EXSR      FUNVAR
132600000000     C*
132700000000     C* PULISCO CAMPI DI LAVORO
132800000000     C                   Z-ADD     0             I                 5 0
132900000000     C                   Z-ADD     0             W                 5 0
133000000000     C*
133100000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
133200000000     C     FUNKEY        IFNE      *BLANKS
133300000000     C     1             DO        24            I
133400000000     C                   MOVEL     FUD(I)        FUDRIG
133500000000     C*
133600000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
133700000000     C     FUNKEY        IFEQ      FUDFUN
133800000000     C     FUDATT        ANDEQ     'S'
133900000000     C                   Z-ADD     I             W
134000000000     C                   END
134100000000     C                   END                                                    DO WHILE
134200000000     C*
134300000000     C* FUNZIONE NON ATTIVA (ERRORE)
134400000000     C     W             IFEQ      0
134500000000     C*
134600000000     C* EMISSIONE MESSAGGIO DI ERRORE
134700000000     C                   MOVEL     MSG(2)        DSMSMS
134800000000     C                   END                                                    W=0
134900000000     C                   END                                                    FUNKEY NE *BLANKS
135000000000     C*
135100000000     C* FUNZIONE ATTIVA (PROCEDI ...)
135200000000     C     W             IFNE      0
135300000000     C     FUNKEY        OREQ      *BLANKS
135400000000     C*
135500000000     C     FUNKEY        IFEQ      *BLANKS
135600000000     C                   EXSR      ENTGES
135700000000     C                   END
135800000000     C     FUNKEY        IFEQ      '03'
135900000000     C                   EXSR      F03GES
136000000000     C                   END
136100000000     C     FUNKEY        IFEQ      '05'
136200000000     C                   EXSR      F05GES
136300000000     C                   END
136400000000     C     FUNKEY        IFEQ      '06'
136500000000     C                   EXSR      F06GES
136600000000     C                   END
136700000000     C     FUNKEY        IFEQ      '12'
136800000000     C                   EXSR      F12GES
136900000000     C                   END
137000000000     C     FUNKEY        IFEQ      '17'
137100000000     C                   EXSR      F17GES
137200000000     C                   END
137300000000     C     FUNKEY        IFEQ      '18'
137400000000     C                   EXSR      F18GES
137500000000     C                   END
137600000000     C     FUNKEY        IFEQ      '21'
137700000000     C                   EXSR      F21GES
137800000000     C                   END
137900000000     C     FUNKEY        IFEQ      '22'
138000000000     C                   EXSR      F22GES
138100000000     C                   END
138200000000     C     FUNKEY        IFEQ      '23'
138300000000     C                   EXSR      F23GES
138400000000     C                   END
138500000000     C     FUNKEY        IFEQ      '24'
138600000000     C                   EXSR      F24GES
138700000000     C                   END
138800000000     C     FUNKEY        IFEQ      'RU'                                         ROLLUP
138900000000     C                   EXSR      RUPGES
139000000000     C                   END
139100000000     C     FUNKEY        IFEQ      'HP'                                         HELP
139200000000     C                   EXSR      HELGES
139300000000     C                   END
139400000000     C*
139500000000     C                   END
139600000000     C*
139700000000     C                   ENDSR
139800000000     C*================================================================
139900000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
140000000000     C*================================================================
140100000000     C     FUNVAR        BEGSR
140200000000     C*
140300000000     C* INIZIALIZZA VARIABILE -FUNKEY-
140400000000     C*
140500000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
140600000000     C     KEY           IFEQ      ENTER
140700000000     C                   MOVEL     *BLANKS       FUNKEY
140800000000     C                   END
140900000000     C     KEY           IFEQ      F1
141000000000     C                   MOVEL     '01'          FUNKEY
141100000000     C                   END
141200000000     C     KEY           IFEQ      F2
141300000000     C                   MOVEL     '02'          FUNKEY
141400000000     C                   END
141500000000     C     KEY           IFEQ      F3
141600000000     C                   MOVEL     '03'          FUNKEY
141700000000     C                   END
141800000000     C     KEY           IFEQ      F4
141900000000     C                   MOVEL     '04'          FUNKEY
142000000000     C                   END
142100000000     C     KEY           IFEQ      F5
142200000000     C                   MOVEL     '05'          FUNKEY
142300000000     C                   END
142400000000     C     KEY           IFEQ      F6
142500000000     C                   MOVEL     '06'          FUNKEY
142600000000     C                   END
142700000000     C     KEY           IFEQ      F7
142800000000     C                   MOVEL     '07'          FUNKEY
142900000000     C                   END
143000000000     C     KEY           IFEQ      F8
143100000000     C                   MOVEL     '08'          FUNKEY
143200000000     C                   END
143300000000     C     KEY           IFEQ      F9
143400000000     C                   MOVEL     '09'          FUNKEY
143500000000     C                   END
143600000000     C     KEY           IFEQ      F10
143700000000     C                   MOVEL     '10'          FUNKEY
143800000000     C                   END
143900000000     C     KEY           IFEQ      F11
144000000000     C                   MOVEL     '11'          FUNKEY
144100000000     C                   END
144200000000     C     KEY           IFEQ      F12
144300000000     C                   MOVEL     '12'          FUNKEY
144400000000     C                   END
144500000000     C     KEY           IFEQ      F13
144600000000     C                   MOVEL     '13'          FUNKEY
144700000000     C                   END
144800000000     C     KEY           IFEQ      F14
144900000000     C                   MOVEL     '14'          FUNKEY
145000000000     C                   END
145100000000     C     KEY           IFEQ      F15
145200000000     C                   MOVEL     '15'          FUNKEY
145300000000     C                   END
145400000000     C     KEY           IFEQ      F16
145500000000     C                   MOVEL     '16'          FUNKEY
145600000000     C                   END
145700000000     C     KEY           IFEQ      F17
145800000000     C                   MOVEL     '17'          FUNKEY
145900000000     C                   END
146000000000     C     KEY           IFEQ      F18
146100000000     C                   MOVEL     '18'          FUNKEY
146200000000     C                   END
146300000000     C     KEY           IFEQ      F19
146400000000     C                   MOVEL     '19'          FUNKEY
146500000000     C                   END
146600000000     C     KEY           IFEQ      F20
146700000000     C                   MOVEL     '20'          FUNKEY
146800000000     C                   END
146900000000     C     KEY           IFEQ      F21
147000000000     C                   MOVEL     '21'          FUNKEY
147100000000     C                   END
147200000000     C     KEY           IFEQ      F22
147300000000     C                   MOVEL     '22'          FUNKEY
147400000000     C                   END
147500000000     C     KEY           IFEQ      F23
147600000000     C                   MOVEL     '23'          FUNKEY
147700000000     C                   END
147800000000     C     KEY           IFEQ      F24
147900000000     C                   MOVEL     '24'          FUNKEY
148000000000     C                   END
148100000000     C     KEY           IFEQ      ROLLUP
148200000000     C                   MOVEL     'RU'          FUNKEY
148300000000     C                   END
148400000000     C     KEY           IFEQ      HELP
148500000000     C                   MOVEL     'HP'          FUNKEY
148600000000     C                   END
148700000000     C*
148800000000     C                   ENDSR
148900000000     C*================================================================
149000000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
149100000000     C*================================================================
149200000000     C     FUNGES        BEGSR
149300000000     C*
149400000000     C* PULISCO CAMPI DI LAVORO
149500000000     C                   Z-ADD     0             W                 5 0
149600000000     C*
149700000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
149800000000     C* E CARICO TABELLA FUNZIONI ATTIVE
149900000000     C     1             DO        24            I
150000000000     C                   MOVEL     FUD(I)        FUDRIG
150100000000     C*
150200000000     C* PULIZIA FUNZIONI PRECEDENTI
150300000000     C                   MOVEL     *BLANKS       FUV(I)
150400000000     C*
150500000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
150600000000     C* (SALTA F23 E F24)
150700000000     C     FUDATT        IFEQ      'S'
150800000000     C     FUDFUN        ANDNE     '23'
150900000000     C     FUDFUN        ANDNE     '24'
151000000000     C                   ADD       1             W
151100000000     C                   MOVEL     FUDDES        FUV(W)
151200000000     C                   END
151300000000     C                   END                                                    DO WHILE
151400000000     C*
151500000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
151600000000     C* ALTRIMENTI DISATTIVO F24
151700000000     C     1             DO        24            I
151800000000     C                   MOVEL     FUD(I)        FUDRIG
151900000000     C*
152000000000     C     FUDFUN        IFEQ      '24'
152100000000     C     W             IFGT      6
152200000000     C                   MOVEL     'S'           FUDATT
152300000000     C                   ELSE
152400000000     C                   MOVEL     *BLANKS       FUDATT
152500000000     C                   END                                                    W GT 8
152600000000     C                   MOVEL     FUDRIG        FUD(I)
152700000000     C                   END
152800000000     C                   END                                                    DO WHILE
152900000000     C*
153000000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
153100000000     C     W             DIV       6             I
153200000000     C                   MVR                     Z
153300000000     C*
153400000000     C     Z             IFNE      0
153500000000     C     I             ADD       1             Z
153600000000     C                   ELSE
153700000000     C                   Z-ADD     I             Z
153800000000     C                   END
153900000000     C*
154000000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
154100000000     C                   ADD       1             FUNGRU
154200000000     C     FUNGRU        IFGT      Z
154300000000     C     FUNGRU        ORLT      1
154400000000     C                   Z-ADD     1             FUNGRU
154500000000     C                   END
154600000000     C*
154700000000     C* PRIMO GRUPPO DI 6 FUNZIONI
154800000000     C     FUNGRU        IFEQ      1
154900000000     C                   MOVEL     FUV(1)        FUN001
155000000000     C                   MOVEL     FUV(2)        FUN002
155100000000     C                   MOVEL     FUV(3)        FUN003
155200000000     C                   MOVEL     FUV(4)        FUN004
155300000000     C                   MOVEL     FUV(5)        FUN005
155400000000     C                   MOVEL     FUV(6)        FUN006
155500000000     C                   END
155600000000     C*
155700000000     C* SECONDO GRUPPO DI 6 FUNZIONI
155800000000     C     FUNGRU        IFEQ      2
155900000000     C                   MOVEL     FUV(7)        FUN001
156000000000     C                   MOVEL     FUV(8)        FUN002
156100000000     C                   MOVEL     FUV(9)        FUN003
156200000000     C                   MOVEL     FUV(10)       FUN004
156300000000     C                   MOVEL     FUV(11)       FUN005
156400000000     C                   MOVEL     FUV(12)       FUN006
156500000000     C                   END
156600000000     C*
156700000000     C* TERZO GRUPPO DI 6 FUNZIONI
156800000000     C     FUNGRU        IFEQ      3
156900000000     C                   MOVEL     FUV(13)       FUN001
157000000000     C                   MOVEL     FUV(14)       FUN002
157100000000     C                   MOVEL     FUV(15)       FUN003
157200000000     C                   MOVEL     FUV(16)       FUN004
157300000000     C                   MOVEL     FUV(17)       FUN005
157400000000     C                   MOVEL     FUV(18)       FUN006
157500000000     C                   END
157600000000     C*
157700000000     C* QUARTO GRUPPO DI 6 FUNZIONI
157800000000     C     FUNGRU        IFEQ      4
157900000000     C                   MOVEL     FUV(19)       FUN001
158000000000     C                   MOVEL     FUV(20)       FUN002
158100000000     C                   MOVEL     FUV(21)       FUN003
158200000000     C                   MOVEL     FUV(22)       FUN004
158300000000     C                   MOVEL     *BLANKS       FUN005
158400000000     C                   MOVEL     *BLANKS       FUN006
158500000000     C                   END
158600000000     C*
158700000000     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
158800000000     C                   MOVEL     *BLANKS       FUN007
158900000000     C                   MOVEL     *BLANKS       FUN008
159000000000     C*
159100000000     C     1             DO        24            I
159200000000     C                   MOVEL     FUD(I)        FUDRIG
159300000000     C     FUDFUN        IFEQ      '23'
159400000000     C     FUDATT        ANDEQ     'S'
159500000000     C                   MOVEL     FUDDES        FUN007
159600000000     C                   END
159700000000     C     FUDFUN        IFEQ      '24'
159800000000     C     FUDATT        ANDEQ     'S'
159900000000     C                   MOVEL     FUDDES        FUN008
160000000000     C                   END
160100000000     C                   END                                                    DO
160200000000     C*
160300000000     C                   ENDSR
160400000000     C*================================================================
160500000000     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
160600000000     C*================================================================
160700000000     C     OPZGES        BEGSR
160800000000     C*
160900000000     C* PULISCO CAMPI DI LAVORO
161000000000     C                   Z-ADD     0             W                 5 0
161100000000     C*
161200000000     C* RICERCO QUANTE OPZIONI SONO ATTIVE
161300000000     C* E CARICO TABELLA OPZIONI ATTIVE
161400000000     C     1             DO        24            I
161500000000     C                   MOVEL     OPD(I)        OPDRIG
161600000000     C*
161700000000     C* PULIZIA OPZIONI PRECEDENTI
161800000000     C                   MOVEL     *BLANKS       OPV(I)
161900000000     C*
162000000000     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
162100000000     C     OPDATT        IFEQ      'S'
162200000000     C                   ADD       1             W
162300000000     C                   MOVEL     OPDDES        OPV(W)
162400000000     C                   END
162500000000     C                   END                                                    DO WHILE
162600000000     C*
162700000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
162800000000     C* ALTRIMENTI DISATTIVO F23
162900000000     C     1             DO        24            I
163000000000     C                   MOVEL     FUD(I)        FUDRIG
163100000000     C*
163200000000     C     FUDFUN        IFEQ      '23'
163300000000     C     W             IFGT      8
163400000000     C                   MOVEL     'S'           FUDATT
163500000000     C                   ELSE
163600000000     C                   MOVEL     *BLANKS       FUDATT
163700000000     C                   END                                                    W GT 8
163800000000     C                   MOVEL     FUDRIG        FUD(I)
163900000000     C                   END
164000000000     C                   END                                                    DO WHILE
164100000000     C*
164200000000     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
164300000000     C     W             DIV       8             I
164400000000     C                   MVR                     Z                 5 0
164500000000     C*
164600000000     C     Z             IFNE      0
164700000000     C     I             ADD       1             Z
164800000000     C                   ELSE
164900000000     C                   Z-ADD     I             Z
165000000000     C                   END
165100000000     C*
165200000000     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
165300000000     C                   ADD       1             OPZGRU
165400000000     C     OPZGRU        IFGT      Z
165500000000     C     OPZGRU        ORLT      1
165600000000     C                   Z-ADD     1             OPZGRU
165700000000     C                   END
165800000000     C*
165900000000     C* PRIMO GRUPPO DI 8 OPZIONI
166000000000     C     OPZGRU        IFEQ      1
166100000000     C                   MOVEL     OPV(1)        OPZ001
166200000000     C                   MOVEL     OPV(2)        OPZ002
166300000000     C                   MOVEL     OPV(3)        OPZ003
166400000000     C                   MOVEL     OPV(4)        OPZ004
166500000000     C                   MOVEL     OPV(5)        OPZ005
166600000000     C                   MOVEL     OPV(6)        OPZ006
166700000000     C                   MOVEL     OPV(7)        OPZ007
166800000000     C                   MOVEL     OPV(8)        OPZ008
166900000000     C                   END
167000000000     C*
167100000000     C* SECONDO GRUPPO DI 8 OPZIONI
167200000000     C     OPZGRU        IFEQ      2
167300000000     C                   MOVEL     OPV(9)        OPZ001
167400000000     C                   MOVEL     OPV(10)       OPZ002
167500000000     C                   MOVEL     OPV(11)       OPZ003
167600000000     C                   MOVEL     OPV(12)       OPZ004
167700000000     C                   MOVEL     OPV(13)       OPZ005
167800000000     C                   MOVEL     OPV(14)       OPZ006
167900000000     C                   MOVEL     OPV(15)       OPZ007
168000000000     C                   MOVEL     OPV(16)       OPZ008
168100000000     C                   END
168200000000     C*
168300000000     C* TERZO GRUPPO DI 8 OPZIONI
168400000000     C     OPZGRU        IFEQ      3
168500000000     C                   MOVEL     OPV(17)       OPZ001
168600000000     C                   MOVEL     OPV(18)       OPZ002
168700000000     C                   MOVEL     OPV(19)       OPZ003
168800000000     C                   MOVEL     OPV(20)       OPZ004
168900000000     C                   MOVEL     OPV(21)       OPZ005
169000000000     C                   MOVEL     OPV(22)       OPZ006
169100000000     C                   MOVEL     OPV(23)       OPZ007
169200000000     C                   MOVEL     OPV(24)       OPZ008
169300000000     C                   END
169400000000     C                   ENDSR
169500000000     C*================================================================
169600000000     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
169700000000     C*================================================================
169800000000     C     OPZCON        BEGSR
169900000000     C*
170000000000     C* PULISCO CAMPI DI LAVORO
170100000000     C                   Z-ADD     0             I                 5 0
170200000000     C                   Z-ADD     0             W                 5 0
170300000000     C*
170400000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
170500000000     C     1             DO        24            I
170600000000     C                   MOVEL     OPD(I)        OPDRIG
170700000000     C*
170800000000     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
170900000000     C     OPZKEY        IFEQ      OPDOPZ
171000000000     C     OPZKEY        OREQ      OPDOP1
171100000000     C*
171200000000     C     OPDATT        IFEQ      'S'
171300000000     C                   MOVEL     OPDOPZ        OPZKEY
171400000000     C                   Z-ADD     I             W
171500000000     C                   END
171600000000     C                   END
171700000000     C*
171800000000     C                   END                                                    DO WHILE
171900000000     C*
172000000000     C* OPZIONE NON ATTIVA (ERRORE)
172100000000     C     W             IFEQ      0
172200000000     C*
172300000000     C* OPZIONE NON OK
172400000000     C                   Z-ADD     1             OPZOK
172500000000     C*
172600000000     C* EMISSIONE MESSAGGIO DI ERRORE
172700000000     C                   MOVEL     MSG(1)        DSMSMS
172800000000     C                   ELSE
172900000000     C*
173000000000     C* OPZIONE ATTIVA (PROCEDI ...)
173100000000     C*
173200000000     C     OPZKEY        IFEQ      ' 1'
173300000000     C                   EXSR      O01GES
173400000000     C                   END
173500000000     C     OPZKEY        IFEQ      ' 2'
173600000000     C                   EXSR      O02GES
173700000000     C                   END
173800000000     C     OPZKEY        IFEQ      ' 3'
173900000000     C                   EXSR      O03GES
174000000000     C                   END
174100000000     C     OPZKEY        IFEQ      ' 4'
174200000000     C                   EXSR      O04GES
174300000000     C                   END
174400000000     C     OPZKEY        IFEQ      ' 5'
174500000000     C                   EXSR      O05GES
174600000000     C                   END
174700000000     C     OPZKEY        IFEQ      ' 6'
174800000000     C                   EXSR      O06GES
174900000000     C                   END
175000000000     C     OPZKEY        IFEQ      ' 7'
175100000000     C                   EXSR      O07GES
175200000000     C                   END
175300000000     C*
175400000000     C                   END                                                    OPZ.NON ATTIVA
175500000000     C*
175600000000     C                   ENDSR
175700000000     C*================================================================
175800000000     C* NOTE PER COMPILAZIONE SCHIERE
175900000000     C*================================================================
176000000000     C* -MSG-
176100000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
176200000000     C* (NON TOCCARE MSG,1 E MSG,2
176300000000     C*
176400000000     C* -CMD-
176500000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
176600000000     C*
176700000000     C* -OPD-
176800000000     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
176900000000     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
177000000000     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
177100000000     C*    ESEMPIO: OPZIONI -1 -
177200000000     C*                     - 1-  SONO LA STESSA COSA
177300000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
177400000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
177500000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
177600000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
177700000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
177800000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
177900000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
178000000000     C*
178100000000     C* -FUD-
178200000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
178300000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
178400000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
178500000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
178600000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
178700000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
178800000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
178900000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
179000000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
179100000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
179200000000     C*
179300000000     C*================================================================
179400000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
179500000000TNSDUCR- Opzione non gestita.                                                  1
179600000000TNSDUCR- Tasto funzionale non gestito.                                         2
179700000000TNSDUCR- Non abilitato alla gestione di questa Unità.                          3
179800960927TNSDUCR- Dotazione P.operativo annullata. Consentito solo il ripristino        4
179900960927TNSDUCR- Dotazione P.operativo non annullata, impossibile ripristinare.        5
180000000000TNSDUCR- Accettato solo un parametro di inizio lista.                          6
180100000000TNSDUCR- Tipo unità non accettabile.                                           7
180200020405TNSDUCR- Magazzino ubicazione non accettabile.                                 8
180300020717TNSDUCR- Profilo non abilitato alla funzione                                   9
180400000000TNSDUCR- 10
180500000000TNSDUCR- 11
180600000000TNSDUCR- 12
180700000000TNSDUCR- 13
180800000000TNSDUCR- 14
180900000000TNSDUCR- 15
181000000000TNSDUCR- 16
181100000000TNSDUCR- 17
181200000000TNSDUCR- 18
181300000000TNSDUCR- 19
181400000000TNSDUCR- 20
181500000000** ======== SCHIERA: CMD  (COMANDI)                        ================
181600000000WRKSPLF
181700000000
181800000000
181900000000
182000000000
182100000000** ======== SCHIERA: OPD  OPZIONI GESTITE NEL PGM (ATTIVA=S) ==========
182200000000 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
182300000000 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
182400000000                   -3 -     (S/N)=N                        opz. 03¬
182500000000 4=Cancellazione   -4 -     (S/N)=N                        opz. 04¬
182600000000 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
182700000000                   -6 -     (S/N)=N                        opz. 06¬
182800000000 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
182900000000                   -8 -     (S/N)=N                        opz. 08¬
183000000000                   -9 -     (S/N)=N                        opz. 09¬
183100000000                   -  -     (S/N)=N                        opz. 10¬
183200000000                   -  -     (S/N)=N                        opz. 11¬
183300000000                   -  -     (S/N)=N                        opz. 12¬
183400000000                   -  -     (S/N)=N                        opz. 13¬
183500000000                   -  -     (S/N)=N                        opz. 14¬
183600000000                   -  -     (S/N)=N                        opz. 15¬
183700000000                   -  -     (S/N)=N                        opz. 16¬
183800000000                   -  -     (S/N)=N                        opz. 17¬
183900000000                   -  -     (S/N)=N                        opz. 18¬
184000000000                   -  -     (S/N)=N                        opz. 19¬
184100000000                   -  -     (S/N)=N                        opz. 20¬
184200000000                   -  -     (S/N)=N                        opz. 21¬
184300000000                   -  -     (S/N)=N                        opz. 22¬
184400000000                   -  -     (S/N)=N                        opz. 23¬
184500000000                   -  -     (S/N)=N                        opz. 24¬
184600000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
184700000000                   -        (S/N)=N                        funz.01¬
184800000000                   -        (S/N)=N                        funz.02¬
184900000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
185000000000                   -        (S/N)=N                        funz.04¬
185100971013F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
185200000000F06=Inserimento    -        (S/N)=N                        funz.06¬
185300000000                   -        (S/N)=N                        funz.07¬
185400000000                   -        (S/N)=N                        funz.08¬
185500000000                   -        (S/N)=N                        funz.09¬
185600000000 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.10¬
185700000000 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.11¬
185800000000F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
185900000000                   -        (S/N)=N                        funz.13¬
186000000000                   -        (S/N)=N                        funz.14¬
186100000000                   -        (S/N)=N                        funz.15¬
186200000000                   -        (S/N)=N                        funz.16¬
186300000000F17=Inizio dati    -        (S/N)=S  <== S-FISSO           funz.17¬
186400000000F18=Fine dati      -        (S/N)=S  <== S-FISSO           funz.18¬
186500000000                   -        (S/N)=N                        funz.19¬
186600000000                   -        (S/N)=N                        funz.20¬
186700020405F21=Stampa         -        (S/N)=S                        funz.21¬
186800941220F22=Gestione stampe-        (S/N)=S                        funz.22¬
186900000000F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
187000000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
