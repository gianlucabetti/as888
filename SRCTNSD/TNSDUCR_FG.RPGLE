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
004000050603     D* TIPI RAGGRUPPAMENTO 2° LIVELLO E LORO DECODIFICHE
004100050603     D RG2             S              3  0 DIM(100)
004200050603     D DRG2            S             20    DIM(100)
004300000000     D*------------------------------------------------------------------------*
004400000000     D* INPUT
004500000000     D*------------------------------------------------------------------------*
004600020326
004700020711     d WokUni          s              1    inz('0')
004800020711     d WokDot          s              1    inz('0')
004900020326     d wnosfl          s              1    inz('0')
005000020326     d wunmag          s                   like(unamag)
005100020326     d wuntip          s                   like(unatip)
005200020711     d SavMag          s                   Like(UnaMag)
005300020711     d SavTip          s                   Like(UnaTip)
005400020711     d SavAtb          s                   Like(DotAtb)
005500020711     d SavQta          s                   Like(DotQta)
005600020326     d depdot          s                   like(vdpdot)
005700020405     d depmag          s                   like(vdpmag)
005800020326     d kdofil          s                   like(dotfil)
005900020326
006000000000     D*----------------
006100000000     D* RIDEFINIZIONE SCHIERA -OPD-
006200000000     D*----------------
006300000000     D OPDDS           DS
006400000000     D*                -OPZIONE PRINCIPALE
006500000000     D  OPDOPZ                 1      2
006600000000     D*                -DESCRIZIONE PARZIALE
006700000000     D  OPDDE1                 3     19
006800000000     D*                -OPZIONE ALTERNATIVA
006900000000     D  OPDOP1                21     22
007000000000     D*                -FILLER
007100000000     D  OPDFI0                23     34
007200000000     D*                -FLAG OPZ. ATTIVA (S/N)
007300000000     D  OPDATT                35     35
007400000000     D*                -FILLER
007500000000     D  OPDFI1                36     80
007600000000     D*                -DESCRIZIONE COMPLETA
007700000000     D  OPDDES                 1     19
007800000000     D*                -RIDEFINIZIONE RIGA
007900000000     D  OPDRIG                 1     80
008000000000     D*----------------
008100000000     D* RIDEFINIZIONE SCHIERA -FUD-
008200000000     D*----------------
008300000000     D FUDDS           DS
008400000000     D*                -DESCRIZIONE PARZIALE
008500000000     D  FUDDE1                 1      1
008600000000     D*                -FUNZIONE
008700000000     D  FUDFUN                 2      3
008800000000     D*                -DESCRIZIONE PARZIALE
008900000000     D  FUDDE2                 4     19
009000000000     D*                -FILLER
009100000000     D  FUDFI0                20     34
009200000000     D*                -FLAG FUNZ.ATTIVA (S/N)
009300000000     D  FUDATT                35     35
009400000000     D*                -FILLER
009500000000     D  FUDFI1                36     80
009600000000     D*                -DESCRIZIONE COMPLETA
009700000000     D  FUDDES                 1     19
009800000000     D*                -RIDEFINIZIONE RIGA
009900000000     D  FUDRIG                 1     80
010000000000     D*----------------
010100000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
010200000000     D*----------------
010300000000     D DSMSG         E DS
010400000000     D*----------------
010500000000     D* PARAMETRI PER LANCIO ALTRI PGM DELLA PROCEDURA
010600000000     D*----------------
010700000000     D DSUNIT        E DS
010800000000     D*----------------
010900000000     D* DS PER GESTIONE TASTI FUNZIONALI
011000000000     D*----------------
011100000000     D DSKEY         E DS
011200000000     D DEVDS1          DS
011300000000     D  KEY                  369    369
011400000000     D*----------------
011500000000     D* ARCHITETTURA
011600000000     D*----------------
011700000000     D KPJBA         E DS
011800050603     D*----------------
011900050603     D* DS D WRK X SCRITTURA RECORD SUB-FILE DETTAGLIO RAGGRUPPAMENTI
012000050603     D*----------------
012100050603     D DSDUCRDET       DS
012200050603     D  DUCR_TIP                      2  0
012300050603     D  DUCR_MAG                      3  0
012400050603     D  DUCR_RG2                      3  0
012500050603     D  DUCR_TID                     20
012600050603     D  DUCR_QTA                      7  0
012700050603     D*----------------
012800050603     D* SCHIERA WRK X SCRITTURA RECORD SUB-FILE DETTAGLIO RAGGRUPPAMENTI
012900050603     D*----------------
013000050606     D SKDUCRDET       S                   like(DSDUCRDET) DIM(9999) DESCEND
013100020709
013200020709     d ddatiute      e ds
013300020709     d azuteds       e ds                  extname(azute00f)
013400020709     d tibs34ds      e ds                  inz
013500020710      *------------------------------------------------------------------------*
013600020710     Iunana000      01
013700020710     Iundot000      02
013800020710     Iunana02       03
013900020710     Iundot02       04
014000000000     C*------------------------------------------------------------------------*
014100000000     C* MAIN LINE
014200000000     C*------------------------------------------------------------------------*
014300000000     C*
014400000000     C* SE CONTROLLI PRELIMINARI NON OK VADO A FINE PGM
014500000000     C     ERRL00        IFNE      '1'                                          --- 1 -->
014600000000     C*
014700000000     C* CARICA IL SFL DAI PARAMETRI IN INPUT AL PROGRAMMA
014800000000     C                   EXSR      POSSFL
014900000000     C*
015000000000     C* CICLO FINO A CHE:
015100000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
015200000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
015300000000     C*  -FINE PGM PER ENTER SENZA SCELTA
015400000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
015500000000     C     F12L00        OREQ      '1'
015600020404     C**!!!FINL00        OREQ      '1'
015700000000     C*
015800000000     C* AZZERA FLAG DI ERROE
015900000000     C                   MOVEL     '0'           ERRL00
016000020717      * Se Profilo non OK devo uscire dal pgm
016100020717     c                   If        *In76
016200020717     c                   eval      f03l00 = *on
016300020717     c                   EndIf
016400000000     C*
016500000000     C* EMETTE SFL A VIDEO
016600000000     C                   EXSR      OUTSFL
016700000000     C*
016800000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI -ROUTINE FUNCON
016900000000     C                   EXSR      FUNCON
017000000000     C*
017100000000     C                   END                                                    <-- 2 ---
017200000000     C*
017300000000     C                   END                                                    <-- 1 --
017400000000     C*
017500000000     C                   MOVEL     DSUNIT        KPJBU
017600000000     C                   SETON                                        LR
017700000000     C*================================================================
017800000000     C* F03GES -GEST.TASTO FUNZ. F03 ===> FINE
017900000000     C*================================================================
018000000000     C     F03GES        BEGSR
018100000000     C*
018200000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PGM 00)
018300000000     C                   MOVEL     '1'           F03L00
018400000000     C*
018500000000     C                   ENDSR
018600000000     C*================================================================
018700000000     C* F05GES -GEST.TASTO FUNZ. F05 ===> RIVISUALIZZAZIONE
018800000000     C*================================================================
018900000000     C     F05GES        BEGSR
019000000000     C*
019100000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
019200000000     C                   EXSR      POSSFL
019300000000     C*
019400000000     C                   ENDSR
019500000000     C*================================================================
019600000000     C* F06GES -GEST.TASTO FUNZ. F06 ===> INSERIMENTO
019700000000     C*================================================================
019800000000     C     F06GES        BEGSR
019900000000     C*
020000000000     C* INSERIMENTO CONSENTITO SOLO IN SEDE
020100020709     c                   If        DutLpo <> 'S'
020200000000     C                   MOVEL     '1'           ERRL00
020300000000     C                   MOVEL     MSG(3)        DSMSMS
020400000000     C                   END
020500000000     C*
020600000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
020700000000     C*
020800000000     C* LANCIO IL PGM PER L'INSERIMENTO DEL RECORD (TNSDUDR)
020900000000     C                   MOVEL     'F06'         WOPZ1             3
021000000000     C                   MOVEL     'TNSDUDR'     WPGM              8
021100000000     C                   EXSR      LANPGM
021200000000     C*
021300000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
021400000000     C                   EXSR      POSSFL
021500000000     C*
021600000000     C                   END                                                    <-- 1 ---
021700000000     C*
021800000000     C                   ENDSR
021900000000     C*================================================================
022000000000     C* F12GES -GEST.TASTO FUNZ. F12 ===> RITORNO
022100000000     C*================================================================
022200000000     C     F12GES        BEGSR
022300000000     C*
022400000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12 (A LIVELLO PGM 00)
022500000000     C                   MOVEL     '1'           F12L00
022600000000     C*
022700000000     C                   ENDSR
022800000000     C*================================================================
022900000000     C* F17GES -GEST.TASTO FUNZ. F17 ===> INIZIO DATI
023000000000     C*================================================================
023100000000     C     F17GES        BEGSR
023200000000     C*
023300000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
023400000000     C                   Z-ADD     1             SFLNBR
023500000000     C*
023600000000     C                   ENDSR
023700000000     C*================================================================
023800000000     C* F18GES -GEST.TASTO FUNZ. F18 ===> FINE DATI
023900000000     C*================================================================
024000000000     C     F18GES        BEGSR
024100000000     C*
024200000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
024300000000     C                   Z-ADD     SFLULT        SFLNBR
024400000000     C*
024500000000     C                   ENDSR
024600000000     C*================================================================
024700000000     C* F21GES -GEST.TASTO FUNZ. F21 ===> STAMPA DIFFERENZE
024800000000     C*================================================================
024900000000     C     F21GES        BEGSR
025000000000     C*
025100000000     C* LANCIA PROGRAMMA DI STAMPA DELLE UNITA
025200000000     C                   MOVEL     DSUNIT        KPJBU
025300000000     C                   CALL      'TNSDUER'
025400000000     C                   PARM                    KPJBA
025500000000     C                   MOVEL     KPJBU         DSUNIT
025600000000     C*
025700000000     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
025800000000     C     DSUF03        IFEQ      '1'
025900000000     C                   MOVEL     '1'           F03L00
026000000000     C                   MOVEL     '1'           ERRL00
026100000000     C                   ENDIF
026200000000     C*
026300000000     C                   ENDSR
026400000000     C*================================================================
026500000000     C* F22GES -GEST.TASTO FUNZ. F22 ===> STAMPE
026600000000     C*================================================================
026700000000     C     F22GES        BEGSR
026800000000     C*
026900000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
027000000000     C                   Z-ADD     80            LENGH            15 5
027100000000     C                   CALL      'QCAEXEC'
027200000000     C                   PARM                    CMD(1)
027300000000     C                   PARM                    LENGH
027400000000     C*
027500000000     C                   ENDSR
027600000000     C*================================================================
027700000000     C* F23GES -GEST.TASTO FUNZ. F23 ===> ALTRE OPZIONI
027800000000     C*================================================================
027900000000     C     F23GES        BEGSR
028000000000     C*
028100000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
028200000000     C                   EXSR      OPZGES
028300000000     C*
028400000000     C                   ENDSR
028500000000     C*================================================================
028600000000     C* F24GES -GEST.TASTO FUNZ. F24 ===> ALTRI TASTI
028700000000     C*================================================================
028800000000     C     F24GES        BEGSR
028900000000     C*
029000000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
029100000000     C                   EXSR      FUNGES
029200000000     C*
029300000000     C                   ENDSR
029400000000     C*================================================================
029500000000     C* RUPGES -GEST.TASTO FUNZ. ROLLUP ===> PAGINA SUCCESSIVA
029600000000     C*================================================================
029700000000     C     RUPGES        BEGSR
029800000000     C*
029900000000     C* CARICA IL SFL DA FILE "FISICO"
030000000000     C                   EXSR      CARSFL
030100000000     C*
030200000000     C                   ENDSR
030300000000     C*================================================================
030400000000     C* HELGES - GEST.TASTO FUNZ. HELP ===> AIUTO
030500000000     C*================================================================
030600000000     C     HELGES        BEGSR
030700000000     C*
030800000000     C                   ENDSR
030900000000     C*================================================================
031000000000     C* ENTGES - GEST.TASTO FUNZ. ENTER ===> INVIO
031100000000     C*================================================================
031200000000     C     ENTGES        BEGSR
031300000000     C*
031400000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
031500000000     C                   EXSR      NEWPOS
031600000000     C*
031700000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
031800000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
031900000000     C*
032000000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
032100000000     C     SWPOSI        IFNE      0                                            --- 2 -->
032200000000     C                   EXSR      POSSFL
032300000000     C*
032400000000     C                   ELSE                                                   <-- 2 -->
032500000000     C*
032600000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
032700000000     C     SFLULT        IFNE      0                                            --- 3 -->
032800000000     C                   EXSR      ENTATT
032900000000     C*
033000000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
033100000000     C     SWMODI        IFNE      0                                            --- 4 -->
033200000000     C                   EXSR      POSSFL
033300000000     C                   END                                                    <-- 4 ---
033400000000     C*
033500000000     C                   END                                                    <-- 3 ---
033600000000     C                   END                                                    <-- 2 ---
033700000000     C                   END                                                    <-- 1 ---
033800000000     C*
033900000000     C                   ENDSR
034000000000     C*================================================================
034100000000     C*  O01GES - GESTIONE OPZIONE 1=SCELTA
034200000000     C*================================================================
034300000000     C     O01GES        BEGSR
034400000000     C                   ENDSR
034500000000     C*================================================================
034600000000     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
034700000000     C*================================================================
034800000000     C     O02GES        BEGSR
034900000000     C*
035000000000     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA MODIFICA
035100000000     C     VIDATB        IFEQ      'A'
035200000000     C                   MOVEL     '1'           ERRL00
035300000000     C                   MOVEL     MSG(4)        DSMSMS
035400000000     C                   END
035500000000     C*
035600000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
035700000000     C*
035800000000     C* LANCIO IL PGM PER LA MODIFICA DEL DETTAGLIO DELL'ANAGRAFICA
035900000000     C                   MOVEL     'O02'         WOPZ1
036000000000     C                   MOVEL     'TNSDUDR'     WPGM
036100000000     C                   EXSR      LANPGM
036200000000     C*
036300000000     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
036400000000     C                   Z-ADD     1             SWMODI
036500000000     C*
036600000000     C                   END                                                    <-- 1 ---
036700000000     C*
036800000000     C                   ENDSR
036900000000     C*================================================================
037000000000     C*  O03GES - GESTIONE OPZIONE 3=COPIA
037100000000     C*================================================================
037200000000     C     O03GES        BEGSR
037300000000     C*
037400000000     C                   ENDSR
037500000000     C*================================================================
037600000000     C*  O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE
037700000000     C*================================================================
037800000000     C     O04GES        BEGSR
037900000000     C*
038000000000     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
038100000000     C     VIDATB        IFEQ      'A'
038200000000     C                   MOVEL     '1'           ERRL00
038300000000     C                   MOVEL     MSG(4)        DSMSMS
038400000000     C                   END
038500000000     C*
038600000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
038700000000     C*
038800000000     C* LANCIO IL PGM DI GESTIONE DELLE UNITA
038900000000     C                   MOVEL     'O04'         WOPZ1
039000000000     C                   MOVEL     'TNSDUDR'     WPGM
039100000000     C                   EXSR      LANPGM
039200000000     C*
039300000000     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
039400000000     C                   Z-ADD     1             SWMODI
039500000000     C*
039600000000     C                   END                                                    <-- 1 ---
039700000000     C*
039800000000     C                   ENDSR
039900000000     C*================================================================
040000000000     C*  O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
040100000000     C*================================================================
040200000000     C     O05GES        BEGSR
040300000000     C*
040400000000     C* LANCIO ROUTINE PER LA VISUALIZZAZIONE DEL DETTAGLIO DELLA RIGA
040500000000     C                   MOVEL     'O05'         WOPZ1
040600000000     C                   MOVEL     'TNSDUDR'     WPGM
040700000000     C                   EXSR      LANPGM
040800000000     C*
040900000000     C                   ENDSR
041000000000     C*================================================================
041100000000     C*  O06GES - GESTIONE OPZIONE 6=STAMPA
041200000000     C*================================================================
041300000000     C     O06GES        BEGSR
041400000000     C                   ENDSR
041500000000     C*================================================================
041600000000     C*  O07GES - GESTIONE OPZIONE 7=RIPRISTINO
041700000000     C*================================================================
041800000000     C     O07GES        BEGSR
041900000000     C*
042000000000     C* SE IL RECORD NON E' ANNULLATO NON CONSENTO IL RIPRISTINO
042100000000     C     VIDATB        IFNE      'A'
042200000000     C                   MOVEL     '1'           ERRL00
042300000000     C                   MOVEL     MSG(5)        DSMSMS
042400000000     C                   END
042500000000     C*
042600000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
042700000000     C*
042800000000     C* LANCIO IL PGM PER IL RIPRISTINO DI UN ANNULLAMENTO
042900000000     C                   MOVEL     'O07'         WOPZ1
043000000000     C                   MOVEL     'TNSDUDR'     WPGM
043100000000     C                   EXSR      LANPGM
043200000000     C*
043300000000     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
043400000000     C                   Z-ADD     1             SWMODI
043500000000     C*
043600000000     C                   END                                                    <-- 1 ---
043700000000     C*
043800000000     C                   ENDSR
043900000000     C*================================================================
044000000000     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
044100000000     C*================================================================
044200000000     C     NEWPOS        BEGSR
044300000000     C*
044400000000     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAM.
044500000000     C*         1= RICHIESTO POSIZIONAM.
044600000000     C                   Z-ADD     0             SWPOSI            7 0
044700000000     C*
044800000000     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
044900000000     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
045000000000     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
045100000000     C                   Z-ADD     0             SWMODI            1 0
045200000000     C*
045300000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
045400020405     c                   if        vdptip <> deptip or vdpdot <> depdot or      --- 1 -->
045500020405     c                             vdpmag <> depmag
045600000000     C*
045700000000     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI DI PARTENZA
045800000000     C                   EXSR      CHKPAR
045900000000     C*
046000000000     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
046100000000     C     ERRL00        IFEQ      '0'                                          --- 2 -->
046200000000     C                   Z-ADD     1             SWPOSI
046300000000     C                   Z-ADD     VDPTIP        DEPTIP
046400020326     c                   eval      depdot = vdpdot
046500020405     c                   eval      depmag = vdpmag
046600000000     C                   END                                                    <-- 2 ---
046700000000     C*
046800000000     C                   END                                                    <-- 1 ---
046900000000     C*
047000000000     C                   ENDSR
047100000000     C*================================================================
047200000000     C* LETTURA RECORD FILE DA CARICARE
047300000000     C*================================================================
047400000000     C     LETFIL        BEGSR
047500020711
047600020711     c                   eval      *In01 = *off
047700020711     c                   eval      *In02 = *off
047800020711     c                   eval      *In03 = *off
047900020711     c                   eval      *In04 = *off
048000000000     C*
048100000000     C     WTIPRI        IFEQ      'TIP'
048200020710     C                   READ      unana01c                               99
048300000000     C                   END
048400020405
048500020405     c                   if        wtipri = 'MAG'
048600020710     c     kanmag        reade     unana02c                               99
048700020405     c                   endif
048800020711
048900020711     c                   If        Not *In99
049000020711
049100020711     c                   If        *In01 or *In03
049200020711     c                   Eval      SavTip = UnaTip
049300020711     c                   Eval      SavMag = UnaMag
049400020711     c                   EndIf
049500020711     c                   If        *In02 or *In04
049600020711     c                   Eval      SavTip = DotTip
049700020711     c                   Eval      SavMag = DotFil
049800020711     c                   EndIf
049900020711
050000020711
050100020711     c                   EndIf
050200020405
050300000000     C*
050400000000     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
050500000000     C     *IN99         IFEQ      '1'
050600000000     C                   Z-ADD     1             SFLEOF
050700000000     C                   END
050800000000     C*
050900000000     C                   ENDSR
051000000000     C*================================================================
051100000000     C* SETFIL - SETLL DEL FILE DA CARICARE
051200000000     C*================================================================
051300000000     C     SETFIL        BEGSR
051400020711
051500020711     c                   eval      *In01 = *off
051600020711     c                   eval      *In02 = *off
051700020711     c                   eval      *In03 = *off
051800020711     c                   eval      *In04 = *off
051900000000     C*
052000000000     C* POSIZIONAMENTO DEL FILE DA CARICARE E PRIMA LETTURA
052100000000     C     WTIPRI        IFEQ      'TIP'
052200020326     c                   eval      kantip = vdptip
052300020710     c     KaNtip        SETLL     unana01c                           99        *FINE-ARCHIV
052400020710     c  N99              READ      unana01c                               99
052500000000     C                   END
052600020405
052700020405     c                   if        wtipri = 'MAG'
052800020405     c                   move      vdpmag        kanmag
052900020710     c     kanmag        setll     unana02c                           99
053000020710     c  n99kanmag        reade     unana02c                               99
053100020405     c                   endif
053200020322
053300020322     c                   if        not *in99
053400020711
053500020711     c                   If        *In01 or *In03
053600020711     c                   If        WunTip = *Zeros
053700020711     c                   Eval      WunTip = UnaTip
053800020711     c                   EndIf
053900020711     c                   If        WunMag = *Zeros
054000020711     c                   Eval      WunMag = UnaMag
054100020711     c                   EndIf
054200020711     c                   Eval      SavTip = UnaTip
054300020711     c                   Eval      SavMag = UnaMag
054400020711     c                   EndIf
054500020711
054600020711     c                   If        *In02 or *In04
054700020711     c                   If        WunTip = *Zeros
054800020711     c                   Eval      WunTip = DotTip
054900020711     c                   EndIf
055000020711     c                   If        WunMag = *Zeros
055100020711     c                   Eval      WunMag = DotFil
055200020711     c                   EndIf
055300020711     c                   Eval      SavTip = DotTip
055400020711     c                   Eval      SavMag = DotFil
055500020711     c                   EndIf
055600020711
055700020322     c                   endif
055800020326
055900000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
056000000000     C     *IN99         IFEQ      '1'                                          --- 1 -->
056100000000     C                   Z-ADD     1             SFLEOF
056200000000     C                   END                                                    <-- 1 ---
056300000000     C*
056400000000     C                   ENDSR
056500020326      *================================================================
056600020326      * CTRSFL - CONTROLLO SE RECORD DA SCRIVERE NEL SUBFILE
056700020326      *================================================================
056800020326     C     CTRSFL        BEGSR
056900020326
057000020711     c                   If        VdpDot = 'S' and
057100020711     c                             WokUni = *On and WokDot = *On
057200020711     c                   Eval      WnoSfl = *On
057300020711     c                   Endif
057400020711     c                   goto      passa
057500020326      * controllo se richieste solo unità NON in dotazione
057600020403     c                   if        (vdpdot = 'S' and vidqta <> 0) or
057700020403     c                             (vdpdot = 'S' and viddif = 0  and
057800020403     c                              vidqta = 0)
057900020326     c                   eval      wnosfl = *on
058000020326     c                   endif
058100020711     c     passa         tag
058200020326
058300020326     C                   ENDSR
058400000000     C*================================================================
058500000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
058600000000     C*================================================================
058700000000     C     IRESFL        BEGSR
058800000000     C*
058900000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
059000020322     C                   Z-ADD     wunTIP        VIDTIP
059100020322     C                   Z-ADD     wunmag        VIDFIL
059200020326
059300020322
059400020711     C                   movel     Savatb        VIDatb
059500020711     C                   Z-ADD     SavQTA        VIDQTA
059600950124     C*
059700950124     C* CALCOLA LA DIFFERENZA FRA LE QUANTITA' IN DOTAZIONE E QUELLE IN ESSERE
059800950124     C                   Z-ADD     *ZEROS        VIDDIF
059900020711     C     TOTFIL        SUB       SavQta        VIDDIF
060000000000     C* DECODIFICHE
060100000000     C                   Z-ADD     1             J
060200000000     C                   SETOFF                                       28
060300020326     C     wuntip        LOOKUP    UNI(J)                                 28
060400000000     C     *IN28         IFEQ      *ON
060500000000     C                   MOVEL     DUNI(J)       VIDTID
060600000000     C                   ELSE
060700000000     C                   MOVEL     *ALL'*'       VIDTID
060800000000     C                   ENDIF
060900000000     C*
061000000000     C                   Z-ADD     1             J
061100000000     C                   SETOFF                                       28
061200020322     C     wunmag        LOOKUP    FIL(J)                                 28
061300000000     C     *IN28         IFEQ      *ON
061400000000     C                   MOVEL     DFIL(J)       VIDFID
061500000000     C                   ELSE
061600000000     C                   MOVEL     *ALL'*'       VIDFID
061700000000     C                   ENDIF
061800000000     C*
061900000000     C                   ENDSR
062000050603     C*================================================================
062100050603     C* IRESFLDET - IMPOSTA I CAMPI DEL RECORD DETTAGLIO DEL SFL
062200050603     C*================================================================
062300050603     C     IRESFLDET     BEGSR
062400050603     C*
062500050603     C* REPERISCE LA DESCRIZIONE DEL RAGGRUPPAMENTO 2° LIVELLO CORRENTE
062600050606     C                   Z-ADD     1             Jrg2
062700050603     C                   SETOFF                                       28
062800050606     C     UNARG2        LOOKUP    RG2(Jrg2)                              28
062900050603     C     *IN28         IFEQ      *ON
063000050603     C                   MOVEL     DRG2(Jrg2)    wDesRG2          15
063100050603     C                   ELSE
063200050603     C                   MOVEL     *ALL'*'       wDesRG2
063300050603     C                   ENDIF
063400050603     C*
063500050603     C* VALORIZZO LA DS D WRK X MEMORIZZAZIONE DETTAGLI RAGGRUPPAMENTI 2° LIVELLO
063600050603     C                   EVAL      DUCR_TIP = UNATIP
063700050603     C                   EVAL      DUCR_MAG = UNAMAG
063800050603     C                   EVAL      DUCR_RG2 = UNARG2
063900050603     C                   EVAL      DUCR_TID = '   ' + %editc(UNARG2:'X') +
064000050603     C                                        ' ' + wDesRG2
064100050603     C                   EVAL      DUCR_QTA = 1
064200050603     C                   ADD       1             jSkDUCR
064300050603     C                   EVAL      SkDUCRDET(jSkDUCR) = DSDUCRDET
064400050603     C*
064500050603     C                   ENDSR
064600050603     C*================================================================
064700050603     C* WRISFLDET - SCRIVE I RECORD DETTAGLIO DEL SFL
064800050603     C*================================================================
064900050603     C     WRISFLDET     BEGSR
065000050603     C*
065100050603     C* SALVO I CAMPI CHIAVE A VIDEO APPENA SCRITTI
065200050603     C                   Z-ADD     VIDTIP        sTIP              2 0
065300050603     C                   Z-ADD     VIDFIL        sFIL              3 0
065400050603     C*
065500050603     C* SALVO GLI INDICATORI SUL FORMATO RECORD DEL SUB-FILE A VIDEO
065600050603     C                   EVAL      *IN60 = *IN80
065700050603     C                   EVAL      *IN70 = *IN90
065800050603     C*
065900050603     C* CLEARO IL FORMATO RECORD VIDEO DEL SUB-FILE
066000050603     C                   CLEAR                   SDUCD05
066100050603     C*
066200050603     C* DOPO LA CLEAR DEL FORMATO RECORD DEL SUB-FILE A VIDEO REIMPOSTO GLI INDICATORI
066300050603     C                   EVAL      *IN80 = *IN60
066400050603     C                   EVAL      *IN90 = *IN70
066500050603     C*
066600050603     C* INIZIALIZZO VARIABILI D WRK X VERIFICA ROTTURA D CODICE
066700050603     C                   Z-ADD     *zeros        wRG2              3 0
066800050606 xxx C                   Z-ADD     *zeros        wGiro             4 0
066900050603     C*
067000050603     C* ORDINO LA SCHIERA
067100050603     C                   SORTA     SkDUCRDET
067200050603     C*
067300050606     C                   Z-ADD     1             jSkDUCR_w         4 0
067400050603     C                   DOW       jSkDUCR <= %elem(SkDUCRDET)
067500050606 xxx C                   IF        SkDUCRDET(jSkDUCR_w) <> *blanks
067600050606     C                   EVAL      DSDUCRDET = SkDUCRDET(jSkDUCR_w)
067700050603     C*
067800050603     C* ELABORO SOLO SE DETTAGLI DEL RECORD SUB-FILE APPENA SCRITTO
067900050603     C                   IF        DUCR_TIP = sTIP AND DUCR_MAG = sFIL
068000050603     C*
068100050603     C* AD AVVENUTA ROTTURA SCRIVO IL RECORD D DETTAGLIO
068200050603     C                   IF        DUCR_RG2 <> wRG2 AND wGiro > 0
068300050603     C*
068400050603     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
068500050603     C                   ADD       1             SFLULT
068600050603     C*
068700050603     C* SCRITTURA RECORD SFL
068800050603     C                   WRITE     SDUCD05
068900050603     C                   ADD       1             SFLRIG
069000050603     C*
069100050603     C* SALVO I NUOVI DEPOSITI X VERIFICA SUCCESSIVA ROTTURA D CODICE
069200050603     C                   EVAL      wRG2 = DUCR_RG2
069300050603     C*
069400050603     C* CLEARO IL FORMATO RECORD VIDEO DEL SUB-FILE
069500050603     C                   CLEAR                   SDUCD05
069600050603     C*
069700050603     C* DOPO LA CLEAR DEL FORMATO RECORD DEL SUB-FILE A VIDEO REIMPOSTO GLI INDICATORI
069800050603     C                   EVAL      *IN80 = *IN60
069900050603     C                   EVAL      *IN90 = *IN70
070000050606     C                   ENDIF
070100050606     C*
070200050603     C                   EVAL      VIDTIP = DUCR_TIP
070300050603     C                   EVAL      VIDFIL = DUCR_MAG
070400050603     C                   EVAL      VIDTID = DUCR_TID
070500050603     C                   EVAL      VIDQTA = VIDQTA + DUCR_QTA
070600050603     C                   EVAL      wGiro  = 1
070700050603     C                   ENDIF
070800050606     C*
070900050606     C                   ELSE
071000050606     C                   LEAVE
071100050606     C                   ENDIF
071200050606     C*
071300050606     C                   ADD       1             jSkDUCR_w
071400050603     C                   ENDDO
071500050603     C*
071600050603     C* AL TERMINE SCARICO IL RECORD D DETTAGLIO RIMASTO IN CANNA (SOLO SE DA SCRIVERE)
071700050606 xxx C                   IF        VIDTIP = sTIP AND VIDFIL = sFIL
071800050603     C*
071900050603     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
072000050603     C                   ADD       1             SFLULT
072100050603     C*
072200050603     C* SCRITTURA RECORD SFL
072300050603     C                   WRITE     SDUCD05
072400050603     C                   ADD       1             SFLRIG
072500050603     C                   ENDIF
072600050603     C*
072700050603     C                   ENDSR
072800000000     C*================================================================
072900000000     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
073000000000     C*================================================================
073100000000     C     CHKPAR        BEGSR
073200020405
073300020405      * IMPOSTO FLAG EVIDENZAZIONE TIPO DI RICERCA
073400020405     c                   eval       *in90 = *off                                x tipo
073500020405     c                   eval       *in91 = *off                                x magazzino
073600000000     C*
073700000000     C* IMPOSTO FLAG EVIDENZAZIONE ERRORI SUI PARAMETRI
073800020405     C                   SETOFF                                       80        tipo
073900020405     c                   eval       *in81 = *off                                magazzino
074000000000     C*
074100000000     C* IMPOSTO A BLANK LE DECODIFICHE
074200000000     C                   MOVEL     *BLANKS       VDPTID
074300000000     C*
074400000000     C* SE TUTTI I PARAMETRI SONO UGUALI A BLANK O ZERO, UTILIZZO CHIAVE X TIP
074500020405     c                   if        vdptip = *zeros and vdpmag = *blanks
074600000000     C                   MOVEL     'TIP'         WTIPRI            3
074700000000     C                   SETON                                        90
074800000000     C*
074900000000X   1C                   ELSE
075000000000     C*
075100000000     C                   MOVEL     *BLANKS       WTIPRI
075200000000     C* ===> ACCETTO SOLO UN PARAMETRO DI INIZIO RICERCA
075300000000     C* CONTROLLO E DECODIFICO IL TIPO UNITA
075400000000IF  2C     VDPTIP        IFNE      *ZEROS
075500000000     C                   Z-ADD     1             J
075600000000     C                   SETOFF                                           28
075700000000     C     VDPTIP        LOOKUP    UNI(J)                                 28
075800000000IF  3C     *IN28         IFEQ      *OFF
075900000000     C                   MOVEL     *ALL'*'       VDPTID
076000000000     C                   MOVEL     '1'           ERRL00
076100000000     C                   MOVEL     MSG(7)        DSMSMS
076200000000     C                   SETON                                        80
076300000000     C                   ELSE
076400000000     C                   MOVEL     DUNI(J)       VDPTID
076500000000     C                   MOVEL     'TIP'         WTIPRI
076600020405     C                   SETON                                        90
076700000000E   3C                   END
076800000000E   2C                   END
076900020405
077000020405      * Magazzino / tipo unità
077100020405     c                   if        vdpmag <> *blanks and errl00 = *off          --- 1 -->
077200020405     c                   if        wtipri <> *blanks                            --- 2 -->
077300020405     c                   eval      errl00 = *on
077400020405     c                   eval      dsmsms = msg(6)
077500020405     c                   else                                                   <-- 2 -->
077600020405      * controllo magazzino
077700020405     c                   if        vdpmag <> *blanks
077800020405     c                   exsr      chkmag
077900020405     c                   endif
078000020405     c                   eval      wtipri = 'MAG'
078100020405     c                   eval      *in91 = *on
078200020405     c                   endif                                                  <-- 2 ---
078300020405     c                   endif                                                  <-- 1 ---
078400000000     C*
078500000000E   1C                   END
078600000000     C*
078700000000     C                   ENDSR
078800020405     C*================================================================
078900020405     C*  CHKMAG - CONTROLLA IL MAGAZZINO UBICAZIONE
079000020405     C*================================================================
079100020405     C     CHKMAG        BEGSR
079200020405     C*
079300020405     C* CONTROLLO SE E' STATO INSERITO '?'
079400020405     C     '?'           SCAN      VDPMAG                                 99
079500020405     C*
079600020405     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
079700020405     C     *IN99         IFEQ      '1'                                          --- 1 -->
079800020405     C                   MOVEL     *BLANKS       §COD1             3
079900020405     C                   MOVE      *BLANKS       §DES1
080000020405     C                   CALL      'TNSD20R'
080100020405     C                   PARM                    §COD1
080200020405     C                   PARM                    §TIP              1
080300020405     C                   PARM                    §DES1            25
080400020405     C                   MOVEL     §COD1         VDPMAG
080500020405     C*
080600020405     C                   END                                                    <-- 1 ---
080700020405     C*
080800020405     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
080900020405     C                   Z-ADD     1             J
081000020405     C                   SETOFF                                           28
081100020405     C                   MOVE      VDPMAG        KORFIL
081200020405     C     KORFIL        LOOKUP    FIL(J)                                 28
081300020405IF  3C     *IN28         IFEQ      *OFF
081400020405     C                   MOVEL     '1'           ERRL00
081500020405     C                   MOVEL     MSG(8)        DSMSMS
081600020405     C                   SETON                                        81
081700020405     C                   END
081800020405     C*
081900020405     C                   ENDSR
082000000000     C*================================================================
082100000000     C* LANPGM - LANCIA UN PROGRAMMA
082200000000     C*================================================================
082300000000     C     LANPGM        BEGSR
082400000000     C*
082500000000     C* IMPOSTO PARAMETRI PER IL PGM
082600000000     C                   MOVEL     '0'           DSUF03                         PREMUTO F03
082700000000     C                   MOVEL     '0'           DSUERR                         '1'=ERRORE
082800000000     C                   MOVEL     *BLANKS       DSUMSG                         MESSAGGIO
082900000000     C                   MOVE      WOPZ1         DSUOP1                         *OPZIONE LIV.PGM
083000000000     C                   Z-ADD     VIDTIP        DSUTIP                         TIPO
083100000000     C                   Z-ADD     VIDFIL        DSUMAG                         MAGAZZINO
083200000000     C*
083300000000     C* LANCIO PROGRAMMA
083400000000     C                   MOVEL     DSUNIT        KPJBU
083500000000     C                   CALL      WPGM
083600000000     C                   PARM                    KPJBA
083700000000     C                   MOVEL     KPJBU         DSUNIT
083800000000     C*
083900000000     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
084000000000     C     DSUF03        IFEQ      '1'
084100000000     C                   MOVEL     '1'           F03L00
084200000000     C                   MOVEL     '1'           ERRL00
084300000000     C                   ELSE
084400000000     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
084500000000     C                   MOVEL     DSUMSG        DSMSMS
084600000000     C                   END
084700000000     C*
084800000000     C                   ENDSR
084900000000     C*================================================================
085000000000     C* CHKFOP - CONTROLLA TIPO GESTIONE E IMPOSTA FUNZ/OPZ
085100000000     C*================================================================
085200000000     C     CHKFOP        BEGSR
085300000000     C*
085400000000     C* CONTROLLO CHE VI SIA UNA OPZIONE DI PROCEDURA VALIDA IN INPUT
085500000000     C* ALTRIMENTI FORZO INTERROGAZIONE
085600000000     C     DSUOP0        IFNE      ' 2'                                         *GESTIONE
085700000000     C     DSUOP0        ANDNE     ' 5'                                         *INTERROGAZIONE
085800000000     C                   MOVE      ' 5'          DSUOP0
085900000000     C                   END
086000000000     C*
086100000000     C* SE L'UTENTE E' ENTRATO IN GESTIONE LO ABILITO A TUTTE LE FUNZIONI
086200000000     C     DSUOP0        IFEQ      ' 2'
086300000000     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
086400000000     C                   MOVEL     'S'           OPDATT
086500000000     C                   MOVEL     OPDRIG        OPD(2)
086600000000     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
086700000000     C                   MOVEL     'S'           OPDATT
086800000000     C                   MOVEL     OPDRIG        OPD(4)
086900000000     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
087000000000     C                   MOVEL     'S'           OPDATT
087100000000     C                   MOVEL     OPDRIG        OPD(5)
087200000000     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
087300000000     C                   MOVEL     'S'           OPDATT
087400000000     C                   MOVEL     OPDRIG        OPD(7)
087500000000     C                   MOVEL     OPD(8)        OPDRIG                         *GEST.MOVIMENTI
087600000000     C                   MOVEL     'S'           OPDATT
087700000000     C                   MOVEL     OPDRIG        OPD(8)
087800000000     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
087900000000     C                   MOVEL     'S'           FUDATT
088000000000     C                   MOVEL     FUDRIG        FUD(6)
088100000000     C                   END
088200000000     C*
088300000000     C* SE IL PGM E' STATO LANCIATO IN INTERROGAZIONE ABILITO SOLO QUELLA
088400000000     C     DSUOP0        IFEQ      ' 5'
088500000000     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
088600000000     C                   MOVEL     'S'           OPDATT
088700000000     C                   MOVEL     OPDRIG        OPD(5)
088800000000     C                   MOVEL     OPD(8)        OPDRIG                         *GEST.MOVIMENTI
088900000000     C                   MOVEL     'S'           OPDATT
089000000000     C                   MOVEL     OPDRIG        OPD(8)
089100000000     C                   END
089200000000     C*
089300000000     C* LANCIO ROUTINE PER VERIFICARE SE
089400000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
089500000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
089600020709
089700020709     c     *dtaara       define    §azute        azuteds
089800020709     c     *dtaara       define    §datiute      ddatiute
089900020709
090000020709     c                   in(E)     *dtaara
090100020709     c                   if        %Error or rsut = *blanks
090200020709     c                   clear                   tibs34ds
090300020709     c                   call      'TIBS34R'
090400020709     c                   parm                    Tibs34Ds
090500020709     c                   in        *dtaara
090600020709     c                   endif
090700020717
090800020717      * Controllo il profilo utente
090900020717     c                   ExSr      CtrUte
091000000000     C*
091100000000     C* SE SONO IN FILIALE NON CONCEDO CANCELLAZIONE,INSERIMENTO,RIPRISTINO
091200020709     c                   If        DutLpo <> 'S'
091300000000     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
091400000000     C                   MOVEL     'N'           OPDATT
091500000000     C                   MOVEL     OPDRIG        OPD(4)
091600000000     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
091700000000     C                   MOVEL     'N'           OPDATT
091800000000     C                   MOVEL     OPDRIG        OPD(7)
091900000000     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
092000000000     C                   MOVEL     'N'           FUDATT
092100000000     C                   MOVEL     FUDRIG        FUD(6)
092200000000     C                   END
092300000000     C*
092400000000     C* IMPOSTO INDICE PER EMISSIONE OPZIONI A VIDEO
092500000000     C                   Z-ADD     0             OPZGRU            5 0
092600000000     C*
092700000000     C* GESTIONE OPZIONI A VIDEO
092800000000     C                   EXSR      OPZGES
092900000000     C*
093000000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
093100000000     C                   Z-ADD     0             FUNGRU            5 0
093200000000     C*
093300000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
093400000000     C                   EXSR      FUNGES
093500000000     C*
093600000000     C                   ENDSR
093700020717      *------------------------------------------------------------------------*
093800020717      *  CTRUTE - ROUTINE CONTROLLO UTENTE
093900020717      *------------------------------------------------------------------------*
094000020717     c     CtrUte        BegSr
094100020717
094200020717     c                   Eval      *In76 = *Off
094300020717
094400020717      * Controllo se il profilo è ok da tibs34
094500020717     c                   If        DutErr = 'E'
094600020717     c                   Eval      DsmSms = DutMsg
094700020717     c                   Eval      *In76 = *On
094800020717     c                   EndIf
094900020717
095000020717      * Controllo se il profilo di sede
095100020717     c                   If        DutLpo <> 'S'
095200020717     c                   Eval      DsmSms = Msg(9)
095300020717     c                   Eval      *In76 = *On
095400020717     c                   EndIf
095500020717
095600020717     c                   endSr
095700000000     C*================================================================
095800000000     C* *INZSR - ROUTINE INIZIALE
095900000000     C*================================================================
096000000000     C     *INZSR        BEGSR
096100000000     C*
096200000000     C* RICEVIMENTO PARAMETRI DALL'AZIONE (' 2'=GEST.,' 5'=INTERR.)
096300000000     C     *ENTRY        PLIST
096400000000     C                   PARM                    KPJBA
096500000000     C                   MOVEL     KPJBU         DSUNIT
096600000000     C*
096700000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
096800000000     C     XMSGPA        PLIST
096900000000     C                   PARM                    DSMSMS
097000000000     C                   PARM                    DSMSPG
097100000000     C                   PARM                    DSMSKE
097200000000     C                   PARM                    DSMSER
097300000000     C* DEFINIZIONE CAMPI
097400000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
097500000000     C     *LIKE         DEFINE    DOTTIP        KDOTIP                         *UNDOT00F
097600950125     C     *LIKE         DEFINE    UNAMAG        KANMAG                         *UNANA00F
097700950125     C     *LIKE         DEFINE    UNATIP        KANTIP
097800000000     C*
097900000000     C* CHIAVE LETTURA UNDOT01L - PARZIALE
098000000000     C     KUNDOT        KLIST
098100020322     C                   KFLD                    KDOTIP
098200020322     C                   KFLD                    KDOfil
098300950124     C*
098400020322     C* CHIAVE LETTURA UNANA09L - PARZIALE
098500950124     C     KEYANA        KLIST
098600020322     C                   KFLD                    KANTIP
098700950124     C                   KFLD                    KANMAG
098800000000     C*
098900020528     C* CHIAVE LETTURA AZORG21l/azorg01l - COMPLETA
099000000000     C     KEYORG        KLIST
099100000000     C                   KFLD                    KORFIL
099200000000     C*
099300000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
099400000000     C                   CALL      'XKEY'
099500000000     C                   PARM                    DSKEY
099600000000     C*
099700000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
099800000000     C     *LIKE         DEFINE    SFLNBR        SFLPAG
099900000000     C     *LIKE         DEFINE    SFLNBR        SFLEOF
100000000000     C     *LIKE         DEFINE    SFLNBR        SFLERR
100100000000     C     *LIKE         DEFINE    SFLNBR        SFLULT
100200000000     C     *LIKE         DEFINE    SFLNBR        WFLULT
100300000000     C     *LIKE         DEFINE    SFLNBR        MSGULT
100400000000     C     *LIKE         DEFINE    SFLNBR        SFLRIG
100500000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
100600000000     C     *LIKE         DEFINE    VDPTIP        DEPTIP
100700000000     C*
100800000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
100900000000     C*   -NOME PGM CHIAMANTE
101000000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
101100000000     C                   MOVEL     'TNSDUCR'     DSMSPG
101200000000     C                   MOVEL     'TNSDUCR'     MSGQUE
101300000000     C                   MOVEL     *BLANKS       MSGKEY
101400000000     C                   MOVEL     *BLANKS       DSMSMS
101500000000     C                   MOVEL     *BLANKS       DSMSKE
101600000000     C                   Z-ADD     0             DSMSER
101700000000     C*
101800000000     C* INIZIALIZZO SWITCH DI LIVELLO 0 (PGM)
101900000000     C                   MOVEL     '0'           F12L00            1
102000000000     C                   MOVEL     '0'           F03L00            1
102100000000     C                   MOVEL     '0'           FINL00            1
102200000000     C                   MOVEL     '0'           ERRL00            1
102300000000     C                   MOVEL     '0'           FLFINE            1
102400950124     C                   MOVEL     '0'           $FIN2             1
102500000000     C*
102600000000     C* IMPOSTO DEPOSITI PER CONFRONTI
102700000000     C                   MOVEL     *BLANKS       DEPTIP
102800020405     c                   clear                   depdot
102900020405     c                   clear                   depmag
103000020322     c                   clear                   wuntip
103100020322     c                   clear                   wunmag
103200000000     C*
103300000000     C* IMPOSTO TIPO DI RICERCA DI DEFAULT --> 'TIP'
103400000000     C                   MOVEL     'TIP'         WTIPRI
103500000000     C                   SETON                                        90
103600000000     C*
103700000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
103800000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
103900020405     C                   Z-ADD     11            SFLPAG
104000000000     C*
104100000000     C* IMPOSTO LE OPZIONI E LE FUNZIONI A VIDEO
104200000000     C                   EXSR      CHKFOP
104300000000     C*
104400000000     C* IMPOSTO INTERROGAZIONE/GESTIONE NELLA TESTATA DELLA VIDEATA
104500000000     C     DSUOP0        IFEQ      ' 2'
104600000000     C                   MOVEL     '    GEST'    VIDING
104700000000     C                   MOVE      'IONE    '    VIDING
104800000000     C                   ELSE
104900000000     C                   MOVEL     ' INTERRO'    VIDING
105000000000     C                   MOVE      'GAZIONE '    VIDING
105100000000     C                   END
105200000000     C*
105300000000     C* CARICA ORGANIGRAMMA
105400000000     C                   EXSR      CARORG
105500000000     C*
105600000000     C* CARICA TABELLE
105700000000     C                   EXSR      CARTAB
105800000000     C*
105900000000     C                   ENDSR
106000000000     C*================================================================
106100000000     C* CARORG - CARICA ORGANIGRAMMA
106200000000     C*================================================================
106300000000     C     CARORG        BEGSR
106400000000     C*
106500000000     C                   Z-ADD     0             J                 3 0          *INDICE SCHIERA
106600000000     C                   Z-ADD     *ZEROS        KORFIL
106700020528     C     KEYORG        SETLL     AZORG21l                           99        *FINE FILE
106800020528     C  N99              READ      AZORG21l                               99
106900000000     C*
107000000000     C     *IN99         DOWEQ     *OFF
107100000000     C                   ADD       1             J
107200000000     C                   Z-ADD     ORGFIL        FIL(J)
107300000000     C                   MOVEL     ORGDES        DFIL(J)
107400000000     C*
107500020528     C                   READ      AZORG21l                               99
107600000000     C                   ENDDO
107700020528
107800020528     c     keyorg        setll     azorg01l
107900020528     c                   do        *hival
108000020528     c                   read      azorg01l
108100020528     c                   if        %eof(azorg01l)
108200020528     c                   leave
108300020528     c                   endif
108400020528     c                   z-add     1             j
108500020528     c     orgfil        lookup    fil(j)                                 99
108600020528     c                   if        not *in99
108700020528     c                   z-add     1             j
108800020528     c     *zeros        lookup    fil(j)                                 99
108900020528     c                   if        *in99
109000020528     c                   movel     orgfil        fil(j)
109100020528     c                   movel     orgdes        dfil(j)
109200020528     c                   endif
109300020528     c                   endif
109400020528     c                   enddo
109500000000     C*
109600000000     C                   ENDSR
109700000000     C*================================================================
109800000000     C* CARTAB - CARICA TABELLE
109900000000     C*================================================================
110000000000     C     CARTAB        BEGSR
110100000000     C*--------------------
110200000000     C* TIPI UNITA'
110300000000     C*--------------------
110400000000     C                   Z-ADD     0             J                 3 0          *INDICE SCHIERA
110500020322     C                   MOVEL     'UNT'         tbecod
110600020322     C     tbecod        SETLL     tntbe01l                           99        *FINE FILE
110700020322     C  N99tbecod        READE     tntbe01l                               99
110800000000     C*
110900000000     C     *IN99         DOWEQ     *OFF
111000020322     C     TBeatb        IFEQ      *BLANKS
111100000000     C                   ADD       1             J
111200020322     C                   MOVEL     TBeke1        UNI(J)
111300020322     C                   MOVEL     tbeuni        DUNI(J)
111400000000     C                   ENDIF
111500000000     C*
111600020322     C  N99tbecod        READE     tntbe01l                               99
111700000000     C                   ENDDO
111800050603     C*--------------------
111900050603     C* TIPI RAGGRUPPAMENTO 2° LIVELLO
112000050603     C*--------------------
112100050603     C                   Z-ADD     0             Jrg2              3 0          *INDICE SCHIERA
112200050603     C                   MOVEL     'UNR'         tbecod
112300050603     C     tbecod        SETLL     tntbe01l                           99        *FINE FILE
112400050603     C  N99tbecod        READE     tntbe01l                               99
112500050603     C*
112600050603     C     *IN99         DOWEQ     *OFF
112700050603     C     TBeatb        IFEQ      *BLANKS
112800050603     C                   ADD       1             Jrg2
112900050603     C                   MOVEL     TBeke1        RG2(Jrg2)
113000050603     C                   MOVEL     tbeuni        DRG2(Jrg2)
113100050603     C                   ENDIF
113200050603     C*
113300050603     C  N99tbecod        READE     tntbe01l                               99
113400050603     C                   ENDDO
113500000000     C*
113600000000     C                   ENDSR
113700000000     C*================================================================
113800000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
113900000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
114000000000     C* LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
114100000000     C* STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
114200000000     C* NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
114300000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
114400000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
114500000000     C*================================================================
114600000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA
114700000000     C*          CARICARE
114800000000     C*================================================================
114900000000     C     POSSFL        BEGSR
115000000000     C*
115100000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
115200000000     C                   EXSR      CLRSFL
115300000000     C*
115400000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
115500000000     C                   EXSR      SETFIL
115600000000     C*
115700000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
115800000000     C     SFLEOF        IFEQ      0
115900000000     C                   EXSR      CARSFL
116000000000     C                   END
116100000000     C*
116200000000     C                   ENDSR
116300000000     C*================================================================
116400000000     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
116500000000     C*================================================================
116600000000     C     ENTATT        BEGSR
116700000000     C*
116800000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
116900000000     C                   Z-ADD     0             OPZOK             1 0
117000000000     C*
117100000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
117200000000     C                   Z-ADD     0             SFLVAR            5 0
117300000000     C*
117400000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
117500000000     C                   Z-ADD     0             SFLERR
117600000000     C*
117700000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
117800000000     C                   Z-ADD     SFLULT        WFLULT
117900000000     C*
118000000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
118100000000     C                   READC     SDUCD05                                98
118200000000     C     *IN98         DOWEQ     '0'
118300000000     C*
118400000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
118500000000     C                   ADD       1             SFLVAR
118600000000     C*
118700000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
118800000000     C     OPZOK         IFEQ      0
118900000000     C*
119000000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
119100000000     C     OPZKEY        IFNE      *BLANKS
119200000000     C                   EXSR      OPZCON
119300000000     C                   END
119400000000     C*
119500000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
119600000000     C*            2. PULIZIA SCELTA IN SFL
119700000000     C     OPZOK         IFEQ      0
119800000000     C                   MOVEL     '0'           *IN41
119900000000     C                   MOVEL     *BLANKS       OPZKEY
120000000000     C                   ELSE
120100000000     C*
120200000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
120300000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
120400000000     C                   MOVEL     '1'           *IN41
120500000000     C                   Z-ADD     SFLULT        SFLERR
120600000000     C                   END
120700000000     C                   ELSE
120800000000     C*
120900000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
121000000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
121100000000     C                   MOVEL     '1'           *IN41
121200000000     C                   END                                                    OPZOK=0
121300000000     C*
121400000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
121500000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
121600000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
121700000000     C     OPZOK         IFNE      0
121800000000     C                   Z-ADD     SFLERR        SFLNBR
121900000000     C                   ELSE
122000000000     C                   Z-ADD     SFLULT        SFLNBR
122100000000     C                   END
122200000000     C*
122300000000     C* AGGIORNO RECORD SFL
122400000000     C                   UPDATE    SDUCD05
122500000000     C*
122600000000     C                   READC     SDUCD05                                98
122700000000     C                   END                                                    DO WHILE
122800000000     C*
122900000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
123000000000     C                   Z-ADD     WFLULT        SFLULT
123100000000     C*
123200000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
123300000000     C     SFLVAR        IFEQ      0
123400000000     C                   MOVEL     '1'           FINL00
123500000000     C                   END
123600000000     C*
123700000000     C                   ENDSR
123800000000     C*================================================================
123900000000     C* ELIMINAZIONE CONTENUTO SFL
124000000000     C*================================================================
124100000000     C     CLRSFL        BEGSR
124200000000     C*
124300000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
124400000000     C* (0=NON FINE FILE  1=FINE FILE)
124500000000     C                   Z-ADD     0             SFLEOF
124600000000     C*
124700000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
124800000000     C                   Z-ADD     0             SFLULT
124900000000     C*
125000000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
125100000000     C                   MOVEL     '1'           *IN40
125200000000     C                   WRITE     SDUCD06
125300000000     C                   MOVEL     '0'           *IN40
125400050603     C*
125500050603     C* CLEARO ANCHE LA SCHIERA D WRK X SCRITTURA RECORD DETTAGLIO SUB-FILE
125600050603     C                   CLEAR                   SKDUCRDET
125700000000     C*
125800000000     C                   ENDSR
125900000000     C*================================================================
126000000000     C* CARSFL - CARICAMENTO SFL
126100000000     C*================================================================
126200000000     C     CARSFL        BEGSR
126300000000     C*
126400000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
126500000000     C     SFLULT        IFEQ      0
126600020322     c                   clear                   totfil
126700020322     c                   clear                   wuntip
126800020322     c                   clear                   wunmag
126900020711     c                   Clear                   SavTip
127000020711     c                   Clear                   SavMag
127100020711     c                   Clear                   SavAtb
127200020711     c                   Clear                   SavQta
127300000000     C                   EXSR      SETFIL
127400000000     C                   END
127500000000     C*
127600000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
127700000000     C                   Z-ADD     SFLULT        WFLULT
127800000000     C*
127900000000     C* CICLO:
128000000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
128100000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
128200000000     C                   Z-ADD     1             SFLRIG
128300050603     C***  SFLRIG        DOWLE     SFLPAG
128400050603     C***  SFLEOF        ANDEQ     0
128500050603     C                   DOW       SFLEOF = 0
128600050603     C***                DOW       SFLRIG+jSkDUCR <= SFLPAG AND SFLEOF = 0
128700020326
128800020326     c                   eval      wnosfl = *off
128900020326
129000000000     C*
129100000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
129200000000     C                   MOVEL     *BLANKS       OPZKEY
129300020326
129400020322      * emetto solo a rottura di magazzino
129500020711if  2c                   If        SavMag <> WunMag or SavTip <> WunTip
129600020711if  3c                   if        wunmag > *zeros
129700000000     C*
129800000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
129900050603     C                   EXSR      IRESFL
130000020326
130100020322     C                   Z-ADD     *ZEROS        TOTFIL            7 0
130200020711     c                   Clear                   SavAtb
130300020711     c                   Clear                   SavQta
130400020711
130500020711     c                   If        *In01 or *In03
130600020711     c                   eval      wunmag = unamag
130700020711     c                   eval      wuntip = unatip
130800020711     c                   EndIf
130900020711     c                   If        *In02 or *In04
131000020711     c                   eval      wunmag = dotfil
131100020711     c                   eval      wuntip = dottip
131200020711     c                   EndIf
131300020326
131400020326      * controllo se devo scrivere il record nel subfile
131500020326     c                   exsr      ctrsfl
131600020711     c                   Eval      WokUni = *Off
131700020711     c                   Eval      WokDot = *Off
131800020326
131900020711if  4c                   if        wnosfl = *off
132000000000     C*
132100000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
132200000000     C                   ADD       1             SFLULT
132300000000     C*
132400000000     C* SCRITTURA RECORD SFL
132500000000     C                   WRITE     SDUCD05
132600000000     C                   ADD       1             SFLRIG
132700050603     C*
132800050603     C* SCRITTURA RECORD SFL - DETTAGLI
132900050603     C                   EXSR      WRISFLDET
133000050603     C*
133100020711e   4c                   endif
133200020322
133300020711     c                   If        *In01 or *In03
133400020322     C                   ADD       1             TOTFIL
133500020711     c                   Eval      WokUni = *On
133600020711     c                   EndIf
133700020711e   3c                   EndIf
133800020711     c                   If        *In02 or *In04
133900020711     C                   Eval      SavAtb = DotAtb
134000020711     C                   Eval      SavQta = DotQta
134100020711     c                   Eval      WokDot = *On
134200020711     c                   EndIf
134300020322
134400020322      * conto le unità presenti in magazzino
134500020711x   2c                   else
134600020711     c                   If        *In01 or *In03
134700020711     C                   ADD       1             TOTFIL
134800020711     c                   Eval      WokUni = *On
134900020711     c                   EndIf
135000020711     c                   If        *In02 or *In04
135100020711     C                   Eval      SavAtb = DotAtb
135200020711     C                   Eval      SavQta = DotQta
135300020711     c                   Eval      WokDot = *On
135400020711     c                   EndIf
135500020711e   2c                   EndIf
135600000000     C*
135700000000     C* LETTURA FILE DA CARICARE
135800000000     C                   EXSR      LETFIL
135900050603     C*
136000050603     C* In ogni caso emetto il dettaglio unità x raggruppamento 2° livello
136100050603     C                   EXSR      IRESFLDET
136200050603     C*
136300050603     C                   ENDDO                                                  DO WHILE
136400020322
136500020322      * emetto l'ultimo record del file
136600020322     c                   if        wunmag > *zeros and sfleof = 1
136700020711     c                             and not *in42
136800020322     C*
136900020322     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
137000020322     C                   EXSR      IRESFL
137100020326
137200020326      * controllo se devo scrivere il record nel subfile
137300020326     c                   exsr      ctrsfl
137400020326
137500020326     c                   if        wnosfl = *off
137600020322     C*
137700020322     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
137800020322     C                   ADD       1             SFLULT
137900020322     C*
138000020322     C* SCRITTURA RECORD SFL
138100020322     C                   WRITE     SDUCD05
138200020322     C                   ADD       1             SFLRIG
138300050603     C*
138400050603     C* SCRITTURA RECORD SFL - DETTAGLI
138500050603     C                   EXSR      WRISFLDET
138600020326     c                   endif
138700020322     c                   endif
138800000000     C*
138900000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
139000000000     C     WFLULT        IFNE      SFLULT
139100000000     C                   Z-ADD     WFLULT        SFLNBR
139200000000     C                   ADD       1             SFLNBR
139300000000     C                   END
139400000000     C*
139500000000     C                   ENDSR
139600000000     C*================================================================
139700000000     C* EMISSIONE SUBFILE A VIDEO
139800000000     C*================================================================
139900000000     C     OUTSFL        BEGSR
140000000000     C*
140100000000     C* WRITE RECORD TESTATA VIDEO
140200000000     C                   WRITE     SDUCD01
140300000000     C*
140400000000     C* VISUALIZZAZIONE MESSAGGIO
140500000000     C                   EXSR      SNDMSG
140600000000     C*
140700000000     C* WRITE RECORD OPZIONI VIDEO
140800000000     C                   WRITE     SDUCD02
140900000000     C*
141000000000     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
141100000000     C                   WRITE     SDUCD03
141200000000     C*
141300000000     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
141400000000     C                   WRITE     SDUCD04
141500000000     C*
141600000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
141700000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
141800000000     C     SFLEOF        IFEQ      0
141900000000     C                   MOVEL     '0'           *IN42
142000000000     C                   ELSE
142100000000     C*
142200000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
142300000000     C                   MOVEL     '1'           *IN42
142400000000     C                   END
142500000000     C*
142600000000     C* WRITE RECORD TASTI FUNZIONALI
142700000000     C                   WRITE     SDUCD09
142800000000     C*
142900000000     C* CASO DI SFL VUOTO
143000000000     C     SFLULT        IFLE      0
143100000000     C*
143200000000     C*    - WRITE RECORD VUOTO
143300000000     C                   WRITE     SDUCD0A
143400000000     C                   READ      SDUCD0A                                99
143500000000     C*
143600000000     C                   ELSE
143700000000     C*
143800000000     C* CASO DI SFL CON RECORDS
143900000000     C*
144000000000     C*    - WRITE RECORD CONTROLLO SFL
144100000000     C                   WRITE     SDUCD06
144200000000     C                   READ      SDUCD06                                99
144300000000     C                   END
144400000000     C*
144500000000     C* READ RECORD PER POSIZIONAMEMTO VIDEO
144600000000     C                   READ      SDUCD03                                99
144700000000     C*
144800000000     C                   ENDSR
144900000000     C*================================================================
145000000000     C* SNDMSG - MANDA MESSAGGIO
145100000000     C*================================================================
145200000000     C     SNDMSG        BEGSR
145300000000     C*
145400000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
145500000000     C                   MOVEL     '1'           *IN40
145600000000     C                   WRITE     SDUCDMC
145700000000     C                   MOVEL     '0'           *IN40
145800000000     C*
145900000000     C* VISUALIZZAZIONE MESSAGGIO
146000000000     C                   Z-ADD     0             MSGULT
146100000000     C                   CALL      'XMSG'        XMSGPA
146200000000     C     DSMSER        IFEQ      0
146300000000     C                   MOVEL     DSMSKE        MSGKEY
146400000000     C                   END
146500000000     C                   Z-ADD     1             MSGULT
146600000000     C                   WRITE     SDUCDMS                                      SFL MESS
146700000000     C                   WRITE     SDUCDMC                                      CONTR MESS
146800000000     C*
146900000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
147000000000     C                   MOVEL     *BLANKS       DSMSMS
147100000000     C*
147200000000     C                   ENDSR
147300000000     C*================================================================
147400000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
147500000000     C*================================================================
147600000000     C     FUNCON        BEGSR
147700000000     C*
147800000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
147900000000     C                   EXSR      FUNVAR
148000000000     C*
148100000000     C* PULISCO CAMPI DI LAVORO
148200000000     C                   Z-ADD     0             I                 5 0
148300000000     C                   Z-ADD     0             W                 5 0
148400000000     C*
148500000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
148600000000     C     FUNKEY        IFNE      *BLANKS
148700000000     C     1             DO        24            I
148800000000     C                   MOVEL     FUD(I)        FUDRIG
148900000000     C*
149000000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
149100000000     C     FUNKEY        IFEQ      FUDFUN
149200000000     C     FUDATT        ANDEQ     'S'
149300000000     C                   Z-ADD     I             W
149400000000     C                   END
149500000000     C                   END                                                    DO WHILE
149600000000     C*
149700000000     C* FUNZIONE NON ATTIVA (ERRORE)
149800000000     C     W             IFEQ      0
149900000000     C*
150000000000     C* EMISSIONE MESSAGGIO DI ERRORE
150100000000     C                   MOVEL     MSG(2)        DSMSMS
150200000000     C                   END                                                    W=0
150300000000     C                   END                                                    FUNKEY NE *BLANKS
150400000000     C*
150500000000     C* FUNZIONE ATTIVA (PROCEDI ...)
150600000000     C     W             IFNE      0
150700000000     C     FUNKEY        OREQ      *BLANKS
150800000000     C*
150900000000     C     FUNKEY        IFEQ      *BLANKS
151000000000     C                   EXSR      ENTGES
151100000000     C                   END
151200000000     C     FUNKEY        IFEQ      '03'
151300000000     C                   EXSR      F03GES
151400000000     C                   END
151500000000     C     FUNKEY        IFEQ      '05'
151600000000     C                   EXSR      F05GES
151700000000     C                   END
151800000000     C     FUNKEY        IFEQ      '06'
151900000000     C                   EXSR      F06GES
152000000000     C                   END
152100000000     C     FUNKEY        IFEQ      '12'
152200000000     C                   EXSR      F12GES
152300000000     C                   END
152400000000     C     FUNKEY        IFEQ      '17'
152500000000     C                   EXSR      F17GES
152600000000     C                   END
152700000000     C     FUNKEY        IFEQ      '18'
152800000000     C                   EXSR      F18GES
152900000000     C                   END
153000000000     C     FUNKEY        IFEQ      '21'
153100000000     C                   EXSR      F21GES
153200000000     C                   END
153300000000     C     FUNKEY        IFEQ      '22'
153400000000     C                   EXSR      F22GES
153500000000     C                   END
153600000000     C     FUNKEY        IFEQ      '23'
153700000000     C                   EXSR      F23GES
153800000000     C                   END
153900000000     C     FUNKEY        IFEQ      '24'
154000000000     C                   EXSR      F24GES
154100000000     C                   END
154200000000     C     FUNKEY        IFEQ      'RU'                                         ROLLUP
154300000000     C                   EXSR      RUPGES
154400000000     C                   END
154500000000     C     FUNKEY        IFEQ      'HP'                                         HELP
154600000000     C                   EXSR      HELGES
154700000000     C                   END
154800000000     C*
154900000000     C                   END
155000000000     C*
155100000000     C                   ENDSR
155200000000     C*================================================================
155300000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
155400000000     C*================================================================
155500000000     C     FUNVAR        BEGSR
155600000000     C*
155700000000     C* INIZIALIZZA VARIABILE -FUNKEY-
155800000000     C*
155900000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
156000000000     C     KEY           IFEQ      ENTER
156100000000     C                   MOVEL     *BLANKS       FUNKEY
156200000000     C                   END
156300000000     C     KEY           IFEQ      F1
156400000000     C                   MOVEL     '01'          FUNKEY
156500000000     C                   END
156600000000     C     KEY           IFEQ      F2
156700000000     C                   MOVEL     '02'          FUNKEY
156800000000     C                   END
156900000000     C     KEY           IFEQ      F3
157000000000     C                   MOVEL     '03'          FUNKEY
157100000000     C                   END
157200000000     C     KEY           IFEQ      F4
157300000000     C                   MOVEL     '04'          FUNKEY
157400000000     C                   END
157500000000     C     KEY           IFEQ      F5
157600000000     C                   MOVEL     '05'          FUNKEY
157700000000     C                   END
157800000000     C     KEY           IFEQ      F6
157900000000     C                   MOVEL     '06'          FUNKEY
158000000000     C                   END
158100000000     C     KEY           IFEQ      F7
158200000000     C                   MOVEL     '07'          FUNKEY
158300000000     C                   END
158400000000     C     KEY           IFEQ      F8
158500000000     C                   MOVEL     '08'          FUNKEY
158600000000     C                   END
158700000000     C     KEY           IFEQ      F9
158800000000     C                   MOVEL     '09'          FUNKEY
158900000000     C                   END
159000000000     C     KEY           IFEQ      F10
159100000000     C                   MOVEL     '10'          FUNKEY
159200000000     C                   END
159300000000     C     KEY           IFEQ      F11
159400000000     C                   MOVEL     '11'          FUNKEY
159500000000     C                   END
159600000000     C     KEY           IFEQ      F12
159700000000     C                   MOVEL     '12'          FUNKEY
159800000000     C                   END
159900000000     C     KEY           IFEQ      F13
160000000000     C                   MOVEL     '13'          FUNKEY
160100000000     C                   END
160200000000     C     KEY           IFEQ      F14
160300000000     C                   MOVEL     '14'          FUNKEY
160400000000     C                   END
160500000000     C     KEY           IFEQ      F15
160600000000     C                   MOVEL     '15'          FUNKEY
160700000000     C                   END
160800000000     C     KEY           IFEQ      F16
160900000000     C                   MOVEL     '16'          FUNKEY
161000000000     C                   END
161100000000     C     KEY           IFEQ      F17
161200000000     C                   MOVEL     '17'          FUNKEY
161300000000     C                   END
161400000000     C     KEY           IFEQ      F18
161500000000     C                   MOVEL     '18'          FUNKEY
161600000000     C                   END
161700000000     C     KEY           IFEQ      F19
161800000000     C                   MOVEL     '19'          FUNKEY
161900000000     C                   END
162000000000     C     KEY           IFEQ      F20
162100000000     C                   MOVEL     '20'          FUNKEY
162200000000     C                   END
162300000000     C     KEY           IFEQ      F21
162400000000     C                   MOVEL     '21'          FUNKEY
162500000000     C                   END
162600000000     C     KEY           IFEQ      F22
162700000000     C                   MOVEL     '22'          FUNKEY
162800000000     C                   END
162900000000     C     KEY           IFEQ      F23
163000000000     C                   MOVEL     '23'          FUNKEY
163100000000     C                   END
163200000000     C     KEY           IFEQ      F24
163300000000     C                   MOVEL     '24'          FUNKEY
163400000000     C                   END
163500000000     C     KEY           IFEQ      ROLLUP
163600000000     C                   MOVEL     'RU'          FUNKEY
163700000000     C                   END
163800000000     C     KEY           IFEQ      HELP
163900000000     C                   MOVEL     'HP'          FUNKEY
164000000000     C                   END
164100000000     C*
164200000000     C                   ENDSR
164300000000     C*================================================================
164400000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
164500000000     C*================================================================
164600000000     C     FUNGES        BEGSR
164700000000     C*
164800000000     C* PULISCO CAMPI DI LAVORO
164900000000     C                   Z-ADD     0             W                 5 0
165000000000     C*
165100000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
165200000000     C* E CARICO TABELLA FUNZIONI ATTIVE
165300000000     C     1             DO        24            I
165400000000     C                   MOVEL     FUD(I)        FUDRIG
165500000000     C*
165600000000     C* PULIZIA FUNZIONI PRECEDENTI
165700000000     C                   MOVEL     *BLANKS       FUV(I)
165800000000     C*
165900000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
166000000000     C* (SALTA F23 E F24)
166100000000     C     FUDATT        IFEQ      'S'
166200000000     C     FUDFUN        ANDNE     '23'
166300000000     C     FUDFUN        ANDNE     '24'
166400000000     C                   ADD       1             W
166500000000     C                   MOVEL     FUDDES        FUV(W)
166600000000     C                   END
166700000000     C                   END                                                    DO WHILE
166800000000     C*
166900000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
167000000000     C* ALTRIMENTI DISATTIVO F24
167100000000     C     1             DO        24            I
167200000000     C                   MOVEL     FUD(I)        FUDRIG
167300000000     C*
167400000000     C     FUDFUN        IFEQ      '24'
167500000000     C     W             IFGT      6
167600000000     C                   MOVEL     'S'           FUDATT
167700000000     C                   ELSE
167800000000     C                   MOVEL     *BLANKS       FUDATT
167900000000     C                   END                                                    W GT 8
168000000000     C                   MOVEL     FUDRIG        FUD(I)
168100000000     C                   END
168200000000     C                   END                                                    DO WHILE
168300000000     C*
168400000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
168500000000     C     W             DIV       6             I
168600000000     C                   MVR                     Z
168700000000     C*
168800000000     C     Z             IFNE      0
168900000000     C     I             ADD       1             Z
169000000000     C                   ELSE
169100000000     C                   Z-ADD     I             Z
169200000000     C                   END
169300000000     C*
169400000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
169500000000     C                   ADD       1             FUNGRU
169600000000     C     FUNGRU        IFGT      Z
169700000000     C     FUNGRU        ORLT      1
169800000000     C                   Z-ADD     1             FUNGRU
169900000000     C                   END
170000000000     C*
170100000000     C* PRIMO GRUPPO DI 6 FUNZIONI
170200000000     C     FUNGRU        IFEQ      1
170300000000     C                   MOVEL     FUV(1)        FUN001
170400000000     C                   MOVEL     FUV(2)        FUN002
170500000000     C                   MOVEL     FUV(3)        FUN003
170600000000     C                   MOVEL     FUV(4)        FUN004
170700000000     C                   MOVEL     FUV(5)        FUN005
170800000000     C                   MOVEL     FUV(6)        FUN006
170900000000     C                   END
171000000000     C*
171100000000     C* SECONDO GRUPPO DI 6 FUNZIONI
171200000000     C     FUNGRU        IFEQ      2
171300000000     C                   MOVEL     FUV(7)        FUN001
171400000000     C                   MOVEL     FUV(8)        FUN002
171500000000     C                   MOVEL     FUV(9)        FUN003
171600000000     C                   MOVEL     FUV(10)       FUN004
171700000000     C                   MOVEL     FUV(11)       FUN005
171800000000     C                   MOVEL     FUV(12)       FUN006
171900000000     C                   END
172000000000     C*
172100000000     C* TERZO GRUPPO DI 6 FUNZIONI
172200000000     C     FUNGRU        IFEQ      3
172300000000     C                   MOVEL     FUV(13)       FUN001
172400000000     C                   MOVEL     FUV(14)       FUN002
172500000000     C                   MOVEL     FUV(15)       FUN003
172600000000     C                   MOVEL     FUV(16)       FUN004
172700000000     C                   MOVEL     FUV(17)       FUN005
172800000000     C                   MOVEL     FUV(18)       FUN006
172900000000     C                   END
173000000000     C*
173100000000     C* QUARTO GRUPPO DI 6 FUNZIONI
173200000000     C     FUNGRU        IFEQ      4
173300000000     C                   MOVEL     FUV(19)       FUN001
173400000000     C                   MOVEL     FUV(20)       FUN002
173500000000     C                   MOVEL     FUV(21)       FUN003
173600000000     C                   MOVEL     FUV(22)       FUN004
173700000000     C                   MOVEL     *BLANKS       FUN005
173800000000     C                   MOVEL     *BLANKS       FUN006
173900000000     C                   END
174000000000     C*
174100000000     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
174200000000     C                   MOVEL     *BLANKS       FUN007
174300000000     C                   MOVEL     *BLANKS       FUN008
174400000000     C*
174500000000     C     1             DO        24            I
174600000000     C                   MOVEL     FUD(I)        FUDRIG
174700000000     C     FUDFUN        IFEQ      '23'
174800000000     C     FUDATT        ANDEQ     'S'
174900000000     C                   MOVEL     FUDDES        FUN007
175000000000     C                   END
175100000000     C     FUDFUN        IFEQ      '24'
175200000000     C     FUDATT        ANDEQ     'S'
175300000000     C                   MOVEL     FUDDES        FUN008
175400000000     C                   END
175500000000     C                   END                                                    DO
175600000000     C*
175700000000     C                   ENDSR
175800000000     C*================================================================
175900000000     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
176000000000     C*================================================================
176100000000     C     OPZGES        BEGSR
176200000000     C*
176300000000     C* PULISCO CAMPI DI LAVORO
176400000000     C                   Z-ADD     0             W                 5 0
176500000000     C*
176600000000     C* RICERCO QUANTE OPZIONI SONO ATTIVE
176700000000     C* E CARICO TABELLA OPZIONI ATTIVE
176800000000     C     1             DO        24            I
176900000000     C                   MOVEL     OPD(I)        OPDRIG
177000000000     C*
177100000000     C* PULIZIA OPZIONI PRECEDENTI
177200000000     C                   MOVEL     *BLANKS       OPV(I)
177300000000     C*
177400000000     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
177500000000     C     OPDATT        IFEQ      'S'
177600000000     C                   ADD       1             W
177700000000     C                   MOVEL     OPDDES        OPV(W)
177800000000     C                   END
177900000000     C                   END                                                    DO WHILE
178000000000     C*
178100000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
178200000000     C* ALTRIMENTI DISATTIVO F23
178300000000     C     1             DO        24            I
178400000000     C                   MOVEL     FUD(I)        FUDRIG
178500000000     C*
178600000000     C     FUDFUN        IFEQ      '23'
178700000000     C     W             IFGT      8
178800000000     C                   MOVEL     'S'           FUDATT
178900000000     C                   ELSE
179000000000     C                   MOVEL     *BLANKS       FUDATT
179100000000     C                   END                                                    W GT 8
179200000000     C                   MOVEL     FUDRIG        FUD(I)
179300000000     C                   END
179400000000     C                   END                                                    DO WHILE
179500000000     C*
179600000000     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
179700000000     C     W             DIV       8             I
179800000000     C                   MVR                     Z                 5 0
179900000000     C*
180000000000     C     Z             IFNE      0
180100000000     C     I             ADD       1             Z
180200000000     C                   ELSE
180300000000     C                   Z-ADD     I             Z
180400000000     C                   END
180500000000     C*
180600000000     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
180700000000     C                   ADD       1             OPZGRU
180800000000     C     OPZGRU        IFGT      Z
180900000000     C     OPZGRU        ORLT      1
181000000000     C                   Z-ADD     1             OPZGRU
181100000000     C                   END
181200000000     C*
181300000000     C* PRIMO GRUPPO DI 8 OPZIONI
181400000000     C     OPZGRU        IFEQ      1
181500000000     C                   MOVEL     OPV(1)        OPZ001
181600000000     C                   MOVEL     OPV(2)        OPZ002
181700000000     C                   MOVEL     OPV(3)        OPZ003
181800000000     C                   MOVEL     OPV(4)        OPZ004
181900000000     C                   MOVEL     OPV(5)        OPZ005
182000000000     C                   MOVEL     OPV(6)        OPZ006
182100000000     C                   MOVEL     OPV(7)        OPZ007
182200000000     C                   MOVEL     OPV(8)        OPZ008
182300000000     C                   END
182400000000     C*
182500000000     C* SECONDO GRUPPO DI 8 OPZIONI
182600000000     C     OPZGRU        IFEQ      2
182700000000     C                   MOVEL     OPV(9)        OPZ001
182800000000     C                   MOVEL     OPV(10)       OPZ002
182900000000     C                   MOVEL     OPV(11)       OPZ003
183000000000     C                   MOVEL     OPV(12)       OPZ004
183100000000     C                   MOVEL     OPV(13)       OPZ005
183200000000     C                   MOVEL     OPV(14)       OPZ006
183300000000     C                   MOVEL     OPV(15)       OPZ007
183400000000     C                   MOVEL     OPV(16)       OPZ008
183500000000     C                   END
183600000000     C*
183700000000     C* TERZO GRUPPO DI 8 OPZIONI
183800000000     C     OPZGRU        IFEQ      3
183900000000     C                   MOVEL     OPV(17)       OPZ001
184000000000     C                   MOVEL     OPV(18)       OPZ002
184100000000     C                   MOVEL     OPV(19)       OPZ003
184200000000     C                   MOVEL     OPV(20)       OPZ004
184300000000     C                   MOVEL     OPV(21)       OPZ005
184400000000     C                   MOVEL     OPV(22)       OPZ006
184500000000     C                   MOVEL     OPV(23)       OPZ007
184600000000     C                   MOVEL     OPV(24)       OPZ008
184700000000     C                   END
184800000000     C                   ENDSR
184900000000     C*================================================================
185000000000     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
185100000000     C*================================================================
185200000000     C     OPZCON        BEGSR
185300000000     C*
185400000000     C* PULISCO CAMPI DI LAVORO
185500000000     C                   Z-ADD     0             I                 5 0
185600000000     C                   Z-ADD     0             W                 5 0
185700000000     C*
185800000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
185900000000     C     1             DO        24            I
186000000000     C                   MOVEL     OPD(I)        OPDRIG
186100000000     C*
186200000000     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
186300000000     C     OPZKEY        IFEQ      OPDOPZ
186400000000     C     OPZKEY        OREQ      OPDOP1
186500000000     C*
186600000000     C     OPDATT        IFEQ      'S'
186700000000     C                   MOVEL     OPDOPZ        OPZKEY
186800000000     C                   Z-ADD     I             W
186900000000     C                   END
187000000000     C                   END
187100000000     C*
187200000000     C                   END                                                    DO WHILE
187300000000     C*
187400000000     C* OPZIONE NON ATTIVA (ERRORE)
187500000000     C     W             IFEQ      0
187600000000     C*
187700000000     C* OPZIONE NON OK
187800000000     C                   Z-ADD     1             OPZOK
187900000000     C*
188000000000     C* EMISSIONE MESSAGGIO DI ERRORE
188100000000     C                   MOVEL     MSG(1)        DSMSMS
188200000000     C                   ELSE
188300000000     C*
188400000000     C* OPZIONE ATTIVA (PROCEDI ...)
188500000000     C*
188600000000     C     OPZKEY        IFEQ      ' 1'
188700000000     C                   EXSR      O01GES
188800000000     C                   END
188900000000     C     OPZKEY        IFEQ      ' 2'
189000000000     C                   EXSR      O02GES
189100000000     C                   END
189200000000     C     OPZKEY        IFEQ      ' 3'
189300000000     C                   EXSR      O03GES
189400000000     C                   END
189500000000     C     OPZKEY        IFEQ      ' 4'
189600000000     C                   EXSR      O04GES
189700000000     C                   END
189800000000     C     OPZKEY        IFEQ      ' 5'
189900000000     C                   EXSR      O05GES
190000000000     C                   END
190100000000     C     OPZKEY        IFEQ      ' 6'
190200000000     C                   EXSR      O06GES
190300000000     C                   END
190400000000     C     OPZKEY        IFEQ      ' 7'
190500000000     C                   EXSR      O07GES
190600000000     C                   END
190700000000     C*
190800000000     C                   END                                                    OPZ.NON ATTIVA
190900000000     C*
191000000000     C                   ENDSR
191100000000     C*================================================================
191200000000     C* NOTE PER COMPILAZIONE SCHIERE
191300000000     C*================================================================
191400000000     C* -MSG-
191500000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
191600000000     C* (NON TOCCARE MSG,1 E MSG,2
191700000000     C*
191800000000     C* -CMD-
191900000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
192000000000     C*
192100000000     C* -OPD-
192200000000     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
192300000000     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
192400000000     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
192500000000     C*    ESEMPIO: OPZIONI -1 -
192600000000     C*                     - 1-  SONO LA STESSA COSA
192700000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
192800000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
192900000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
193000000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
193100000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
193200000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
193300000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
193400000000     C*
193500000000     C* -FUD-
193600000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
193700000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
193800000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
193900000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
194000000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
194100000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
194200000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
194300000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
194400000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
194500000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
194600000000     C*
194700000000     C*================================================================
194800000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
194900000000TNSDUCR- Opzione non gestita.                                                  1
195000000000TNSDUCR- Tasto funzionale non gestito.                                         2
195100000000TNSDUCR- Non abilitato alla gestione di questa Unità.                          3
195200960927TNSDUCR- Dotazione P.operativo annullata. Consentito solo il ripristino        4
195300960927TNSDUCR- Dotazione P.operativo non annullata, impossibile ripristinare.        5
195400000000TNSDUCR- Accettato solo un parametro di inizio lista.                          6
195500000000TNSDUCR- Tipo unità non accettabile.                                           7
195600020405TNSDUCR- Magazzino ubicazione non accettabile.                                 8
195700020717TNSDUCR- Profilo non abilitato alla funzione                                   9
195800000000TNSDUCR- 10
195900000000TNSDUCR- 11
196000000000TNSDUCR- 12
196100000000TNSDUCR- 13
196200000000TNSDUCR- 14
196300000000TNSDUCR- 15
196400000000TNSDUCR- 16
196500000000TNSDUCR- 17
196600000000TNSDUCR- 18
196700000000TNSDUCR- 19
196800000000TNSDUCR- 20
196900000000** ======== SCHIERA: CMD  (COMANDI)                        ================
197000000000WRKSPLF
197100000000
197200000000
197300000000
197400000000
197500000000** ======== SCHIERA: OPD  OPZIONI GESTITE NEL PGM (ATTIVA=S) ==========
197600000000 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
197700000000 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
197800000000                   -3 -     (S/N)=N                        opz. 03¬
197900000000 4=Cancellazione   -4 -     (S/N)=N                        opz. 04¬
198000000000 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
198100000000                   -6 -     (S/N)=N                        opz. 06¬
198200000000 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
198300000000                   -8 -     (S/N)=N                        opz. 08¬
198400000000                   -9 -     (S/N)=N                        opz. 09¬
198500000000                   -  -     (S/N)=N                        opz. 10¬
198600000000                   -  -     (S/N)=N                        opz. 11¬
198700000000                   -  -     (S/N)=N                        opz. 12¬
198800000000                   -  -     (S/N)=N                        opz. 13¬
198900000000                   -  -     (S/N)=N                        opz. 14¬
199000000000                   -  -     (S/N)=N                        opz. 15¬
199100000000                   -  -     (S/N)=N                        opz. 16¬
199200000000                   -  -     (S/N)=N                        opz. 17¬
199300000000                   -  -     (S/N)=N                        opz. 18¬
199400000000                   -  -     (S/N)=N                        opz. 19¬
199500000000                   -  -     (S/N)=N                        opz. 20¬
199600000000                   -  -     (S/N)=N                        opz. 21¬
199700000000                   -  -     (S/N)=N                        opz. 22¬
199800000000                   -  -     (S/N)=N                        opz. 23¬
199900000000                   -  -     (S/N)=N                        opz. 24¬
200000000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
200100000000                   -        (S/N)=N                        funz.01¬
200200000000                   -        (S/N)=N                        funz.02¬
200300000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
200400000000                   -        (S/N)=N                        funz.04¬
200500971013F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
200600000000F06=Inserimento    -        (S/N)=N                        funz.06¬
200700000000                   -        (S/N)=N                        funz.07¬
200800000000                   -        (S/N)=N                        funz.08¬
200900000000                   -        (S/N)=N                        funz.09¬
201000000000 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.10¬
201100000000 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.11¬
201200000000F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
201300000000                   -        (S/N)=N                        funz.13¬
201400000000                   -        (S/N)=N                        funz.14¬
201500000000                   -        (S/N)=N                        funz.15¬
201600000000                   -        (S/N)=N                        funz.16¬
201700000000F17=Inizio dati    -        (S/N)=S  <== S-FISSO           funz.17¬
201800000000F18=Fine dati      -        (S/N)=S  <== S-FISSO           funz.18¬
201900000000                   -        (S/N)=N                        funz.19¬
202000000000                   -        (S/N)=N                        funz.20¬
202100020405F21=Stampa         -        (S/N)=S                        funz.21¬
202200941220F22=Gestione stampe-        (S/N)=S                        funz.22¬
202300000000F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
202400000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
