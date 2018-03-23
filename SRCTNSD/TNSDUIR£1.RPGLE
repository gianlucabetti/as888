000100020319     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020319      *------------------------------------------------------------------------*
000300020319      * GESTIONE UNITA' EDP - SCELTA
000400020319      *------------------------------------------------------------------------*
000500020319
000600020319      *------------------------------------------------------------------------*
000700020319      * DATA BASE
000800020319      *------------------------------------------------------------------------*
000900020517     FTNSDUID   CF   E             WORKSTN INFDS(DEVDS1)
001000020517     F                                     SFILE(SDUID05:SFLULT)
001100020517     F                                     SFILE(SDUIDMS:MSGULT)
001200020319     FUNANA02L  IF   E           K DISK
001300971015     F                                     RENAME(UNANA000:UNANA2)
001400020319     FUNANA07L  IF   E           K DISK
001500020319     F                                     RENAME(UNANA000:UNANA7)
001600020517     FUNANA10L  IF   E           K DISK
001700020517     F                                     RENAME(UNANA000:UNANA10)
001800020529     FAZORG21l  IF   E           K DISK
001900020529     fazorg01l  if   e           k disk
002000020319
002100020319      *------------------------------------------------------------------------*
002200020319      * SCHIERE
002300020319      *------------------------------------------------------------------------*
002400020319      * MESSAGGI
002500930127     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002600020319      * COMANDI
002700000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002800020319      * DESCRIZIONI OPZIONI
002900000000     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003000020319      * DESCRIZIONI TASTI FUNZIONALI
003100000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003200020319      * TABELLA OPZIONI DA CARICARE A VIDEO
003300000000     D OPV             S             19    DIM(24)
003400020319      * TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
003500000000     D FUV             S             19    DIM(24)
003600020523
003700020523     d  skpoabi        s              3  0 dim(500)
003800020319
003900020319      *------------------------------------------------------------------------*
004000020319      * INPUT
004100020319      *------------------------------------------------------------------------*
004200020319
004300020517     d deptip          s                   like(vdptip)
004400020517     d depmat          s                   like(vdpmat)
004500020517     d depric          s                   like(vdpric)
004600020517     d depksc          s                   like(vdpksc)
004700020517     d deprsc          s                   like(vdprsc)
004800020517
004900020517     d paxccc          s                   LIKE(ACOkcc)
005000020517     d paxdmt          s                   LIKE(ACOrag)
005100020517     d PAXdut          s             30
005200020517     d PAXdit          s              3
005300020517     d PAXflr          s             90
005400020517     d PAXsta          s              1  0
005500020517
005600020319     d wtipri          s              3
005700020325     d wfil            s              3
005800020325     d wksc            s              3  0
005900020319     d wnosfl          s              1    inz('0')
006000020410     d wprimo          s              1    inz('0')
006100020325     d xx              s              3  0
006200020517     d ii              s              3  0
006300020516     d wabi            s                   like(uteaut)
006400020523     d wmag            s              3  0
006500020319
006600020319      * RIDEFINIZIONE SCHIERA -OPD-
006700000000     D OPDDS           DS
006800000000     D  OPDOPZ                 1      2
006900000000     D  OPDDE1                 3     19
007000000000     D  OPDOP1                21     22
007100000000     D  OPDFI0                23     34
007200000000     D  OPDATT                35     35
007300000000     D  OPDFI1                36     80
007400000000     D  OPDDES                 1     19
007500000000     D  OPDRIG                 1     80
007600020319      * RIDEFINIZIONE SCHIERA -FUD-
007700000000     D FUDDS           DS
007800000000     D  FUDDE1                 1      1
007900000000     D  FUDFUN                 2      3
008000000000     D  FUDDE2                 4     19
008100000000     D  FUDFI0                20     34
008200000000     D  FUDATT                35     35
008300000000     D  FUDFI1                36     80
008400000000     D  FUDDES                 1     19
008500000000     D  FUDRIG                 1     80
008600020319      * PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
008700930127     D DSMSG         E DS
008800020319      *------------------
008900020319      * DS "XSRDA8" - CONTROLLA DATA (8)
009000020319      *------------------
009100971010     D WLBDA8          DS
009200971010     D  G08DAT                 1      8  0
009300971010     D  G08INV                 9     16  0
009400971010     D  G08ERR                17     17
009500971010     D  G08TGI                18     22  0
009600020319      * PARAMETRI PER LANCIO ALTRI PGM DELLA PROCEDURA
009700930707     D DSUNIT        E DS
009800020319      * DS PER GESTIONE TASTI FUNZIONALI
009900930127     D DSKEY         E DS
010000000000     D DEVDS1          DS
010100000000     D  KEY                  369    369
010200020319      * ARCHITETTURA
010300930312     D KPJBA         E DS
010400020318
010500020318     d tibs02ds      e ds
010600020517     d tibs69ds      e ds
010700020517     d ds_cnaco      e ds                  extname(cnaco00f)
010800020517     d ds_cnind      e ds                  extname(cnind00f)
010900020517     d ds_cnclp      e ds                  extname(cnclp00f)
011000020517     d ds_fncls      e ds                  extname(fncls00f)
011100020516
011200020516     d ddatiute      e ds
011300020516     d azuteds       e ds                  extname(azute00f)
011400020516     d tibs34ds      e ds                  inz
011500020516     d dute01        e ds
011600020516     d dlat          e ds
011700020516     d trul31ds      e ds
011800020523     d  skpo                  10    759  0 dim(250)
011900020325
012000971010     C*------------------------------------------------------------------------*
012100971010     C* MAIN LINE
012200971010     C*------------------------------------------------------------------------*
012300000000     C* MAIN DEL PGM
012400000000     C                   EXSR      RUTINZ
012500000000     C*
012600930126     C* SE CONTROLLI PRELIMINARI NON OK VADO A FINE PGM
012700930126     C     ERRL00        IFNE      '1'                                          --- 1 -->
012800020516
012900020516      * controllo il profilo utente
013000020516     c                   exsr      ctrute
013100020516     c                   if        *in76
013200020516     c                   eval      sflult = 0
013300020516     c                   endif
013400000000     C*
013500000000     C* CARICA IL SFL DAI PARAMETRI IN INPUT AL PROGRAMMA
013600000000     C                   EXSR      POSSFL
013700000000     C*
013800000000     C* CICLO FINO A CHE:
013900000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
014000000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
014100930707     C*  -FINE PGM PER ENTER SENZA SCELTA
014200000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
014300000000     C     F12L00        OREQ      '1'
014400000000     C*
014500000000     C* AZZERA FLAG DI ERROE
014600000000     C                   MOVEL     '0'           ERRL00
014700020410
014800020410      * posiziono il cursore sulla matricola se prima volta
014900020410     c                   if        wprimo = *off
015000020410     c                   eval      *in88 = *on
015100020410     c                   eval      wprimo = *on
015200020410     c                   else
015300020410     c                   eval      *in88 = *off
015400020410     c                   endif
015500020516
015600020516      * profilo non abilitato
015700020516     c                   if        *in76
015800020516     c                   eval      f03l00 = *on
015900020516     c                   endif
016000000000     C*
016100000000     C* EMETTE SFL A VIDEO
016200000000     C                   EXSR      OUTSFL
016300000000     C*
016400000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI -ROUTINE FUNCON
016500000000     C                   EXSR      FUNCON
016600000000     C*
016700000000     C                   END                                                    <-- 2 ---
016800000000     C*
016900000000     C                   END                                                    <-- 1 --
017000000000     C*
017100940105     C                   MOVEL     DSUNIT        KPJBU
017200000000     C                   SETON                                        LR
017300000000     C*================================================================
017400930126     C* F03GES -GEST.TASTO FUNZ. F03 ===> FINE
017500000000     C*================================================================
017600000000     C     F03GES        BEGSR
017700000000     C*
017800000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PGM 00)
017900000000     C                   MOVEL     '1'           F03L00
018000000000     C*
018100000000     C                   ENDSR
018200000000     C*================================================================
018300930129     C* F05GES -GEST.TASTO FUNZ. F05 ===> RIVISUALIZZAZIONE
018400000000     C*================================================================
018500000000     C     F05GES        BEGSR
018600000000     C*
018700000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
018800000000     C                   EXSR      POSSFL
018900000000     C*
019000000000     C                   ENDSR
019100000000     C*================================================================
019200931116     C* F06GES -GEST.TASTO FUNZ. F06 ===> INSERIMENTO
019300000000     C*================================================================
019400930126     C     F06GES        BEGSR
019500930201     C*
019600930201     C* INSERIMENTO CONSENTITO SOLO IN SEDE
019700020709     c                   If        DutLpo <> 'S'
019800930201     C                   MOVEL     '1'           ERRL00
019900930201     C                   MOVEL     MSG(3)        DSMSMS
020000930201     C                   END
020100930201     C*
020200930201     C     ERRL00        IFEQ      '0'                                          --- 1 -->
020300930127     C*
020400941115     C* LANCIO IL PGM PER L'INSERIMENTO DEL RECORD (TNSDU2R)
020500930201     C                   MOVEL     'F06'         WOPZ1             3
020600020517     C                   MOVEL     'TNSDULR'     WPGM              8
020700930201     C                   EXSR      LANPGM
020800930128     C*
020900930128     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
021000930128     C                   EXSR      POSSFL
021100930201     C*
021200930201     C                   END                                                    <-- 1 ---
021300930127     C*
021400000000     C                   ENDSR
021500000000     C*================================================================
021600930129     C* F12GES -GEST.TASTO FUNZ. F12 ===> RITORNO
021700000000     C*================================================================
021800000000     C     F12GES        BEGSR
021900000000     C*
022000000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12 (A LIVELLO PGM 00)
022100000000     C                   MOVEL     '1'           F12L00
022200000000     C*
022300000000     C                   ENDSR
022400000000     C*================================================================
022500930126     C* F17GES -GEST.TASTO FUNZ. F17 ===> INIZIO DATI
022600000000     C*================================================================
022700000000     C     F17GES        BEGSR
022800000000     C*
022900000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
023000000000     C                   Z-ADD     1             SFLNBR
023100000000     C*
023200000000     C                   ENDSR
023300000000     C*================================================================
023400930126     C* F18GES -GEST.TASTO FUNZ. F18 ===> FINE DATI
023500000000     C*================================================================
023600000000     C     F18GES        BEGSR
023700000000     C*
023800000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
023900000000     C                   Z-ADD     SFLULT        SFLNBR
024000000000     C*
024100000000     C                   ENDSR
024200020517     C*================================================================
024300020517     C* F21GES -GEST.TASTO FUNZ. F21 ===> STAMPA ELENCO
024400020517     C*================================================================
024500020517     C     F21GES        BEGSR
024600020517     C*
024700020517     C                   ENDSR
024800000000     C*================================================================
024900930126     C* F22GES -GEST.TASTO FUNZ. F22 ===> STAMPE
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
026100930126     C* F23GES -GEST.TASTO FUNZ. F23 ===> ALTRE OPZIONI
026200000000     C*================================================================
026300000000     C     F23GES        BEGSR
026400000000     C*
026500000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
026600000000     C                   EXSR      OPZGES
026700000000     C*
026800000000     C                   ENDSR
026900000000     C*================================================================
027000930126     C* F24GES -GEST.TASTO FUNZ. F24 ===> ALTRI TASTI
027100000000     C*================================================================
027200000000     C     F24GES        BEGSR
027300000000     C*
027400000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
027500000000     C                   EXSR      FUNGES
027600000000     C*
027700000000     C                   ENDSR
027800000000     C*================================================================
027900930126     C* RUPGES -GEST.TASTO FUNZ. ROLLUP ===> PAGINA SUCCESSIVA
028000000000     C*================================================================
028100000000     C     RUPGES        BEGSR
028200000000     C*
028300000000     C* CARICA IL SFL DA FILE "FISICO"
028400000000     C                   EXSR      CARSFL
028500000000     C*
028600000000     C                   ENDSR
028700000000     C*================================================================
028800930126     C* HELGES - GEST.TASTO FUNZ. HELP ===> AIUTO
028900000000     C*================================================================
029000000000     C     HELGES        BEGSR
029100000000     C*
029200000000     C                   ENDSR
029300000000     C*================================================================
029400930126     C* ENTGES - GEST.TASTO FUNZ. ENTER ===> INVIO
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
032500930126     C*  O01GES - GESTIONE OPZIONE 1=SCELTA
032600000000     C*================================================================
032700000000     C     O01GES        BEGSR
032800000000     C                   ENDSR
032900000000     C*================================================================
033000000000     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
033100000000     C*================================================================
033200000000     C     O02GES        BEGSR
033300000000     C*
033400000000     C                   ENDSR
033500000000     C*================================================================
033600971015     C* O03GES - GESTIONE OPZIONE 3=DUPLICA
033700000000     C*================================================================
033800000000     C     O03GES        BEGSR
033900000000     C                   ENDSR
034000000000     C*================================================================
034100000000     C*  O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE
034200000000     C*================================================================
034300000000     C     O04GES        BEGSR
034400000000     C*
034500000000     C                   ENDSR
034600000000     C*================================================================
034700000000     C*  O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
034800000000     C*================================================================
034900000000     C     O05GES        BEGSR
035000000000     C*
035100000000     C* LANCIO ROUTINE PER LA VISUALIZZAZIONE DEL DETTAGLIO DELLA RIGA
035200930201     C                   MOVEL     'O05'         WOPZ1
035300020517     C                   MOVEL     'TNSDU2R'     WPGM
035400930201     C                   EXSR      LANPGM
035500000000     C*
035600000000     C                   ENDSR
035700000000     C*================================================================
035800000000     C*  O06GES - GESTIONE OPZIONE 6=STAMPA
035900000000     C*================================================================
036000000000     C     O06GES        BEGSR
036100000000     C                   ENDSR
036200930128     C*================================================================
036300930128     C*  O07GES - GESTIONE OPZIONE 7=RIPRISTINO
036400930128     C*================================================================
036500930128     C     O07GES        BEGSR
036600930128     C*
036700930128     C                   ENDSR
036800930201     C*================================================================
036900940105     C*  O08GES - GESTIONE OPZIONE 8=GESTIONE MOVIMENTI
037000930201     C*================================================================
037100930201     C     O08GES        BEGSR
037200930201     C*
037300930201     C* LANCIO IL PGM PER LA GESTIONE DELLE LOCALITA'
037400971013     C     ERRL00        IFEQ      '0'                                          --- 1 -->
037500930201     C                   MOVEL     'O08'         WOPZ1
037600941115     C                   MOVEL     'TNSDU4R'     WPGM
037700930201     C                   EXSR      LANPGM
037800930201     C                   END                                                    <-- 1 ---
037900930201     C*
038000930201     C                   ENDSR
038100000000     C*================================================================
038200000000     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
038300000000     C*================================================================
038400000000     C     NEWPOS        BEGSR
038500000000     C*
038600000000     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAM.
038700000000     C*         1= RICHIESTO POSIZIONAM.
038800000000     C                   Z-ADD     0             SWPOSI            7 0
038900000000     C*
039000000000     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
039100000000     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
039200000000     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
039300000000     C                   Z-ADD     0             SWMODI            1 0
039400000000     C*
039500000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
039600020319     c                   if        vdpmat <> depmat or vdptip <> deptip or      --- 1 -->
039700020517     c                             vdpksc <> depksc or vdprsc <> deprsc or
039800020325     c                             vdpric <> depric
039900930202     C*
040000930202     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI DI PARTENZA
040100930202     C                   EXSR      CHKPAR
040200930202     C*
040300930202     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
040400930202     C     ERRL00        IFEQ      '0'                                          --- 2 -->
040500000000     C                   Z-ADD     1             SWPOSI
040600020517     c                   eval      deptip = vdptip
040700020319     c                   eval      depmat = vdpmat
040800020325     c                   eval      depric = vdpric
040900020517     c                   eval      depksc = vdpksc
041000020517     c                   eval      deprsc = vdprsc
041100930202     C                   END                                                    <-- 2 ---
041200930202     C*
041300000000     C                   END                                                    <-- 1 ---
041400000000     C*
041500000000     C                   ENDSR
041600000000     C*================================================================
041700000000     C* LETTURA RECORD FILE DA CARICARE
041800000000     C*================================================================
041900000000     C     LETFIL        BEGSR
042000020319
042100020319     c                   select
042200020319
042300020319     c                   when      wtipri = 'TIP'
042400020319     c                   read      unana02l                               99
042500020319
042600020319     c                   when      wtipri = 'MAT'
042700020319     c                   read      unana07l                               99
042800020319
042900020517     c                   when      wtipri = 'KSC'
043000020517     c     kuna10        reade     unana10l                               99
043100020319
043200020319     c                   endsl
043300020319
043400000000     C*
043500930201     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
043600930707     C     *IN99         IFEQ      '1'
043700000000     C                   Z-ADD     1             SFLEOF
043800930707     C                   END
043900000000     C*
044000000000     C                   ENDSR
044100000000     C*================================================================
044200000000     C* SETFIL - SETLL DEL FILE DA CARICARE
044300000000     C*================================================================
044400000000     C     SETFIL        BEGSR
044500020319
044600020319      * POSIZIONAMENTO DEL FILE DA CARICARE E PRIMA LETTURA
044700020319
044800020319     c                   select
044900020319
045000020517      * selezione per tipo
045100020319     c                   when      wtipri = 'TIP'
045200020319     c                   move      vdptip        unatip
045300020319     c                   move      vdpmat        unamat
045400020319     c     kuna02        setll     unana02l
045500020319     c                   if        not %found(unana02l)
045600020319     c                   eval      sfleof = 1
045700020319     c                   else
045800020319     c                   read      unana02l
045900020319     c                   if        %eof(unana02l)
046000020319     c                   eval      sfleof = 1
046100020319     c                   endif
046200020319     c                   endif
046300020319
046400020517      * selezione per matricola
046500020319     c                   when      wtipri = 'MAT'
046600020325      * no ricerca per contenuto
046700020325     c                   if        vdpric = *blanks
046800020319     c                   move      vdpmat        unamat
046900020319     c                   move      vdptip        unatip
047000020319     c     kuna07        setll     unana07l
047100020319     c                   if        not %found(unana07l)
047200020319     c                   eval      sfleof = 1
047300020319     c                   else
047400020319     c                   read      unana07l
047500020319     c                   if        %eof(unana07l)
047600020319     c                   eval      sfleof = 1
047700020319     c                   endif
047800020319     c                   endif
047900020325      * si ricerca per contenuto
048000020325     c                   else
048100020325     c     *loval        setll     unana07l
048200020325     c                   if        not %found(unana07l)
048300020325     c                   eval      sfleof = 1
048400020325     c                   else
048500020325     c                   read      unana07l
048600020325     c                   if        %eof(unana07l)
048700020325     c                   eval      sfleof = 1
048800020325     c                   endif
048900020325     c                   endif
049000020325     c                   endif
049100020319
049200020517      * selezione per cliente
049300020517     c                   when      wtipri = 'KSC'
049400020517     c                   eval      unaksc = vdpksc
049500020517     c     kuna10        setll     unana10l
049600020517     c                   if        not %found(unana10l)
049700020319     c                   eval      sfleof = 1
049800020319     c                   else
049900020517     c     kuna10        reade     unana10l
050000020517     c                   if        %eof(unana10l)
050100020319     c                   eval      sfleof = 1
050200020319     c                   endif
050300020319     c                   endif
050400020517
050500020319     c                   endsl
050600020319
050700000000     C                   ENDSR
050800020319      *================================================================
050900020319      * CTRSFL - CONTROLLO SE RECORD DA SCRIVERE NEL SUBFILE
051000020319      *================================================================
051100020319     C     CTRSFL        BEGSR
051200020319
051300020325      * record letto ok
051400020517
051500020517     c                   if        unaksc = *zeros and wnosfl = *off
051600020517     c                   eval      wnosfl = *on
051700020517     c                   endif
051800020517
051900020517     c                   if        wnosfl = *off
052000020517     c                   movel     unaksc        wmag
052100020517     c     wmag          lookup    skpoabi                                99
052200020517     c                   if        not *in99
052300020517     c                   eval      wnosfl = *on
052400020517     c                   endif
052500020517     c                   endif
052600020325
052700020325     c                   if        wtipri = 'MAT' and vdpric = 'S'
052800020517     c                             and wnosfl = *off
052900020325     c     ' '           checkr    vdpmat        xx
053000020325     c     vdpmat:xx     scan      unamat                                 99
053100020325     c                   if        not *in99
053200020325     c                   eval      wnosfl = *on
053300020325     c                   endif
053400020325     c                   endif
053500020325
053600020319     C                   ENDSR
053700000000     C*================================================================
053800000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
053900000000     C*================================================================
054000000000     C     IRESFL        BEGSR
054100000000     C*
054200000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
054300930127     C*
054400930707     C                   MOVEL     UNAATB        VIDATB
054500930707     C                   Z-ADD     UNATIP        VIDTIP
054600930707     C                   Z-ADD     UNACOD        VIDCOD
054700930707     C                   MOVEL     UNAMAT        VIDMAT
054800930707     C                   MOVEL     UNAMOD        VIDMOD
054900020517     c                   eval      vidksc = unaksc
055000020517     c                   clear                   TIBS69DS
055100020517     c                   z-add     vidksc        I69kac
055200020517     c                   call      'TIBS69R'
055300020517     c                   parm                    tibs69DS
055400020517     c                   parm                    ds_cnaco
055500020517     c                   parm                    ds_cnind
055600020517     c                   parm                    ds_cnclp
055700020517     c                   parm                    ds_fncls
055800020517     c                   if        o69err <> '1'
055900020517     c                   movel     acorag        vidrsc
056000020517     c                   endif
056100000000     C*
056200000000     C                   ENDSR
056300930202     C*================================================================
056400930202     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
056500930202     C*================================================================
056600930202     C     CHKPAR        BEGSR
056700020319
056800020319      * IMPOSTO FLAG EVIDENZAZIONE TIPO DI RICERCA
056900020319     c                   eval       *in90 = *off                                x tipo
057000020319     c                   eval       *in91 = *off                                x matricola
057100020517     c                   eval       *in92 = *off                                x cliente
057200020319
057300020319      * IMPOSTO FLAG EVIDENZAZIONE ERRORI SUI PARAMETRI
057400020319     c                   eval       *in80 = *off                                tipo
057500020319     c                   eval       *in81 = *off                                tipo
057600020319     c                   eval       *in82 = *off                                matricola
057700020517     c                   eval       *in83 = *off                                cliente
057800020326
057900020326     c                   clear                   wtipri
058000020319
058100020319      * Se non è stata scelta una ricerca utilizzo la ricerca per tipo
058200020319     c                   if        vdpmat = *blanks and vdptip = *blanks and
058300020517     c                             vdpksc = *zeros and vdprsc = *blanks
058400020319     c                   eval      wtipri = 'TIP'
058500020319     c                   eval      *in90 = *on
058600020319     c                   endif
058700020319
058800020319      * ===> ACCETTO SOLO UN PARAMETRO DI INIZIO RICERCA
058900020319
059000020319      * Tipo unità / matricola
059100020319     c                   if        vdptip <> *blanks
059200020319      * controllo tipo unità
059300020319     c                   exsr      chktip
059400020319     c                   eval      wtipri = 'TIP'
059500020319     c                   eval      *in90 = *on
059600020319     c                   endif
059700020319
059800020319      * Matricola
059900020410     c                   if        vdpmat <> *blanks and errl00 = *off and
060000020410     c                             vdptip = *blanks
060100020319     c                   eval      wtipri = 'MAT'
060200020319     c                   eval      *in91 = *on
060300020319     c                   endif
060400020325      * ricerca x matricla
060500020325     c                   if        errl00 = *off and vdpmat = *blanks and
060600020325     c                             vdpric = 'S'
060700020325     c                   eval      errl00 = *on
060800020325     c                   eval      dsmsms = msg(12)
060900020517     c                   eval      *in82 = *on
061000020325     c                   endif
061100020517
061200020517      * Cliente
061300020517     c                   if        vdpksc <> *zeros and vdprsc <> *blanks and
061400020517     c                             errl00 = *off
061500020517     c                   if        wtipri <> *blanks                            --- 2 -->
061600020517     c                   eval      errl00 = *on
061700020517     c                   eval      dsmsms = msg(6)
061800020517     c                   else                                                   <-- 2 -->
061900020517     c                   eval      errl00 = *on
062000020517     c                   eval      dsmsms = msg(8)
062100020517     c                   eval      *in83 = *on
062200020517     c                   endif
062300020517     c                   endif
062400020517      * controllo cliente
062500020517     c                   if        (vdpksc <> *zeros or vdprsc <> *blanks)
062600020517     c                             and errl00 = *off
062700020517     c                   if        wtipri <> *blanks                            --- 2 -->
062800020517     c                   eval      errl00 = *on
062900020517     c                   eval      dsmsms = msg(6)
063000020517     c                   else                                                   <-- 2 -->
063100020517     c                   exsr      chkksc
063200020517     c                   eval      wtipri = 'KSC'
063300020517     c                   eval      *in92 = *on
063400020517     c                   endif
063500020517     c                   endif
063600020326
063700020326      * SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
063800020326      * CURSORE SUL CAMPO DEL CONTROL
063900020326     C     errl00        IFEQ      '1'
064000020326     C                   MOVEL     '0'           *IN43                          *NON EMETTO SFL
064100020326     C                   ELSE
064200020326     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
064300020326     C                   END
064400020319
064500930202     C                   ENDSR
064600930707     C*================================================================
064700930707     C*  CHKTIP - CONTROLLA IL TIPO DI UNITA
064800930707     C*================================================================
064900930707     C     CHKTIP        BEGSR
065000020318
065100020318      * CONTROLLO SE E' STATO INSERITO '?'
065200020319     C     '?'           SCAN      VDPTIP                                 99
065300020318
065400020318      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
065500020319     C                   if        *in99                                        --- 1 -->
065600020318     C                   clear                   TIBS02DS
065700020318     C                   movel     'R'           t02mod
065800020318     C                   movel     knsif         t02sif
065900020321     C                   movel     'UNT'         t02cod
066000020318     C                   call      'TIBS02R'
066100020318     C                   parm                    KPJBA
066200020318     C                   parm                    TIBS02DS
066300020318     C                   movel     T02ke1        vdptip
066400020318
066500020319     C                   endif                                                  <-- 1 ---
066600020318
066700020318      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
066800020318     C                   clear                   TIBS02DS
066900020318     C                   movel     'C'           T02mod
067000020318     C                   movel     knsif         t02sif
067100020321     C                   movel     'UNT'         t02cod
067200020318     C                   movel(p)  vdptip        T02ke1
067300020318     C                   call      'TIBS02R'
067400020318     C                   parm                    KPJBA
067500020318     C                   parm                    TIBS02DS
067600020318     C                   if        t02err <> *blanks
067700020319     C                   eval      errl00 = '1'
067800020319     C                   eval      *in80 = *on
067900020318     C                   movel     msg(7)        dsmsms
068000020318     C                   endif
068100020319
068200930707     C                   ENDSR
068300930707     C*================================================================
068400020517     C*  CHKKSC - CONTROLLA IL CLIENTE
068500930707     C*================================================================
068600020517     C     CHKKSC        BEGSR
068700020517
068800020517      * se non inserito il codice ma inserita una parte della ragione sociale
068900020517      * richiamo il programma della ricerca alfabetica
069000020517     c                   if        vdpksc = *zeros and vdprsc <> *blanks
069100020517     c                   exsr      ricalf
069200020517     c                   endif
069300020517
069400020517     c                   movel     vdpksc        wmag
069500020517     c     wmag          lookup    skpoabi                                99
069600020517     c                   if        not *in99
069700020517     c                   eval      errl00 = *on
069800020517     c                   eval      *in83 = *on
069900020517     c                   movel     msg(14)       dsmsms
070000020517     c                   endif
070100020517
070200020517      * se inserito il codice controllo che esista sul piano dei conti
070300020517     c                   if        vdpksc <> *zeros and errl00 = *off
070400020517     c                   clear                   TIBS69DS
070500020517     c                   z-add     vdpksc        I69kac
070600020517     c                   call      'TIBS69R'
070700020517     c                   parm                    tibs69DS
070800020517     c                   parm                    ds_cnaco
070900020517     c                   parm                    ds_cnind
071000020517     c                   parm                    ds_cnclp
071100020517     c                   parm                    ds_fncls
071200020517
071300020517     c                   if        o69err <> '1'
071400020517     c                   movel     acorag        vdprsc
071500020517     c                   else
071600020517     c                   eval      errl00 = *on
071700020517     c                   eval      *in83 = *on
071800020517     c                   movel     msg(9)        dsmsms
071900020517     c                   endif
072000020517     c                   endif
072100020517
072200930707     C                   ENDSR
072300020517      *================================================================
072400020517      *  RICALF - RICERCA ALFABETICA
072500020517      *================================================================
072600020517     c     ricalf        begsr
072700020517
072800020517     C                   MOVEL     rsut          PAXdut                         Descrizione Azienda
072900081203     C*******            Z-ADD     9             PAXsta                         9=Escludo Annullati
073000081203     c* visulizzare anche i bloccati
073100081203     C                   Z-ADD     0             PAXsta                         9=Escludo Annullati
073200020517     C                   Z-ADD     dutkci        PAXccc                         Capoconto Clienti
073300020517     c                   movel(P)  vdprsc        PAXdmt
073400020517     C                   z-add     1             PAXnum
073500020517
073600020517     C                   CALL      'XALFA3BR'
073700020517     C                   PARM                    PAXDUT
073800020517     C                   PARM                    CODUT             1 0
073900020517     C                   PARM                    PAXDMT
074000020517     C                   PARM                    PAXCCC
074100020517     C                   PARM                    PAXSTA
074200020517     C                   PARM      *blanks       PAXFLR
074300020517     C                   PARM      *blanks       PAXDIT
074400020517     C                   PARM                    PAXNUM            2 0
074500020517     C                   PARM                    PAXKCM           80
074600020517     C                   PARM                    PAXKSM          140
074700020517     C                   PARM                    PAXKDM           60
074800020517
074900020517      * NON TROVATO O PREMUTO CMD12
075000020517    2C     paxsta        IFGT      -1
075100020517     C                   MOVEL     PAXKSM        vdpksc
075200020517    2C                   ENDIF
075300020517
075400020517     c                   endsr
075500020516      *------------------------------------------------------------------------*
075600020516      *  CTRUTE - ROUTINE CONTROLLO UTENTE
075700020516      *------------------------------------------------------------------------*
075800020516     c     ctrute        begsr
075900020516
076000020516     c                   eval      *in76 = *off
076100020516
076200020516      * controllo se il profilo è abilitato
076300020516     c                   if        duterr = 'E'
076400020516     c                   eval      dsmsms = dutmsg
076500020516     c                   eval      *in76 = *on
076600020516     c                   endif
076700020516      * controllo le abilitazionei del profilo
076800020516     c                   if        *in76 = *off
076900020516     c                   movel     utefaf        dute01
077000020516     c                   if        §uteiun <> *blanks
077100020516     c                   eval      wabi = §uteiun
077200020516     c                   else
077300020516     c                   eval      wabi = uteaut
077400020516     c                   endif
077500020516     c                   clear                   tibs02ds
077600020516     c                   movel     'C'           t02mod
077700020516     c                   movel     knsif         t02sif
077800020516     c                   movel     'LAT'         t02cod
077900020516     c                   movel(p)  wabi          t02ke1
078000020516     c                   call      'TIBS02R'
078100020516     c                   parm                    kpjba
078200020516     c                   parm                    tibs02ds
078300020516     c                   movel     t02uni        dlat
078400020516     c                   if        §latabi = 'S'
078500020516     c                   eval      dsmsms= msg(13)
078600020516     c                   eval      *in76 = *on
078700020516     c                   endif
078800020516     c                   endif
078900020516
079000020516      * Utente ok richiamo pgm che ritorna i p.o. abilitati
079100020523     c                   if        *in76 = *off  and wabi <> 'AZ'
079200020516     c                   clear                   trul31ds
079300020516     c                   eval      i31abi = wabi
079400020516     c                   eval      i31cdi = dutdis
079500020516     c                   eval      i31car = dutare
079600020516     c                   eval      i31cpo = dutpou
079700020516     c                   call      'TRUL31R'
079800020516     c                   parm                    kpjba
079900020516     c                   parm                    trul31ds
080000020516     c                   endif
080100020517
080200020529      * per abilitazione 'AZ' devo caricare tutti i p.o. di azorg21l
080300020517     c                   if        wabi = 'AZ'
080400020529     c     *loval        setll     azorg21l
080500020517     c                   do        *hival
080600020529     c                   read      azorg21l
080700020517     c                   if        %eof
080800020517     c                   leave
080900020517     c                   endif
081000020517     c                   z-add     1             ii
081100020523     c     orgfil        lookup    skpoabi(ii)                            99
081200020517     c                   if        not *in99
081300020517     c                   z-add     1             ii
081400020517     c     *zeros        lookup    skpoabi(ii)                            99
081500020517     c                   if        *in99
081600020517     c                   movel     orgfil        skpoabi(ii)
081700020517     c                   endif
081800020517     c                   endif
081900020517     c                   enddo
082000020529      * e tutti i p.o. di azorg01l
082100020529     c     *loval        setll     azorg01l
082200020529     c                   do        *hival
082300020529     c                   read      azorg01l
082400020529     c                   if        %eof
082500020529     c                   leave
082600020529     c                   endif
082700020529     c                   z-add     1             ii
082800020529     c     orgfil        lookup    skpoabi(ii)                            99
082900020529     c                   if        not *in99
083000020529     c                   z-add     1             ii
083100020529     c     *zeros        lookup    skpoabi(ii)                            99
083200020529     c                   if        *in99
083300020529     c                   movel     orgfil        skpoabi(ii)
083400020529     c                   endif
083500020529     c                   endif
083600020529     c                   enddo
083700020523      * se non è abilitazione 'AZ' passo la sk tornata dal TRUL31 nella sk di 500 elementi
083800020523     c                   else
083900020523     c                   movea     skpo          skpoabi
084000020523     c                   z-add     1             ii
084100020523     c     951           lookup    skpoabi(ii)                            99
084200020523     c                   if        not *in99
084300020523     c                   z-add     1             ii
084400020523     c     *zeros        lookup    skpoabi(ii)                            99
084500020523     c                   if        *in99
084600021014     c                   movel     951           skpoabi(ii)
084700020523     c                   endif
084800020523     c                   endif
084900020517     c                   endif
085000020516
085100020516     c                   endsr
085200930201     C*================================================================
085300930201     C* LANPGM - LANCIA UN PROGRAMMA
085400930201     C*================================================================
085500930201     C     LANPGM        BEGSR
085600930201     C*
085700930201     C* IMPOSTO PARAMETRI PER IL PGM
085800930707     C                   MOVEL     '0'           DSUF03                         PREMUTO F03
085900930707     C                   MOVEL     '0'           DSUERR                         '1'=ERRORE
086000930707     C                   MOVEL     *BLANKS       DSUMSG                         MESSAGGIO
086100930707     C                   MOVE      WOPZ1         DSUOP1                         *OPZIONE LIV.PGM
086200020322     C                   Z-ADD     vidtip        DSUTIP                         TIPO
086300930707     C                   Z-ADD     VIDCOD        DSUCOD                         CODICE
086400930712     C                   Z-ADD     *ZEROS        DSUMAG                         MAGAZZINO
086500930712     C                   Z-ADD     *ZEROS        DSUPRG                         PROGRESSIVO MOV.
086600930906     C                   Z-ADD     *ZEROS        DSUDTM                         DATA MOVIMENTO
086700930201     C*
086800930201     C* LANCIO PROGRAMMA
086900940105     C                   MOVEL     DSUNIT        KPJBU
087000930312     C                   CALL      WPGM
087100940105     C                   PARM                    KPJBA
087200940105     C                   MOVEL     KPJBU         DSUNIT
087300930201     C*
087400930201     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
087500930707     C     DSUF03        IFEQ      '1'
087600930201     C                   MOVEL     '1'           F03L00
087700930201     C                   MOVEL     '1'           ERRL00
087800930201     C                   ELSE
087900930201     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
088000930707     C                   MOVEL     DSUMSG        DSMSMS
088100930201     C                   END
088200930201     C*
088300930201     C                   ENDSR
088400930312     C*================================================================
088500930312     C* CHKFOP - CONTROLLA TIPO GESTIONE E IMPOSTA FUNZ/OPZ
088600930312     C*================================================================
088700930312     C     CHKFOP        BEGSR
088800930312     C*
088900930312     C* CONTROLLO CHE VI SIA UNA OPZIONE DI PROCEDURA VALIDA IN INPUT
089000930312     C* ALTRIMENTI FORZO INTERROGAZIONE
089100020517     C     DSUOP0        IFNE      ' 5'                                         *INTERROGAZIONE
089200940105     C                   MOVE      ' 5'          DSUOP0
089300930312     C                   END
089400930312     C*
089500930312     C* SE IL PGM E' STATO LANCIATO IN INTERROGAZIONE ABILITO SOLO QUELLA
089600940105     C     DSUOP0        IFEQ      ' 5'
089700930312     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
089800930312     C                   MOVEL     'S'           OPDATT
089900930312     C                   MOVEL     OPDRIG        OPD(5)
090000971015     C                   MOVEL     OPD(8)        OPDRIG                         *MOVIMENTI
090100971015     C                   MOVEL     'S'           OPDATT
090200971015     C                   MOVEL     OPDRIG        OPD(8)
090300930312     C                   END
090400930312     C*
090500930312     C* IMPOSTO INDICE PER EMISSIONE OPZIONI A VIDEO
090600930312     C                   Z-ADD     0             OPZGRU            5 0
090700930312     C*
090800930312     C* GESTIONE OPZIONI A VIDEO
090900930312     C                   EXSR      OPZGES
091000930312     C*
091100930312     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
091200930312     C                   Z-ADD     0             FUNGRU            5 0
091300930312     C*
091400930312     C* GESTIONE TASTI FUNZIONALI A VIDEO
091500930312     C                   EXSR      FUNGES
091600930312     C*
091700930312     C                   ENDSR
091800000000     C*================================================================
091900000000     C*  RUTINZ - ROUTINE INIZIALE
092000000000     C*================================================================
092100000000     C     RUTINZ        BEGSR
092200000000     C*
092300020517     C* RICEVIMENTO PARAMETRI DALL'AZIONE (' 5'=INTERR.)
092400000000     C     *ENTRY        PLIST
092500930707     C                   PARM                    KPJBA
092600940105     C                   MOVEL     KPJBU         DSUNIT
092700020516
092800020516     c     *dtaara       define    §azute        azuteds
092900020516     c     *dtaara       define    §datiute      ddatiute
093000020516
093100020516     c                   in(E)     *dtaara
093200020516     c                   if        %Error or rsut = *blanks
093300020516     c                   clear                   tibs34ds
093400020516     c                   call      'TIBS34R'
093500020516     c                   parm                    Tibs34Ds
093600020516     c                   in        *dtaara
093700020516     c                   endif
093800020516
093900930204     C*
094000930204     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
094100930204     C     XMSGPA        PLIST
094200930204     C                   PARM                    DSMSMS
094300930204     C                   PARM                    DSMSPG
094400930204     C                   PARM                    DSMSKE
094500930204     C                   PARM                    DSMSER
094600020319
094700020319      * CHIAVE LETTURA UNANA02L
094800930707     C     KUNA02        KLIST
094900020319     C                   KFLD                    UNATIP
095000020319     C                   KFLD                    UNAMAT
095100020319
095200020319      * CHIAVE LETTURA UNANA07L
095300020319     C     KUNA07        KLIST
095400020319     C                   KFLD                    UNAMAT
095500020319     C                   KFLD                    UNATIP
095600020517
095700020517      * CHIAVE LETTURA UNANA10L
095800020517     C     KUNA10        KLIST
095900020517     C                   KFLD                    UNAKSC
096000020319
096100020529     C* CHIAVE LETTURA AZORG21l/azorg01l - COMPLETA
096200930707     C     KEYORG        KLIST
096300930707     C                   KFLD                    ORGFIL
096400930707     C*
096500000000     C*
096600930129     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
096700930204     C                   CALL      'XKEY'
096800930204     C                   PARM                    DSKEY
096900000000     C*
097000000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
097100000000     C     *LIKE         DEFINE    SFLNBR        SFLPAG
097200000000     C     *LIKE         DEFINE    SFLPAG        WFLPAG
097300000000     C     *LIKE         DEFINE    SFLNBR        SFLEOF
097400000000     C     *LIKE         DEFINE    SFLNBR        SFLERR
097500000000     C     *LIKE         DEFINE    SFLNBR        SFLULT
097600000000     C     *LIKE         DEFINE    SFLNBR        WFLULT
097700000000     C     *LIKE         DEFINE    SFLNBR        MSGULT
097800000000     C     *LIKE         DEFINE    SFLNBR        SFLRIG
097900000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
098000000000     C*
098100930127     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
098200000000     C*   -NOME PGM CHIAMANTE
098300000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
098400020517     C                   MOVEL     'TNSDUIR'     DSMSPG
098500020517     C                   MOVEL     'TNSDUIR'     MSGQUE
098600000000     C                   MOVEL     *BLANKS       MSGKEY
098700930127     C                   MOVEL     *BLANKS       DSMSMS
098800930204     C                   MOVEL     *BLANKS       DSMSKE
098900930204     C                   Z-ADD     0             DSMSER
099000000000     C*
099100930129     C* INIZIALIZZO SWITCH DI LIVELLO 0 (PGM)
099200930127     C                   MOVEL     '0'           F12L00            1
099300930127     C                   MOVEL     '0'           F03L00            1
099400930127     C                   MOVEL     '0'           FINL00            1
099500930127     C                   MOVEL     '0'           ERRL00            1
099600930128     C                   MOVEL     '0'           FLFINE            1
099700930127     C*
099800930312     C* IMPOSTO DEPOSITI PER CONFRONTI
099900930707     C                   MOVEL     *BLANKS       DEPTIP
100000930707     C                   MOVEL     *BLANKS       DEPMAT
100100020325     c                   clear                   depric
100200020517     c                   clear                   depksc
100300020517     c                   clear                   deprsc
100400930202     C*
100500941128     C* IMPOSTO TIPO DI RICERCA DI DEFAULT --> MAT
100600020319     C                   MOVEL     'TIP'         WTIPRI
100700020319     c                   eval      *in90 = *on
100800000000     C*
100900000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
101000000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
101100020517     C                   Z-ADD     12            SFLPAG
101200930312     C*
101300930312     C* IMPOSTO LE OPZIONI E LE FUNZIONI A VIDEO
101400930312     C                   EXSR      CHKFOP
101500930225     C*
101600930225     C* IMPOSTO INTERROGAZIONE/GESTIONE NELLA TESTATA DELLA VIDEATA
101700020517     C     DSUOP0        IFEQ      ' 5'
101800020517     C                   eval      viding = ' INTERROGAZIONE COMODATI '
101900930225     C                   END
102000020325
102100000000     C                   ENDSR
102200020326
102300930312     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
102400930312     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
102500930312     C* LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
102600930312     C* STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
102700930312     C* NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
102800930312     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
102900930312     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
103000930312     C*
103100930312     C*================================================================
103200930312     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA
103300930312     C*          CARICARE
103400930312     C*================================================================
103500930312     C     POSSFL        BEGSR
103600930312     C*
103700930312     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
103800930312     C                   EXSR      CLRSFL
103900930312     C*
104000930312     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
104100930312     C                   EXSR      SETFIL
104200930312     C*
104300930312     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
104400930312     C     SFLEOF        IFEQ      0
104500020326     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
104600930312     C                   EXSR      CARSFL
104700930312     C                   END
104800930312     C*
104900930312     C                   ENDSR
105000000000     C*================================================================
105100000000     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
105200000000     C*================================================================
105300000000     C     ENTATT        BEGSR
105400000000     C*
105500000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
105600000000     C                   Z-ADD     0             OPZOK             1 0
105700000000     C*
105800000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
105900000000     C                   Z-ADD     0             SFLVAR            5 0
106000000000     C*
106100000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
106200000000     C                   Z-ADD     0             SFLERR
106300000000     C*
106400000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
106500000000     C                   Z-ADD     SFLULT        WFLULT
106600000000     C*
106700000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
106800020517     C                   READC     SDUID05                                98
106900930202     C     *IN98         DOWEQ     '0'
107000000000     C*
107100000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
107200000000     C                   ADD       1             SFLVAR
107300000000     C*
107400000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
107500000000     C     OPZOK         IFEQ      0
107600000000     C*
107700000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
107800000000     C     OPZKEY        IFNE      *BLANKS
107900000000     C                   EXSR      OPZCON
108000000000     C                   END
108100000000     C*
108200000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
108300000000     C*            2. PULIZIA SCELTA IN SFL
108400000000     C     OPZOK         IFEQ      0
108500000000     C                   MOVEL     '0'           *IN41
108600000000     C                   MOVEL     *BLANKS       OPZKEY
108700000000     C                   ELSE
108800000000     C*
108900000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
109000000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
109100000000     C                   MOVEL     '1'           *IN41
109200000000     C                   Z-ADD     SFLULT        SFLERR
109300000000     C                   END
109400000000     C                   ELSE
109500000000     C*
109600000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
109700000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
109800000000     C                   MOVEL     '1'           *IN41
109900000000     C                   END                                                    OPZOK=0
110000000000     C*
110100000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
110200000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
110300000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
110400000000     C     OPZOK         IFNE      0
110500000000     C                   Z-ADD     SFLERR        SFLNBR
110600000000     C                   ELSE
110700000000     C                   Z-ADD     SFLULT        SFLNBR
110800000000     C                   END
110900000000     C*
111000000000     C* AGGIORNO RECORD SFL
111100020517     C                   UPDATE    SDUID05
111200000000     C*
111300020517     C                   READC     SDUID05                                98
111400000000     C                   END                                                    DO WHILE
111500000000     C*
111600000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
111700000000     C                   Z-ADD     WFLULT        SFLULT
111800000000     C*
111900000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
112000000000     C     SFLVAR        IFEQ      0
112100000000     C                   MOVEL     '1'           FINL00
112200000000     C                   END
112300000000     C*
112400000000     C                   ENDSR
112500000000     C*================================================================
112600000000     C* ELIMINAZIONE CONTENUTO SFL
112700000000     C*================================================================
112800000000     C     CLRSFL        BEGSR
112900000000     C*
113000000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
113100000000     C* (0=NON FINE FILE  1=FINE FILE)
113200000000     C                   Z-ADD     0             SFLEOF
113300000000     C*
113400000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
113500000000     C                   Z-ADD     0             SFLULT
113600000000     C*
113700000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
113800000000     C                   MOVEL     '1'           *IN40
113900020517     C                   WRITE     SDUID06
114000000000     C                   MOVEL     '0'           *IN40
114100000000     C*
114200000000     C                   ENDSR
114300000000     C*================================================================
114400000000     C* CARSFL - CARICAMENTO SFL
114500000000     C*================================================================
114600000000     C     CARSFL        BEGSR
114700000000     C*
114800000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
114900000000     C     SFLULT        IFEQ      0
115000000000     C                   EXSR      SETFIL
115100000000     C                   END
115200000000     C*
115300000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
115400000000     C                   Z-ADD     SFLULT        WFLULT
115500000000     C*
115600000000     C* CICLO:
115700000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
115800000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
115900000000     C                   Z-ADD     1             SFLRIG
116000000000     C     SFLRIG        DOWLE     SFLPAG
116100000000     C     SFLEOF        ANDEQ     0
116200020319
116300020319     c                   eval      wnosfl = *off
116400020319
116500000000     C*
116600000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
116700000000     C                   MOVEL     *BLANKS       OPZKEY
116800020319
116900020326      * controllo se devo scrivere il record nel subfile
117000020319     c                   exsr      ctrsfl
117100020319
117200020319     c                   if        wnosfl = *off
117300000000     C*
117400000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
117500000000     C                   EXSR      IRESFL
117600000000     C*
117700000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
117800000000     C                   ADD       1             SFLULT
117900000000     C*
118000000000     C* SCRITTURA RECORD SFL
118100020517     C                   WRITE     SDUID05
118200000000     C                   ADD       1             SFLRIG
118300020319
118400020319     c                   endif
118500000000     C*
118600000000     C* LETTURA FILE DA CARICARE
118700000000     C                   EXSR      LETFIL
118800000000     C                   END                                                    DO WHILE
118900000000     C*
119000000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
119100000000     C     WFLULT        IFNE      SFLULT
119200000000     C                   Z-ADD     WFLULT        SFLNBR
119300000000     C                   ADD       1             SFLNBR
119400000000     C                   END
119500000000     C*
119600000000     C                   ENDSR
119700000000     C*================================================================
119800000000     C* EMISSIONE SUBFILE A VIDEO
119900000000     C*================================================================
120000000000     C     OUTSFL        BEGSR
120100000000     C*
120200000000     C* WRITE RECORD TESTATA VIDEO
120300020517     C                   WRITE     SDUID01
120400930205     C*
120500930205     C* VISUALIZZAZIONE MESSAGGIO
120600930205     C                   EXSR      SNDMSG
120700000000     C*
120800000000     C* WRITE RECORD OPZIONI VIDEO
120900020517     C                   WRITE     SDUID02
121000000000     C*
121100000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
121200000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
121300000000     C     SFLEOF        IFEQ      0
121400000000     C                   MOVEL     '0'           *IN42
121500000000     C                   ELSE
121600000000     C*
121700000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
121800000000     C                   MOVEL     '1'           *IN42
121900000000     C                   END
122000000000     C*
122100000000     C* WRITE RECORD TASTI FUNZIONALI
122200020517     C                   WRITE     SDUID09
122300000000     C*
122400000000     C* CASO DI SFL VUOTO
122500000000     C     SFLULT        IFLE      0
122600000000     C*
122700000000     C*    - WRITE RECORD VUOTO
122800020326     C                   MOVEL     '0'           *IN43                          *NON VISUALIZZA SFL
122900020517     C                   WRITE     SDUID0A
123000020517     C                   WRITE     SDUID06                                      *SFL CONTROL
123100020517     C                   READ      SDUID06                                99
123200000000     C*
123300000000     C                   ELSE
123400000000     C*
123500000000     C* CASO DI SFL CON RECORDS
123600000000     C*
123700000000     C*    - WRITE RECORD CONTROLLO SFL
123800020517     C                   WRITE     SDUID06
123900020517     C                   READ      SDUID06                                99
124000000000     C                   END
124100000000     C*
124200000000     C                   ENDSR
124300000000     C*================================================================
124400000000     C* SNDMSG - MANDA MESSAGGIO
124500000000     C*================================================================
124600000000     C     SNDMSG        BEGSR
124700000000     C*
124800000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
124900000000     C                   MOVEL     '1'           *IN40
125000020517     C                   WRITE     SDUIDMC
125100000000     C                   MOVEL     '0'           *IN40
125200000000     C*
125300000000     C* VISUALIZZAZIONE MESSAGGIO
125400000000     C                   Z-ADD     0             MSGULT
125500930204     C                   CALL      'XMSG'        XMSGPA
125600930127     C     DSMSER        IFEQ      0
125700930127     C                   MOVEL     DSMSKE        MSGKEY
125800000000     C                   END
125900000000     C                   Z-ADD     1             MSGULT
126000020517     C                   WRITE     SDUIDMS                                      SFL MESS
126100020517     C                   WRITE     SDUIDMC                                      CONTR MESS
126200000000     C*
126300000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
126400930127     C                   MOVEL     *BLANKS       DSMSMS
126500000000     C*
126600000000     C                   ENDSR
126700000000     C*================================================================
126800000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
126900000000     C*================================================================
127000000000     C     FUNCON        BEGSR
127100000000     C*
127200000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
127300000000     C                   EXSR      FUNVAR
127400000000     C*
127500000000     C* PULISCO CAMPI DI LAVORO
127600000000     C                   Z-ADD     0             I                 5 0
127700000000     C                   Z-ADD     0             W                 5 0
127800000000     C*
127900000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
128000000000     C     FUNKEY        IFNE      *BLANKS
128100000000     C     1             DO        24            I
128200000000     C                   MOVEL     FUD(I)        FUDRIG
128300000000     C*
128400000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
128500000000     C     FUNKEY        IFEQ      FUDFUN
128600000000     C     FUDATT        ANDEQ     'S'
128700000000     C                   Z-ADD     I             W
128800000000     C                   END
128900000000     C                   END                                                    DO WHILE
129000000000     C*
129100000000     C* FUNZIONE NON ATTIVA (ERRORE)
129200000000     C     W             IFEQ      0
129300000000     C*
129400000000     C* EMISSIONE MESSAGGIO DI ERRORE
129500930127     C                   MOVEL     MSG(2)        DSMSMS
129600000000     C                   END                                                    W=0
129700000000     C                   END                                                    FUNKEY NE *BLANKS
129800000000     C*
129900000000     C* FUNZIONE ATTIVA (PROCEDI ...)
130000000000     C     W             IFNE      0
130100000000     C     FUNKEY        OREQ      *BLANKS
130200000000     C*
130300000000     C     FUNKEY        IFEQ      *BLANKS
130400000000     C                   EXSR      ENTGES
130500000000     C                   END
130600000000     C     FUNKEY        IFEQ      '03'
130700000000     C                   EXSR      F03GES
130800000000     C                   END
130900000000     C     FUNKEY        IFEQ      '05'
131000000000     C                   EXSR      F05GES
131100000000     C                   END
131200930126     C     FUNKEY        IFEQ      '06'
131300930126     C                   EXSR      F06GES
131400000000     C                   END
131500000000     C     FUNKEY        IFEQ      '12'
131600000000     C                   EXSR      F12GES
131700000000     C                   END
131800000000     C     FUNKEY        IFEQ      '17'
131900000000     C                   EXSR      F17GES
132000000000     C                   END
132100000000     C     FUNKEY        IFEQ      '18'
132200000000     C                   EXSR      F18GES
132300000000     C                   END
132400930129     C     FUNKEY        IFEQ      '21'
132500930129     C                   EXSR      F21GES
132600930129     C                   END
132700000000     C     FUNKEY        IFEQ      '22'
132800000000     C                   EXSR      F22GES
132900000000     C                   END
133000000000     C     FUNKEY        IFEQ      '23'
133100000000     C                   EXSR      F23GES
133200000000     C                   END
133300000000     C     FUNKEY        IFEQ      '24'
133400000000     C                   EXSR      F24GES
133500000000     C                   END
133600000000     C     FUNKEY        IFEQ      'RU'                                         ROLLUP
133700000000     C                   EXSR      RUPGES
133800000000     C                   END
133900000000     C     FUNKEY        IFEQ      'HP'                                         HELP
134000000000     C                   EXSR      HELGES
134100000000     C                   END
134200000000     C*
134300000000     C                   END
134400000000     C*
134500000000     C                   ENDSR
134600000000     C*================================================================
134700000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
134800000000     C*================================================================
134900000000     C     FUNVAR        BEGSR
135000000000     C*
135100000000     C* INIZIALIZZA VARIABILE -FUNKEY-
135200000000     C*
135300000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
135400000000     C     KEY           IFEQ      ENTER
135500000000     C                   MOVEL     *BLANKS       FUNKEY
135600000000     C                   END
135700000000     C     KEY           IFEQ      F1
135800000000     C                   MOVEL     '01'          FUNKEY
135900000000     C                   END
136000000000     C     KEY           IFEQ      F2
136100000000     C                   MOVEL     '02'          FUNKEY
136200000000     C                   END
136300000000     C     KEY           IFEQ      F3
136400000000     C                   MOVEL     '03'          FUNKEY
136500000000     C                   END
136600000000     C     KEY           IFEQ      F4
136700000000     C                   MOVEL     '04'          FUNKEY
136800000000     C                   END
136900000000     C     KEY           IFEQ      F5
137000000000     C                   MOVEL     '05'          FUNKEY
137100000000     C                   END
137200000000     C     KEY           IFEQ      F6
137300000000     C                   MOVEL     '06'          FUNKEY
137400000000     C                   END
137500000000     C     KEY           IFEQ      F7
137600000000     C                   MOVEL     '07'          FUNKEY
137700000000     C                   END
137800000000     C     KEY           IFEQ      F8
137900000000     C                   MOVEL     '08'          FUNKEY
138000000000     C                   END
138100000000     C     KEY           IFEQ      F9
138200000000     C                   MOVEL     '09'          FUNKEY
138300000000     C                   END
138400000000     C     KEY           IFEQ      F10
138500000000     C                   MOVEL     '10'          FUNKEY
138600000000     C                   END
138700000000     C     KEY           IFEQ      F11
138800000000     C                   MOVEL     '11'          FUNKEY
138900000000     C                   END
139000000000     C     KEY           IFEQ      F12
139100000000     C                   MOVEL     '12'          FUNKEY
139200000000     C                   END
139300000000     C     KEY           IFEQ      F13
139400000000     C                   MOVEL     '13'          FUNKEY
139500000000     C                   END
139600000000     C     KEY           IFEQ      F14
139700000000     C                   MOVEL     '14'          FUNKEY
139800000000     C                   END
139900000000     C     KEY           IFEQ      F15
140000000000     C                   MOVEL     '15'          FUNKEY
140100000000     C                   END
140200000000     C     KEY           IFEQ      F16
140300000000     C                   MOVEL     '16'          FUNKEY
140400000000     C                   END
140500000000     C     KEY           IFEQ      F17
140600000000     C                   MOVEL     '17'          FUNKEY
140700000000     C                   END
140800000000     C     KEY           IFEQ      F18
140900000000     C                   MOVEL     '18'          FUNKEY
141000000000     C                   END
141100000000     C     KEY           IFEQ      F19
141200000000     C                   MOVEL     '19'          FUNKEY
141300000000     C                   END
141400000000     C     KEY           IFEQ      F20
141500000000     C                   MOVEL     '20'          FUNKEY
141600000000     C                   END
141700000000     C     KEY           IFEQ      F21
141800000000     C                   MOVEL     '21'          FUNKEY
141900000000     C                   END
142000000000     C     KEY           IFEQ      F22
142100000000     C                   MOVEL     '22'          FUNKEY
142200000000     C                   END
142300000000     C     KEY           IFEQ      F23
142400000000     C                   MOVEL     '23'          FUNKEY
142500000000     C                   END
142600000000     C     KEY           IFEQ      F24
142700000000     C                   MOVEL     '24'          FUNKEY
142800000000     C                   END
142900000000     C     KEY           IFEQ      ROLLUP
143000000000     C                   MOVEL     'RU'          FUNKEY
143100000000     C                   END
143200000000     C     KEY           IFEQ      HELP
143300000000     C                   MOVEL     'HP'          FUNKEY
143400000000     C                   END
143500000000     C*
143600000000     C                   ENDSR
143700000000     C*================================================================
143800000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
143900000000     C*================================================================
144000000000     C     FUNGES        BEGSR
144100000000     C*
144200000000     C* PULISCO CAMPI DI LAVORO
144300000000     C                   Z-ADD     0             W                 5 0
144400000000     C*
144500000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
144600000000     C* E CARICO TABELLA FUNZIONI ATTIVE
144700000000     C     1             DO        24            I
144800000000     C                   MOVEL     FUD(I)        FUDRIG
144900000000     C*
145000000000     C* PULIZIA FUNZIONI PRECEDENTI
145100000000     C                   MOVEL     *BLANKS       FUV(I)
145200000000     C*
145300000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
145400000000     C* (SALTA F23 E F24)
145500000000     C     FUDATT        IFEQ      'S'
145600000000     C     FUDFUN        ANDNE     '23'
145700000000     C     FUDFUN        ANDNE     '24'
145800000000     C                   ADD       1             W
145900000000     C                   MOVEL     FUDDES        FUV(W)
146000000000     C                   END
146100000000     C                   END                                                    DO WHILE
146200000000     C*
146300000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
146400000000     C* ALTRIMENTI DISATTIVO F24
146500000000     C     1             DO        24            I
146600000000     C                   MOVEL     FUD(I)        FUDRIG
146700000000     C*
146800000000     C     FUDFUN        IFEQ      '24'
146900000000     C     W             IFGT      6
147000000000     C                   MOVEL     'S'           FUDATT
147100000000     C                   ELSE
147200000000     C                   MOVEL     *BLANKS       FUDATT
147300000000     C                   END                                                    W GT 8
147400000000     C                   MOVEL     FUDRIG        FUD(I)
147500000000     C                   END
147600000000     C                   END                                                    DO WHILE
147700000000     C*
147800000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
147900000000     C     W             DIV       6             I
148000000000     C                   MVR                     Z
148100000000     C*
148200000000     C     Z             IFNE      0
148300000000     C     I             ADD       1             Z
148400000000     C                   ELSE
148500000000     C                   Z-ADD     I             Z
148600000000     C                   END
148700000000     C*
148800000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
148900000000     C                   ADD       1             FUNGRU
149000000000     C     FUNGRU        IFGT      Z
149100000000     C     FUNGRU        ORLT      1
149200000000     C                   Z-ADD     1             FUNGRU
149300000000     C                   END
149400000000     C*
149500000000     C* PRIMO GRUPPO DI 6 FUNZIONI
149600000000     C     FUNGRU        IFEQ      1
149700000000     C                   MOVEL     FUV(1)        FUN001
149800000000     C                   MOVEL     FUV(2)        FUN002
149900000000     C                   MOVEL     FUV(3)        FUN003
150000000000     C                   MOVEL     FUV(4)        FUN004
150100000000     C                   MOVEL     FUV(5)        FUN005
150200000000     C                   MOVEL     FUV(6)        FUN006
150300000000     C                   END
150400000000     C*
150500000000     C* SECONDO GRUPPO DI 6 FUNZIONI
150600000000     C     FUNGRU        IFEQ      2
150700000000     C                   MOVEL     FUV(7)        FUN001
150800000000     C                   MOVEL     FUV(8)        FUN002
150900000000     C                   MOVEL     FUV(9)        FUN003
151000000000     C                   MOVEL     FUV(10)       FUN004
151100000000     C                   MOVEL     FUV(11)       FUN005
151200000000     C                   MOVEL     FUV(12)       FUN006
151300000000     C                   END
151400000000     C*
151500000000     C* TERZO GRUPPO DI 6 FUNZIONI
151600000000     C     FUNGRU        IFEQ      3
151700000000     C                   MOVEL     FUV(13)       FUN001
151800000000     C                   MOVEL     FUV(14)       FUN002
151900000000     C                   MOVEL     FUV(15)       FUN003
152000000000     C                   MOVEL     FUV(16)       FUN004
152100000000     C                   MOVEL     FUV(17)       FUN005
152200000000     C                   MOVEL     FUV(18)       FUN006
152300000000     C                   END
152400000000     C*
152500000000     C* QUARTO GRUPPO DI 6 FUNZIONI
152600000000     C     FUNGRU        IFEQ      4
152700000000     C                   MOVEL     FUV(19)       FUN001
152800000000     C                   MOVEL     FUV(20)       FUN002
152900000000     C                   MOVEL     FUV(21)       FUN003
153000000000     C                   MOVEL     FUV(22)       FUN004
153100000000     C                   MOVEL     *BLANKS       FUN005
153200000000     C                   MOVEL     *BLANKS       FUN006
153300000000     C                   END
153400000000     C*
153500000000     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
153600000000     C                   MOVEL     *BLANKS       FUN007
153700000000     C                   MOVEL     *BLANKS       FUN008
153800000000     C*
153900000000     C     1             DO        24            I
154000000000     C                   MOVEL     FUD(I)        FUDRIG
154100000000     C     FUDFUN        IFEQ      '23'
154200000000     C     FUDATT        ANDEQ     'S'
154300000000     C                   MOVEL     FUDDES        FUN007
154400000000     C                   END
154500000000     C     FUDFUN        IFEQ      '24'
154600000000     C     FUDATT        ANDEQ     'S'
154700000000     C                   MOVEL     FUDDES        FUN008
154800000000     C                   END
154900000000     C                   END                                                    DO
155000000000     C*
155100000000     C                   ENDSR
155200000000     C*================================================================
155300000000     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
155400000000     C*================================================================
155500000000     C     OPZGES        BEGSR
155600000000     C*
155700000000     C* PULISCO CAMPI DI LAVORO
155800000000     C                   Z-ADD     0             W                 5 0
155900000000     C*
156000000000     C* RICERCO QUANTE OPZIONI SONO ATTIVE
156100000000     C* E CARICO TABELLA OPZIONI ATTIVE
156200000000     C     1             DO        24            I
156300000000     C                   MOVEL     OPD(I)        OPDRIG
156400000000     C*
156500000000     C* PULIZIA OPZIONI PRECEDENTI
156600000000     C                   MOVEL     *BLANKS       OPV(I)
156700000000     C*
156800000000     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
156900000000     C     OPDATT        IFEQ      'S'
157000000000     C                   ADD       1             W
157100000000     C                   MOVEL     OPDDES        OPV(W)
157200000000     C                   END
157300000000     C                   END                                                    DO WHILE
157400000000     C*
157500000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
157600000000     C* ALTRIMENTI DISATTIVO F23
157700000000     C     1             DO        24            I
157800000000     C                   MOVEL     FUD(I)        FUDRIG
157900000000     C*
158000000000     C     FUDFUN        IFEQ      '23'
158100000000     C     W             IFGT      8
158200000000     C                   MOVEL     'S'           FUDATT
158300000000     C                   ELSE
158400000000     C                   MOVEL     *BLANKS       FUDATT
158500000000     C                   END                                                    W GT 8
158600000000     C                   MOVEL     FUDRIG        FUD(I)
158700000000     C                   END
158800000000     C                   END                                                    DO WHILE
158900000000     C*
159000000000     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
159100000000     C     W             DIV       8             I
159200000000     C                   MVR                     Z                 5 0
159300000000     C*
159400000000     C     Z             IFNE      0
159500000000     C     I             ADD       1             Z
159600000000     C                   ELSE
159700000000     C                   Z-ADD     I             Z
159800000000     C                   END
159900000000     C*
160000000000     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
160100000000     C                   ADD       1             OPZGRU
160200000000     C     OPZGRU        IFGT      Z
160300000000     C     OPZGRU        ORLT      1
160400000000     C                   Z-ADD     1             OPZGRU
160500000000     C                   END
160600000000     C*
160700000000     C* PRIMO GRUPPO DI 8 OPZIONI
160800000000     C     OPZGRU        IFEQ      1
160900000000     C                   MOVEL     OPV(1)        OPZ001
161000000000     C                   MOVEL     OPV(2)        OPZ002
161100000000     C                   MOVEL     OPV(3)        OPZ003
161200000000     C                   MOVEL     OPV(4)        OPZ004
161300000000     C                   MOVEL     OPV(5)        OPZ005
161400000000     C                   MOVEL     OPV(6)        OPZ006
161500000000     C                   MOVEL     OPV(7)        OPZ007
161600000000     C                   MOVEL     OPV(8)        OPZ008
161700000000     C                   END
161800000000     C*
161900000000     C* SECONDO GRUPPO DI 8 OPZIONI
162000000000     C     OPZGRU        IFEQ      2
162100000000     C                   MOVEL     OPV(9)        OPZ001
162200000000     C                   MOVEL     OPV(10)       OPZ002
162300000000     C                   MOVEL     OPV(11)       OPZ003
162400000000     C                   MOVEL     OPV(12)       OPZ004
162500000000     C                   MOVEL     OPV(13)       OPZ005
162600000000     C                   MOVEL     OPV(14)       OPZ006
162700000000     C                   MOVEL     OPV(15)       OPZ007
162800000000     C                   MOVEL     OPV(16)       OPZ008
162900000000     C                   END
163000000000     C*
163100000000     C* TERZO GRUPPO DI 8 OPZIONI
163200000000     C     OPZGRU        IFEQ      3
163300000000     C                   MOVEL     OPV(17)       OPZ001
163400000000     C                   MOVEL     OPV(18)       OPZ002
163500000000     C                   MOVEL     OPV(19)       OPZ003
163600000000     C                   MOVEL     OPV(20)       OPZ004
163700000000     C                   MOVEL     OPV(21)       OPZ005
163800000000     C                   MOVEL     OPV(22)       OPZ006
163900000000     C                   MOVEL     OPV(23)       OPZ007
164000000000     C                   MOVEL     OPV(24)       OPZ008
164100000000     C                   END
164200000000     C                   ENDSR
164300000000     C*================================================================
164400000000     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
164500000000     C*================================================================
164600000000     C     OPZCON        BEGSR
164700000000     C*
164800000000     C* PULISCO CAMPI DI LAVORO
164900000000     C                   Z-ADD     0             I                 5 0
165000000000     C                   Z-ADD     0             W                 5 0
165100000000     C*
165200000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
165300000000     C     1             DO        24            I
165400000000     C                   MOVEL     OPD(I)        OPDRIG
165500000000     C*
165600000000     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
165700000000     C     OPZKEY        IFEQ      OPDOPZ
165800000000     C     OPZKEY        OREQ      OPDOP1
165900000000     C*
166000000000     C     OPDATT        IFEQ      'S'
166100000000     C                   MOVEL     OPDOPZ        OPZKEY
166200000000     C                   Z-ADD     I             W
166300000000     C                   END
166400000000     C                   END
166500000000     C*
166600000000     C                   END                                                    DO WHILE
166700000000     C*
166800000000     C* OPZIONE NON ATTIVA (ERRORE)
166900000000     C     W             IFEQ      0
167000000000     C*
167100000000     C* OPZIONE NON OK
167200000000     C                   Z-ADD     1             OPZOK
167300000000     C*
167400000000     C* EMISSIONE MESSAGGIO DI ERRORE
167500930127     C                   MOVEL     MSG(1)        DSMSMS
167600000000     C                   ELSE
167700000000     C*
167800000000     C* OPZIONE ATTIVA (PROCEDI ...)
167900000000     C*
168000930129     C     OPZKEY        IFEQ      ' 1'
168100000000     C                   EXSR      O01GES
168200000000     C                   END
168300000000     C     OPZKEY        IFEQ      ' 2'
168400000000     C                   EXSR      O02GES
168500000000     C                   END
168600000000     C     OPZKEY        IFEQ      ' 3'
168700000000     C                   EXSR      O03GES
168800000000     C                   END
168900000000     C     OPZKEY        IFEQ      ' 4'
169000020709     c                   If        §UteCun = *Blanks
169100020320     c                   eval      dsmsms = msg(11)
169200020320     c                   z-add     1             opzok
169300020320     c                   else
169400000000     C                   EXSR      O04GES
169500020320     c                   endif
169600000000     C                   END
169700000000     C     OPZKEY        IFEQ      ' 5'
169800000000     C                   EXSR      O05GES
169900000000     C                   END
170000000000     C     OPZKEY        IFEQ      ' 6'
170100000000     C                   EXSR      O06GES
170200000000     C                   END
170300930128     C     OPZKEY        IFEQ      ' 7'
170400930128     C                   EXSR      O07GES
170500930128     C                   END
170600930201     C     OPZKEY        IFEQ      ' 8'
170700930201     C                   EXSR      O08GES
170800930201     C                   END
170900000000     C*
171000000000     C                   END                                                    OPZ.NON ATTIVA
171100000000     C*
171200000000     C                   ENDSR
171300000000     C*================================================================
171400000000     C* NOTE PER COMPILAZIONE SCHIERE
171500000000     C*================================================================
171600000000     C* -MSG-
171700000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
171800000000     C* (NON TOCCARE MSG,1 E MSG,2
171900000000     C*
172000000000     C* -CMD-
172100000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
172200000000     C*
172300000000     C* -OPD-
172400000000     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
172500000000     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
172600000000     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
172700000000     C*    ESEMPIO: OPZIONI -1 -
172800000000     C*                     - 1-  SONO LA STESSA COSA
172900000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
173000000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
173100000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
173200000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
173300000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
173400000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
173500000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
173600000000     C*
173700000000     C* -FUD-
173800000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
173900000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
174000000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
174100000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
174200000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
174300000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
174400000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
174500000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
174600000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
174700000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
174800000000     C*
174900000000     C*================================================================
175000000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
175100020517TNSDUIR- Opzione non gestita.                                                  1
175200020517TNSDUIR- Tasto funzionale non gestito.                                         2
175300020517TNSDUIR- Non abilitato alla gestione di questa Unità.                          3
175400020517TNSDUIR-                                                                       4
175500020517TNSDUIR-                                                                       5
175600020517TNSDUIR- Accettato solo un parametro di inizio lista.                          6
175700020517TNSDUIR- Tipo unità non accettabile.                                           7
175800020517TNSDUIR- Immettere o il codice cliente o la ragione sociale                    8
175900020517TNSDUIR- Cliente non accettabile                                               9
176000020517TNSDUIR- Matricola non accettabile.                                           10
176100020517TNSDUIR-                                                                      11
176200020517TNSDUIR- Matricola obbligatoria se richiesta ricerca per contenuto            12
176300020517TNSDUIR- Profilo non abilitato alla funzione                                  13
176400020517TNSDUIR- Profilo non abilitato al cliente richiesto                           14
176500020517TNSDUIR-                                                                      15
176600020517TNSDUIR- 16                                                                   16
176700020517TNSDUIR- 17                                                                   17
176800020517TNSDUIR- 18                                                                   18
176900020517TNSDUIR- 19                                                                   19
177000020517TNSDUIR- 20                                                                   20
177100000000** ======== SCHIERA: CMD  (COMANDI)                        ================
177200000000WRKSPLF
177300000000
177400000000
177500000000
177600000000
177700000000** ======== SCHIERA: OPD  OPZIONI GESTITE NEL PGM (ATTIVA=S) ==========
177800930312 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
177900930312 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
178000971015 3=Duplica         -3 -     (S/N)=N                        opz. 03¬
178100930312 4=Cancellazione   -4 -     (S/N)=N                        opz. 04¬
178200930312 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
178300000000                   -6 -     (S/N)=N                        opz. 06¬
178400930312 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
178500971013 8=Movimenti       -8 -     (S/N)=N                        opz. 08¬
178600930129                   -9 -     (S/N)=N                        opz. 09¬
178700000000                   -  -     (S/N)=N                        opz. 10¬
178800000000                   -  -     (S/N)=N                        opz. 11¬
178900000000                   -  -     (S/N)=N                        opz. 12¬
179000000000                   -  -     (S/N)=N                        opz. 13¬
179100000000                   -  -     (S/N)=N                        opz. 14¬
179200000000                   -  -     (S/N)=N                        opz. 15¬
179300000000                   -  -     (S/N)=N                        opz. 16¬
179400000000                   -  -     (S/N)=N                        opz. 17¬
179500000000                   -  -     (S/N)=N                        opz. 18¬
179600000000                   -  -     (S/N)=N                        opz. 19¬
179700000000                   -  -     (S/N)=N                        opz. 20¬
179800000000                   -  -     (S/N)=N                        opz. 21¬
179900000000                   -  -     (S/N)=N                        opz. 22¬
180000000000                   -  -     (S/N)=N                        opz. 23¬
180100000000                   -  -     (S/N)=N                        opz. 24¬
180200000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
180300930129                   -        (S/N)=N                        funz.01¬
180400930129                   -        (S/N)=N                        funz.02¬
180500930129F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
180600930129                   -        (S/N)=N                        funz.04¬
180700971013F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
180800930312F06=Inserimento    -        (S/N)=N                        funz.06¬
180900000000                   -        (S/N)=N                        funz.07¬
181000000000                   -        (S/N)=N                        funz.08¬
181100000000                   -        (S/N)=N                        funz.09¬
181200930204 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.10¬
181300930204 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.11¬
181400930129F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
181500000000                   -        (S/N)=N                        funz.13¬
181600930129                   -        (S/N)=N                        funz.14¬
181700930129                   -        (S/N)=N                        funz.15¬
181800930126                   -        (S/N)=N                        funz.16¬
181900000000F17=Inizio dati    -        (S/N)=S  <== S-FISSO           funz.17¬
182000000000F18=Fine dati      -        (S/N)=S  <== S-FISSO           funz.18¬
182100930128                   -        (S/N)=N                        funz.19¬
182200930129                   -        (S/N)=N                        funz.20¬
182300020517F21=Stampa         -        (S/N)=N                        funz.21¬
182400940105F22=Gestione stampe-        (S/N)=N                        funz.22¬
182500000000F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
182600000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
