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
003100100922     D FUD             S             80    DIM(25) CTDATA PERRCD(1)
003200020319      * TABELLA OPZIONI DA CARICARE A VIDEO
003300000000     D OPV             S             19    DIM(24)
003400020319      * TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
003500000000     D FUV             S             19    DIM(24)
003600020523
003700140502     d  skpoabi        s              3  0 dim(500)
003800140502
003900140502     d  sav_poabi      s              3  0 dim(500)
004000020319
004100020319      *------------------------------------------------------------------------*
004200020319      * INPUT
004300020319      *------------------------------------------------------------------------*
004400020319
004500020517     d deptip          s                   like(vdptip)
004600020517     d depmat          s                   like(vdpmat)
004700020517     d depric          s                   like(vdpric)
004800020517     d depksc          s                   like(vdpksc)
004900020517     d deprsc          s                   like(vdprsc)
005000140502     d deparx          s                   like(vdparx)
005100170517     d deplsw          s                   like(vdplsw)
005200020517
005300020517     d paxccc          s                   LIKE(ACOkcc)
005400020517     d paxdmt          s                   LIKE(ACOrag)
005500020517     d PAXdut          s             30
005600020517     d PAXdit          s              3
005700020517     d PAXflr          s             90
005800020517     d PAXsta          s              1  0
005900020517
006000020319     d wtipri          s              3
006100140505     d wfil            s              3  0
006200020325     d wksc            s              3  0
006300020319     d wnosfl          s              1    inz('0')
006400020410     d wprimo          s              1    inz('0')
006500020325     d xx              s              3  0
006600020517     d ii              s              3  0
006700020516     d wabi            s                   like(uteaut)
006800020523     d wmag            s              3  0
006900020319
007000020319      * RIDEFINIZIONE SCHIERA -OPD-
007100000000     D OPDDS           DS
007200000000     D  OPDOPZ                 1      2
007300000000     D  OPDDE1                 3     19
007400000000     D  OPDOP1                21     22
007500000000     D  OPDFI0                23     34
007600000000     D  OPDATT                35     35
007700000000     D  OPDFI1                36     80
007800000000     D  OPDDES                 1     19
007900000000     D  OPDRIG                 1     80
008000020319      * RIDEFINIZIONE SCHIERA -FUD-
008100000000     D FUDDS           DS
008200000000     D  FUDDE1                 1      1
008300000000     D  FUDFUN                 2      3
008400000000     D  FUDDE2                 4     19
008500000000     D  FUDFI0                20     34
008600000000     D  FUDATT                35     35
008700000000     D  FUDFI1                36     80
008800000000     D  FUDDES                 1     19
008900000000     D  FUDRIG                 1     80
009000020319      * PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
009100930127     D DSMSG         E DS
009200020319      *------------------
009300020319      * DS "XSRDA8" - CONTROLLA DATA (8)
009400020319      *------------------
009500971010     D WLBDA8          DS
009600971010     D  G08DAT                 1      8  0
009700971010     D  G08INV                 9     16  0
009800971010     D  G08ERR                17     17
009900971010     D  G08TGI                18     22  0
010000020319      * PARAMETRI PER LANCIO ALTRI PGM DELLA PROCEDURA
010100930707     D DSUNIT        E DS
010200020319      * DS PER GESTIONE TASTI FUNZIONALI
010300930127     D DSKEY         E DS
010400000000     D DEVDS1          DS
010500000000     D  KEY                  369    369
010600020319      * ARCHITETTURA
010700930312     D KPJBA         E DS
010800020318
010900020318     d tibs02ds      e ds
011000020517     d tibs69ds      e ds
011100020517     d ds_cnaco      e ds                  extname(cnaco00f)
011200020517     d ds_cnind      e ds                  extname(cnind00f)
011300020517     d ds_cnclp      e ds                  extname(cnclp00f)
011400020517     d ds_fncls      e ds                  extname(fncls00f)
011500020516
011600020516     d ddatiute      e ds
011700020516     d azuteds       e ds                  extname(azute00f)
011800020516     d tibs34ds      e ds                  inz
011900020516     d dute01        e ds
012000020516     d dlat          e ds
012100020516     d trul31ds      e ds
012200020523     d  skpo                  10    759  0 dim(250)
012300100906      *
012400100906     d Og143         e ds                  inz
012500100906
012600100906     d c_Logistica     c                   const('LOG')
012700020325
012800971010     C*------------------------------------------------------------------------*
012900971010     C* MAIN LINE
013000971010     C*------------------------------------------------------------------------*
013100000000     C* MAIN DEL PGM
013200000000     C                   EXSR      RUTINZ
013300000000     C*
013400930126     C* SE CONTROLLI PRELIMINARI NON OK VADO A FINE PGM
013500930126     C     ERRL00        IFNE      '1'                                          --- 1 -->
013600020516
013700020516      * controllo il profilo utente
013800020516     c                   exsr      ctrute
013900020516     c                   if        *in76
014000020516     c                   eval      sflult = 0
014100020516     c                   endif
014200000000     C*
014300000000     C* CARICA IL SFL DAI PARAMETRI IN INPUT AL PROGRAMMA
014400000000     C                   EXSR      POSSFL
014500000000     C*
014600000000     C* CICLO FINO A CHE:
014700000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
014800000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
014900930707     C*  -FINE PGM PER ENTER SENZA SCELTA
015000000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
015100000000     C     F12L00        OREQ      '1'
015200000000     C*
015300000000     C* AZZERA FLAG DI ERROE
015400000000     C                   MOVEL     '0'           ERRL00
015500020410
015600020410      * posiziono il cursore sulla matricola se prima volta
015700020410     c                   if        wprimo = *off
015800140428     c                   if        %parms>1
015900140428     c                   eval      *in89=*on
016000140428     c                   else
016100020410     c                   eval      *in88 = *on
016200140428     c                   endif
016300020410     c                   eval      wprimo = *on
016400020410     c                   else
016500020410     c                   eval      *in88 = *off
016600140428     c                   eval      *in89 = *off
016700020410     c                   endif
016800020516
016900020516      * profilo non abilitato
017000020516     c                   if        *in76
017100020516     c                   eval      f03l00 = *on
017200020516     c                   endif
017300000000     C*
017400000000     C* EMETTE SFL A VIDEO
017500000000     C                   EXSR      OUTSFL
017600000000     C*
017700000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI -ROUTINE FUNCON
017800000000     C                   EXSR      FUNCON
017900000000     C*
018000000000     C                   END                                                    <-- 2 ---
018100000000     C*
018200000000     C                   END                                                    <-- 1 --
018300000000     C*
018400940105     C                   MOVEL     DSUNIT        KPJBU
018500000000     C                   SETON                                        LR
018600000000     C*================================================================
018700930126     C* F03GES -GEST.TASTO FUNZ. F03 ===> FINE
018800000000     C*================================================================
018900000000     C     F03GES        BEGSR
019000000000     C*
019100000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PGM 00)
019200000000     C                   MOVEL     '1'           F03L00
019300000000     C*
019400000000     C                   ENDSR
019500000000     C*================================================================
019600930129     C* F05GES -GEST.TASTO FUNZ. F05 ===> RIVISUALIZZAZIONE
019700000000     C*================================================================
019800000000     C     F05GES        BEGSR
019900000000     C*
020000000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
020100000000     C                   EXSR      POSSFL
020200000000     C*
020300000000     C                   ENDSR
020400000000     C*================================================================
020500931116     C* F06GES -GEST.TASTO FUNZ. F06 ===> INSERIMENTO
020600000000     C*================================================================
020700930126     C     F06GES        BEGSR
020800930201     C*
020900930201     C* INSERIMENTO CONSENTITO SOLO IN SEDE
021000020709     c                   If        DutLpo <> 'S'
021100930201     C                   MOVEL     '1'           ERRL00
021200930201     C                   MOVEL     MSG(3)        DSMSMS
021300930201     C                   END
021400930201     C*
021500930201     C     ERRL00        IFEQ      '0'                                          --- 1 -->
021600930127     C*
021700941115     C* LANCIO IL PGM PER L'INSERIMENTO DEL RECORD (TNSDU2R)
021800930201     C                   MOVEL     'F06'         WOPZ1             3
021900100906     C                   MOVEL     'TNSDU2R'     WPGM              8
022000930201     C                   EXSR      LANPGM
022100930128     C*
022200930128     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
022300930128     C                   EXSR      POSSFL
022400930201     C*
022500930201     C                   END                                                    <-- 1 ---
022600930127     C*
022700000000     C                   ENDSR
022800000000     C*================================================================
022900140424     C* F14GES -GEST.TASTO FUNZ. F14 ===> richiamo pgm gestine contratti comod
023000000000     C*================================================================
023100100922     C     F14GES        BEGSR
023200000000     C*
023300000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12 (A LIVELLO PGM 00)
023400100922     C                   clear                   kpjbu
023500140424     c                   call      'TNSDUNR'
023600100922     c                   parm                    kpjba
023700000000     C*
023800000000     C                   ENDSR
023900100922     C*================================================================
024000100922     C* F12GES -GEST.TASTO FUNZ. F12 ===> RITORNO
024100100922     C*================================================================
024200100922     C     F12GES        BEGSR
024300100922     C*
024400100922     C* IMPOSTO FLAG DI PREMUTO TASTO F12 (A LIVELLO PGM 00)
024500100922     C                   MOVEL     '1'           F12L00
024600100922     C*
024700100922     C                   ENDSR
024800000000     C*================================================================
024900930126     C* F17GES -GEST.TASTO FUNZ. F17 ===> INIZIO DATI
025000000000     C*================================================================
025100000000     C     F17GES        BEGSR
025200000000     C*
025300000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
025400000000     C                   Z-ADD     1             SFLNBR
025500000000     C*
025600000000     C                   ENDSR
025700000000     C*================================================================
025800930126     C* F18GES -GEST.TASTO FUNZ. F18 ===> FINE DATI
025900000000     C*================================================================
026000000000     C     F18GES        BEGSR
026100000000     C*
026200000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
026300000000     C                   Z-ADD     SFLULT        SFLNBR
026400000000     C*
026500000000     C                   ENDSR
026600020517     C*================================================================
026700020517     C* F21GES -GEST.TASTO FUNZ. F21 ===> STAMPA ELENCO
026800020517     C*================================================================
026900020517     C     F21GES        BEGSR
027000020517     C*
027100020517     C                   ENDSR
027200000000     C*================================================================
027300930126     C* F22GES -GEST.TASTO FUNZ. F22 ===> STAMPE
027400000000     C*================================================================
027500000000     C     F22GES        BEGSR
027600000000     C*
027700000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
027800000000     C                   Z-ADD     80            LENGH            15 5
027900000000     C                   CALL      'QCAEXEC'
028000000000     C                   PARM                    CMD(1)
028100000000     C                   PARM                    LENGH
028200000000     C*
028300000000     C                   ENDSR
028400000000     C*================================================================
028500930126     C* F23GES -GEST.TASTO FUNZ. F23 ===> ALTRE OPZIONI
028600000000     C*================================================================
028700000000     C     F23GES        BEGSR
028800000000     C*
028900000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
029000000000     C                   EXSR      OPZGES
029100000000     C*
029200000000     C                   ENDSR
029300000000     C*================================================================
029400930126     C* F24GES -GEST.TASTO FUNZ. F24 ===> ALTRI TASTI
029500000000     C*================================================================
029600000000     C     F24GES        BEGSR
029700000000     C*
029800000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
029900000000     C                   EXSR      FUNGES
030000000000     C*
030100000000     C                   ENDSR
030200000000     C*================================================================
030300930126     C* RUPGES -GEST.TASTO FUNZ. ROLLUP ===> PAGINA SUCCESSIVA
030400000000     C*================================================================
030500000000     C     RUPGES        BEGSR
030600000000     C*
030700000000     C* CARICA IL SFL DA FILE "FISICO"
030800000000     C                   EXSR      CARSFL
030900000000     C*
031000000000     C                   ENDSR
031100000000     C*================================================================
031200930126     C* HELGES - GEST.TASTO FUNZ. HELP ===> AIUTO
031300000000     C*================================================================
031400000000     C     HELGES        BEGSR
031500000000     C*
031600000000     C                   ENDSR
031700000000     C*================================================================
031800930126     C* ENTGES - GEST.TASTO FUNZ. ENTER ===> INVIO
031900000000     C*================================================================
032000000000     C     ENTGES        BEGSR
032100000000     C*
032200000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
032300000000     C                   EXSR      NEWPOS
032400000000     C*
032500000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
032600000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
032700000000     C*
032800000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
032900000000     C     SWPOSI        IFNE      0                                            --- 2 -->
033000000000     C                   EXSR      POSSFL
033100000000     C*
033200000000     C                   ELSE                                                   <-- 2 -->
033300000000     C*
033400000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
033500000000     C     SFLULT        IFNE      0                                            --- 3 -->
033600000000     C                   EXSR      ENTATT
033700000000     C*
033800000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
033900000000     C     SWMODI        IFNE      0                                            --- 4 -->
034000000000     C                   EXSR      POSSFL
034100000000     C                   END                                                    <-- 4 ---
034200000000     C*
034300000000     C                   END                                                    <-- 3 ---
034400000000     C                   END                                                    <-- 2 ---
034500000000     C                   END                                                    <-- 1 ---
034600000000     C*
034700000000     C                   ENDSR
034800000000     C*================================================================
034900930126     C*  O01GES - GESTIONE OPZIONE 1=SCELTA
035000000000     C*================================================================
035100000000     C     O01GES        BEGSR
035200000000     C                   ENDSR
035300000000     C*================================================================
035400000000     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
035500000000     C*================================================================
035600000000     C     O02GES        BEGSR
035700100906     C*
035800100906     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA MODIFICA
035900100906     C     VIDATB        IFEQ      'A'
036000100906     C                   MOVEL     '1'           ERRL00
036100100906     C                   MOVEL     MSG(4)        DSMSMS
036200100906     C                   END
036300100906     C*
036400100906     C     ERRL00        IFEQ      '0'                                          --- 1 -->
036500100906     C*
036600100906     C* LANCIO IL PGM PER LA MODIFICA DEL DETTAGLIO DELL'ANAGRAFICA
036700100906     C                   MOVEL     'O02'         WOPZ1
036800100906     C                   MOVEL     'TNSDU2R'     WPGM
036900100906     C                   EXSR      LANPGM
037000100906     C*
037100100906     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
037200100906     C                   Z-ADD     1             SWMODI
037300100906     C*
037400100906     C                   END                                                    <-- 1 ---
037500100906     C*
037600000000     C*
037700000000     C                   ENDSR
037800000000     C*================================================================
037900971015     C* O03GES - GESTIONE OPZIONE 3=DUPLICA
038000000000     C*================================================================
038100000000     C     O03GES        BEGSR
038200100906     C*
038300100906     C* DUPLICA CONSENTITA SOLO IN SEDE
038400100906     c                   If        DutLpo <> 'S'
038500100906     C                   MOVEL     '1'           ERRL00
038600100906     C                   MOVEL     MSG(3)        DSMSMS
038700100906E   1C                   ENDIF
038800100906     C*
038900100906     C* LANCIA IL PGM PER L'INSERIMENTO
039000100906IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
039100100906     C                   MOVEL     'O03'         WOPZ1
039200100906     C                   MOVEL     'TNSDU2R'     WPGM
039300100906     C                   EXSR      LANPGM
039400100906     C*
039500100906     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
039600100906     C                   Z-ADD     1             SWMODI
039700100906E   1C                   ENDIF
039800100906     C*
039900000000     C                   ENDSR
040000000000     C*================================================================
040100000000     C*  O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE
040200000000     C*================================================================
040300000000     C     O04GES        BEGSR
040400100906     C*
040500100906     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
040600100906     C     VIDATB        IFEQ      'A'
040700100906     C                   MOVEL     '1'           ERRL00
040800100906     C                   MOVEL     MSG(4)        DSMSMS
040900100906     C                   END
041000100906     C*
041100100906     C     ERRL00        IFEQ      '0'                                          --- 1 -->
041200100906     C*
041300100906     C* LANCIO IL PGM DI GESTIONE DELLE UNITA
041400100906     C                   MOVEL     'O04'         WOPZ1
041500100906     C                   MOVEL     'TNSDU2R'     WPGM
041600100906     C                   EXSR      LANPGM
041700100906     C*
041800100906     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
041900100906     C                   Z-ADD     1             SWMODI
042000100906     C*
042100100906     C                   END                                                    <-- 1 ---
042200100906     C*
042300000000     C*
042400000000     C                   ENDSR
042500000000     C*================================================================
042600000000     C*  O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
042700000000     C*================================================================
042800000000     C     O05GES        BEGSR
042900000000     C*
043000000000     C* LANCIO ROUTINE PER LA VISUALIZZAZIONE DEL DETTAGLIO DELLA RIGA
043100930201     C                   MOVEL     'O05'         WOPZ1
043200020517     C                   MOVEL     'TNSDU2R'     WPGM
043300930201     C                   EXSR      LANPGM
043400000000     C*
043500000000     C                   ENDSR
043600000000     C*================================================================
043700000000     C*  O06GES - GESTIONE OPZIONE 6=STAMPA
043800000000     C*================================================================
043900000000     C     O06GES        BEGSR
044000000000     C                   ENDSR
044100930128     C*================================================================
044200930128     C*  O07GES - GESTIONE OPZIONE 7=RIPRISTINO
044300930128     C*================================================================
044400930128     C     O07GES        BEGSR
044500100906     C*
044600100906     C* SE IL RECORD NON E' ANNULLATO NON CONSENTO IL RIPRISTINO
044700100906     C     VIDATB        IFNE      'A'
044800100906     C                   MOVEL     '1'           ERRL00
044900100906     C                   MOVEL     MSG(5)        DSMSMS
045000100906     C                   END
045100100906     C*
045200100906     C     ERRL00        IFEQ      '0'                                          --- 1 -->
045300100906     C*
045400100906     C* LANCIO IL PGM PER IL RIPRISTINO DI UN ANNULLAMENTO
045500100906     C                   MOVEL     'O07'         WOPZ1
045600100906     C                   MOVEL     'TNSDU2R'     WPGM
045700100906     C                   EXSR      LANPGM
045800100906     C*
045900100906     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
046000100906     C                   Z-ADD     1             SWMODI
046100100906     C*
046200100906     c                   endif
046300930128     C*
046400930128     C                   ENDSR
046500930201     C*================================================================
046600940105     C*  O08GES - GESTIONE OPZIONE 8=GESTIONE MOVIMENTI
046700930201     C*================================================================
046800930201     C     O08GES        BEGSR
046900930201     C*
047000930201     C* LANCIO IL PGM PER LA GESTIONE DELLE LOCALITA'
047100971013     C     ERRL00        IFEQ      '0'                                          --- 1 -->
047200930201     C                   MOVEL     'O08'         WOPZ1
047300941115     C                   MOVEL     'TNSDU4R'     WPGM
047400930201     C                   EXSR      LANPGM
047500930201     C                   END                                                    <-- 1 ---
047600930201     C*
047700930201     C                   ENDSR
047800000000     C*================================================================
047900000000     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
048000000000     C*================================================================
048100000000     C     NEWPOS        BEGSR
048200000000     C*
048300000000     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAM.
048400000000     C*         1= RICHIESTO POSIZIONAM.
048500000000     C                   Z-ADD     0             SWPOSI            7 0
048600000000     C*
048700000000     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
048800000000     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
048900000000     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
049000000000     C                   Z-ADD     0             SWMODI            1 0
049100000000     C*
049200000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
049300020319     c                   if        vdpmat <> depmat or vdptip <> deptip or      --- 1 -->
049400020517     c                             vdpksc <> depksc or vdprsc <> deprsc or
049500170517     c                             vdpric <> depric or vdplsw <> deplsw
049600140502     c                             or vdparx <> deparx
049700930202     C*
049800930202     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI DI PARTENZA
049900930202     C                   EXSR      CHKPAR
050000930202     C*
050100930202     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
050200930202     C     ERRL00        IFEQ      '0'                                          --- 2 -->
050300000000     C                   Z-ADD     1             SWPOSI
050400020517     c                   eval      deptip = vdptip
050500020319     c                   eval      depmat = vdpmat
050600020325     c                   eval      depric = vdpric
050700020517     c                   eval      depksc = vdpksc
050800020517     c                   eval      deprsc = vdprsc
050900140502     c                   eval      deparx = vdparx
051000170517     c                   eval      deplsw = vdplsw
051100930202     C                   END                                                    <-- 2 ---
051200930202     C*
051300000000     C                   END                                                    <-- 1 ---
051400000000     C*
051500000000     C                   ENDSR
051600000000     C*================================================================
051700000000     C* LETTURA RECORD FILE DA CARICARE
051800000000     C*================================================================
051900000000     C     LETFIL        BEGSR
052000020319
052100020319     c                   select
052200020319
052300020319     c                   when      wtipri = 'TIP'
052400020319     c                   read      unana02l                               99
052500020319
052600020319     c                   when      wtipri = 'MAT'
052700020319     c                   read      unana07l                               99
052800020319
052900020517     c                   when      wtipri = 'KSC'
053000020517     c     kuna10        reade     unana10l                               99
053100020319
053200020319     c                   endsl
053300020319
053400000000     C*
053500930201     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
053600930707     C     *IN99         IFEQ      '1'
053700000000     C                   Z-ADD     1             SFLEOF
053800930707     C                   END
053900000000     C*
054000000000     C                   ENDSR
054100000000     C*================================================================
054200000000     C* SETFIL - SETLL DEL FILE DA CARICARE
054300000000     C*================================================================
054400000000     C     SETFIL        BEGSR
054500020319
054600020319      * POSIZIONAMENTO DEL FILE DA CARICARE E PRIMA LETTURA
054700020319
054800020319     c                   select
054900020319
055000020517      * selezione per tipo
055100020319     c                   when      wtipri = 'TIP'
055200020319     c                   move      vdptip        unatip
055300020319     c                   move      vdpmat        unamat
055400020319     c     kuna02        setll     unana02l
055500020319     c                   if        not %found(unana02l)
055600020319     c                   eval      sfleof = 1
055700020319     c                   else
055800020319     c                   read      unana02l
055900020319     c                   if        %eof(unana02l)
056000020319     c                   eval      sfleof = 1
056100020319     c                   endif
056200020319     c                   endif
056300020319
056400020517      * selezione per matricola
056500020319     c                   when      wtipri = 'MAT'
056600020325      * no ricerca per contenuto
056700020325     c                   if        vdpric = *blanks
056800020319     c                   move      vdpmat        unamat
056900020319     c                   move      vdptip        unatip
057000020319     c     kuna07        setll     unana07l
057100020319     c                   if        not %found(unana07l)
057200020319     c                   eval      sfleof = 1
057300020319     c                   else
057400020319     c                   read      unana07l
057500020319     c                   if        %eof(unana07l)
057600020319     c                   eval      sfleof = 1
057700020319     c                   endif
057800020319     c                   endif
057900020325      * si ricerca per contenuto
058000020325     c                   else
058100020325     c     *loval        setll     unana07l
058200020325     c                   if        not %found(unana07l)
058300020325     c                   eval      sfleof = 1
058400020325     c                   else
058500020325     c                   read      unana07l
058600020325     c                   if        %eof(unana07l)
058700020325     c                   eval      sfleof = 1
058800020325     c                   endif
058900020325     c                   endif
059000020325     c                   endif
059100020319
059200020517      * selezione per cliente
059300020517     c                   when      wtipri = 'KSC'
059400020517     c                   eval      unaksc = vdpksc
059500020517     c     kuna10        setll     unana10l
059600020517     c                   if        not %found(unana10l)
059700020319     c                   eval      sfleof = 1
059800020319     c                   else
059900020517     c     kuna10        reade     unana10l
060000020517     c                   if        %eof(unana10l)
060100020319     c                   eval      sfleof = 1
060200020319     c                   endif
060300020319     c                   endif
060400020517
060500020319     c                   endsl
060600020319
060700000000     C                   ENDSR
060800020319      *================================================================
060900020319      * CTRSFL - CONTROLLO SE RECORD DA SCRIVERE NEL SUBFILE
061000020319      *================================================================
061100020319     C     CTRSFL        BEGSR
061200020319
061300020325      * record letto ok
061400020517
061500020517     c                   if        unaksc = *zeros and wnosfl = *off
061600020517     c                   eval      wnosfl = *on
061700020517     c                   endif
061800020517
061900020517     c                   if        wnosfl = *off
062000020517     c                   movel     unaksc        wmag
062100020517     c     wmag          lookup    skpoabi                                99
062200020517     c                   if        not *in99
062300020517     c                   eval      wnosfl = *on
062400020517     c                   endif
062500020517     c                   endif
062600020325
062700020325     c                   if        wtipri = 'MAT' and vdpric = 'S'
062800020517     c                             and wnosfl = *off
062900020325     c     ' '           checkr    vdpmat        xx
063000020325     c     vdpmat:xx     scan      unamat                                 99
063100020325     c                   if        not *in99
063200020325     c                   eval      wnosfl = *on
063300020325     c                   endif
063400020325     c                   endif
063500170517     c
063600170517     c* se ho parzializzato per contratto, verifico se corrisponde
063700170517     c                   if        vdplsw<>*blanks and
063800170517     c                             vdplsw<>%subst(unalsw:1:14)
063900170517     c                   eval      wnosfl = *on
064000170517     c                   endif
064100020325
064200020319     C                   ENDSR
064300000000     C*================================================================
064400000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
064500000000     C*================================================================
064600000000     C     IRESFL        BEGSR
064700000000     C*
064800000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
064900930127     C*
065000930707     C                   MOVEL     UNAATB        VIDATB
065100930707     C                   Z-ADD     UNATIP        VIDTIP
065200930707     C                   Z-ADD     UNACOD        VIDCOD
065300930707     C                   MOVEL     UNAMAT        VIDMAT
065400930707     C                   MOVEL     UNAMOD        VIDMOD
065500020517     c                   eval      vidksc = unaksc
065600020517     c                   clear                   TIBS69DS
065700020517     c                   z-add     vidksc        I69kac
065800020517     c                   call      'TIBS69R'
065900020517     c                   parm                    tibs69DS
066000020517     c                   parm                    ds_cnaco
066100020517     c                   parm                    ds_cnind
066200020517     c                   parm                    ds_cnclp
066300020517     c                   parm                    ds_fncls
066400020517     c                   if        o69err <> '1'
066500020517     c                   movel     acorag        vidrsc
066600020517     c                   endif
066700000000     C*
066800000000     C                   ENDSR
066900930202     C*================================================================
067000930202     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
067100930202     C*================================================================
067200930202     C     CHKPAR        BEGSR
067300020319
067400020319      * IMPOSTO FLAG EVIDENZAZIONE TIPO DI RICERCA
067500020319     c                   eval       *in90 = *off                                x tipo
067600020319     c                   eval       *in91 = *off                                x matricola
067700020517     c                   eval       *in92 = *off                                x cliente
067800020319
067900020319      * IMPOSTO FLAG EVIDENZAZIONE ERRORI SUI PARAMETRI
068000020319     c                   eval       *in80 = *off                                tipo
068100020319     c                   eval       *in81 = *off                                tipo
068200020319     c                   eval       *in82 = *off                                matricola
068300020517     c                   eval       *in83 = *off                                cliente
068400140502     c                   eval       *in85 = *off                                area ex/poc
068500020326
068600020326     c                   clear                   wtipri
068700020319
068800020319      * Se non è stata scelta una ricerca utilizzo la ricerca per tipo
068900020319     c                   if        vdpmat = *blanks and vdptip = *blanks and
069000020517     c                             vdpksc = *zeros and vdprsc = *blanks
069100020319     c                   eval      wtipri = 'TIP'
069200020319     c                   eval      *in90 = *on
069300020319     c                   endif
069400020319
069500020319      * ===> ACCETTO SOLO UN PARAMETRO DI INIZIO RICERCA
069600020319
069700020319      * Tipo unità / matricola
069800020319     c                   if        vdptip <> *blanks
069900020319      * controllo tipo unità
070000020319     c                   exsr      chktip
070100020319     c                   eval      wtipri = 'TIP'
070200020319     c                   eval      *in90 = *on
070300020319     c                   endif
070400020319
070500020319      * Matricola
070600020410     c                   if        vdpmat <> *blanks and errl00 = *off and
070700020410     c                             vdptip = *blanks
070800020319     c                   eval      wtipri = 'MAT'
070900020319     c                   eval      *in91 = *on
071000020319     c                   endif
071100020325      * ricerca x matricla
071200020325     c                   if        errl00 = *off and vdpmat = *blanks and
071300020325     c                             vdpric = 'S'
071400020325     c                   eval      errl00 = *on
071500020325     c                   eval      dsmsms = msg(12)
071600020517     c                   eval      *in82 = *on
071700020325     c                   endif
071800140508     c* area ex poc
071900140508    1c                   if        errl00=*off and *in84
072000140508     c* può contenere LOG=Logistica o un codice di filiale
072100140508    2c                   if        vdparx= 'LOG'
072200140508     c                   clear                   skpoabi
072300140508     c                   exsr      car_log
072400140508   x2c                   else
072500140508    3c                   if        vdparx>*zeros
072600140508     c                   exsr      chkcar
072700140508    4c                   if        errl00=*off
072800140508     c                   clear                   skpoabi
072900140508     c                   clear                   trul31ds
073000140508     c                   eval      i31abi = 'AR'
073100140508     c                   move      vdparx        i31car
073200140508     c                   call      'TRUL31R'
073300140508     c                   parm                    kpjba
073400140508     c                   parm                    trul31ds
073500140508     c                   movea     skpo          skpoabi
073600140508    4c                   endif
073700140508   x3c                   else
073800140508     c                   movea     sav_poabi     skpoabi
073900140508    3c                   endif
074000140508    2c                   endif
074100140508    1c                   endif
074200020517
074300020517      * Cliente
074400140508     c**                 if        vdpksc <> *zeros and vdprsc <> *blanks and
074500140508     c**                           errl00 = *off
074600140508     c**                 if        wtipri <> *blanks                            --- 2 -->
074700140508     c**                 eval      errl00 = *on
074800140508     c**                 eval      dsmsms = msg(6)
074900140508     c**                 else                                                   <-- 2 -->
075000140508     c**                 eval      errl00 = *on
075100140508     c**                 eval      dsmsms = msg(8)
075200140508     c**                 eval      *in83 = *on
075300140508     c**                 endif
075400140508     c**                 endif
075500020517      * controllo cliente
075600020517     c                   if        (vdpksc <> *zeros or vdprsc <> *blanks)
075700020517     c                             and errl00 = *off
075800020517     c                   if        wtipri <> *blanks                            --- 2 -->
075900020517     c                   eval      errl00 = *on
076000020517     c                   eval      dsmsms = msg(6)
076100020517     c                   else                                                   <-- 2 -->
076200020517     c                   exsr      chkksc
076300020517     c                   eval      wtipri = 'KSC'
076400020517     c                   eval      *in92 = *on
076500020517     c                   endif
076600020517     c                   endif
076700020326
076800020326      * SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
076900020326      * CURSORE SUL CAMPO DEL CONTROL
077000020326     C     errl00        IFEQ      '1'
077100020326     C                   MOVEL     '0'           *IN43                          *NON EMETTO SFL
077200020326     C                   ELSE
077300020326     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
077400020326     C                   END
077500020319
077600930202     C                   ENDSR
077700930707     C*================================================================
077800930707     C*  CHKTIP - CONTROLLA IL TIPO DI UNITA
077900930707     C*================================================================
078000930707     C     CHKTIP        BEGSR
078100020318
078200020318      * CONTROLLO SE E' STATO INSERITO '?'
078300020319     C     '?'           SCAN      VDPTIP                                 99
078400020318
078500020318      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
078600020319     C                   if        *in99                                        --- 1 -->
078700020318     C                   clear                   TIBS02DS
078800020318     C                   movel     'R'           t02mod
078900020318     C                   movel     knsif         t02sif
079000020321     C                   movel     'UNT'         t02cod
079100020318     C                   call      'TIBS02R'
079200020318     C                   parm                    KPJBA
079300020318     C                   parm                    TIBS02DS
079400020318     C                   movel     T02ke1        vdptip
079500020318
079600020319     C                   endif                                                  <-- 1 ---
079700020318
079800020318      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
079900020318     C                   clear                   TIBS02DS
080000020318     C                   movel     'C'           T02mod
080100020318     C                   movel     knsif         t02sif
080200020321     C                   movel     'UNT'         t02cod
080300020318     C                   movel(p)  vdptip        T02ke1
080400020318     C                   call      'TIBS02R'
080500020318     C                   parm                    KPJBA
080600020318     C                   parm                    TIBS02DS
080700020318     C                   if        t02err <> *blanks
080800020319     C                   eval      errl00 = '1'
080900020319     C                   eval      *in80 = *on
081000020318     C                   movel     msg(7)        dsmsms
081100020318     C                   endif
081200020319
081300930707     C                   ENDSR
081400930707     C*================================================================
081500020517     C*  CHKKSC - CONTROLLA IL CLIENTE
081600930707     C*================================================================
081700020517     C     CHKKSC        BEGSR
081800020517
081900020517      * se non inserito il codice ma inserita una parte della ragione sociale
082000020517      * richiamo il programma della ricerca alfabetica
082100020517     c                   if        vdpksc = *zeros and vdprsc <> *blanks
082200020517     c                   exsr      ricalf
082300020517     c                   endif
082400020517
082500020517     c                   movel     vdpksc        wmag
082600020517     c     wmag          lookup    skpoabi                                99
082700020517     c                   if        not *in99
082800020517     c                   eval      errl00 = *on
082900140508     c                   if        wabi<>'AZ'
083000020517     c                   eval      *in83 = *on
083100020517     c                   movel     msg(14)       dsmsms
083200020517     c                   endif
083300140508     c                   endif
083400020517
083500020517      * se inserito il codice controllo che esista sul piano dei conti
083600020517     c                   if        vdpksc <> *zeros and errl00 = *off
083700020517     c                   clear                   TIBS69DS
083800020517     c                   z-add     vdpksc        I69kac
083900020517     c                   call      'TIBS69R'
084000020517     c                   parm                    tibs69DS
084100020517     c                   parm                    ds_cnaco
084200020517     c                   parm                    ds_cnind
084300020517     c                   parm                    ds_cnclp
084400020517     c                   parm                    ds_fncls
084500020517
084600020517     c                   if        o69err <> '1'
084700020517     c                   movel     acorag        vdprsc
084800020517     c                   else
084900020517     c                   eval      errl00 = *on
085000020517     c                   eval      *in83 = *on
085100020517     c                   movel     msg(9)        dsmsms
085200020517     c                   endif
085300020517     c                   endif
085400020517
085500930707     C                   ENDSR
085600140502     C*================================================================
085700140502     C*  CHKcar - CONTROLLA area ex as400
085800140502     C*================================================================
085900140502     C     CHKcar        BEGSR
086000140505      * controllo che il campo contenga solo numeri
086100140505     C                   IF        %subst(vdparx:1:1)<'0' or
086200140505     c                             %subst(vdparx:2:1)<'0' or
086300140505     C                             %subst(vdparx:3:1)<'0'
086400140505     C                   eval      errl00 = '1'
086500140505     C                   eval      *in85 = *on
086600140505     c                   eval      dsmsms=msg(15)
086700140505     C                   ELSE
086800140505     C                   move      vdparx        wfil
086900140505     C     wfil          CHAIN     AZORG01l
087000140505     C                   If        %found(azorg01l) and
087100140505     C                             OrgFag='F'
087200140505     C                   else
087300140505     C                   eval      errl00 = '1'
087400140505     C                   eval      *in85 = *on
087500140505     c                   eval      dsmsms=msg(15)
087600140505     C                   endif
087700140505     C                   ENDIF
087800140502     c                   endsr
087900020517      *================================================================
088000020517      *  RICALF - RICERCA ALFABETICA
088100020517      *================================================================
088200020517     c     ricalf        begsr
088300020517
088400020517     C                   MOVEL     rsut          PAXdut                         Descrizione Azienda
088500081203     C*******            Z-ADD     9             PAXsta                         9=Escludo Annullati
088600081203     c* visulizzare anche i bloccati
088700081203     C                   Z-ADD     0             PAXsta                         9=Escludo Annullati
088800020517     C                   Z-ADD     dutkci        PAXccc                         Capoconto Clienti
088900020517     c                   movel(P)  vdprsc        PAXdmt
089000020517     C                   z-add     1             PAXnum
089100020517
089200020517     C                   CALL      'XALFA3BR'
089300020517     C                   PARM                    PAXDUT
089400020517     C                   PARM                    CODUT             1 0
089500020517     C                   PARM                    PAXDMT
089600020517     C                   PARM                    PAXCCC
089700020517     C                   PARM                    PAXSTA
089800020517     C                   PARM      *blanks       PAXFLR
089900020517     C                   PARM      *blanks       PAXDIT
090000020517     C                   PARM                    PAXNUM            2 0
090100020517     C                   PARM                    PAXKCM           80
090200020517     C                   PARM                    PAXKSM          140
090300020517     C                   PARM                    PAXKDM           60
090400020517
090500020517      * NON TROVATO O PREMUTO CMD12
090600020517    2C     paxsta        IFGT      -1
090700020517     C                   MOVEL     PAXKSM        vdpksc
090800020517    2C                   ENDIF
090900020517
091000020517     c                   endsr
091100020516      *------------------------------------------------------------------------*
091200020516      *  CTRUTE - ROUTINE CONTROLLO UTENTE
091300020516      *------------------------------------------------------------------------*
091400020516     c     ctrute        begsr
091500020516
091600020516     c                   eval      *in76 = *off
091700140502     c                   eval      *in84 = *off
091800020516
091900020516      * controllo se il profilo è abilitato
092000020516     c                   if        duterr = 'E'
092100020516     c                   eval      dsmsms = dutmsg
092200020516     c                   eval      *in76 = *on
092300020516     c                   endif
092400020516      * controllo le abilitazionei del profilo
092500020516     c                   if        *in76 = *off
092600020516     c                   movel     utefaf        dute01
092700020516     c                   if        §uteiun <> *blanks
092800020516     c                   eval      wabi = §uteiun
092900020516     c                   else
093000020516     c                   eval      wabi = uteaut
093100020516     c                   endif
093200020516     c                   clear                   tibs02ds
093300020516     c                   movel     'C'           t02mod
093400020516     c                   movel     knsif         t02sif
093500020516     c                   movel     'LAT'         t02cod
093600020516     c                   movel(p)  wabi          t02ke1
093700020516     c                   call      'TIBS02R'
093800020516     c                   parm                    kpjba
093900020516     c                   parm                    tibs02ds
094000020516     c                   movel     t02uni        dlat
094100020516     c                   if        §latabi = 'S'
094200020516     c                   eval      dsmsms= msg(13)
094300020516     c                   eval      *in76 = *on
094400020516     c                   endif
094500020516     c                   endif
094600020516
094700020516      * Utente ok richiamo pgm che ritorna i p.o. abilitati
094800020523     c                   if        *in76 = *off  and wabi <> 'AZ'
094900020516     c                   clear                   trul31ds
095000020516     c                   eval      i31abi = wabi
095100020516     c                   eval      i31cdi = dutdis
095200020516     c                   eval      i31car = dutare
095300020516     c                   eval      i31cpo = dutpou
095400020516     c                   call      'TRUL31R'
095500020516     c                   parm                    kpjba
095600020516     c                   parm                    trul31ds
095700020516     c                   endif
095800100906
095900100906      /free
096000100906         SELECT;
096100100906
096200100906           // - Reperimento filiali gestibili per utente della LOGISTICA
096300100906           WHEN  DUTntw = c_Logistica;
096400100906
096500140505             exsr   car_log;
096600100906
096700100906      /end-free
096800100906
096900020517
097000020529      * per abilitazione 'AZ' devo caricare tutti i p.o. di azorg21l
097100100906     c                   when      wabi = 'AZ'
097200140502     c* attivo campo sfl control  per parzializzazione per area ex poc
097300140502     c                   eval      *in84=*on
097400140502     c                   clear                   vdparx
097500020529     c     *loval        setll     azorg21l
097600020517     c                   do        *hival
097700020529     c                   read      azorg21l
097800020517     c                   if        %eof
097900020517     c                   leave
098000020517     c                   endif
098100020517     c                   z-add     1             ii
098200020523     c     orgfil        lookup    skpoabi(ii)                            99
098300020517     c                   if        not *in99
098400020517     c                   z-add     1             ii
098500020517     c     *zeros        lookup    skpoabi(ii)                            99
098600020517     c                   if        *in99
098700020517     c                   movel     orgfil        skpoabi(ii)
098800020517     c                   endif
098900020517     c                   endif
099000020517     c                   enddo
099100020529      * e tutti i p.o. di azorg01l
099200020529     c     *loval        setll     azorg01l
099300020529     c                   do        *hival
099400020529     c                   read      azorg01l
099500020529     c                   if        %eof
099600020529     c                   leave
099700020529     c                   endif
099800020529     c                   z-add     1             ii
099900020529     c     orgfil        lookup    skpoabi(ii)                            99
100000020529     c                   if        not *in99
100100020529     c                   z-add     1             ii
100200020529     c     *zeros        lookup    skpoabi(ii)                            99
100300020529     c                   if        *in99
100400020529     c                   movel     orgfil        skpoabi(ii)
100500020529     c                   endif
100600020529     c                   endif
100700020529     c                   enddo
100800140502     c* salvo la schiera caricata
100900140502     c                   movea     skpoabi       sav_poabi
101000020523      * se non è abilitazione 'AZ' passo la sk tornata dal TRUL31 nella sk di 500 elementi
101100100906     c                   other
101200020523     c                   movea     skpo          skpoabi
101300020523     c                   z-add     1             ii
101400020523     c     951           lookup    skpoabi(ii)                            99
101500020523     c                   if        not *in99
101600020523     c                   z-add     1             ii
101700020523     c     *zeros        lookup    skpoabi(ii)                            99
101800020523     c                   if        *in99
101900021014     c                   movel     951           skpoabi(ii)
102000020523     c                   endif
102100020523     c                   endif
102200100906     c                   endsl
102300020516
102400020516     c                   endsr
102500140505     C*================================================================
102600140505     C* Caricamento schiera po abnilitati per logistica
102700140505     C*================================================================
102800140505     C     car_log       BEGSR
102900140505     c                   z-add     0             ii
103000140505      /free
103100140505             setll  *loval  AZORG;
103200140505             read   AZORG;
103300140505
103400140505             DoW  Not  %eof(AZORG01L);
103500140505
103600140505               if  ORGfva = *blank;
103700140505
103800140505                 Og143 = ORGde3;
103900140505                 if  §OgNtw = c_Logistica;
104000140505
104100140505                   ii += 1;
104200140505                   skPOabi(ii) = ORGfil;
104300140505
104400140505                 endif;
104500140505
104600140505               endif;
104700140505
104800140505               read   AZORG;
104900140505
105000140505             EndDo;
105100140505      /end-free
105200140505     c                   endsr
105300930201     C*================================================================
105400930201     C* LANPGM - LANCIA UN PROGRAMMA
105500930201     C*================================================================
105600930201     C     LANPGM        BEGSR
105700930201     C*
105800930201     C* IMPOSTO PARAMETRI PER IL PGM
105900930707     C                   MOVEL     '0'           DSUF03                         PREMUTO F03
106000930707     C                   MOVEL     '0'           DSUERR                         '1'=ERRORE
106100930707     C                   MOVEL     *BLANKS       DSUMSG                         MESSAGGIO
106200930707     C                   MOVE      WOPZ1         DSUOP1                         *OPZIONE LIV.PGM
106300020322     C                   Z-ADD     vidtip        DSUTIP                         TIPO
106400930707     C                   Z-ADD     VIDCOD        DSUCOD                         CODICE
106500930712     C                   Z-ADD     *ZEROS        DSUMAG                         MAGAZZINO
106600930712     C                   Z-ADD     *ZEROS        DSUPRG                         PROGRESSIVO MOV.
106700930906     C                   Z-ADD     *ZEROS        DSUDTM                         DATA MOVIMENTO
106800930201     C*
106900930201     C* LANCIO PROGRAMMA
107000940105     C                   MOVEL     DSUNIT        KPJBU
107100930312     C                   CALL      WPGM
107200940105     C                   PARM                    KPJBA
107300940105     C                   MOVEL     KPJBU         DSUNIT
107400930201     C*
107500930201     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
107600930707     C     DSUF03        IFEQ      '1'
107700930201     C                   MOVEL     '1'           F03L00
107800930201     C                   MOVEL     '1'           ERRL00
107900930201     C                   ELSE
108000930201     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
108100930707     C                   MOVEL     DSUMSG        DSMSMS
108200930201     C                   END
108300930201     C*
108400930201     C                   ENDSR
108500930312     C*================================================================
108600930312     C* CHKFOP - CONTROLLA TIPO GESTIONE E IMPOSTA FUNZ/OPZ
108700930312     C*================================================================
108800930312     C     CHKFOP        BEGSR
108900930312     C*
109000930312     C* CONTROLLO CHE VI SIA UNA OPZIONE DI PROCEDURA VALIDA IN INPUT
109100930312     C* ALTRIMENTI FORZO INTERROGAZIONE
109200020517     C     DSUOP0        IFNE      ' 5'                                         *INTERROGAZIONE
109300100906     C     DSUOP0        andne     ' 2'                                         *INTERROGAZIONE
109400940105     C                   MOVE      ' 5'          DSUOP0
109500930312     C                   END
109600100906     C* SE L'UTENTE E' ENTRATO IN GESTIONE LO ABILITO A TUTTE LE FUNZIONI
109700100906     C     DSUOP0        IFEQ      ' 2'
109800100906     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
109900100906     C                   MOVEL     'S'           OPDATT
110000100906     C                   MOVEL     OPDRIG        OPD(2)
110100100906     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
110200100906     C                   MOVEL     'S'           OPDATT
110300100906     C                   MOVEL     OPDRIG        OPD(3)
110400100906     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
110500100906     C                   MOVEL     'S'           OPDATT
110600100906     C                   MOVEL     OPDRIG        OPD(4)
110700100906     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
110800100906     C                   MOVEL     'S'           OPDATT
110900100906     C                   MOVEL     OPDRIG        OPD(5)
111000100906     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
111100100906     C                   MOVEL     'S'           OPDATT
111200100906     C                   MOVEL     OPDRIG        OPD(7)
111300100906     C                   MOVEL     OPD(8)        OPDRIG                         *MOVIMENTI
111400100906     C                   MOVEL     'S'           OPDATT
111500100906     C                   MOVEL     OPDRIG        OPD(8)
111600100906     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
111700100906     C                   MOVEL     'S'           FUDATT
111800100906     C                   MOVEL     FUDRIG        FUD(6)
111900140428     c* F14=Contratti Comodati lo attivo solo se non è da li che provengo
112000140428     c                   if        %parms<=1
112100100922     C                   MOVEL     FUD(14)       FUDRIG                         *pgm scheda cli
112200100922     C                   MOVEL     'S'           FUDATT
112300100922     C                   MOVEL     FUDRIG        FUD(14)
112400140428     c                   endif
112500100906     C                   END
112600100906     C*
112700930312     C*
112800930312     C* SE IL PGM E' STATO LANCIATO IN INTERROGAZIONE ABILITO SOLO QUELLA
112900940105     C     DSUOP0        IFEQ      ' 5'
113000930312     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
113100930312     C                   MOVEL     'S'           OPDATT
113200930312     C                   MOVEL     OPDRIG        OPD(5)
113300971015     C                   MOVEL     OPD(8)        OPDRIG                         *MOVIMENTI
113400971015     C                   MOVEL     'S'           OPDATT
113500971015     C                   MOVEL     OPDRIG        OPD(8)
113600930312     C                   END
113700930312     C*
113800930312     C* IMPOSTO INDICE PER EMISSIONE OPZIONI A VIDEO
113900930312     C                   Z-ADD     0             OPZGRU            5 0
114000930312     C*
114100930312     C* GESTIONE OPZIONI A VIDEO
114200930312     C                   EXSR      OPZGES
114300930312     C*
114400930312     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
114500930312     C                   Z-ADD     0             FUNGRU            5 0
114600930312     C*
114700930312     C* GESTIONE TASTI FUNZIONALI A VIDEO
114800930312     C                   EXSR      FUNGES
114900930312     C*
115000930312     C                   ENDSR
115100000000     C*================================================================
115200000000     C*  RUTINZ - ROUTINE INIZIALE
115300000000     C*================================================================
115400000000     C     RUTINZ        BEGSR
115500000000     C*
115600020517     C* RICEVIMENTO PARAMETRI DALL'AZIONE (' 5'=INTERR.)
115700000000     C     *ENTRY        PLIST
115800930707     C                   PARM                    KPJBA
115900140328     C                   PARM                    parksc            7 0
116000940105     C                   MOVEL     KPJBU         DSUNIT
116100020516
116200020516     c     *dtaara       define    §azute        azuteds
116300020516     c     *dtaara       define    §datiute      ddatiute
116400020516
116500020516     c                   in(E)     *dtaara
116600020516     c                   if        %Error or rsut = *blanks
116700020516     c                   clear                   tibs34ds
116800020516     c                   call      'TIBS34R'
116900020516     c                   parm                    Tibs34Ds
117000020516     c                   in        *dtaara
117100020516     c                   endif
117200020516
117300930204     C*
117400930204     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
117500930204     C     XMSGPA        PLIST
117600930204     C                   PARM                    DSMSMS
117700930204     C                   PARM                    DSMSPG
117800930204     C                   PARM                    DSMSKE
117900930204     C                   PARM                    DSMSER
118000020319
118100020319      * CHIAVE LETTURA UNANA02L
118200930707     C     KUNA02        KLIST
118300020319     C                   KFLD                    UNATIP
118400020319     C                   KFLD                    UNAMAT
118500020319
118600020319      * CHIAVE LETTURA UNANA07L
118700020319     C     KUNA07        KLIST
118800020319     C                   KFLD                    UNAMAT
118900020319     C                   KFLD                    UNATIP
119000020517
119100020517      * CHIAVE LETTURA UNANA10L
119200020517     C     KUNA10        KLIST
119300020517     C                   KFLD                    UNAKSC
119400020319
119500020529     C* CHIAVE LETTURA AZORG21l/azorg01l - COMPLETA
119600930707     C     KEYORG        KLIST
119700930707     C                   KFLD                    ORGFIL
119800930707     C*
119900000000     C*
120000930129     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
120100930204     C                   CALL      'XKEY'
120200930204     C                   PARM                    DSKEY
120300000000     C*
120400000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
120500000000     C     *LIKE         DEFINE    SFLNBR        SFLPAG
120600000000     C     *LIKE         DEFINE    SFLPAG        WFLPAG
120700000000     C     *LIKE         DEFINE    SFLNBR        SFLEOF
120800000000     C     *LIKE         DEFINE    SFLNBR        SFLERR
120900000000     C     *LIKE         DEFINE    SFLNBR        SFLULT
121000000000     C     *LIKE         DEFINE    SFLNBR        WFLULT
121100000000     C     *LIKE         DEFINE    SFLNBR        MSGULT
121200000000     C     *LIKE         DEFINE    SFLNBR        SFLRIG
121300000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
121400000000     C*
121500930127     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
121600000000     C*   -NOME PGM CHIAMANTE
121700000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
121800020517     C                   MOVEL     'TNSDUIR'     DSMSPG
121900020517     C                   MOVEL     'TNSDUIR'     MSGQUE
122000000000     C                   MOVEL     *BLANKS       MSGKEY
122100930127     C                   MOVEL     *BLANKS       DSMSMS
122200930204     C                   MOVEL     *BLANKS       DSMSKE
122300930204     C                   Z-ADD     0             DSMSER
122400000000     C*
122500930129     C* INIZIALIZZO SWITCH DI LIVELLO 0 (PGM)
122600930127     C                   MOVEL     '0'           F12L00            1
122700930127     C                   MOVEL     '0'           F03L00            1
122800930127     C                   MOVEL     '0'           FINL00            1
122900930127     C                   MOVEL     '0'           ERRL00            1
123000930128     C                   MOVEL     '0'           FLFINE            1
123100930127     C*
123200930312     C* IMPOSTO DEPOSITI PER CONFRONTI
123300930707     C                   MOVEL     *BLANKS       DEPTIP
123400100922     C                   MOVEL     *BLanks       DEPMAT
123500020325     c                   clear                   depric
123600020517     c                   clear                   depksc
123700020517     c                   clear                   deprsc
123800140502     c                   clear                   deparx
123900930202     C*
124000941128     C* IMPOSTO TIPO DI RICERCA DI DEFAULT --> MAT
124100020319     C                   MOVEL     'TIP'         WTIPRI
124200020319     c                   eval      *in90 = *on
124300140328     c                   if        %parms>1
124400140328     C                   MOVEL     'KSC'         WTIPRI
124500140328     c                   z-add     parksc        vdpksc
124600140331     c                   z-add     parksc        depksc
124700140328     c                   endif
124800000000     C*
124900000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
125000000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
125100020517     C                   Z-ADD     12            SFLPAG
125200930312     C*
125300930312     C* IMPOSTO LE OPZIONI E LE FUNZIONI A VIDEO
125400930312     C                   EXSR      CHKFOP
125500930225     C*
125600930225     C* IMPOSTO INTERROGAZIONE/GESTIONE NELLA TESTATA DELLA VIDEATA
125700100906     c                   select
125800100906     C     DSUOP0        wheneq    ' 5'
125900020517     C                   eval      viding = ' INTERROGAZIONE COMODATI '
126000100906     C     DSUOP0        wheneq    ' 2'
126100100906     C                   eval      viding = 'G E S T I O N E  COMODATI'
126200100906     C                   ENDsl
126300020325
126400000000     C                   ENDSR
126500020326
126600930312     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
126700930312     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
126800930312     C* LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
126900930312     C* STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
127000930312     C* NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
127100930312     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
127200930312     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
127300930312     C*
127400930312     C*================================================================
127500930312     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA
127600930312     C*          CARICARE
127700930312     C*================================================================
127800930312     C     POSSFL        BEGSR
127900930312     C*
128000930312     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
128100930312     C                   EXSR      CLRSFL
128200930312     C*
128300930312     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
128400930312     C                   EXSR      SETFIL
128500930312     C*
128600930312     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
128700930312     C     SFLEOF        IFEQ      0
128800020326     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
128900930312     C                   EXSR      CARSFL
129000930312     C                   END
129100930312     C*
129200930312     C                   ENDSR
129300000000     C*================================================================
129400000000     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
129500000000     C*================================================================
129600000000     C     ENTATT        BEGSR
129700000000     C*
129800000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
129900000000     C                   Z-ADD     0             OPZOK             1 0
130000000000     C*
130100000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
130200000000     C                   Z-ADD     0             SFLVAR            5 0
130300000000     C*
130400000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
130500000000     C                   Z-ADD     0             SFLERR
130600000000     C*
130700000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
130800000000     C                   Z-ADD     SFLULT        WFLULT
130900000000     C*
131000000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
131100020517     C                   READC     SDUID05                                98
131200930202     C     *IN98         DOWEQ     '0'
131300000000     C*
131400000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
131500000000     C                   ADD       1             SFLVAR
131600000000     C*
131700000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
131800000000     C     OPZOK         IFEQ      0
131900000000     C*
132000000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
132100000000     C     OPZKEY        IFNE      *BLANKS
132200000000     C                   EXSR      OPZCON
132300000000     C                   END
132400000000     C*
132500000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
132600000000     C*            2. PULIZIA SCELTA IN SFL
132700000000     C     OPZOK         IFEQ      0
132800000000     C                   MOVEL     '0'           *IN41
132900000000     C                   MOVEL     *BLANKS       OPZKEY
133000000000     C                   ELSE
133100000000     C*
133200000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
133300000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
133400000000     C                   MOVEL     '1'           *IN41
133500000000     C                   Z-ADD     SFLULT        SFLERR
133600000000     C                   END
133700000000     C                   ELSE
133800000000     C*
133900000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
134000000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
134100000000     C                   MOVEL     '1'           *IN41
134200000000     C                   END                                                    OPZOK=0
134300000000     C*
134400000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
134500000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
134600000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
134700000000     C     OPZOK         IFNE      0
134800000000     C                   Z-ADD     SFLERR        SFLNBR
134900000000     C                   ELSE
135000000000     C                   Z-ADD     SFLULT        SFLNBR
135100000000     C                   END
135200000000     C*
135300000000     C* AGGIORNO RECORD SFL
135400020517     C                   UPDATE    SDUID05
135500000000     C*
135600020517     C                   READC     SDUID05                                98
135700000000     C                   END                                                    DO WHILE
135800000000     C*
135900000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
136000000000     C                   Z-ADD     WFLULT        SFLULT
136100000000     C*
136200000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
136300000000     C     SFLVAR        IFEQ      0
136400000000     C                   MOVEL     '1'           FINL00
136500000000     C                   END
136600000000     C*
136700000000     C                   ENDSR
136800000000     C*================================================================
136900000000     C* ELIMINAZIONE CONTENUTO SFL
137000000000     C*================================================================
137100000000     C     CLRSFL        BEGSR
137200000000     C*
137300000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
137400000000     C* (0=NON FINE FILE  1=FINE FILE)
137500000000     C                   Z-ADD     0             SFLEOF
137600000000     C*
137700000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
137800000000     C                   Z-ADD     0             SFLULT
137900000000     C*
138000000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
138100000000     C                   MOVEL     '1'           *IN40
138200020517     C                   WRITE     SDUID06
138300000000     C                   MOVEL     '0'           *IN40
138400000000     C*
138500000000     C                   ENDSR
138600000000     C*================================================================
138700000000     C* CARSFL - CARICAMENTO SFL
138800000000     C*================================================================
138900000000     C     CARSFL        BEGSR
139000000000     C*
139100000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
139200000000     C     SFLULT        IFEQ      0
139300000000     C                   EXSR      SETFIL
139400000000     C                   END
139500000000     C*
139600000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
139700000000     C                   Z-ADD     SFLULT        WFLULT
139800000000     C*
139900000000     C* CICLO:
140000000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
140100000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
140200000000     C                   Z-ADD     1             SFLRIG
140300000000     C     SFLRIG        DOWLE     SFLPAG
140400000000     C     SFLEOF        ANDEQ     0
140500020319
140600020319     c                   eval      wnosfl = *off
140700020319
140800000000     C*
140900000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
141000000000     C                   MOVEL     *BLANKS       OPZKEY
141100020319
141200020326      * controllo se devo scrivere il record nel subfile
141300020319     c                   exsr      ctrsfl
141400020319
141500020319     c                   if        wnosfl = *off
141600000000     C*
141700000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
141800000000     C                   EXSR      IRESFL
141900000000     C*
142000000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
142100000000     C                   ADD       1             SFLULT
142200000000     C*
142300000000     C* SCRITTURA RECORD SFL
142400020517     C                   WRITE     SDUID05
142500000000     C                   ADD       1             SFLRIG
142600020319
142700020319     c                   endif
142800000000     C*
142900000000     C* LETTURA FILE DA CARICARE
143000000000     C                   EXSR      LETFIL
143100000000     C                   END                                                    DO WHILE
143200000000     C*
143300000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
143400000000     C     WFLULT        IFNE      SFLULT
143500000000     C                   Z-ADD     WFLULT        SFLNBR
143600000000     C                   ADD       1             SFLNBR
143700000000     C                   END
143800000000     C*
143900000000     C                   ENDSR
144000000000     C*================================================================
144100000000     C* EMISSIONE SUBFILE A VIDEO
144200000000     C*================================================================
144300000000     C     OUTSFL        BEGSR
144400000000     C*
144500000000     C* WRITE RECORD TESTATA VIDEO
144600020517     C                   WRITE     SDUID01
144700930205     C*
144800930205     C* VISUALIZZAZIONE MESSAGGIO
144900930205     C                   EXSR      SNDMSG
145000000000     C*
145100000000     C* WRITE RECORD OPZIONI VIDEO
145200020517     C                   WRITE     SDUID02
145300000000     C*
145400000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
145500000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
145600000000     C     SFLEOF        IFEQ      0
145700000000     C                   MOVEL     '0'           *IN42
145800000000     C                   ELSE
145900000000     C*
146000000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
146100000000     C                   MOVEL     '1'           *IN42
146200000000     C                   END
146300000000     C*
146400000000     C* WRITE RECORD TASTI FUNZIONALI
146500020517     C                   WRITE     SDUID09
146600000000     C*
146700000000     C* CASO DI SFL VUOTO
146800000000     C     SFLULT        IFLE      0
146900000000     C*
147000000000     C*    - WRITE RECORD VUOTO
147100020326     C                   MOVEL     '0'           *IN43                          *NON VISUALIZZA SFL
147200020517     C                   WRITE     SDUID0A
147300020517     C                   WRITE     SDUID06                                      *SFL CONTROL
147400020517     C                   READ      SDUID06                                99
147500000000     C*
147600000000     C                   ELSE
147700000000     C*
147800000000     C* CASO DI SFL CON RECORDS
147900000000     C*
148000000000     C*    - WRITE RECORD CONTROLLO SFL
148100020517     C                   WRITE     SDUID06
148200020517     C                   READ      SDUID06                                99
148300000000     C                   END
148400000000     C*
148500000000     C                   ENDSR
148600000000     C*================================================================
148700000000     C* SNDMSG - MANDA MESSAGGIO
148800000000     C*================================================================
148900000000     C     SNDMSG        BEGSR
149000000000     C*
149100000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
149200000000     C                   MOVEL     '1'           *IN40
149300020517     C                   WRITE     SDUIDMC
149400000000     C                   MOVEL     '0'           *IN40
149500000000     C*
149600000000     C* VISUALIZZAZIONE MESSAGGIO
149700000000     C                   Z-ADD     0             MSGULT
149800930204     C                   CALL      'XMSG'        XMSGPA
149900930127     C     DSMSER        IFEQ      0
150000930127     C                   MOVEL     DSMSKE        MSGKEY
150100000000     C                   END
150200000000     C                   Z-ADD     1             MSGULT
150300020517     C                   WRITE     SDUIDMS                                      SFL MESS
150400020517     C                   WRITE     SDUIDMC                                      CONTR MESS
150500000000     C*
150600000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
150700930127     C                   MOVEL     *BLANKS       DSMSMS
150800000000     C*
150900000000     C                   ENDSR
151000000000     C*================================================================
151100000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
151200000000     C*================================================================
151300000000     C     FUNCON        BEGSR
151400000000     C*
151500000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
151600000000     C                   EXSR      FUNVAR
151700000000     C*
151800000000     C* PULISCO CAMPI DI LAVORO
151900000000     C                   Z-ADD     0             I                 5 0
152000000000     C                   Z-ADD     0             W                 5 0
152100000000     C*
152200000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
152300100922     c* ROLL in 25° posizione
152400100922    1C     FUNKEY        IFNE      *BLANKS
152500100922    2C     1             DO        25            I
152600000000     C                   MOVEL     FUD(I)        FUDRIG
152700000000     C*
152800000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
152900100922    3C     FUNKEY        IFEQ      FUDFUN
153000000000     C     FUDATT        ANDEQ     'S'
153100000000     C                   Z-ADD     I             W
153200100922    3C                   END
153300100922    2C                   END                                                    DO WHILE
153400000000     C*
153500000000     C* FUNZIONE NON ATTIVA (ERRORE)
153600100922    2C     W             IFEQ      0
153700000000     C*
153800000000     C* EMISSIONE MESSAGGIO DI ERRORE
153900930127     C                   MOVEL     MSG(2)        DSMSMS
154000100922    2C                   END                                                    W=0
154100100922    1C                   END                                                    FUNKEY NE *BLANKS
154200000000     C*
154300000000     C* FUNZIONE ATTIVA (PROCEDI ...)
154400000000     C     W             IFNE      0
154500000000     C     FUNKEY        OREQ      *BLANKS
154600000000     C*
154700000000     C     FUNKEY        IFEQ      *BLANKS
154800000000     C                   EXSR      ENTGES
154900000000     C                   END
155000000000     C     FUNKEY        IFEQ      '03'
155100000000     C                   EXSR      F03GES
155200000000     C                   END
155300000000     C     FUNKEY        IFEQ      '05'
155400000000     C                   EXSR      F05GES
155500000000     C                   END
155600930126     C     FUNKEY        IFEQ      '06'
155700930126     C                   EXSR      F06GES
155800000000     C                   END
155900000000     C     FUNKEY        IFEQ      '12'
156000000000     C                   EXSR      F12GES
156100000000     C                   END
156200100922     C     FUNKEY        IFEQ      '14'
156300100922     C                   EXSR      F14GES
156400100922     C                   END
156500000000     C     FUNKEY        IFEQ      '17'
156600000000     C                   EXSR      F17GES
156700000000     C                   END
156800000000     C     FUNKEY        IFEQ      '18'
156900000000     C                   EXSR      F18GES
157000000000     C                   END
157100930129     C     FUNKEY        IFEQ      '21'
157200930129     C                   EXSR      F21GES
157300930129     C                   END
157400000000     C     FUNKEY        IFEQ      '22'
157500000000     C                   EXSR      F22GES
157600000000     C                   END
157700000000     C     FUNKEY        IFEQ      '23'
157800000000     C                   EXSR      F23GES
157900000000     C                   END
158000000000     C     FUNKEY        IFEQ      '24'
158100000000     C                   EXSR      F24GES
158200000000     C                   END
158300000000     C     FUNKEY        IFEQ      'RU'                                         ROLLUP
158400000000     C                   EXSR      RUPGES
158500000000     C                   END
158600000000     C     FUNKEY        IFEQ      'HP'                                         HELP
158700000000     C                   EXSR      HELGES
158800000000     C                   END
158900000000     C*
159000000000     C                   END
159100000000     C*
159200000000     C                   ENDSR
159300000000     C*================================================================
159400000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
159500000000     C*================================================================
159600000000     C     FUNVAR        BEGSR
159700000000     C*
159800000000     C* INIZIALIZZA VARIABILE -FUNKEY-
159900000000     C*
160000000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
160100000000     C     KEY           IFEQ      ENTER
160200000000     C                   MOVEL     *BLANKS       FUNKEY
160300000000     C                   END
160400000000     C     KEY           IFEQ      F1
160500000000     C                   MOVEL     '01'          FUNKEY
160600000000     C                   END
160700000000     C     KEY           IFEQ      F2
160800000000     C                   MOVEL     '02'          FUNKEY
160900000000     C                   END
161000000000     C     KEY           IFEQ      F3
161100000000     C                   MOVEL     '03'          FUNKEY
161200000000     C                   END
161300000000     C     KEY           IFEQ      F4
161400000000     C                   MOVEL     '04'          FUNKEY
161500000000     C                   END
161600000000     C     KEY           IFEQ      F5
161700000000     C                   MOVEL     '05'          FUNKEY
161800000000     C                   END
161900000000     C     KEY           IFEQ      F6
162000000000     C                   MOVEL     '06'          FUNKEY
162100000000     C                   END
162200000000     C     KEY           IFEQ      F7
162300000000     C                   MOVEL     '07'          FUNKEY
162400000000     C                   END
162500000000     C     KEY           IFEQ      F8
162600000000     C                   MOVEL     '08'          FUNKEY
162700000000     C                   END
162800000000     C     KEY           IFEQ      F9
162900000000     C                   MOVEL     '09'          FUNKEY
163000000000     C                   END
163100000000     C     KEY           IFEQ      F10
163200000000     C                   MOVEL     '10'          FUNKEY
163300000000     C                   END
163400000000     C     KEY           IFEQ      F11
163500000000     C                   MOVEL     '11'          FUNKEY
163600000000     C                   END
163700000000     C     KEY           IFEQ      F12
163800000000     C                   MOVEL     '12'          FUNKEY
163900000000     C                   END
164000000000     C     KEY           IFEQ      F13
164100000000     C                   MOVEL     '13'          FUNKEY
164200000000     C                   END
164300000000     C     KEY           IFEQ      F14
164400000000     C                   MOVEL     '14'          FUNKEY
164500000000     C                   END
164600000000     C     KEY           IFEQ      F15
164700000000     C                   MOVEL     '15'          FUNKEY
164800000000     C                   END
164900000000     C     KEY           IFEQ      F16
165000000000     C                   MOVEL     '16'          FUNKEY
165100000000     C                   END
165200000000     C     KEY           IFEQ      F17
165300000000     C                   MOVEL     '17'          FUNKEY
165400000000     C                   END
165500000000     C     KEY           IFEQ      F18
165600000000     C                   MOVEL     '18'          FUNKEY
165700000000     C                   END
165800000000     C     KEY           IFEQ      F19
165900000000     C                   MOVEL     '19'          FUNKEY
166000000000     C                   END
166100000000     C     KEY           IFEQ      F20
166200000000     C                   MOVEL     '20'          FUNKEY
166300000000     C                   END
166400000000     C     KEY           IFEQ      F21
166500000000     C                   MOVEL     '21'          FUNKEY
166600000000     C                   END
166700000000     C     KEY           IFEQ      F22
166800000000     C                   MOVEL     '22'          FUNKEY
166900000000     C                   END
167000000000     C     KEY           IFEQ      F23
167100000000     C                   MOVEL     '23'          FUNKEY
167200000000     C                   END
167300000000     C     KEY           IFEQ      F24
167400000000     C                   MOVEL     '24'          FUNKEY
167500000000     C                   END
167600000000     C     KEY           IFEQ      ROLLUP
167700000000     C                   MOVEL     'RU'          FUNKEY
167800000000     C                   END
167900000000     C     KEY           IFEQ      HELP
168000000000     C                   MOVEL     'HP'          FUNKEY
168100000000     C                   END
168200000000     C*
168300000000     C                   ENDSR
168400000000     C*================================================================
168500000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
168600000000     C*================================================================
168700000000     C     FUNGES        BEGSR
168800000000     C*
168900000000     C* PULISCO CAMPI DI LAVORO
169000000000     C                   Z-ADD     0             W                 5 0
169100000000     C*
169200000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
169300000000     C* E CARICO TABELLA FUNZIONI ATTIVE
169400000000     C     1             DO        24            I
169500000000     C                   MOVEL     FUD(I)        FUDRIG
169600000000     C*
169700000000     C* PULIZIA FUNZIONI PRECEDENTI
169800000000     C                   MOVEL     *BLANKS       FUV(I)
169900000000     C*
170000000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
170100000000     C* (SALTA F23 E F24)
170200000000     C     FUDATT        IFEQ      'S'
170300000000     C     FUDFUN        ANDNE     '23'
170400000000     C     FUDFUN        ANDNE     '24'
170500000000     C                   ADD       1             W
170600000000     C                   MOVEL     FUDDES        FUV(W)
170700000000     C                   END
170800000000     C                   END                                                    DO WHILE
170900000000     C*
171000000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
171100000000     C* ALTRIMENTI DISATTIVO F24
171200000000     C     1             DO        24            I
171300000000     C                   MOVEL     FUD(I)        FUDRIG
171400000000     C*
171500000000     C     FUDFUN        IFEQ      '24'
171600000000     C     W             IFGT      6
171700000000     C                   MOVEL     'S'           FUDATT
171800000000     C                   ELSE
171900000000     C                   MOVEL     *BLANKS       FUDATT
172000000000     C                   END                                                    W GT 8
172100000000     C                   MOVEL     FUDRIG        FUD(I)
172200000000     C                   END
172300000000     C                   END                                                    DO WHILE
172400000000     C*
172500000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
172600000000     C     W             DIV       6             I
172700000000     C                   MVR                     Z
172800000000     C*
172900000000     C     Z             IFNE      0
173000000000     C     I             ADD       1             Z
173100000000     C                   ELSE
173200000000     C                   Z-ADD     I             Z
173300000000     C                   END
173400000000     C*
173500000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
173600000000     C                   ADD       1             FUNGRU
173700000000     C     FUNGRU        IFGT      Z
173800000000     C     FUNGRU        ORLT      1
173900000000     C                   Z-ADD     1             FUNGRU
174000000000     C                   END
174100000000     C*
174200000000     C* PRIMO GRUPPO DI 6 FUNZIONI
174300000000     C     FUNGRU        IFEQ      1
174400000000     C                   MOVEL     FUV(1)        FUN001
174500000000     C                   MOVEL     FUV(2)        FUN002
174600000000     C                   MOVEL     FUV(3)        FUN003
174700000000     C                   MOVEL     FUV(4)        FUN004
174800000000     C                   MOVEL     FUV(5)        FUN005
174900000000     C                   MOVEL     FUV(6)        FUN006
175000000000     C                   END
175100000000     C*
175200000000     C* SECONDO GRUPPO DI 6 FUNZIONI
175300000000     C     FUNGRU        IFEQ      2
175400000000     C                   MOVEL     FUV(7)        FUN001
175500000000     C                   MOVEL     FUV(8)        FUN002
175600000000     C                   MOVEL     FUV(9)        FUN003
175700000000     C                   MOVEL     FUV(10)       FUN004
175800000000     C                   MOVEL     FUV(11)       FUN005
175900000000     C                   MOVEL     FUV(12)       FUN006
176000000000     C                   END
176100000000     C*
176200000000     C* TERZO GRUPPO DI 6 FUNZIONI
176300000000     C     FUNGRU        IFEQ      3
176400000000     C                   MOVEL     FUV(13)       FUN001
176500000000     C                   MOVEL     FUV(14)       FUN002
176600000000     C                   MOVEL     FUV(15)       FUN003
176700000000     C                   MOVEL     FUV(16)       FUN004
176800000000     C                   MOVEL     FUV(17)       FUN005
176900000000     C                   MOVEL     FUV(18)       FUN006
177000000000     C                   END
177100000000     C*
177200000000     C* QUARTO GRUPPO DI 6 FUNZIONI
177300000000     C     FUNGRU        IFEQ      4
177400000000     C                   MOVEL     FUV(19)       FUN001
177500000000     C                   MOVEL     FUV(20)       FUN002
177600000000     C                   MOVEL     FUV(21)       FUN003
177700000000     C                   MOVEL     FUV(22)       FUN004
177800000000     C                   MOVEL     *BLANKS       FUN005
177900000000     C                   MOVEL     *BLANKS       FUN006
178000000000     C                   END
178100000000     C*
178200000000     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
178300000000     C                   MOVEL     *BLANKS       FUN007
178400000000     C                   MOVEL     *BLANKS       FUN008
178500000000     C*
178600000000     C     1             DO        24            I
178700000000     C                   MOVEL     FUD(I)        FUDRIG
178800000000     C     FUDFUN        IFEQ      '23'
178900000000     C     FUDATT        ANDEQ     'S'
179000000000     C                   MOVEL     FUDDES        FUN007
179100000000     C                   END
179200000000     C     FUDFUN        IFEQ      '24'
179300000000     C     FUDATT        ANDEQ     'S'
179400000000     C                   MOVEL     FUDDES        FUN008
179500000000     C                   END
179600000000     C                   END                                                    DO
179700000000     C*
179800000000     C                   ENDSR
179900000000     C*================================================================
180000000000     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
180100000000     C*================================================================
180200000000     C     OPZGES        BEGSR
180300000000     C*
180400000000     C* PULISCO CAMPI DI LAVORO
180500000000     C                   Z-ADD     0             W                 5 0
180600000000     C*
180700000000     C* RICERCO QUANTE OPZIONI SONO ATTIVE
180800000000     C* E CARICO TABELLA OPZIONI ATTIVE
180900000000     C     1             DO        24            I
181000000000     C                   MOVEL     OPD(I)        OPDRIG
181100000000     C*
181200000000     C* PULIZIA OPZIONI PRECEDENTI
181300000000     C                   MOVEL     *BLANKS       OPV(I)
181400000000     C*
181500000000     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
181600000000     C     OPDATT        IFEQ      'S'
181700000000     C                   ADD       1             W
181800000000     C                   MOVEL     OPDDES        OPV(W)
181900000000     C                   END
182000000000     C                   END                                                    DO WHILE
182100000000     C*
182200000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
182300000000     C* ALTRIMENTI DISATTIVO F23
182400000000     C     1             DO        24            I
182500000000     C                   MOVEL     FUD(I)        FUDRIG
182600000000     C*
182700000000     C     FUDFUN        IFEQ      '23'
182800000000     C     W             IFGT      8
182900000000     C                   MOVEL     'S'           FUDATT
183000000000     C                   ELSE
183100000000     C                   MOVEL     *BLANKS       FUDATT
183200000000     C                   END                                                    W GT 8
183300000000     C                   MOVEL     FUDRIG        FUD(I)
183400000000     C                   END
183500000000     C                   END                                                    DO WHILE
183600000000     C*
183700000000     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
183800000000     C     W             DIV       8             I
183900000000     C                   MVR                     Z                 5 0
184000000000     C*
184100000000     C     Z             IFNE      0
184200000000     C     I             ADD       1             Z
184300000000     C                   ELSE
184400000000     C                   Z-ADD     I             Z
184500000000     C                   END
184600000000     C*
184700000000     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
184800000000     C                   ADD       1             OPZGRU
184900000000     C     OPZGRU        IFGT      Z
185000000000     C     OPZGRU        ORLT      1
185100000000     C                   Z-ADD     1             OPZGRU
185200000000     C                   END
185300000000     C*
185400000000     C* PRIMO GRUPPO DI 8 OPZIONI
185500000000     C     OPZGRU        IFEQ      1
185600000000     C                   MOVEL     OPV(1)        OPZ001
185700000000     C                   MOVEL     OPV(2)        OPZ002
185800000000     C                   MOVEL     OPV(3)        OPZ003
185900000000     C                   MOVEL     OPV(4)        OPZ004
186000000000     C                   MOVEL     OPV(5)        OPZ005
186100000000     C                   MOVEL     OPV(6)        OPZ006
186200000000     C                   MOVEL     OPV(7)        OPZ007
186300000000     C                   MOVEL     OPV(8)        OPZ008
186400000000     C                   END
186500000000     C*
186600000000     C* SECONDO GRUPPO DI 8 OPZIONI
186700000000     C     OPZGRU        IFEQ      2
186800000000     C                   MOVEL     OPV(9)        OPZ001
186900000000     C                   MOVEL     OPV(10)       OPZ002
187000000000     C                   MOVEL     OPV(11)       OPZ003
187100000000     C                   MOVEL     OPV(12)       OPZ004
187200000000     C                   MOVEL     OPV(13)       OPZ005
187300000000     C                   MOVEL     OPV(14)       OPZ006
187400000000     C                   MOVEL     OPV(15)       OPZ007
187500000000     C                   MOVEL     OPV(16)       OPZ008
187600000000     C                   END
187700000000     C*
187800000000     C* TERZO GRUPPO DI 8 OPZIONI
187900000000     C     OPZGRU        IFEQ      3
188000000000     C                   MOVEL     OPV(17)       OPZ001
188100000000     C                   MOVEL     OPV(18)       OPZ002
188200000000     C                   MOVEL     OPV(19)       OPZ003
188300000000     C                   MOVEL     OPV(20)       OPZ004
188400000000     C                   MOVEL     OPV(21)       OPZ005
188500000000     C                   MOVEL     OPV(22)       OPZ006
188600000000     C                   MOVEL     OPV(23)       OPZ007
188700000000     C                   MOVEL     OPV(24)       OPZ008
188800000000     C                   END
188900000000     C                   ENDSR
189000000000     C*================================================================
189100000000     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
189200000000     C*================================================================
189300000000     C     OPZCON        BEGSR
189400000000     C*
189500000000     C* PULISCO CAMPI DI LAVORO
189600000000     C                   Z-ADD     0             I                 5 0
189700000000     C                   Z-ADD     0             W                 5 0
189800000000     C*
189900000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
190000000000     C     1             DO        24            I
190100000000     C                   MOVEL     OPD(I)        OPDRIG
190200000000     C*
190300000000     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
190400000000     C     OPZKEY        IFEQ      OPDOPZ
190500000000     C     OPZKEY        OREQ      OPDOP1
190600000000     C*
190700000000     C     OPDATT        IFEQ      'S'
190800000000     C                   MOVEL     OPDOPZ        OPZKEY
190900000000     C                   Z-ADD     I             W
191000000000     C                   END
191100000000     C                   END
191200000000     C*
191300000000     C                   END                                                    DO WHILE
191400000000     C*
191500000000     C* OPZIONE NON ATTIVA (ERRORE)
191600000000     C     W             IFEQ      0
191700000000     C*
191800000000     C* OPZIONE NON OK
191900000000     C                   Z-ADD     1             OPZOK
192000000000     C*
192100000000     C* EMISSIONE MESSAGGIO DI ERRORE
192200930127     C                   MOVEL     MSG(1)        DSMSMS
192300000000     C                   ELSE
192400000000     C*
192500000000     C* OPZIONE ATTIVA (PROCEDI ...)
192600000000     C*
192700930129     C     OPZKEY        IFEQ      ' 1'
192800000000     C                   EXSR      O01GES
192900000000     C                   END
193000000000     C     OPZKEY        IFEQ      ' 2'
193100000000     C                   EXSR      O02GES
193200000000     C                   END
193300000000     C     OPZKEY        IFEQ      ' 3'
193400000000     C                   EXSR      O03GES
193500000000     C                   END
193600000000     C     OPZKEY        IFEQ      ' 4'
193700020709     c                   If        §UteCun = *Blanks
193800020320     c                   eval      dsmsms = msg(11)
193900020320     c                   z-add     1             opzok
194000020320     c                   else
194100000000     C                   EXSR      O04GES
194200020320     c                   endif
194300000000     C                   END
194400000000     C     OPZKEY        IFEQ      ' 5'
194500000000     C                   EXSR      O05GES
194600000000     C                   END
194700000000     C     OPZKEY        IFEQ      ' 6'
194800000000     C                   EXSR      O06GES
194900000000     C                   END
195000930128     C     OPZKEY        IFEQ      ' 7'
195100930128     C                   EXSR      O07GES
195200930128     C                   END
195300930201     C     OPZKEY        IFEQ      ' 8'
195400930201     C                   EXSR      O08GES
195500930201     C                   END
195600000000     C*
195700000000     C                   END                                                    OPZ.NON ATTIVA
195800000000     C*
195900000000     C                   ENDSR
196000000000     C*================================================================
196100000000     C* NOTE PER COMPILAZIONE SCHIERE
196200000000     C*================================================================
196300000000     C* -MSG-
196400000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
196500000000     C* (NON TOCCARE MSG,1 E MSG,2
196600000000     C*
196700000000     C* -CMD-
196800000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
196900000000     C*
197000000000     C* -OPD-
197100000000     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
197200000000     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
197300000000     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
197400000000     C*    ESEMPIO: OPZIONI -1 -
197500000000     C*                     - 1-  SONO LA STESSA COSA
197600000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
197700000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
197800000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
197900000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
198000000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
198100000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
198200000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
198300000000     C*
198400000000     C* -FUD-
198500000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
198600000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
198700000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
198800000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
198900000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
199000000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
199100000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
199200000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
199300000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
199400000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
199500000000     C*
199600000000     C*================================================================
199700000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
199800020517TNSDUIR- Opzione non gestita.                                                  1
199900020517TNSDUIR- Tasto funzionale non gestito.                                         2
200000020517TNSDUIR- Non abilitato alla gestione di questa Unità.                          3
200100100906TNSDU1R- Unità annullata, non gestibile.    Consentito solo Ripristino.        4
200200100906TNSDU1R- Unità non annullata, impossibile ripristinare.                        5
200300020517TNSDUIR- Accettato solo un parametro di inizio lista.                          6
200400020517TNSDUIR- Tipo unità non accettabile.                                           7
200500020517TNSDUIR- Immettere o il codice cliente o la ragione sociale                    8
200600020517TNSDUIR- Cliente non accettabile                                               9
200700020517TNSDUIR- Matricola non accettabile.                                           10
200800020517TNSDUIR-                                                                      11
200900020517TNSDUIR- Matricola obbligatoria se richiesta ricerca per contenuto            12
201000020517TNSDUIR- Profilo non abilitato alla funzione                                  13
201100020517TNSDUIR- Profilo non abilitato al cliente richiesto                           14
201200140505TNSDUIR- Codice non valido                                                    15
201300020517TNSDUIR- 16                                                                   16
201400020517TNSDUIR- 17                                                                   17
201500020517TNSDUIR- 18                                                                   18
201600020517TNSDUIR- 19                                                                   19
201700020517TNSDUIR- 20                                                                   20
201800000000** ======== SCHIERA: CMD  (COMANDI)                        ================
201900000000WRKSPLF
202000000000
202100000000
202200000000
202300000000
202400000000** ======== SCHIERA: OPD  OPZIONI GESTITE NEL PGM (ATTIVA=S) ==========
202500930312 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
202600930312 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
202700971015 3=Duplica         -3 -     (S/N)=N                        opz. 03¬
202800930312 4=Cancellazione   -4 -     (S/N)=N                        opz. 04¬
202900930312 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
203000000000                   -6 -     (S/N)=N                        opz. 06¬
203100930312 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
203200971013 8=Movimenti       -8 -     (S/N)=N                        opz. 08¬
203300930129                   -9 -     (S/N)=N                        opz. 09¬
203400000000                   -  -     (S/N)=N                        opz. 10¬
203500000000                   -  -     (S/N)=N                        opz. 11¬
203600000000                   -  -     (S/N)=N                        opz. 12¬
203700000000                   -  -     (S/N)=N                        opz. 13¬
203800000000                   -  -     (S/N)=N                        opz. 14¬
203900000000                   -  -     (S/N)=N                        opz. 15¬
204000000000                   -  -     (S/N)=N                        opz. 16¬
204100000000                   -  -     (S/N)=N                        opz. 17¬
204200000000                   -  -     (S/N)=N                        opz. 18¬
204300000000                   -  -     (S/N)=N                        opz. 19¬
204400000000                   -  -     (S/N)=N                        opz. 20¬
204500000000                   -  -     (S/N)=N                        opz. 21¬
204600000000                   -  -     (S/N)=N                        opz. 22¬
204700000000                   -  -     (S/N)=N                        opz. 23¬
204800000000                   -  -     (S/N)=N                        opz. 24¬
204900000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
205000930129                   -        (S/N)=N                        funz.01¬
205100930129                   -        (S/N)=N                        funz.02¬
205200930129F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
205300930129                   -        (S/N)=N                        funz.04¬
205400971013F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
205500930312F06=Inserimento    -        (S/N)=N                        funz.06¬
205600000000                   -        (S/N)=N                        funz.07¬
205700000000                   -        (S/N)=N                        funz.08¬
205800000000                   -        (S/N)=N                        funz.09¬
205900100922                   -        (S/N)=N                        funz.10¬
206000100922                   -        (S/N)=N                        funz.11¬
206100930129F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
206200000000                   -        (S/N)=N                        funz.13¬
206300140424F14=ContrattiComod.-        (S/N)=N                        funz.14¬
206400930129                   -        (S/N)=N                        funz.15¬
206500930126                   -        (S/N)=N                        funz.16¬
206600000000F17=Inizio dati    -        (S/N)=S  <== S-FISSO           funz.17¬
206700000000F18=Fine dati      -        (S/N)=S  <== S-FISSO           funz.18¬
206800100922                   -        (S/N)=N                        funz.19¬
206900100922                   -        (S/N)=N                        funz.20¬
207000020517F21=Stampa         -        (S/N)=N                        funz.21¬
207100940105F22=Gestione stampe-        (S/N)=N                        funz.22¬
207200000000F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
207300000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
207400100922 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.RU¬
