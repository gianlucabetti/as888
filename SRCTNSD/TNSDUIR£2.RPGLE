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
011900100906      *
012000100906     d Og143         e ds                  inz
012100100906
012200100906     d c_Logistica     c                   const('LOG')
012300020325
012400971010     C*------------------------------------------------------------------------*
012500971010     C* MAIN LINE
012600971010     C*------------------------------------------------------------------------*
012700000000     C* MAIN DEL PGM
012800000000     C                   EXSR      RUTINZ
012900000000     C*
013000930126     C* SE CONTROLLI PRELIMINARI NON OK VADO A FINE PGM
013100930126     C     ERRL00        IFNE      '1'                                          --- 1 -->
013200020516
013300020516      * controllo il profilo utente
013400020516     c                   exsr      ctrute
013500020516     c                   if        *in76
013600020516     c                   eval      sflult = 0
013700020516     c                   endif
013800000000     C*
013900000000     C* CARICA IL SFL DAI PARAMETRI IN INPUT AL PROGRAMMA
014000000000     C                   EXSR      POSSFL
014100000000     C*
014200000000     C* CICLO FINO A CHE:
014300000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
014400000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
014500930707     C*  -FINE PGM PER ENTER SENZA SCELTA
014600000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
014700000000     C     F12L00        OREQ      '1'
014800000000     C*
014900000000     C* AZZERA FLAG DI ERROE
015000000000     C                   MOVEL     '0'           ERRL00
015100020410
015200020410      * posiziono il cursore sulla matricola se prima volta
015300020410     c                   if        wprimo = *off
015400020410     c                   eval      *in88 = *on
015500020410     c                   eval      wprimo = *on
015600020410     c                   else
015700020410     c                   eval      *in88 = *off
015800020410     c                   endif
015900020516
016000020516      * profilo non abilitato
016100020516     c                   if        *in76
016200020516     c                   eval      f03l00 = *on
016300020516     c                   endif
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
017500940105     C                   MOVEL     DSUNIT        KPJBU
017600000000     C                   SETON                                        LR
017700000000     C*================================================================
017800930126     C* F03GES -GEST.TASTO FUNZ. F03 ===> FINE
017900000000     C*================================================================
018000000000     C     F03GES        BEGSR
018100000000     C*
018200000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PGM 00)
018300000000     C                   MOVEL     '1'           F03L00
018400000000     C*
018500000000     C                   ENDSR
018600000000     C*================================================================
018700930129     C* F05GES -GEST.TASTO FUNZ. F05 ===> RIVISUALIZZAZIONE
018800000000     C*================================================================
018900000000     C     F05GES        BEGSR
019000000000     C*
019100000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
019200000000     C                   EXSR      POSSFL
019300000000     C*
019400000000     C                   ENDSR
019500000000     C*================================================================
019600931116     C* F06GES -GEST.TASTO FUNZ. F06 ===> INSERIMENTO
019700000000     C*================================================================
019800930126     C     F06GES        BEGSR
019900930201     C*
020000930201     C* INSERIMENTO CONSENTITO SOLO IN SEDE
020100020709     c                   If        DutLpo <> 'S'
020200930201     C                   MOVEL     '1'           ERRL00
020300930201     C                   MOVEL     MSG(3)        DSMSMS
020400930201     C                   END
020500930201     C*
020600930201     C     ERRL00        IFEQ      '0'                                          --- 1 -->
020700930127     C*
020800941115     C* LANCIO IL PGM PER L'INSERIMENTO DEL RECORD (TNSDU2R)
020900930201     C                   MOVEL     'F06'         WOPZ1             3
021000100906     C                   MOVEL     'TNSDU2R'     WPGM              8
021100930201     C                   EXSR      LANPGM
021200930128     C*
021300930128     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
021400930128     C                   EXSR      POSSFL
021500930201     C*
021600930201     C                   END                                                    <-- 1 ---
021700930127     C*
021800000000     C                   ENDSR
021900000000     C*================================================================
022000100922     C* F14GES -GEST.TASTO FUNZ. F14 ===> richiamo pgm gestine scheda clienti
022100000000     C*================================================================
022200100922     C     F14GES        BEGSR
022300000000     C*
022400000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12 (A LIVELLO PGM 00)
022500100922     C                   clear                   kpjbu
022600100922     c                   call      'TRTB47R'
022700100922     c                   parm                    kpjba
022800000000     C*
022900000000     C                   ENDSR
023000100922     C*================================================================
023100100922     C* F12GES -GEST.TASTO FUNZ. F12 ===> RITORNO
023200100922     C*================================================================
023300100922     C     F12GES        BEGSR
023400100922     C*
023500100922     C* IMPOSTO FLAG DI PREMUTO TASTO F12 (A LIVELLO PGM 00)
023600100922     C                   MOVEL     '1'           F12L00
023700100922     C*
023800100922     C                   ENDSR
023900000000     C*================================================================
024000930126     C* F17GES -GEST.TASTO FUNZ. F17 ===> INIZIO DATI
024100000000     C*================================================================
024200000000     C     F17GES        BEGSR
024300000000     C*
024400000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
024500000000     C                   Z-ADD     1             SFLNBR
024600000000     C*
024700000000     C                   ENDSR
024800000000     C*================================================================
024900930126     C* F18GES -GEST.TASTO FUNZ. F18 ===> FINE DATI
025000000000     C*================================================================
025100000000     C     F18GES        BEGSR
025200000000     C*
025300000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
025400000000     C                   Z-ADD     SFLULT        SFLNBR
025500000000     C*
025600000000     C                   ENDSR
025700020517     C*================================================================
025800020517     C* F21GES -GEST.TASTO FUNZ. F21 ===> STAMPA ELENCO
025900020517     C*================================================================
026000020517     C     F21GES        BEGSR
026100020517     C*
026200020517     C                   ENDSR
026300000000     C*================================================================
026400930126     C* F22GES -GEST.TASTO FUNZ. F22 ===> STAMPE
026500000000     C*================================================================
026600000000     C     F22GES        BEGSR
026700000000     C*
026800000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
026900000000     C                   Z-ADD     80            LENGH            15 5
027000000000     C                   CALL      'QCAEXEC'
027100000000     C                   PARM                    CMD(1)
027200000000     C                   PARM                    LENGH
027300000000     C*
027400000000     C                   ENDSR
027500000000     C*================================================================
027600930126     C* F23GES -GEST.TASTO FUNZ. F23 ===> ALTRE OPZIONI
027700000000     C*================================================================
027800000000     C     F23GES        BEGSR
027900000000     C*
028000000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
028100000000     C                   EXSR      OPZGES
028200000000     C*
028300000000     C                   ENDSR
028400000000     C*================================================================
028500930126     C* F24GES -GEST.TASTO FUNZ. F24 ===> ALTRI TASTI
028600000000     C*================================================================
028700000000     C     F24GES        BEGSR
028800000000     C*
028900000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
029000000000     C                   EXSR      FUNGES
029100000000     C*
029200000000     C                   ENDSR
029300000000     C*================================================================
029400930126     C* RUPGES -GEST.TASTO FUNZ. ROLLUP ===> PAGINA SUCCESSIVA
029500000000     C*================================================================
029600000000     C     RUPGES        BEGSR
029700000000     C*
029800000000     C* CARICA IL SFL DA FILE "FISICO"
029900000000     C                   EXSR      CARSFL
030000000000     C*
030100000000     C                   ENDSR
030200000000     C*================================================================
030300930126     C* HELGES - GEST.TASTO FUNZ. HELP ===> AIUTO
030400000000     C*================================================================
030500000000     C     HELGES        BEGSR
030600000000     C*
030700000000     C                   ENDSR
030800000000     C*================================================================
030900930126     C* ENTGES - GEST.TASTO FUNZ. ENTER ===> INVIO
031000000000     C*================================================================
031100000000     C     ENTGES        BEGSR
031200000000     C*
031300000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
031400000000     C                   EXSR      NEWPOS
031500000000     C*
031600000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
031700000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
031800000000     C*
031900000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
032000000000     C     SWPOSI        IFNE      0                                            --- 2 -->
032100000000     C                   EXSR      POSSFL
032200000000     C*
032300000000     C                   ELSE                                                   <-- 2 -->
032400000000     C*
032500000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
032600000000     C     SFLULT        IFNE      0                                            --- 3 -->
032700000000     C                   EXSR      ENTATT
032800000000     C*
032900000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
033000000000     C     SWMODI        IFNE      0                                            --- 4 -->
033100000000     C                   EXSR      POSSFL
033200000000     C                   END                                                    <-- 4 ---
033300000000     C*
033400000000     C                   END                                                    <-- 3 ---
033500000000     C                   END                                                    <-- 2 ---
033600000000     C                   END                                                    <-- 1 ---
033700000000     C*
033800000000     C                   ENDSR
033900000000     C*================================================================
034000930126     C*  O01GES - GESTIONE OPZIONE 1=SCELTA
034100000000     C*================================================================
034200000000     C     O01GES        BEGSR
034300000000     C                   ENDSR
034400000000     C*================================================================
034500000000     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
034600000000     C*================================================================
034700000000     C     O02GES        BEGSR
034800100906     C*
034900100906     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA MODIFICA
035000100906     C     VIDATB        IFEQ      'A'
035100100906     C                   MOVEL     '1'           ERRL00
035200100906     C                   MOVEL     MSG(4)        DSMSMS
035300100906     C                   END
035400100906     C*
035500100906     C     ERRL00        IFEQ      '0'                                          --- 1 -->
035600100906     C*
035700100906     C* LANCIO IL PGM PER LA MODIFICA DEL DETTAGLIO DELL'ANAGRAFICA
035800100906     C                   MOVEL     'O02'         WOPZ1
035900100906     C                   MOVEL     'TNSDU2R'     WPGM
036000100906     C                   EXSR      LANPGM
036100100906     C*
036200100906     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
036300100906     C                   Z-ADD     1             SWMODI
036400100906     C*
036500100906     C                   END                                                    <-- 1 ---
036600100906     C*
036700000000     C*
036800000000     C                   ENDSR
036900000000     C*================================================================
037000971015     C* O03GES - GESTIONE OPZIONE 3=DUPLICA
037100000000     C*================================================================
037200000000     C     O03GES        BEGSR
037300100906     C*
037400100906     C* DUPLICA CONSENTITA SOLO IN SEDE
037500100906     c                   If        DutLpo <> 'S'
037600100906     C                   MOVEL     '1'           ERRL00
037700100906     C                   MOVEL     MSG(3)        DSMSMS
037800100906E   1C                   ENDIF
037900100906     C*
038000100906     C* LANCIA IL PGM PER L'INSERIMENTO
038100100906IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
038200100906     C                   MOVEL     'O03'         WOPZ1
038300100906     C                   MOVEL     'TNSDU2R'     WPGM
038400100906     C                   EXSR      LANPGM
038500100906     C*
038600100906     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
038700100906     C                   Z-ADD     1             SWMODI
038800100906E   1C                   ENDIF
038900100906     C*
039000000000     C                   ENDSR
039100000000     C*================================================================
039200000000     C*  O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE
039300000000     C*================================================================
039400000000     C     O04GES        BEGSR
039500100906     C*
039600100906     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
039700100906     C     VIDATB        IFEQ      'A'
039800100906     C                   MOVEL     '1'           ERRL00
039900100906     C                   MOVEL     MSG(4)        DSMSMS
040000100906     C                   END
040100100906     C*
040200100906     C     ERRL00        IFEQ      '0'                                          --- 1 -->
040300100906     C*
040400100906     C* LANCIO IL PGM DI GESTIONE DELLE UNITA
040500100906     C                   MOVEL     'O04'         WOPZ1
040600100906     C                   MOVEL     'TNSDU2R'     WPGM
040700100906     C                   EXSR      LANPGM
040800100906     C*
040900100906     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
041000100906     C                   Z-ADD     1             SWMODI
041100100906     C*
041200100906     C                   END                                                    <-- 1 ---
041300100906     C*
041400000000     C*
041500000000     C                   ENDSR
041600000000     C*================================================================
041700000000     C*  O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
041800000000     C*================================================================
041900000000     C     O05GES        BEGSR
042000000000     C*
042100000000     C* LANCIO ROUTINE PER LA VISUALIZZAZIONE DEL DETTAGLIO DELLA RIGA
042200930201     C                   MOVEL     'O05'         WOPZ1
042300020517     C                   MOVEL     'TNSDU2R'     WPGM
042400930201     C                   EXSR      LANPGM
042500000000     C*
042600000000     C                   ENDSR
042700000000     C*================================================================
042800000000     C*  O06GES - GESTIONE OPZIONE 6=STAMPA
042900000000     C*================================================================
043000000000     C     O06GES        BEGSR
043100000000     C                   ENDSR
043200930128     C*================================================================
043300930128     C*  O07GES - GESTIONE OPZIONE 7=RIPRISTINO
043400930128     C*================================================================
043500930128     C     O07GES        BEGSR
043600100906     C*
043700100906     C* SE IL RECORD NON E' ANNULLATO NON CONSENTO IL RIPRISTINO
043800100906     C     VIDATB        IFNE      'A'
043900100906     C                   MOVEL     '1'           ERRL00
044000100906     C                   MOVEL     MSG(5)        DSMSMS
044100100906     C                   END
044200100906     C*
044300100906     C     ERRL00        IFEQ      '0'                                          --- 1 -->
044400100906     C*
044500100906     C* LANCIO IL PGM PER IL RIPRISTINO DI UN ANNULLAMENTO
044600100906     C                   MOVEL     'O07'         WOPZ1
044700100906     C                   MOVEL     'TNSDU2R'     WPGM
044800100906     C                   EXSR      LANPGM
044900100906     C*
045000100906     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
045100100906     C                   Z-ADD     1             SWMODI
045200100906     C*
045300100906     c                   endif
045400930128     C*
045500930128     C                   ENDSR
045600930201     C*================================================================
045700940105     C*  O08GES - GESTIONE OPZIONE 8=GESTIONE MOVIMENTI
045800930201     C*================================================================
045900930201     C     O08GES        BEGSR
046000930201     C*
046100930201     C* LANCIO IL PGM PER LA GESTIONE DELLE LOCALITA'
046200971013     C     ERRL00        IFEQ      '0'                                          --- 1 -->
046300930201     C                   MOVEL     'O08'         WOPZ1
046400941115     C                   MOVEL     'TNSDU4R'     WPGM
046500930201     C                   EXSR      LANPGM
046600930201     C                   END                                                    <-- 1 ---
046700930201     C*
046800930201     C                   ENDSR
046900000000     C*================================================================
047000000000     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
047100000000     C*================================================================
047200000000     C     NEWPOS        BEGSR
047300000000     C*
047400000000     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAM.
047500000000     C*         1= RICHIESTO POSIZIONAM.
047600000000     C                   Z-ADD     0             SWPOSI            7 0
047700000000     C*
047800000000     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
047900000000     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
048000000000     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
048100000000     C                   Z-ADD     0             SWMODI            1 0
048200000000     C*
048300000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
048400020319     c                   if        vdpmat <> depmat or vdptip <> deptip or      --- 1 -->
048500020517     c                             vdpksc <> depksc or vdprsc <> deprsc or
048600020325     c                             vdpric <> depric
048700930202     C*
048800930202     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI DI PARTENZA
048900930202     C                   EXSR      CHKPAR
049000930202     C*
049100930202     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
049200930202     C     ERRL00        IFEQ      '0'                                          --- 2 -->
049300000000     C                   Z-ADD     1             SWPOSI
049400020517     c                   eval      deptip = vdptip
049500020319     c                   eval      depmat = vdpmat
049600020325     c                   eval      depric = vdpric
049700020517     c                   eval      depksc = vdpksc
049800020517     c                   eval      deprsc = vdprsc
049900930202     C                   END                                                    <-- 2 ---
050000930202     C*
050100000000     C                   END                                                    <-- 1 ---
050200000000     C*
050300000000     C                   ENDSR
050400000000     C*================================================================
050500000000     C* LETTURA RECORD FILE DA CARICARE
050600000000     C*================================================================
050700000000     C     LETFIL        BEGSR
050800020319
050900020319     c                   select
051000020319
051100020319     c                   when      wtipri = 'TIP'
051200020319     c                   read      unana02l                               99
051300020319
051400020319     c                   when      wtipri = 'MAT'
051500020319     c                   read      unana07l                               99
051600020319
051700020517     c                   when      wtipri = 'KSC'
051800020517     c     kuna10        reade     unana10l                               99
051900020319
052000020319     c                   endsl
052100020319
052200000000     C*
052300930201     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
052400930707     C     *IN99         IFEQ      '1'
052500000000     C                   Z-ADD     1             SFLEOF
052600930707     C                   END
052700000000     C*
052800000000     C                   ENDSR
052900000000     C*================================================================
053000000000     C* SETFIL - SETLL DEL FILE DA CARICARE
053100000000     C*================================================================
053200000000     C     SETFIL        BEGSR
053300020319
053400020319      * POSIZIONAMENTO DEL FILE DA CARICARE E PRIMA LETTURA
053500020319
053600020319     c                   select
053700020319
053800020517      * selezione per tipo
053900020319     c                   when      wtipri = 'TIP'
054000020319     c                   move      vdptip        unatip
054100020319     c                   move      vdpmat        unamat
054200020319     c     kuna02        setll     unana02l
054300020319     c                   if        not %found(unana02l)
054400020319     c                   eval      sfleof = 1
054500020319     c                   else
054600020319     c                   read      unana02l
054700020319     c                   if        %eof(unana02l)
054800020319     c                   eval      sfleof = 1
054900020319     c                   endif
055000020319     c                   endif
055100020319
055200020517      * selezione per matricola
055300020319     c                   when      wtipri = 'MAT'
055400020325      * no ricerca per contenuto
055500020325     c                   if        vdpric = *blanks
055600020319     c                   move      vdpmat        unamat
055700020319     c                   move      vdptip        unatip
055800020319     c     kuna07        setll     unana07l
055900020319     c                   if        not %found(unana07l)
056000020319     c                   eval      sfleof = 1
056100020319     c                   else
056200020319     c                   read      unana07l
056300020319     c                   if        %eof(unana07l)
056400020319     c                   eval      sfleof = 1
056500020319     c                   endif
056600020319     c                   endif
056700020325      * si ricerca per contenuto
056800020325     c                   else
056900020325     c     *loval        setll     unana07l
057000020325     c                   if        not %found(unana07l)
057100020325     c                   eval      sfleof = 1
057200020325     c                   else
057300020325     c                   read      unana07l
057400020325     c                   if        %eof(unana07l)
057500020325     c                   eval      sfleof = 1
057600020325     c                   endif
057700020325     c                   endif
057800020325     c                   endif
057900020319
058000020517      * selezione per cliente
058100020517     c                   when      wtipri = 'KSC'
058200020517     c                   eval      unaksc = vdpksc
058300020517     c     kuna10        setll     unana10l
058400020517     c                   if        not %found(unana10l)
058500020319     c                   eval      sfleof = 1
058600020319     c                   else
058700020517     c     kuna10        reade     unana10l
058800020517     c                   if        %eof(unana10l)
058900020319     c                   eval      sfleof = 1
059000020319     c                   endif
059100020319     c                   endif
059200020517
059300020319     c                   endsl
059400020319
059500000000     C                   ENDSR
059600020319      *================================================================
059700020319      * CTRSFL - CONTROLLO SE RECORD DA SCRIVERE NEL SUBFILE
059800020319      *================================================================
059900020319     C     CTRSFL        BEGSR
060000020319
060100020325      * record letto ok
060200020517
060300020517     c                   if        unaksc = *zeros and wnosfl = *off
060400020517     c                   eval      wnosfl = *on
060500020517     c                   endif
060600020517
060700020517     c                   if        wnosfl = *off
060800020517     c                   movel     unaksc        wmag
060900020517     c     wmag          lookup    skpoabi                                99
061000020517     c                   if        not *in99
061100020517     c                   eval      wnosfl = *on
061200020517     c                   endif
061300020517     c                   endif
061400020325
061500020325     c                   if        wtipri = 'MAT' and vdpric = 'S'
061600020517     c                             and wnosfl = *off
061700020325     c     ' '           checkr    vdpmat        xx
061800020325     c     vdpmat:xx     scan      unamat                                 99
061900020325     c                   if        not *in99
062000020325     c                   eval      wnosfl = *on
062100020325     c                   endif
062200020325     c                   endif
062300020325
062400020319     C                   ENDSR
062500000000     C*================================================================
062600000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
062700000000     C*================================================================
062800000000     C     IRESFL        BEGSR
062900000000     C*
063000000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
063100930127     C*
063200930707     C                   MOVEL     UNAATB        VIDATB
063300930707     C                   Z-ADD     UNATIP        VIDTIP
063400930707     C                   Z-ADD     UNACOD        VIDCOD
063500930707     C                   MOVEL     UNAMAT        VIDMAT
063600930707     C                   MOVEL     UNAMOD        VIDMOD
063700020517     c                   eval      vidksc = unaksc
063800020517     c                   clear                   TIBS69DS
063900020517     c                   z-add     vidksc        I69kac
064000020517     c                   call      'TIBS69R'
064100020517     c                   parm                    tibs69DS
064200020517     c                   parm                    ds_cnaco
064300020517     c                   parm                    ds_cnind
064400020517     c                   parm                    ds_cnclp
064500020517     c                   parm                    ds_fncls
064600020517     c                   if        o69err <> '1'
064700020517     c                   movel     acorag        vidrsc
064800020517     c                   endif
064900000000     C*
065000000000     C                   ENDSR
065100930202     C*================================================================
065200930202     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
065300930202     C*================================================================
065400930202     C     CHKPAR        BEGSR
065500020319
065600020319      * IMPOSTO FLAG EVIDENZAZIONE TIPO DI RICERCA
065700020319     c                   eval       *in90 = *off                                x tipo
065800020319     c                   eval       *in91 = *off                                x matricola
065900020517     c                   eval       *in92 = *off                                x cliente
066000020319
066100020319      * IMPOSTO FLAG EVIDENZAZIONE ERRORI SUI PARAMETRI
066200020319     c                   eval       *in80 = *off                                tipo
066300020319     c                   eval       *in81 = *off                                tipo
066400020319     c                   eval       *in82 = *off                                matricola
066500020517     c                   eval       *in83 = *off                                cliente
066600020326
066700020326     c                   clear                   wtipri
066800020319
066900020319      * Se non è stata scelta una ricerca utilizzo la ricerca per tipo
067000020319     c                   if        vdpmat = *blanks and vdptip = *blanks and
067100020517     c                             vdpksc = *zeros and vdprsc = *blanks
067200020319     c                   eval      wtipri = 'TIP'
067300020319     c                   eval      *in90 = *on
067400020319     c                   endif
067500020319
067600020319      * ===> ACCETTO SOLO UN PARAMETRO DI INIZIO RICERCA
067700020319
067800020319      * Tipo unità / matricola
067900020319     c                   if        vdptip <> *blanks
068000020319      * controllo tipo unità
068100020319     c                   exsr      chktip
068200020319     c                   eval      wtipri = 'TIP'
068300020319     c                   eval      *in90 = *on
068400020319     c                   endif
068500020319
068600020319      * Matricola
068700020410     c                   if        vdpmat <> *blanks and errl00 = *off and
068800020410     c                             vdptip = *blanks
068900020319     c                   eval      wtipri = 'MAT'
069000020319     c                   eval      *in91 = *on
069100020319     c                   endif
069200020325      * ricerca x matricla
069300020325     c                   if        errl00 = *off and vdpmat = *blanks and
069400020325     c                             vdpric = 'S'
069500020325     c                   eval      errl00 = *on
069600020325     c                   eval      dsmsms = msg(12)
069700020517     c                   eval      *in82 = *on
069800020325     c                   endif
069900020517
070000020517      * Cliente
070100020517     c                   if        vdpksc <> *zeros and vdprsc <> *blanks and
070200020517     c                             errl00 = *off
070300020517     c                   if        wtipri <> *blanks                            --- 2 -->
070400020517     c                   eval      errl00 = *on
070500020517     c                   eval      dsmsms = msg(6)
070600020517     c                   else                                                   <-- 2 -->
070700020517     c                   eval      errl00 = *on
070800020517     c                   eval      dsmsms = msg(8)
070900020517     c                   eval      *in83 = *on
071000020517     c                   endif
071100020517     c                   endif
071200020517      * controllo cliente
071300020517     c                   if        (vdpksc <> *zeros or vdprsc <> *blanks)
071400020517     c                             and errl00 = *off
071500020517     c                   if        wtipri <> *blanks                            --- 2 -->
071600020517     c                   eval      errl00 = *on
071700020517     c                   eval      dsmsms = msg(6)
071800020517     c                   else                                                   <-- 2 -->
071900020517     c                   exsr      chkksc
072000020517     c                   eval      wtipri = 'KSC'
072100020517     c                   eval      *in92 = *on
072200020517     c                   endif
072300020517     c                   endif
072400020326
072500020326      * SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
072600020326      * CURSORE SUL CAMPO DEL CONTROL
072700020326     C     errl00        IFEQ      '1'
072800020326     C                   MOVEL     '0'           *IN43                          *NON EMETTO SFL
072900020326     C                   ELSE
073000020326     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
073100020326     C                   END
073200020319
073300930202     C                   ENDSR
073400930707     C*================================================================
073500930707     C*  CHKTIP - CONTROLLA IL TIPO DI UNITA
073600930707     C*================================================================
073700930707     C     CHKTIP        BEGSR
073800020318
073900020318      * CONTROLLO SE E' STATO INSERITO '?'
074000020319     C     '?'           SCAN      VDPTIP                                 99
074100020318
074200020318      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
074300020319     C                   if        *in99                                        --- 1 -->
074400020318     C                   clear                   TIBS02DS
074500020318     C                   movel     'R'           t02mod
074600020318     C                   movel     knsif         t02sif
074700020321     C                   movel     'UNT'         t02cod
074800020318     C                   call      'TIBS02R'
074900020318     C                   parm                    KPJBA
075000020318     C                   parm                    TIBS02DS
075100020318     C                   movel     T02ke1        vdptip
075200020318
075300020319     C                   endif                                                  <-- 1 ---
075400020318
075500020318      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
075600020318     C                   clear                   TIBS02DS
075700020318     C                   movel     'C'           T02mod
075800020318     C                   movel     knsif         t02sif
075900020321     C                   movel     'UNT'         t02cod
076000020318     C                   movel(p)  vdptip        T02ke1
076100020318     C                   call      'TIBS02R'
076200020318     C                   parm                    KPJBA
076300020318     C                   parm                    TIBS02DS
076400020318     C                   if        t02err <> *blanks
076500020319     C                   eval      errl00 = '1'
076600020319     C                   eval      *in80 = *on
076700020318     C                   movel     msg(7)        dsmsms
076800020318     C                   endif
076900020319
077000930707     C                   ENDSR
077100930707     C*================================================================
077200020517     C*  CHKKSC - CONTROLLA IL CLIENTE
077300930707     C*================================================================
077400020517     C     CHKKSC        BEGSR
077500020517
077600020517      * se non inserito il codice ma inserita una parte della ragione sociale
077700020517      * richiamo il programma della ricerca alfabetica
077800020517     c                   if        vdpksc = *zeros and vdprsc <> *blanks
077900020517     c                   exsr      ricalf
078000020517     c                   endif
078100020517
078200020517     c                   movel     vdpksc        wmag
078300020517     c     wmag          lookup    skpoabi                                99
078400020517     c                   if        not *in99
078500020517     c                   eval      errl00 = *on
078600020517     c                   eval      *in83 = *on
078700020517     c                   movel     msg(14)       dsmsms
078800020517     c                   endif
078900020517
079000020517      * se inserito il codice controllo che esista sul piano dei conti
079100020517     c                   if        vdpksc <> *zeros and errl00 = *off
079200020517     c                   clear                   TIBS69DS
079300020517     c                   z-add     vdpksc        I69kac
079400020517     c                   call      'TIBS69R'
079500020517     c                   parm                    tibs69DS
079600020517     c                   parm                    ds_cnaco
079700020517     c                   parm                    ds_cnind
079800020517     c                   parm                    ds_cnclp
079900020517     c                   parm                    ds_fncls
080000020517
080100020517     c                   if        o69err <> '1'
080200020517     c                   movel     acorag        vdprsc
080300020517     c                   else
080400020517     c                   eval      errl00 = *on
080500020517     c                   eval      *in83 = *on
080600020517     c                   movel     msg(9)        dsmsms
080700020517     c                   endif
080800020517     c                   endif
080900020517
081000930707     C                   ENDSR
081100020517      *================================================================
081200020517      *  RICALF - RICERCA ALFABETICA
081300020517      *================================================================
081400020517     c     ricalf        begsr
081500020517
081600020517     C                   MOVEL     rsut          PAXdut                         Descrizione Azienda
081700081203     C*******            Z-ADD     9             PAXsta                         9=Escludo Annullati
081800081203     c* visulizzare anche i bloccati
081900081203     C                   Z-ADD     0             PAXsta                         9=Escludo Annullati
082000020517     C                   Z-ADD     dutkci        PAXccc                         Capoconto Clienti
082100020517     c                   movel(P)  vdprsc        PAXdmt
082200020517     C                   z-add     1             PAXnum
082300020517
082400020517     C                   CALL      'XALFA3BR'
082500020517     C                   PARM                    PAXDUT
082600020517     C                   PARM                    CODUT             1 0
082700020517     C                   PARM                    PAXDMT
082800020517     C                   PARM                    PAXCCC
082900020517     C                   PARM                    PAXSTA
083000020517     C                   PARM      *blanks       PAXFLR
083100020517     C                   PARM      *blanks       PAXDIT
083200020517     C                   PARM                    PAXNUM            2 0
083300020517     C                   PARM                    PAXKCM           80
083400020517     C                   PARM                    PAXKSM          140
083500020517     C                   PARM                    PAXKDM           60
083600020517
083700020517      * NON TROVATO O PREMUTO CMD12
083800020517    2C     paxsta        IFGT      -1
083900020517     C                   MOVEL     PAXKSM        vdpksc
084000020517    2C                   ENDIF
084100020517
084200020517     c                   endsr
084300020516      *------------------------------------------------------------------------*
084400020516      *  CTRUTE - ROUTINE CONTROLLO UTENTE
084500020516      *------------------------------------------------------------------------*
084600020516     c     ctrute        begsr
084700020516
084800020516     c                   eval      *in76 = *off
084900020516
085000020516      * controllo se il profilo è abilitato
085100020516     c                   if        duterr = 'E'
085200020516     c                   eval      dsmsms = dutmsg
085300020516     c                   eval      *in76 = *on
085400020516     c                   endif
085500020516      * controllo le abilitazionei del profilo
085600020516     c                   if        *in76 = *off
085700020516     c                   movel     utefaf        dute01
085800020516     c                   if        §uteiun <> *blanks
085900020516     c                   eval      wabi = §uteiun
086000020516     c                   else
086100020516     c                   eval      wabi = uteaut
086200020516     c                   endif
086300020516     c                   clear                   tibs02ds
086400020516     c                   movel     'C'           t02mod
086500020516     c                   movel     knsif         t02sif
086600020516     c                   movel     'LAT'         t02cod
086700020516     c                   movel(p)  wabi          t02ke1
086800020516     c                   call      'TIBS02R'
086900020516     c                   parm                    kpjba
087000020516     c                   parm                    tibs02ds
087100020516     c                   movel     t02uni        dlat
087200020516     c                   if        §latabi = 'S'
087300020516     c                   eval      dsmsms= msg(13)
087400020516     c                   eval      *in76 = *on
087500020516     c                   endif
087600020516     c                   endif
087700020516
087800020516      * Utente ok richiamo pgm che ritorna i p.o. abilitati
087900020523     c                   if        *in76 = *off  and wabi <> 'AZ'
088000020516     c                   clear                   trul31ds
088100020516     c                   eval      i31abi = wabi
088200020516     c                   eval      i31cdi = dutdis
088300020516     c                   eval      i31car = dutare
088400020516     c                   eval      i31cpo = dutpou
088500020516     c                   call      'TRUL31R'
088600020516     c                   parm                    kpjba
088700020516     c                   parm                    trul31ds
088800020516     c                   endif
088900100906
089000100906      /free
089100100906         SELECT;
089200100906
089300100906           // - Reperimento filiali gestibili per utente della LOGISTICA
089400100906           WHEN  DUTntw = c_Logistica;
089500100906
089600100906             setll  *loval  AZORG;
089700100906             read   AZORG;
089800100906
089900100906             DoW  Not  %eof(AZORG01L);
090000100906
090100100906               if  ORGfva = *blank;
090200100906
090300100906                 Og143 = ORGde3;
090400100906                 if  §OgNtw = DUTntw;
090500100906
090600100906                   ii += 1;
090700100906                   skPOabi(ii) = ORGfil;
090800100906
090900100906                 endif;
091000100906
091100100906               endif;
091200100906
091300100906               read   AZORG;
091400100906
091500100906             EndDo;
091600100906
091700100906      /end-free
091800100906
091900020517
092000020529      * per abilitazione 'AZ' devo caricare tutti i p.o. di azorg21l
092100100906     c                   when      wabi = 'AZ'
092200020529     c     *loval        setll     azorg21l
092300020517     c                   do        *hival
092400020529     c                   read      azorg21l
092500020517     c                   if        %eof
092600020517     c                   leave
092700020517     c                   endif
092800020517     c                   z-add     1             ii
092900020523     c     orgfil        lookup    skpoabi(ii)                            99
093000020517     c                   if        not *in99
093100020517     c                   z-add     1             ii
093200020517     c     *zeros        lookup    skpoabi(ii)                            99
093300020517     c                   if        *in99
093400020517     c                   movel     orgfil        skpoabi(ii)
093500020517     c                   endif
093600020517     c                   endif
093700020517     c                   enddo
093800020529      * e tutti i p.o. di azorg01l
093900020529     c     *loval        setll     azorg01l
094000020529     c                   do        *hival
094100020529     c                   read      azorg01l
094200020529     c                   if        %eof
094300020529     c                   leave
094400020529     c                   endif
094500020529     c                   z-add     1             ii
094600020529     c     orgfil        lookup    skpoabi(ii)                            99
094700020529     c                   if        not *in99
094800020529     c                   z-add     1             ii
094900020529     c     *zeros        lookup    skpoabi(ii)                            99
095000020529     c                   if        *in99
095100020529     c                   movel     orgfil        skpoabi(ii)
095200020529     c                   endif
095300020529     c                   endif
095400020529     c                   enddo
095500020523      * se non è abilitazione 'AZ' passo la sk tornata dal TRUL31 nella sk di 500 elementi
095600100906     c                   other
095700020523     c                   movea     skpo          skpoabi
095800020523     c                   z-add     1             ii
095900020523     c     951           lookup    skpoabi(ii)                            99
096000020523     c                   if        not *in99
096100020523     c                   z-add     1             ii
096200020523     c     *zeros        lookup    skpoabi(ii)                            99
096300020523     c                   if        *in99
096400021014     c                   movel     951           skpoabi(ii)
096500020523     c                   endif
096600020523     c                   endif
096700100906     c                   endsl
096800020516
096900020516     c                   endsr
097000930201     C*================================================================
097100930201     C* LANPGM - LANCIA UN PROGRAMMA
097200930201     C*================================================================
097300930201     C     LANPGM        BEGSR
097400930201     C*
097500930201     C* IMPOSTO PARAMETRI PER IL PGM
097600930707     C                   MOVEL     '0'           DSUF03                         PREMUTO F03
097700930707     C                   MOVEL     '0'           DSUERR                         '1'=ERRORE
097800930707     C                   MOVEL     *BLANKS       DSUMSG                         MESSAGGIO
097900930707     C                   MOVE      WOPZ1         DSUOP1                         *OPZIONE LIV.PGM
098000020322     C                   Z-ADD     vidtip        DSUTIP                         TIPO
098100930707     C                   Z-ADD     VIDCOD        DSUCOD                         CODICE
098200930712     C                   Z-ADD     *ZEROS        DSUMAG                         MAGAZZINO
098300930712     C                   Z-ADD     *ZEROS        DSUPRG                         PROGRESSIVO MOV.
098400930906     C                   Z-ADD     *ZEROS        DSUDTM                         DATA MOVIMENTO
098500930201     C*
098600930201     C* LANCIO PROGRAMMA
098700940105     C                   MOVEL     DSUNIT        KPJBU
098800930312     C                   CALL      WPGM
098900940105     C                   PARM                    KPJBA
099000940105     C                   MOVEL     KPJBU         DSUNIT
099100930201     C*
099200930201     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
099300930707     C     DSUF03        IFEQ      '1'
099400930201     C                   MOVEL     '1'           F03L00
099500930201     C                   MOVEL     '1'           ERRL00
099600930201     C                   ELSE
099700930201     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
099800930707     C                   MOVEL     DSUMSG        DSMSMS
099900930201     C                   END
100000930201     C*
100100930201     C                   ENDSR
100200930312     C*================================================================
100300930312     C* CHKFOP - CONTROLLA TIPO GESTIONE E IMPOSTA FUNZ/OPZ
100400930312     C*================================================================
100500930312     C     CHKFOP        BEGSR
100600930312     C*
100700930312     C* CONTROLLO CHE VI SIA UNA OPZIONE DI PROCEDURA VALIDA IN INPUT
100800930312     C* ALTRIMENTI FORZO INTERROGAZIONE
100900020517     C     DSUOP0        IFNE      ' 5'                                         *INTERROGAZIONE
101000100906     C     DSUOP0        andne     ' 2'                                         *INTERROGAZIONE
101100940105     C                   MOVE      ' 5'          DSUOP0
101200930312     C                   END
101300100906     C* SE L'UTENTE E' ENTRATO IN GESTIONE LO ABILITO A TUTTE LE FUNZIONI
101400100906     C     DSUOP0        IFEQ      ' 2'
101500100906     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
101600100906     C                   MOVEL     'S'           OPDATT
101700100906     C                   MOVEL     OPDRIG        OPD(2)
101800100906     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
101900100906     C                   MOVEL     'S'           OPDATT
102000100906     C                   MOVEL     OPDRIG        OPD(3)
102100100906     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
102200100906     C                   MOVEL     'S'           OPDATT
102300100906     C                   MOVEL     OPDRIG        OPD(4)
102400100906     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
102500100906     C                   MOVEL     'S'           OPDATT
102600100906     C                   MOVEL     OPDRIG        OPD(5)
102700100906     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
102800100906     C                   MOVEL     'S'           OPDATT
102900100906     C                   MOVEL     OPDRIG        OPD(7)
103000100906     C                   MOVEL     OPD(8)        OPDRIG                         *MOVIMENTI
103100100906     C                   MOVEL     'S'           OPDATT
103200100906     C                   MOVEL     OPDRIG        OPD(8)
103300100906     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
103400100906     C                   MOVEL     'S'           FUDATT
103500100906     C                   MOVEL     FUDRIG        FUD(6)
103600100922     C                   MOVEL     FUD(14)       FUDRIG                         *pgm scheda cli
103700100922     C                   MOVEL     'S'           FUDATT
103800100922     C                   MOVEL     FUDRIG        FUD(14)
103900100906     C                   END
104000100906     C*
104100930312     C*
104200930312     C* SE IL PGM E' STATO LANCIATO IN INTERROGAZIONE ABILITO SOLO QUELLA
104300940105     C     DSUOP0        IFEQ      ' 5'
104400930312     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
104500930312     C                   MOVEL     'S'           OPDATT
104600930312     C                   MOVEL     OPDRIG        OPD(5)
104700971015     C                   MOVEL     OPD(8)        OPDRIG                         *MOVIMENTI
104800971015     C                   MOVEL     'S'           OPDATT
104900971015     C                   MOVEL     OPDRIG        OPD(8)
105000930312     C                   END
105100930312     C*
105200930312     C* IMPOSTO INDICE PER EMISSIONE OPZIONI A VIDEO
105300930312     C                   Z-ADD     0             OPZGRU            5 0
105400930312     C*
105500930312     C* GESTIONE OPZIONI A VIDEO
105600930312     C                   EXSR      OPZGES
105700930312     C*
105800930312     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
105900930312     C                   Z-ADD     0             FUNGRU            5 0
106000930312     C*
106100930312     C* GESTIONE TASTI FUNZIONALI A VIDEO
106200930312     C                   EXSR      FUNGES
106300930312     C*
106400930312     C                   ENDSR
106500000000     C*================================================================
106600000000     C*  RUTINZ - ROUTINE INIZIALE
106700000000     C*================================================================
106800000000     C     RUTINZ        BEGSR
106900000000     C*
107000020517     C* RICEVIMENTO PARAMETRI DALL'AZIONE (' 5'=INTERR.)
107100000000     C     *ENTRY        PLIST
107200930707     C                   PARM                    KPJBA
107300940105     C                   MOVEL     KPJBU         DSUNIT
107400020516
107500020516     c     *dtaara       define    §azute        azuteds
107600020516     c     *dtaara       define    §datiute      ddatiute
107700020516
107800020516     c                   in(E)     *dtaara
107900020516     c                   if        %Error or rsut = *blanks
108000020516     c                   clear                   tibs34ds
108100020516     c                   call      'TIBS34R'
108200020516     c                   parm                    Tibs34Ds
108300020516     c                   in        *dtaara
108400020516     c                   endif
108500020516
108600930204     C*
108700930204     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
108800930204     C     XMSGPA        PLIST
108900930204     C                   PARM                    DSMSMS
109000930204     C                   PARM                    DSMSPG
109100930204     C                   PARM                    DSMSKE
109200930204     C                   PARM                    DSMSER
109300020319
109400020319      * CHIAVE LETTURA UNANA02L
109500930707     C     KUNA02        KLIST
109600020319     C                   KFLD                    UNATIP
109700020319     C                   KFLD                    UNAMAT
109800020319
109900020319      * CHIAVE LETTURA UNANA07L
110000020319     C     KUNA07        KLIST
110100020319     C                   KFLD                    UNAMAT
110200020319     C                   KFLD                    UNATIP
110300020517
110400020517      * CHIAVE LETTURA UNANA10L
110500020517     C     KUNA10        KLIST
110600020517     C                   KFLD                    UNAKSC
110700020319
110800020529     C* CHIAVE LETTURA AZORG21l/azorg01l - COMPLETA
110900930707     C     KEYORG        KLIST
111000930707     C                   KFLD                    ORGFIL
111100930707     C*
111200000000     C*
111300930129     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
111400930204     C                   CALL      'XKEY'
111500930204     C                   PARM                    DSKEY
111600000000     C*
111700000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
111800000000     C     *LIKE         DEFINE    SFLNBR        SFLPAG
111900000000     C     *LIKE         DEFINE    SFLPAG        WFLPAG
112000000000     C     *LIKE         DEFINE    SFLNBR        SFLEOF
112100000000     C     *LIKE         DEFINE    SFLNBR        SFLERR
112200000000     C     *LIKE         DEFINE    SFLNBR        SFLULT
112300000000     C     *LIKE         DEFINE    SFLNBR        WFLULT
112400000000     C     *LIKE         DEFINE    SFLNBR        MSGULT
112500000000     C     *LIKE         DEFINE    SFLNBR        SFLRIG
112600000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
112700000000     C*
112800930127     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
112900000000     C*   -NOME PGM CHIAMANTE
113000000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
113100020517     C                   MOVEL     'TNSDUIR'     DSMSPG
113200020517     C                   MOVEL     'TNSDUIR'     MSGQUE
113300000000     C                   MOVEL     *BLANKS       MSGKEY
113400930127     C                   MOVEL     *BLANKS       DSMSMS
113500930204     C                   MOVEL     *BLANKS       DSMSKE
113600930204     C                   Z-ADD     0             DSMSER
113700000000     C*
113800930129     C* INIZIALIZZO SWITCH DI LIVELLO 0 (PGM)
113900930127     C                   MOVEL     '0'           F12L00            1
114000930127     C                   MOVEL     '0'           F03L00            1
114100930127     C                   MOVEL     '0'           FINL00            1
114200930127     C                   MOVEL     '0'           ERRL00            1
114300930128     C                   MOVEL     '0'           FLFINE            1
114400930127     C*
114500930312     C* IMPOSTO DEPOSITI PER CONFRONTI
114600930707     C                   MOVEL     *BLANKS       DEPTIP
114700100922     C                   MOVEL     *BLanks       DEPMAT
114800020325     c                   clear                   depric
114900020517     c                   clear                   depksc
115000020517     c                   clear                   deprsc
115100930202     C*
115200941128     C* IMPOSTO TIPO DI RICERCA DI DEFAULT --> MAT
115300020319     C                   MOVEL     'TIP'         WTIPRI
115400020319     c                   eval      *in90 = *on
115500000000     C*
115600000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
115700000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
115800020517     C                   Z-ADD     12            SFLPAG
115900930312     C*
116000930312     C* IMPOSTO LE OPZIONI E LE FUNZIONI A VIDEO
116100930312     C                   EXSR      CHKFOP
116200930225     C*
116300930225     C* IMPOSTO INTERROGAZIONE/GESTIONE NELLA TESTATA DELLA VIDEATA
116400100906     c                   select
116500100906     C     DSUOP0        wheneq    ' 5'
116600020517     C                   eval      viding = ' INTERROGAZIONE COMODATI '
116700100906     C     DSUOP0        wheneq    ' 2'
116800100906     C                   eval      viding = 'G E S T I O N E  COMODATI'
116900100906     C                   ENDsl
117000020325
117100000000     C                   ENDSR
117200020326
117300930312     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
117400930312     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
117500930312     C* LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
117600930312     C* STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
117700930312     C* NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
117800930312     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
117900930312     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
118000930312     C*
118100930312     C*================================================================
118200930312     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA
118300930312     C*          CARICARE
118400930312     C*================================================================
118500930312     C     POSSFL        BEGSR
118600930312     C*
118700930312     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
118800930312     C                   EXSR      CLRSFL
118900930312     C*
119000930312     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
119100930312     C                   EXSR      SETFIL
119200930312     C*
119300930312     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
119400930312     C     SFLEOF        IFEQ      0
119500020326     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
119600930312     C                   EXSR      CARSFL
119700930312     C                   END
119800930312     C*
119900930312     C                   ENDSR
120000000000     C*================================================================
120100000000     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
120200000000     C*================================================================
120300000000     C     ENTATT        BEGSR
120400000000     C*
120500000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
120600000000     C                   Z-ADD     0             OPZOK             1 0
120700000000     C*
120800000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
120900000000     C                   Z-ADD     0             SFLVAR            5 0
121000000000     C*
121100000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
121200000000     C                   Z-ADD     0             SFLERR
121300000000     C*
121400000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
121500000000     C                   Z-ADD     SFLULT        WFLULT
121600000000     C*
121700000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
121800020517     C                   READC     SDUID05                                98
121900930202     C     *IN98         DOWEQ     '0'
122000000000     C*
122100000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
122200000000     C                   ADD       1             SFLVAR
122300000000     C*
122400000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
122500000000     C     OPZOK         IFEQ      0
122600000000     C*
122700000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
122800000000     C     OPZKEY        IFNE      *BLANKS
122900000000     C                   EXSR      OPZCON
123000000000     C                   END
123100000000     C*
123200000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
123300000000     C*            2. PULIZIA SCELTA IN SFL
123400000000     C     OPZOK         IFEQ      0
123500000000     C                   MOVEL     '0'           *IN41
123600000000     C                   MOVEL     *BLANKS       OPZKEY
123700000000     C                   ELSE
123800000000     C*
123900000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
124000000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
124100000000     C                   MOVEL     '1'           *IN41
124200000000     C                   Z-ADD     SFLULT        SFLERR
124300000000     C                   END
124400000000     C                   ELSE
124500000000     C*
124600000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
124700000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
124800000000     C                   MOVEL     '1'           *IN41
124900000000     C                   END                                                    OPZOK=0
125000000000     C*
125100000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
125200000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
125300000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
125400000000     C     OPZOK         IFNE      0
125500000000     C                   Z-ADD     SFLERR        SFLNBR
125600000000     C                   ELSE
125700000000     C                   Z-ADD     SFLULT        SFLNBR
125800000000     C                   END
125900000000     C*
126000000000     C* AGGIORNO RECORD SFL
126100020517     C                   UPDATE    SDUID05
126200000000     C*
126300020517     C                   READC     SDUID05                                98
126400000000     C                   END                                                    DO WHILE
126500000000     C*
126600000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
126700000000     C                   Z-ADD     WFLULT        SFLULT
126800000000     C*
126900000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
127000000000     C     SFLVAR        IFEQ      0
127100000000     C                   MOVEL     '1'           FINL00
127200000000     C                   END
127300000000     C*
127400000000     C                   ENDSR
127500000000     C*================================================================
127600000000     C* ELIMINAZIONE CONTENUTO SFL
127700000000     C*================================================================
127800000000     C     CLRSFL        BEGSR
127900000000     C*
128000000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
128100000000     C* (0=NON FINE FILE  1=FINE FILE)
128200000000     C                   Z-ADD     0             SFLEOF
128300000000     C*
128400000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
128500000000     C                   Z-ADD     0             SFLULT
128600000000     C*
128700000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
128800000000     C                   MOVEL     '1'           *IN40
128900020517     C                   WRITE     SDUID06
129000000000     C                   MOVEL     '0'           *IN40
129100000000     C*
129200000000     C                   ENDSR
129300000000     C*================================================================
129400000000     C* CARSFL - CARICAMENTO SFL
129500000000     C*================================================================
129600000000     C     CARSFL        BEGSR
129700000000     C*
129800000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
129900000000     C     SFLULT        IFEQ      0
130000000000     C                   EXSR      SETFIL
130100000000     C                   END
130200000000     C*
130300000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
130400000000     C                   Z-ADD     SFLULT        WFLULT
130500000000     C*
130600000000     C* CICLO:
130700000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
130800000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
130900000000     C                   Z-ADD     1             SFLRIG
131000000000     C     SFLRIG        DOWLE     SFLPAG
131100000000     C     SFLEOF        ANDEQ     0
131200020319
131300020319     c                   eval      wnosfl = *off
131400020319
131500000000     C*
131600000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
131700000000     C                   MOVEL     *BLANKS       OPZKEY
131800020319
131900020326      * controllo se devo scrivere il record nel subfile
132000020319     c                   exsr      ctrsfl
132100020319
132200020319     c                   if        wnosfl = *off
132300000000     C*
132400000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
132500000000     C                   EXSR      IRESFL
132600000000     C*
132700000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
132800000000     C                   ADD       1             SFLULT
132900000000     C*
133000000000     C* SCRITTURA RECORD SFL
133100020517     C                   WRITE     SDUID05
133200000000     C                   ADD       1             SFLRIG
133300020319
133400020319     c                   endif
133500000000     C*
133600000000     C* LETTURA FILE DA CARICARE
133700000000     C                   EXSR      LETFIL
133800000000     C                   END                                                    DO WHILE
133900000000     C*
134000000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
134100000000     C     WFLULT        IFNE      SFLULT
134200000000     C                   Z-ADD     WFLULT        SFLNBR
134300000000     C                   ADD       1             SFLNBR
134400000000     C                   END
134500000000     C*
134600000000     C                   ENDSR
134700000000     C*================================================================
134800000000     C* EMISSIONE SUBFILE A VIDEO
134900000000     C*================================================================
135000000000     C     OUTSFL        BEGSR
135100000000     C*
135200000000     C* WRITE RECORD TESTATA VIDEO
135300020517     C                   WRITE     SDUID01
135400930205     C*
135500930205     C* VISUALIZZAZIONE MESSAGGIO
135600930205     C                   EXSR      SNDMSG
135700000000     C*
135800000000     C* WRITE RECORD OPZIONI VIDEO
135900020517     C                   WRITE     SDUID02
136000000000     C*
136100000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
136200000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
136300000000     C     SFLEOF        IFEQ      0
136400000000     C                   MOVEL     '0'           *IN42
136500000000     C                   ELSE
136600000000     C*
136700000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
136800000000     C                   MOVEL     '1'           *IN42
136900000000     C                   END
137000000000     C*
137100000000     C* WRITE RECORD TASTI FUNZIONALI
137200020517     C                   WRITE     SDUID09
137300000000     C*
137400000000     C* CASO DI SFL VUOTO
137500000000     C     SFLULT        IFLE      0
137600000000     C*
137700000000     C*    - WRITE RECORD VUOTO
137800020326     C                   MOVEL     '0'           *IN43                          *NON VISUALIZZA SFL
137900020517     C                   WRITE     SDUID0A
138000020517     C                   WRITE     SDUID06                                      *SFL CONTROL
138100020517     C                   READ      SDUID06                                99
138200000000     C*
138300000000     C                   ELSE
138400000000     C*
138500000000     C* CASO DI SFL CON RECORDS
138600000000     C*
138700000000     C*    - WRITE RECORD CONTROLLO SFL
138800020517     C                   WRITE     SDUID06
138900020517     C                   READ      SDUID06                                99
139000000000     C                   END
139100000000     C*
139200000000     C                   ENDSR
139300000000     C*================================================================
139400000000     C* SNDMSG - MANDA MESSAGGIO
139500000000     C*================================================================
139600000000     C     SNDMSG        BEGSR
139700000000     C*
139800000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
139900000000     C                   MOVEL     '1'           *IN40
140000020517     C                   WRITE     SDUIDMC
140100000000     C                   MOVEL     '0'           *IN40
140200000000     C*
140300000000     C* VISUALIZZAZIONE MESSAGGIO
140400000000     C                   Z-ADD     0             MSGULT
140500930204     C                   CALL      'XMSG'        XMSGPA
140600930127     C     DSMSER        IFEQ      0
140700930127     C                   MOVEL     DSMSKE        MSGKEY
140800000000     C                   END
140900000000     C                   Z-ADD     1             MSGULT
141000020517     C                   WRITE     SDUIDMS                                      SFL MESS
141100020517     C                   WRITE     SDUIDMC                                      CONTR MESS
141200000000     C*
141300000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
141400930127     C                   MOVEL     *BLANKS       DSMSMS
141500000000     C*
141600000000     C                   ENDSR
141700000000     C*================================================================
141800000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
141900000000     C*================================================================
142000000000     C     FUNCON        BEGSR
142100000000     C*
142200000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
142300000000     C                   EXSR      FUNVAR
142400000000     C*
142500000000     C* PULISCO CAMPI DI LAVORO
142600000000     C                   Z-ADD     0             I                 5 0
142700000000     C                   Z-ADD     0             W                 5 0
142800000000     C*
142900000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
143000100922     c* ROLL in 25° posizione
143100100922    1C     FUNKEY        IFNE      *BLANKS
143200100922    2C     1             DO        25            I
143300000000     C                   MOVEL     FUD(I)        FUDRIG
143400000000     C*
143500000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
143600100922    3C     FUNKEY        IFEQ      FUDFUN
143700000000     C     FUDATT        ANDEQ     'S'
143800000000     C                   Z-ADD     I             W
143900100922    3C                   END
144000100922    2C                   END                                                    DO WHILE
144100000000     C*
144200000000     C* FUNZIONE NON ATTIVA (ERRORE)
144300100922    2C     W             IFEQ      0
144400000000     C*
144500000000     C* EMISSIONE MESSAGGIO DI ERRORE
144600930127     C                   MOVEL     MSG(2)        DSMSMS
144700100922    2C                   END                                                    W=0
144800100922    1C                   END                                                    FUNKEY NE *BLANKS
144900000000     C*
145000000000     C* FUNZIONE ATTIVA (PROCEDI ...)
145100000000     C     W             IFNE      0
145200000000     C     FUNKEY        OREQ      *BLANKS
145300000000     C*
145400000000     C     FUNKEY        IFEQ      *BLANKS
145500000000     C                   EXSR      ENTGES
145600000000     C                   END
145700000000     C     FUNKEY        IFEQ      '03'
145800000000     C                   EXSR      F03GES
145900000000     C                   END
146000000000     C     FUNKEY        IFEQ      '05'
146100000000     C                   EXSR      F05GES
146200000000     C                   END
146300930126     C     FUNKEY        IFEQ      '06'
146400930126     C                   EXSR      F06GES
146500000000     C                   END
146600000000     C     FUNKEY        IFEQ      '12'
146700000000     C                   EXSR      F12GES
146800000000     C                   END
146900100922     C     FUNKEY        IFEQ      '14'
147000100922     C                   EXSR      F14GES
147100100922     C                   END
147200000000     C     FUNKEY        IFEQ      '17'
147300000000     C                   EXSR      F17GES
147400000000     C                   END
147500000000     C     FUNKEY        IFEQ      '18'
147600000000     C                   EXSR      F18GES
147700000000     C                   END
147800930129     C     FUNKEY        IFEQ      '21'
147900930129     C                   EXSR      F21GES
148000930129     C                   END
148100000000     C     FUNKEY        IFEQ      '22'
148200000000     C                   EXSR      F22GES
148300000000     C                   END
148400000000     C     FUNKEY        IFEQ      '23'
148500000000     C                   EXSR      F23GES
148600000000     C                   END
148700000000     C     FUNKEY        IFEQ      '24'
148800000000     C                   EXSR      F24GES
148900000000     C                   END
149000000000     C     FUNKEY        IFEQ      'RU'                                         ROLLUP
149100000000     C                   EXSR      RUPGES
149200000000     C                   END
149300000000     C     FUNKEY        IFEQ      'HP'                                         HELP
149400000000     C                   EXSR      HELGES
149500000000     C                   END
149600000000     C*
149700000000     C                   END
149800000000     C*
149900000000     C                   ENDSR
150000000000     C*================================================================
150100000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
150200000000     C*================================================================
150300000000     C     FUNVAR        BEGSR
150400000000     C*
150500000000     C* INIZIALIZZA VARIABILE -FUNKEY-
150600000000     C*
150700000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
150800000000     C     KEY           IFEQ      ENTER
150900000000     C                   MOVEL     *BLANKS       FUNKEY
151000000000     C                   END
151100000000     C     KEY           IFEQ      F1
151200000000     C                   MOVEL     '01'          FUNKEY
151300000000     C                   END
151400000000     C     KEY           IFEQ      F2
151500000000     C                   MOVEL     '02'          FUNKEY
151600000000     C                   END
151700000000     C     KEY           IFEQ      F3
151800000000     C                   MOVEL     '03'          FUNKEY
151900000000     C                   END
152000000000     C     KEY           IFEQ      F4
152100000000     C                   MOVEL     '04'          FUNKEY
152200000000     C                   END
152300000000     C     KEY           IFEQ      F5
152400000000     C                   MOVEL     '05'          FUNKEY
152500000000     C                   END
152600000000     C     KEY           IFEQ      F6
152700000000     C                   MOVEL     '06'          FUNKEY
152800000000     C                   END
152900000000     C     KEY           IFEQ      F7
153000000000     C                   MOVEL     '07'          FUNKEY
153100000000     C                   END
153200000000     C     KEY           IFEQ      F8
153300000000     C                   MOVEL     '08'          FUNKEY
153400000000     C                   END
153500000000     C     KEY           IFEQ      F9
153600000000     C                   MOVEL     '09'          FUNKEY
153700000000     C                   END
153800000000     C     KEY           IFEQ      F10
153900000000     C                   MOVEL     '10'          FUNKEY
154000000000     C                   END
154100000000     C     KEY           IFEQ      F11
154200000000     C                   MOVEL     '11'          FUNKEY
154300000000     C                   END
154400000000     C     KEY           IFEQ      F12
154500000000     C                   MOVEL     '12'          FUNKEY
154600000000     C                   END
154700000000     C     KEY           IFEQ      F13
154800000000     C                   MOVEL     '13'          FUNKEY
154900000000     C                   END
155000000000     C     KEY           IFEQ      F14
155100000000     C                   MOVEL     '14'          FUNKEY
155200000000     C                   END
155300000000     C     KEY           IFEQ      F15
155400000000     C                   MOVEL     '15'          FUNKEY
155500000000     C                   END
155600000000     C     KEY           IFEQ      F16
155700000000     C                   MOVEL     '16'          FUNKEY
155800000000     C                   END
155900000000     C     KEY           IFEQ      F17
156000000000     C                   MOVEL     '17'          FUNKEY
156100000000     C                   END
156200000000     C     KEY           IFEQ      F18
156300000000     C                   MOVEL     '18'          FUNKEY
156400000000     C                   END
156500000000     C     KEY           IFEQ      F19
156600000000     C                   MOVEL     '19'          FUNKEY
156700000000     C                   END
156800000000     C     KEY           IFEQ      F20
156900000000     C                   MOVEL     '20'          FUNKEY
157000000000     C                   END
157100000000     C     KEY           IFEQ      F21
157200000000     C                   MOVEL     '21'          FUNKEY
157300000000     C                   END
157400000000     C     KEY           IFEQ      F22
157500000000     C                   MOVEL     '22'          FUNKEY
157600000000     C                   END
157700000000     C     KEY           IFEQ      F23
157800000000     C                   MOVEL     '23'          FUNKEY
157900000000     C                   END
158000000000     C     KEY           IFEQ      F24
158100000000     C                   MOVEL     '24'          FUNKEY
158200000000     C                   END
158300000000     C     KEY           IFEQ      ROLLUP
158400000000     C                   MOVEL     'RU'          FUNKEY
158500000000     C                   END
158600000000     C     KEY           IFEQ      HELP
158700000000     C                   MOVEL     'HP'          FUNKEY
158800000000     C                   END
158900000000     C*
159000000000     C                   ENDSR
159100000000     C*================================================================
159200000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
159300000000     C*================================================================
159400000000     C     FUNGES        BEGSR
159500000000     C*
159600000000     C* PULISCO CAMPI DI LAVORO
159700000000     C                   Z-ADD     0             W                 5 0
159800000000     C*
159900000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
160000000000     C* E CARICO TABELLA FUNZIONI ATTIVE
160100000000     C     1             DO        24            I
160200000000     C                   MOVEL     FUD(I)        FUDRIG
160300000000     C*
160400000000     C* PULIZIA FUNZIONI PRECEDENTI
160500000000     C                   MOVEL     *BLANKS       FUV(I)
160600000000     C*
160700000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
160800000000     C* (SALTA F23 E F24)
160900000000     C     FUDATT        IFEQ      'S'
161000000000     C     FUDFUN        ANDNE     '23'
161100000000     C     FUDFUN        ANDNE     '24'
161200000000     C                   ADD       1             W
161300000000     C                   MOVEL     FUDDES        FUV(W)
161400000000     C                   END
161500000000     C                   END                                                    DO WHILE
161600000000     C*
161700000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
161800000000     C* ALTRIMENTI DISATTIVO F24
161900000000     C     1             DO        24            I
162000000000     C                   MOVEL     FUD(I)        FUDRIG
162100000000     C*
162200000000     C     FUDFUN        IFEQ      '24'
162300000000     C     W             IFGT      6
162400000000     C                   MOVEL     'S'           FUDATT
162500000000     C                   ELSE
162600000000     C                   MOVEL     *BLANKS       FUDATT
162700000000     C                   END                                                    W GT 8
162800000000     C                   MOVEL     FUDRIG        FUD(I)
162900000000     C                   END
163000000000     C                   END                                                    DO WHILE
163100000000     C*
163200000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
163300000000     C     W             DIV       6             I
163400000000     C                   MVR                     Z
163500000000     C*
163600000000     C     Z             IFNE      0
163700000000     C     I             ADD       1             Z
163800000000     C                   ELSE
163900000000     C                   Z-ADD     I             Z
164000000000     C                   END
164100000000     C*
164200000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
164300000000     C                   ADD       1             FUNGRU
164400000000     C     FUNGRU        IFGT      Z
164500000000     C     FUNGRU        ORLT      1
164600000000     C                   Z-ADD     1             FUNGRU
164700000000     C                   END
164800000000     C*
164900000000     C* PRIMO GRUPPO DI 6 FUNZIONI
165000000000     C     FUNGRU        IFEQ      1
165100000000     C                   MOVEL     FUV(1)        FUN001
165200000000     C                   MOVEL     FUV(2)        FUN002
165300000000     C                   MOVEL     FUV(3)        FUN003
165400000000     C                   MOVEL     FUV(4)        FUN004
165500000000     C                   MOVEL     FUV(5)        FUN005
165600000000     C                   MOVEL     FUV(6)        FUN006
165700000000     C                   END
165800000000     C*
165900000000     C* SECONDO GRUPPO DI 6 FUNZIONI
166000000000     C     FUNGRU        IFEQ      2
166100000000     C                   MOVEL     FUV(7)        FUN001
166200000000     C                   MOVEL     FUV(8)        FUN002
166300000000     C                   MOVEL     FUV(9)        FUN003
166400000000     C                   MOVEL     FUV(10)       FUN004
166500000000     C                   MOVEL     FUV(11)       FUN005
166600000000     C                   MOVEL     FUV(12)       FUN006
166700000000     C                   END
166800000000     C*
166900000000     C* TERZO GRUPPO DI 6 FUNZIONI
167000000000     C     FUNGRU        IFEQ      3
167100000000     C                   MOVEL     FUV(13)       FUN001
167200000000     C                   MOVEL     FUV(14)       FUN002
167300000000     C                   MOVEL     FUV(15)       FUN003
167400000000     C                   MOVEL     FUV(16)       FUN004
167500000000     C                   MOVEL     FUV(17)       FUN005
167600000000     C                   MOVEL     FUV(18)       FUN006
167700000000     C                   END
167800000000     C*
167900000000     C* QUARTO GRUPPO DI 6 FUNZIONI
168000000000     C     FUNGRU        IFEQ      4
168100000000     C                   MOVEL     FUV(19)       FUN001
168200000000     C                   MOVEL     FUV(20)       FUN002
168300000000     C                   MOVEL     FUV(21)       FUN003
168400000000     C                   MOVEL     FUV(22)       FUN004
168500000000     C                   MOVEL     *BLANKS       FUN005
168600000000     C                   MOVEL     *BLANKS       FUN006
168700000000     C                   END
168800000000     C*
168900000000     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
169000000000     C                   MOVEL     *BLANKS       FUN007
169100000000     C                   MOVEL     *BLANKS       FUN008
169200000000     C*
169300000000     C     1             DO        24            I
169400000000     C                   MOVEL     FUD(I)        FUDRIG
169500000000     C     FUDFUN        IFEQ      '23'
169600000000     C     FUDATT        ANDEQ     'S'
169700000000     C                   MOVEL     FUDDES        FUN007
169800000000     C                   END
169900000000     C     FUDFUN        IFEQ      '24'
170000000000     C     FUDATT        ANDEQ     'S'
170100000000     C                   MOVEL     FUDDES        FUN008
170200000000     C                   END
170300000000     C                   END                                                    DO
170400000000     C*
170500000000     C                   ENDSR
170600000000     C*================================================================
170700000000     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
170800000000     C*================================================================
170900000000     C     OPZGES        BEGSR
171000000000     C*
171100000000     C* PULISCO CAMPI DI LAVORO
171200000000     C                   Z-ADD     0             W                 5 0
171300000000     C*
171400000000     C* RICERCO QUANTE OPZIONI SONO ATTIVE
171500000000     C* E CARICO TABELLA OPZIONI ATTIVE
171600000000     C     1             DO        24            I
171700000000     C                   MOVEL     OPD(I)        OPDRIG
171800000000     C*
171900000000     C* PULIZIA OPZIONI PRECEDENTI
172000000000     C                   MOVEL     *BLANKS       OPV(I)
172100000000     C*
172200000000     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
172300000000     C     OPDATT        IFEQ      'S'
172400000000     C                   ADD       1             W
172500000000     C                   MOVEL     OPDDES        OPV(W)
172600000000     C                   END
172700000000     C                   END                                                    DO WHILE
172800000000     C*
172900000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
173000000000     C* ALTRIMENTI DISATTIVO F23
173100000000     C     1             DO        24            I
173200000000     C                   MOVEL     FUD(I)        FUDRIG
173300000000     C*
173400000000     C     FUDFUN        IFEQ      '23'
173500000000     C     W             IFGT      8
173600000000     C                   MOVEL     'S'           FUDATT
173700000000     C                   ELSE
173800000000     C                   MOVEL     *BLANKS       FUDATT
173900000000     C                   END                                                    W GT 8
174000000000     C                   MOVEL     FUDRIG        FUD(I)
174100000000     C                   END
174200000000     C                   END                                                    DO WHILE
174300000000     C*
174400000000     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
174500000000     C     W             DIV       8             I
174600000000     C                   MVR                     Z                 5 0
174700000000     C*
174800000000     C     Z             IFNE      0
174900000000     C     I             ADD       1             Z
175000000000     C                   ELSE
175100000000     C                   Z-ADD     I             Z
175200000000     C                   END
175300000000     C*
175400000000     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
175500000000     C                   ADD       1             OPZGRU
175600000000     C     OPZGRU        IFGT      Z
175700000000     C     OPZGRU        ORLT      1
175800000000     C                   Z-ADD     1             OPZGRU
175900000000     C                   END
176000000000     C*
176100000000     C* PRIMO GRUPPO DI 8 OPZIONI
176200000000     C     OPZGRU        IFEQ      1
176300000000     C                   MOVEL     OPV(1)        OPZ001
176400000000     C                   MOVEL     OPV(2)        OPZ002
176500000000     C                   MOVEL     OPV(3)        OPZ003
176600000000     C                   MOVEL     OPV(4)        OPZ004
176700000000     C                   MOVEL     OPV(5)        OPZ005
176800000000     C                   MOVEL     OPV(6)        OPZ006
176900000000     C                   MOVEL     OPV(7)        OPZ007
177000000000     C                   MOVEL     OPV(8)        OPZ008
177100000000     C                   END
177200000000     C*
177300000000     C* SECONDO GRUPPO DI 8 OPZIONI
177400000000     C     OPZGRU        IFEQ      2
177500000000     C                   MOVEL     OPV(9)        OPZ001
177600000000     C                   MOVEL     OPV(10)       OPZ002
177700000000     C                   MOVEL     OPV(11)       OPZ003
177800000000     C                   MOVEL     OPV(12)       OPZ004
177900000000     C                   MOVEL     OPV(13)       OPZ005
178000000000     C                   MOVEL     OPV(14)       OPZ006
178100000000     C                   MOVEL     OPV(15)       OPZ007
178200000000     C                   MOVEL     OPV(16)       OPZ008
178300000000     C                   END
178400000000     C*
178500000000     C* TERZO GRUPPO DI 8 OPZIONI
178600000000     C     OPZGRU        IFEQ      3
178700000000     C                   MOVEL     OPV(17)       OPZ001
178800000000     C                   MOVEL     OPV(18)       OPZ002
178900000000     C                   MOVEL     OPV(19)       OPZ003
179000000000     C                   MOVEL     OPV(20)       OPZ004
179100000000     C                   MOVEL     OPV(21)       OPZ005
179200000000     C                   MOVEL     OPV(22)       OPZ006
179300000000     C                   MOVEL     OPV(23)       OPZ007
179400000000     C                   MOVEL     OPV(24)       OPZ008
179500000000     C                   END
179600000000     C                   ENDSR
179700000000     C*================================================================
179800000000     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
179900000000     C*================================================================
180000000000     C     OPZCON        BEGSR
180100000000     C*
180200000000     C* PULISCO CAMPI DI LAVORO
180300000000     C                   Z-ADD     0             I                 5 0
180400000000     C                   Z-ADD     0             W                 5 0
180500000000     C*
180600000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
180700000000     C     1             DO        24            I
180800000000     C                   MOVEL     OPD(I)        OPDRIG
180900000000     C*
181000000000     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
181100000000     C     OPZKEY        IFEQ      OPDOPZ
181200000000     C     OPZKEY        OREQ      OPDOP1
181300000000     C*
181400000000     C     OPDATT        IFEQ      'S'
181500000000     C                   MOVEL     OPDOPZ        OPZKEY
181600000000     C                   Z-ADD     I             W
181700000000     C                   END
181800000000     C                   END
181900000000     C*
182000000000     C                   END                                                    DO WHILE
182100000000     C*
182200000000     C* OPZIONE NON ATTIVA (ERRORE)
182300000000     C     W             IFEQ      0
182400000000     C*
182500000000     C* OPZIONE NON OK
182600000000     C                   Z-ADD     1             OPZOK
182700000000     C*
182800000000     C* EMISSIONE MESSAGGIO DI ERRORE
182900930127     C                   MOVEL     MSG(1)        DSMSMS
183000000000     C                   ELSE
183100000000     C*
183200000000     C* OPZIONE ATTIVA (PROCEDI ...)
183300000000     C*
183400930129     C     OPZKEY        IFEQ      ' 1'
183500000000     C                   EXSR      O01GES
183600000000     C                   END
183700000000     C     OPZKEY        IFEQ      ' 2'
183800000000     C                   EXSR      O02GES
183900000000     C                   END
184000000000     C     OPZKEY        IFEQ      ' 3'
184100000000     C                   EXSR      O03GES
184200000000     C                   END
184300000000     C     OPZKEY        IFEQ      ' 4'
184400020709     c                   If        §UteCun = *Blanks
184500020320     c                   eval      dsmsms = msg(11)
184600020320     c                   z-add     1             opzok
184700020320     c                   else
184800000000     C                   EXSR      O04GES
184900020320     c                   endif
185000000000     C                   END
185100000000     C     OPZKEY        IFEQ      ' 5'
185200000000     C                   EXSR      O05GES
185300000000     C                   END
185400000000     C     OPZKEY        IFEQ      ' 6'
185500000000     C                   EXSR      O06GES
185600000000     C                   END
185700930128     C     OPZKEY        IFEQ      ' 7'
185800930128     C                   EXSR      O07GES
185900930128     C                   END
186000930201     C     OPZKEY        IFEQ      ' 8'
186100930201     C                   EXSR      O08GES
186200930201     C                   END
186300000000     C*
186400000000     C                   END                                                    OPZ.NON ATTIVA
186500000000     C*
186600000000     C                   ENDSR
186700000000     C*================================================================
186800000000     C* NOTE PER COMPILAZIONE SCHIERE
186900000000     C*================================================================
187000000000     C* -MSG-
187100000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
187200000000     C* (NON TOCCARE MSG,1 E MSG,2
187300000000     C*
187400000000     C* -CMD-
187500000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
187600000000     C*
187700000000     C* -OPD-
187800000000     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
187900000000     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
188000000000     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
188100000000     C*    ESEMPIO: OPZIONI -1 -
188200000000     C*                     - 1-  SONO LA STESSA COSA
188300000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
188400000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
188500000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
188600000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
188700000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
188800000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
188900000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
189000000000     C*
189100000000     C* -FUD-
189200000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
189300000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
189400000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
189500000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
189600000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
189700000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
189800000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
189900000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
190000000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
190100000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
190200000000     C*
190300000000     C*================================================================
190400000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
190500020517TNSDUIR- Opzione non gestita.                                                  1
190600020517TNSDUIR- Tasto funzionale non gestito.                                         2
190700020517TNSDUIR- Non abilitato alla gestione di questa Unità.                          3
190800100906TNSDU1R- Unità annullata, non gestibile.    Consentito solo Ripristino.        4
190900100906TNSDU1R- Unità non annullata, impossibile ripristinare.                        5
191000020517TNSDUIR- Accettato solo un parametro di inizio lista.                          6
191100020517TNSDUIR- Tipo unità non accettabile.                                           7
191200020517TNSDUIR- Immettere o il codice cliente o la ragione sociale                    8
191300020517TNSDUIR- Cliente non accettabile                                               9
191400020517TNSDUIR- Matricola non accettabile.                                           10
191500020517TNSDUIR-                                                                      11
191600020517TNSDUIR- Matricola obbligatoria se richiesta ricerca per contenuto            12
191700020517TNSDUIR- Profilo non abilitato alla funzione                                  13
191800020517TNSDUIR- Profilo non abilitato al cliente richiesto                           14
191900020517TNSDUIR-                                                                      15
192000020517TNSDUIR- 16                                                                   16
192100020517TNSDUIR- 17                                                                   17
192200020517TNSDUIR- 18                                                                   18
192300020517TNSDUIR- 19                                                                   19
192400020517TNSDUIR- 20                                                                   20
192500000000** ======== SCHIERA: CMD  (COMANDI)                        ================
192600000000WRKSPLF
192700000000
192800000000
192900000000
193000000000
193100000000** ======== SCHIERA: OPD  OPZIONI GESTITE NEL PGM (ATTIVA=S) ==========
193200930312 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
193300930312 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
193400971015 3=Duplica         -3 -     (S/N)=N                        opz. 03¬
193500930312 4=Cancellazione   -4 -     (S/N)=N                        opz. 04¬
193600930312 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
193700000000                   -6 -     (S/N)=N                        opz. 06¬
193800930312 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
193900971013 8=Movimenti       -8 -     (S/N)=N                        opz. 08¬
194000930129                   -9 -     (S/N)=N                        opz. 09¬
194100000000                   -  -     (S/N)=N                        opz. 10¬
194200000000                   -  -     (S/N)=N                        opz. 11¬
194300000000                   -  -     (S/N)=N                        opz. 12¬
194400000000                   -  -     (S/N)=N                        opz. 13¬
194500000000                   -  -     (S/N)=N                        opz. 14¬
194600000000                   -  -     (S/N)=N                        opz. 15¬
194700000000                   -  -     (S/N)=N                        opz. 16¬
194800000000                   -  -     (S/N)=N                        opz. 17¬
194900000000                   -  -     (S/N)=N                        opz. 18¬
195000000000                   -  -     (S/N)=N                        opz. 19¬
195100000000                   -  -     (S/N)=N                        opz. 20¬
195200000000                   -  -     (S/N)=N                        opz. 21¬
195300000000                   -  -     (S/N)=N                        opz. 22¬
195400000000                   -  -     (S/N)=N                        opz. 23¬
195500000000                   -  -     (S/N)=N                        opz. 24¬
195600000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
195700930129                   -        (S/N)=N                        funz.01¬
195800930129                   -        (S/N)=N                        funz.02¬
195900930129F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
196000930129                   -        (S/N)=N                        funz.04¬
196100971013F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
196200930312F06=Inserimento    -        (S/N)=N                        funz.06¬
196300000000                   -        (S/N)=N                        funz.07¬
196400000000                   -        (S/N)=N                        funz.08¬
196500000000                   -        (S/N)=N                        funz.09¬
196600100922                   -        (S/N)=N                        funz.10¬
196700100922                   -        (S/N)=N                        funz.11¬
196800930129F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
196900000000                   -        (S/N)=N                        funz.13¬
197000100922F14=SchedaCliScDati-        (S/N)=N                        funz.14¬
197100930129                   -        (S/N)=N                        funz.15¬
197200930126                   -        (S/N)=N                        funz.16¬
197300000000F17=Inizio dati    -        (S/N)=S  <== S-FISSO           funz.17¬
197400000000F18=Fine dati      -        (S/N)=S  <== S-FISSO           funz.18¬
197500100922                   -        (S/N)=N                        funz.19¬
197600100922                   -        (S/N)=N                        funz.20¬
197700020517F21=Stampa         -        (S/N)=N                        funz.21¬
197800940105F22=Gestione stampe-        (S/N)=N                        funz.22¬
197900000000F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
198000000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
198100100922 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.RU¬
