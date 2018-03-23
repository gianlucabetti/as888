000100020319     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020319      *------------------------------------------------------------------------*
000300020319      * GESTIONE UNITA' EDP - SCELTA
000400020319      *------------------------------------------------------------------------*
000500020319
000600020319      *------------------------------------------------------------------------*
000700020319      * DATA BASE
000800020319      *------------------------------------------------------------------------*
000900941115     FTNSDU1D   CF   E             WORKSTN INFDS(DEVDS1)
001000941115     F                                     SFILE(SDU1D05:SFLULT)
001100941115     F                                     SFILE(SDU1DMS:MSGULT)
001200140128     FUNANA01L  uF   E           K DISK
001300140128     F                                     RENAME(UNANA000:UNANA1)
001400140128     FUNANA02L  IF   E           K DISK
001500140128     F                                     RENAME(UNANA000:UNANA2)
001600930707     FUNANA03L  IF   E           K DISK
001700930707     F                                     RENAME(UNANA000:UNANA3)
001800930707     FUNANA04L  IF   E           K DISK
001900020319     F                                     RENAME(UNANA000:UNANA4)
002000020319     FUNANA07L  IF   E           K DISK
002100020319     F                                     RENAME(UNANA000:UNANA7)
002200100906     FUNANA08L  IF   E           K DISK
002300100906     F                                     RENAME(UNANA000:UNANA8)
002400020529     FAZORG21l  IF   E           K DISK
002500020529     fazorg01l  if   e           k disk
002600090904
002700090904       //--------------------------------------------------------------
002800090904       // COSTANTI
002900090904       //--------------------------------------------------------------
003000090904
003100090904     d c_Logistica     c                   const('LOG')
003200020319
003300020319      *------------------------------------------------------------------------*
003400020319      * SCHIERE
003500020319      *------------------------------------------------------------------------*
003600020319      * MESSAGGI
003700930127     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
003800020319      * COMANDI
003900000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
004000020319      * DESCRIZIONI OPZIONI
004100000000     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
004200020319      * DESCRIZIONI TASTI FUNZIONALI
004300100922     D FUD             S             80    DIM(25) CTDATA PERRCD(1)
004400020319      * TABELLA OPZIONI DA CARICARE A VIDEO
004500000000     D OPV             S             19    DIM(24)
004600020319      * TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
004700000000     D FUV             S             19    DIM(24)
004800020523
004900020523     d  skpoabi        s              3  0 dim(500)
005000020319
005100020319      *------------------------------------------------------------------------*
005200020319      * INPUT
005300020319      *------------------------------------------------------------------------*
005400020319
005500120411     d depmat          s                   like(vdpmat)
005600020325     d depric          s                   like(vdpric)
005700020319     d depmag          s                   like(vdpmag)
005800020319     d deptip          s                   like(vdptip)
005900020319     d depti2          s                   like(vdpti2)
006000020319     d dedsmd          s                   like(vddsmd)
006100020319     d dedsma          s                   like(vddsma)
006200100906     d depmod          s                   like(vdpmod)
006300100906     d depmat2         s                   like(vdpmat2)
006400100906     d dspmag          s                   like(vdsmag)
006500120411     d dspord          s                   like(vdsord)
006600100906     d dsprief         s                   like(vdsrief)
006700100907     d depdes          s                   like(vdpdes)
006800101029     d depnot          s                   like(vdpnot)
006900120412     d alford          s              6
007000120411     d wtipri          s              3
007100020325     d wfil            s              3
007200020325     d wksc            s              3  0
007300020319     d wnosfl          s              1    inz('0')
007400020410     d wprimo          s              1    inz('0')
007500020319     d w_vddsmd        s                   like(vddsmd)
007600020319     d w_vddsma        s                   like(vddsma)
007700101029     d w_vdsrief       s                   like(vdsrief)
007800020325     d xx              s              3  0
007900020517     d ii              s              3  0
008000020516     d wabi            s                   like(uteaut)
008100020523     d wmag            s              3  0
008200100906     d ctrmag          s                   like(vdpmag)
008300101027     d Dataiso         s               d   datfmt(*iso)
008400101027     d Datadmy         s               d   datfmt(*dmy)
008500140128     d Datasys         s               d   datfmt(*iso) inz(*sys)
008600020319
008700020319      * RIDEFINIZIONE SCHIERA -OPD-
008800000000     D OPDDS           DS
008900000000     D  OPDOPZ                 1      2
009000000000     D  OPDDE1                 3     19
009100000000     D  OPDOP1                21     22
009200000000     D  OPDFI0                23     34
009300000000     D  OPDATT                35     35
009400000000     D  OPDFI1                36     80
009500000000     D  OPDDES                 1     19
009600000000     D  OPDRIG                 1     80
009700020319      * RIDEFINIZIONE SCHIERA -FUD-
009800000000     D FUDDS           DS
009900000000     D  FUDDE1                 1      1
010000000000     D  FUDFUN                 2      3
010100000000     D  FUDDE2                 4     19
010200000000     D  FUDFI0                20     34
010300000000     D  FUDATT                35     35
010400000000     D  FUDFI1                36     80
010500000000     D  FUDDES                 1     19
010600000000     D  FUDRIG                 1     80
010700020319      * PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
010800930127     D DSMSG         E DS
010900020319      *------------------
011000020319      * DS "XSRDA8" - CONTROLLA DATA (8)
011100020319      *------------------
011200971010     D WLBDA8          DS
011300971010     D  G08DAT                 1      8  0
011400971010     D  G08INV                 9     16  0
011500971010     D  G08ERR                17     17
011600971010     D  G08TGI                18     22  0
011700020319      * PARAMETRI PER LANCIO ALTRI PGM DELLA PROCEDURA
011800930707     D DSUNIT        E DS
011900020319      * DS PER GESTIONE TASTI FUNZIONALI
012000930127     D DSKEY         E DS
012100000000     D DEVDS1          DS
012200000000     D  KEY                  369    369
012300020319      * ARCHITETTURA
012400930312     D KPJBA         E DS
012500070509      * Altri parametri ricevuti (da restituire)
012600070509     d UNANAds       e ds                  extname(UNANA00F)
012700070509     d                                     prefix(UIM:3)
012800020318
012900020318     d tibs02ds      e ds
013000020516
013100020516     d ddatiute      e ds
013200020516     d azuteds       e ds                  extname(azute00f)
013300020516     d tibs34ds      e ds                  inz
013400020516     d dute01        e ds
013500020516     d dlat          e ds
013600020516     d trul31ds      e ds
013700020523     d  skpo                  10    759  0 dim(250)
013800090904      *
013900090904     d Og143         e ds                  inz
014000070509
014100070509     d Status         sds
014200070509     d   SDSprm          *parms
014300020325
014400971010     C*------------------------------------------------------------------------*
014500971010     C* MAIN LINE
014600971010     C*------------------------------------------------------------------------*
014700000000     C* MAIN DEL PGM
014800000000     C                   EXSR      RUTINZ
014900000000     C*
015000930126     C* SE CONTROLLI PRELIMINARI NON OK VADO A FINE PGM
015100930126     C     ERRL00        IFNE      '1'                                          --- 1 -->
015200000000     C*
015300000000     C* CARICA IL SFL DAI PARAMETRI IN INPUT AL PROGRAMMA
015400000000     C                   EXSR      POSSFL
015500000000     C*
015600000000     C* CICLO FINO A CHE:
015700000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
015800000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
015900930707     C*  -FINE PGM PER ENTER SENZA SCELTA
016000000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
016100000000     C     F12L00        OREQ      '1'
016200020404     C**!!!FINL00        OREQ      '1'
016300000000     C*
016400000000     C* AZZERA FLAG DI ERROE
016500000000     C                   MOVEL     '0'           ERRL00
016600020410
016700020410      * posiziono il cursore sulla matricola se prima volta
016800020410     c                   if        wprimo = *off
016900020410     c                   eval      *in88 = *on
017000020410     c                   eval      wprimo = *on
017100020410     c                   else
017200020410     c                   eval      *in88 = *off
017300020410     c                   endif
017400020516
017500020516      * profilo non abilitato
017600020516     c                   if        *in76
017700020516     c                   eval      f03l00 = *on
017800020516     c                   endif
017900000000     C*
018000000000     C* EMETTE SFL A VIDEO
018100000000     C                   EXSR      OUTSFL
018200000000     C*
018300000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI -ROUTINE FUNCON
018400000000     C                   EXSR      FUNCON
018500000000     C*
018600000000     C                   END                                                    <-- 2 ---
018700000000     C*
018800000000     C                   END                                                    <-- 1 --
018900000000     C*
019000940105     C                   MOVEL     DSUNIT        KPJBU
019100000000     C                   SETON                                        LR
019200000000     C*================================================================
019300930126     C* F03GES -GEST.TASTO FUNZ. F03 ===> FINE
019400000000     C*================================================================
019500000000     C     F03GES        BEGSR
019600000000     C*
019700000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PGM 00)
019800000000     C                   MOVEL     '1'           F03L00
019900000000     C*
020000000000     C                   ENDSR
020100000000     C*================================================================
020200930129     C* F05GES -GEST.TASTO FUNZ. F05 ===> RIVISUALIZZAZIONE
020300000000     C*================================================================
020400000000     C     F05GES        BEGSR
020500000000     C*
020600000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
020700000000     C                   EXSR      POSSFL
020800000000     C*
020900000000     C                   ENDSR
021000000000     C*================================================================
021100931116     C* F06GES -GEST.TASTO FUNZ. F06 ===> INSERIMENTO
021200000000     C*================================================================
021300930126     C     F06GES        BEGSR
021400930201     C*
021500930201     C* INSERIMENTO CONSENTITO SOLO IN SEDE
021600020709     c                   If        DutLpo <> 'S'
021700930201     C                   MOVEL     '1'           ERRL00
021800930201     C                   MOVEL     MSG(3)        DSMSMS
021900930201     C                   END
022000930201     C*
022100930201     C     ERRL00        IFEQ      '0'                                          --- 1 -->
022200930127     C*
022300941115     C* LANCIO IL PGM PER L'INSERIMENTO DEL RECORD (TNSDU2R)
022400930201     C                   MOVEL     'F06'         WOPZ1             3
022500941115     C                   MOVEL     'TNSDU2R'     WPGM              8
022600930201     C                   EXSR      LANPGM
022700930128     C*
022800930128     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
022900930128     C                   EXSR      POSSFL
023000930201     C*
023100930201     C                   END                                                    <-- 1 ---
023200930127     C*
023300000000     C                   ENDSR
023400000000     C*================================================================
023500930129     C* F12GES -GEST.TASTO FUNZ. F12 ===> RITORNO
023600000000     C*================================================================
023700000000     C     F12GES        BEGSR
023800000000     C*
023900000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12 (A LIVELLO PGM 00)
024000000000     C                   MOVEL     '1'           F12L00
024100000000     C*
024200000000     C                   ENDSR
024300000000     C*================================================================
024400930126     C* F17GES -GEST.TASTO FUNZ. F17 ===> INIZIO DATI
024500000000     C*================================================================
024600000000     C     F17GES        BEGSR
024700000000     C*
024800000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
024900000000     C                   Z-ADD     1             SFLNBR
025000000000     C*
025100000000     C                   ENDSR
025200000000     C*================================================================
025300930126     C* F18GES -GEST.TASTO FUNZ. F18 ===> FINE DATI
025400000000     C*================================================================
025500000000     C     F18GES        BEGSR
025600000000     C*
025700000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
025800000000     C                   Z-ADD     SFLULT        SFLNBR
025900000000     C*
026000000000     C                   ENDSR
026100930128     C*================================================================
026200930128     C* F21GES -GEST.TASTO FUNZ. F21 ===> STAMPA ELENCO
026300930128     C*================================================================
026400930128     C     F21GES        BEGSR
026500930128     C*
026600930707     C* LANCIA PROGRAMMA DI STAMPA DELLE UNITA
026700940105     C                   MOVEL     DSUNIT        KPJBU
026800941115     C                   CALL      'TNSDU6R'
026900930209     C                   PARM                    KPJBA
027000940105     C                   MOVEL     KPJBU         DSUNIT
027100940105     C*
027200940105     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
027300940105     C     DSUF03        IFEQ      '1'
027400940105     C                   MOVEL     '1'           F03L00
027500940105     C                   MOVEL     '1'           ERRL00
027600940105     C                   ENDIF
027700930128     C*
027800930128     C                   ENDSR
027900000000     C*================================================================
028000930126     C* F22GES -GEST.TASTO FUNZ. F22 ===> STAMPE
028100000000     C*================================================================
028200000000     C     F22GES        BEGSR
028300000000     C*
028400000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
028500000000     C                   Z-ADD     80            LENGH            15 5
028600000000     C                   CALL      'QCAEXEC'
028700000000     C                   PARM                    CMD(1)
028800000000     C                   PARM                    LENGH
028900000000     C*
029000000000     C                   ENDSR
029100000000     C*================================================================
029200930126     C* F23GES -GEST.TASTO FUNZ. F23 ===> ALTRE OPZIONI
029300000000     C*================================================================
029400000000     C     F23GES        BEGSR
029500000000     C*
029600000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
029700000000     C                   EXSR      OPZGES
029800000000     C*
029900000000     C                   ENDSR
030000000000     C*================================================================
030100930126     C* F24GES -GEST.TASTO FUNZ. F24 ===> ALTRI TASTI
030200000000     C*================================================================
030300000000     C     F24GES        BEGSR
030400000000     C*
030500000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
030600000000     C                   EXSR      FUNGES
030700000000     C*
030800000000     C                   ENDSR
030900000000     C*================================================================
031000930126     C* RUPGES -GEST.TASTO FUNZ. ROLLUP ===> PAGINA SUCCESSIVA
031100000000     C*================================================================
031200000000     C     RUPGES        BEGSR
031300000000     C*
031400000000     C* CARICA IL SFL DA FILE "FISICO"
031500000000     C                   EXSR      CARSFL
031600000000     C*
031700000000     C                   ENDSR
031800000000     C*================================================================
031900930126     C* HELGES - GEST.TASTO FUNZ. HELP ===> AIUTO
032000000000     C*================================================================
032100000000     C     HELGES        BEGSR
032200000000     C*
032300000000     C                   ENDSR
032400000000     C*================================================================
032500930126     C* ENTGES - GEST.TASTO FUNZ. ENTER ===> INVIO
032600000000     C*================================================================
032700000000     C     ENTGES        BEGSR
032800000000     C*
032900000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
033000000000     C                   EXSR      NEWPOS
033100000000     C*
033200000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
033300000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
033400000000     C*
033500000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
033600000000     C     SWPOSI        IFNE      0                                            --- 2 -->
033700000000     C                   EXSR      POSSFL
033800000000     C*
033900000000     C                   ELSE                                                   <-- 2 -->
034000000000     C*
034100000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
034200000000     C     SFLULT        IFNE      0                                            --- 3 -->
034300000000     C                   EXSR      ENTATT
034400000000     C*
034500000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
034600000000     C     SWMODI        IFNE      0                                            --- 4 -->
034700000000     C                   EXSR      POSSFL
034800000000     C                   END                                                    <-- 4 ---
034900000000     C*
035000000000     C                   END                                                    <-- 3 ---
035100000000     C                   END                                                    <-- 2 ---
035200000000     C                   END                                                    <-- 1 ---
035300000000     C*
035400000000     C                   ENDSR
035500000000     C*================================================================
035600930126     C*  O01GES - GESTIONE OPZIONE 1=SCELTA
035700000000     C*================================================================
035800000000     C     O01GES        BEGSR
035900070509      *
036000070509if  1c                   IF        SDSprm <= 1
036100070509     c                   eval      DSUtip  = VIDtip
036200070509     c                   eval      DSUcod  = VIDcod
036300070509     c                   eval      DSUmag  = VIDmag
036400070509     c                   clear                   DSUprg
036500070509     c                   clear                   DSUdtm
036600070509     c                   clear                   DSUtmo
036700070509x   1c                   ELSE
036800070509     c                   clear                   UNANAds
036900070509     c                   eval      UIMtip  = VIDtip
037000070509     c                   eval      UIMcod  = VIDcod
037100070509     c                   eval      UIMmar  = VIDmar
037200070509     c                   eval      UIMmod  = VIDmod
037300070509     c                   eval      UIMmat  = VIDmat
037400070509     c                   eval      UIMmag  = VIDmag
037500070509     c***                eval      UIMdsm  = VIDdsm
037600070509     c                   clear                   WLBda8
037700070509     c                   eval      G08dat  = UIMdsm
037800070509     c                   call      'XSRDA8'
037900070509     c                   parm                    WLBda8
038000070509     c                   eval      UIMdsm  = G08inv
038100070509e   1c                   ENDIF
038200070509      *
038300070509     c                   eval      F12l00  = *on
038400070509     c                   eval      ERRl00  = *on
038500070509      *
038600000000     C                   ENDSR
038700000000     C*================================================================
038800000000     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
038900000000     C*================================================================
039000000000     C     O02GES        BEGSR
039100930128     C*
039200930128     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA MODIFICA
039300930201     C     VIDATB        IFEQ      'A'
039400930128     C                   MOVEL     '1'           ERRL00
039500930201     C                   MOVEL     MSG(4)        DSMSMS
039600930201     C                   END
039700930201     C*
039800930201     C     ERRL00        IFEQ      '0'                                          --- 1 -->
039900000000     C*
040000930707     C* LANCIO IL PGM PER LA MODIFICA DEL DETTAGLIO DELL'ANAGRAFICA
040100930201     C                   MOVEL     'O02'         WOPZ1
040200941115     C                   MOVEL     'TNSDU2R'     WPGM
040300930201     C                   EXSR      LANPGM
040400000000     C*
040500930127     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
040600930707     C                   Z-ADD     1             SWMODI
040700930128     C*
040800930201     C                   END                                                    <-- 1 ---
040900000000     C*
041000000000     C                   ENDSR
041100000000     C*================================================================
041200971015     C* O03GES - GESTIONE OPZIONE 3=DUPLICA
041300000000     C*================================================================
041400000000     C     O03GES        BEGSR
041500971015     C*
041600971015     C* DUPLICA CONSENTITA SOLO IN SEDE
041700020709     c                   If        DutLpo <> 'S'
041800971015     C                   MOVEL     '1'           ERRL00
041900971015     C                   MOVEL     MSG(3)        DSMSMS
042000971015E   1C                   ENDIF
042100971015     C*
042200971015     C* LANCIA IL PGM PER L'INSERIMENTO
042300971015IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
042400971015     C                   MOVEL     'O03'         WOPZ1
042500971015     C                   MOVEL     'TNSDU2R'     WPGM
042600971015     C                   EXSR      LANPGM
042700971015     C*
042800971015     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
042900971015     C                   Z-ADD     1             SWMODI
043000971015E   1C                   ENDIF
043100971015     C*
043200000000     C                   ENDSR
043300000000     C*================================================================
043400000000     C*  O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE
043500000000     C*================================================================
043600000000     C     O04GES        BEGSR
043700930128     C*
043800930128     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
043900930201     C     VIDATB        IFEQ      'A'
044000930128     C                   MOVEL     '1'           ERRL00
044100930201     C                   MOVEL     MSG(4)        DSMSMS
044200930201     C                   END
044300930128     C*
044400930201     C     ERRL00        IFEQ      '0'                                          --- 1 -->
044500000000     C*
044600930707     C* LANCIO IL PGM DI GESTIONE DELLE UNITA
044700930201     C                   MOVEL     'O04'         WOPZ1
044800941115     C                   MOVEL     'TNSDU2R'     WPGM
044900930201     C                   EXSR      LANPGM
045000000000     C*
045100930127     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
045200930707     C                   Z-ADD     1             SWMODI
045300930128     C*
045400930201     C                   END                                                    <-- 1 ---
045500000000     C*
045600000000     C                   ENDSR
045700000000     C*================================================================
045800000000     C*  O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
045900000000     C*================================================================
046000000000     C     O05GES        BEGSR
046100000000     C*
046200000000     C* LANCIO ROUTINE PER LA VISUALIZZAZIONE DEL DETTAGLIO DELLA RIGA
046300930201     C                   MOVEL     'O05'         WOPZ1
046400941115     C                   MOVEL     'TNSDU2R'     WPGM
046500930201     C                   EXSR      LANPGM
046600000000     C*
046700000000     C                   ENDSR
046800160923     C*================================================================
046900160923     C*  O0SGES - GESTIONE OPZIONE S=Storicizzazione
047000160923     C*================================================================
047100160923     C     O0SGES        BEGSR
047200160923     C*
047300160923     C* LANCIO ROUTINE PER LA VISUALIZZAZIONE DEL DETTAGLIO DELLA RIGA
047400160923     C                   MOVEL     'O0S'         WOPZ1
047500160923     C                   MOVEL     'TNSDU2R'     WPGM
047600160923     C                   EXSR      LANPGM
047700160926     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
047800160926     C                   Z-ADD     1             SWMODI
047900160923     C*
048000160923     C                   ENDSR
048100000000     C*================================================================
048200000000     C*  O06GES - GESTIONE OPZIONE 6=STAMPA
048300000000     C*================================================================
048400000000     C     O06GES        BEGSR
048500000000     C                   ENDSR
048600930128     C*================================================================
048700930128     C*  O07GES - GESTIONE OPZIONE 7=RIPRISTINO
048800930128     C*================================================================
048900930128     C     O07GES        BEGSR
049000930128     C*
049100930128     C* SE IL RECORD NON E' ANNULLATO NON CONSENTO IL RIPRISTINO
049200930201     C     VIDATB        IFNE      'A'
049300930128     C                   MOVEL     '1'           ERRL00
049400930201     C                   MOVEL     MSG(5)        DSMSMS
049500930201     C                   END
049600930201     C*
049700930201     C     ERRL00        IFEQ      '0'                                          --- 1 -->
049800930128     C*
049900930201     C* LANCIO IL PGM PER IL RIPRISTINO DI UN ANNULLAMENTO
050000930201     C                   MOVEL     'O07'         WOPZ1
050100941115     C                   MOVEL     'TNSDU2R'     WPGM
050200930201     C                   EXSR      LANPGM
050300930128     C*
050400930128     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
050500930707     C                   Z-ADD     1             SWMODI
050600930128     C*
050700930201     C                   END                                                    <-- 1 ---
050800930128     C*
050900930128     C                   ENDSR
051000930201     C*================================================================
051100940105     C*  O08GES - GESTIONE OPZIONE 8=GESTIONE MOVIMENTI
051200930201     C*================================================================
051300930201     C     O08GES        BEGSR
051400930201     C*
051500930201     C* LANCIO IL PGM PER LA GESTIONE DELLE LOCALITA'
051600971013     C     ERRL00        IFEQ      '0'                                          --- 1 -->
051700930201     C                   MOVEL     'O08'         WOPZ1
051800941115     C                   MOVEL     'TNSDU4R'     WPGM
051900930201     C                   EXSR      LANPGM
052000930201     C                   END                                                    <-- 1 ---
052100930201     C*
052200930201     C                   ENDSR
052300140128     C*================================================================
052400140128     C*  O0RGES - GESTIONE OPZIONE R=si/no rietnro da filiale
052500140128     C*================================================================
052600140128     C     O0RGES        BEGSR
052700140128     C*
052800140128     C* Aggiorno l'anagrafica con reos si/no
052900140128     C     KEYUNA        CHAIN     UNANA01L
053000140128     c                   if        %found(unana01l)
053100140128     c* se c'e' reso lo tolgo e viceversa
053200140128     c                   if        %subst(unalsw:15:1)= 'S'
053300140128     c                   eval      %subst(unalsw:15:1)= ' '
053400140128     c                   clear                   unadtr
053500140128     c                   else
053600140128     c                   eval      %subst(unalsw:15:1)= 'S'
053700140128     c     *iso          movel     datasys       UNADTR
053800140128     c                   endif
053900140128     c                   update    unana1
054000140128     c                   endif
054100140128     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
054200140128     C                   Z-ADD     1             SWMODI
054300140128     C*
054400140128     C                   ENDSR
054500000000     C*================================================================
054600000000     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
054700000000     C*================================================================
054800000000     C     NEWPOS        BEGSR
054900000000     C*
055000000000     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAM.
055100000000     C*         1= RICHIESTO POSIZIONAM.
055200000000     C                   Z-ADD     0             SWPOSI            7 0
055300000000     C*
055400000000     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
055500000000     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
055600000000     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
055700000000     C                   Z-ADD     0             SWMODI            1 0
055800000000     C*
055900000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
056000020319     c                   if        vdpmat <> depmat or vdptip <> deptip or      --- 1 -->
056100020319     c                             vdpmag <> depmag or vdpti2 <> depti2 or
056200020319     c                             vddsmd <> dedsmd or vddsma <> dedsma or
056300100906     c                             vdpric <> depric or depmod <> vdpmod or
056400100906     c                             vdpmat2<> depmat2 or vdsmag<> dspmag or
056500101029     c                             vdpdes<>  depdes  or vdsrief<>dsprief or
056600120411     c                             vdpnot<>  depnot  or vdsord<> dspord
056700930202     C*
056800930202     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI DI PARTENZA
056900930202     C                   EXSR      CHKPAR
057000930202     C*
057100930202     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
057200930202     C     ERRL00        IFEQ      '0'                                          --- 2 -->
057300000000     C                   Z-ADD     1             SWPOSI
057400120411     c                   eval      dspord = vdsord
057500120411     c                   eval      depmat = vdpmat
057600020325     c                   eval      depric = vdpric
057700020319     c                   eval      deptip = vdptip
057800020319     c                   eval      depmag = vdpmag
057900020319     c                   eval      depti2 = vdpti2
058000020319     c                   eval      dedsmd = vddsmd
058100020319     c                   eval      dedsma = vddsma
058200100907     c                   eval      depmod = vdpmod
058300100907     c                   eval      depmat2= vdpmat2
058400100907     c                   eval      dspmag = vdsmag
058500100907     c                   eval      dsprief= vdsrief
058600100907     c                   eval      depdes= vdpdes
058700101029     c                   eval      depnot= vdpnot
058800930202     C                   END                                                    <-- 2 ---
058900930202     C*
059000000000     C                   END                                                    <-- 1 ---
059100000000     C*
059200000000     C                   ENDSR
059300000000     C*================================================================
059400000000     C* LETTURA RECORD FILE DA CARICARE
059500000000     C*================================================================
059600000000     C     LETFIL        BEGSR
059700020319
059800020319     c                   select
059900020319
060000020319     c                   when      wtipri = 'TIP'
060100020319     c                   read      unana02l                               99
060200020319
060300020319     c                   when      wtipri = 'MAT'
060400020319     c                   read      unana07l                               99
060500020319
060600020319     c                   when      wtipri = 'MAG'
060700020319     c                   read      unana03l                               99
060800020319
060900020319     c                   when      wtipri = 'DSM'
061000020319     c                   read      unana04l                               99
061100020319
061200100906     c                   when      wtipri = 'MOD'
061300100906     c                   read      unana08l                               99
061400020319
061500020319     c                   endsl
061600020319
061700000000     C*
061800930201     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
061900930707     C     *IN99         IFEQ      '1'
062000000000     C                   Z-ADD     1             SFLEOF
062100930707     C                   END
062200000000     C*
062300000000     C                   ENDSR
062400000000     C*================================================================
062500000000     C* SETFIL - SETLL DEL FILE DA CARICARE
062600000000     C*================================================================
062700000000     C     SETFIL        BEGSR
062800020325
062900020319
063000020319      * POSIZIONAMENTO DEL FILE DA CARICARE E PRIMA LETTURA
063100020319
063200020319     c                   select
063300020319
063400020319     c                   when      wtipri = 'TIP'
063500020319     c                   move      vdptip        unatip
063600020319     c                   move      vdpmat        unamat
063700020319     c     kuna02        setll     unana02l
063800020319     c                   if        not %found(unana02l)
063900020319     c                   eval      sfleof = 1
064000020319     c                   else
064100020319     c                   read      unana02l
064200020319     c                   if        %eof(unana02l)
064300020319     c                   eval      sfleof = 1
064400020319     c                   endif
064500020319     c                   endif
064600020319
064700020319     c                   when      wtipri = 'MAT'
064800020325      * no ricerca per contenuto
064900020325     c                   if        vdpric = *blanks
065000020319     c                   move      vdpmat        unamat
065100020319     c                   move      vdptip        unatip
065200020319     c     kuna07        setll     unana07l
065300020319     c                   if        not %found(unana07l)
065400020319     c                   eval      sfleof = 1
065500020319     c                   else
065600020319     c                   read      unana07l
065700020319     c                   if        %eof(unana07l)
065800020319     c                   eval      sfleof = 1
065900020319     c                   endif
066000020319     c                   endif
066100020325      * si ricerca per contenuto
066200020325     c                   else
066300020325     c     *loval        setll     unana07l
066400020325     c                   if        not %found(unana07l)
066500020325     c                   eval      sfleof = 1
066600020325     c                   else
066700020325     c                   read      unana07l
066800020325     c                   if        %eof(unana07l)
066900020325     c                   eval      sfleof = 1
067000020325     c                   endif
067100020325     c                   endif
067200020325     c                   endif
067300020319
067400020319     c                   when      wtipri = 'MAG'
067500020319     c                   move      vdpmag        unamag
067600020319     c                   move      vdpti2        unatip
067700020319     c     kuna03        setll     unana03l
067800020319     c                   if        not %found(unana03l)
067900020319     c                   eval      sfleof = 1
068000020319     c                   else
068100020319     c                   read      unana03l
068200020319     c                   if        %eof(unana03l)
068300020319     c                   eval      sfleof = 1
068400020319     c                   endif
068500020319     c                   endif
068600020319
068700020319     c                   when      wtipri = 'DSM'
068800020319     c                   z-add     w_vddsmd      unadsm
068900020319     c     kuna04        setll     unana04l
069000020319     c                   if        not %found(unana04l)
069100020319     c                   eval      sfleof = 1
069200020319     c                   else
069300020319     c                   read      unana04l
069400020319     c                   if        %eof(unana04l)
069500020319     c                   eval      sfleof = 1
069600020319     c                   endif
069700020319     c                   endif
069800020319
069900100906     c                   when      wtipri = 'MOD'
070000100906     c                   movel     vdpmod        unamod
070100100906     c                   movel     vdpmat        unamat
070200100906     c     kuna08        setll     unana08l
070300100906     c                   if        not %found(unana08l)
070400100906     c                   eval      sfleof = 1
070500100906     c                   else
070600100906     c                   read      unana08l
070700100906     c                   if        %eof(unana08l)
070800100906     c                   eval      sfleof = 1
070900100906     c                   endif
071000100906     c                   endif
071100100906     c
071200100906     c**                 when      wtipri = 'DSG'
071300100906     c**                 z-add     w_vddsgd      unadsg
071400100906     c**   kuna06        setll     unana06l
071500100906     c**                 if        not %found(unana06l)
071600100906     c**                 eval      sfleof = 1
071700100906     c**                 else
071800100906     c**                 read      unana06l
071900100906     c**                 if        %eof(unana06l)
072000100906     c**                 eval      sfleof = 1
072100100906     c**                 endif
072200100906     c**                 endif
072300020319
072400020319     c                   endsl
072500020319
072600000000     C                   ENDSR
072700020319      *================================================================
072800020319      * CTRSFL - CONTROLLO SE RECORD DA SCRIVERE NEL SUBFILE
072900020319      *================================================================
073000020319     C     CTRSFL        BEGSR
073100020319
073200020325      * record letto ok
073300020325     c                   if        wnosfl = *off
073400020516
073500020516     c                   if        unamag <> 951
073600020523     c     unamag        lookup    skpoabi                                99
073700020516     c                   if        not *in99
073800020516     c                   eval      wnosfl = *on
073900020516     c                   endif
074000020516     c                   else
074100020516     c                   movel     unaksc        wmag
074200020516     c     wmag          lookup    skpoabi                                99
074300020516     c                   if        not *in99
074400020516     c                   eval      wnosfl = *on
074500020516     c                   endif
074600020516     c                   endif
074700020516     c                   endif
074800020325
074900020325     c                   if        wtipri = 'MAT' and vdpric = 'S'
075000020516     c                             and wnosfl = *off
075100020325     c     ' '           checkr    vdpmat        xx
075200020325     c     vdpmat:xx     scan      unamat                                 99
075300020325     c                   if        not *in99
075400020325     c                   eval      wnosfl = *on
075500020325     c                   endif
075600020325     c                   endif
075700020325
075800020516     c                   if        wnosfl = *off
075900020319     c                   if        (wtipri = 'DSM' and w_vddsma > *zeros
076000100906     c                                             and unadsm > w_vddsma)
076100020319     c                   eval      wnosfl = *on
076200020319     c                   endif
076300020516     c                   endif
076400100907     c
076500101029     c* Ricerca per contenuto descrizione
076600100907     c                   if        vdpdes<>*blanks
076700100907     c     ' '           checkr    vdpdes        xx
076800100907     c     vdpdes:xx     scan      unades                                 99
076900100907     c                   if        not *in99
077000100907     c                   eval      wnosfl = *on
077100100907     c                   endif
077200100907     c                   endif
077300101029     c* Ricerca per contenuto note
077400101029     c                   if        vdpnot<>*blanks
077500101029     c     ' '           checkr    vdpnot        xx
077600101029     c     vdpnot:xx     scan      unanot                                 99
077700101029     c                   if        not *in99
077800101029     c                   eval      wnosfl = *on
077900101029     c                   endif
078000101029     c                   endif
078100100907     c* Parzializza magazzino
078200100907     c                   if        vdsmag>0 and unamag<>vdsmag
078300100907     c                   eval      wnosfl = *on
078400100907     c                   endif
078500100907     c* Parzializza unità da rendere
078600101029     c***                if        vdsrief='SI' and %subst(unalsw:15:1)<>'S'
078700101029     c                   if        vdsrief>0    and %subst(unalsw:15:1)<>'S'
078800100907     c                   eval      wnosfl = *on
078900100907     c                   endif
079000101029     c                   if        vdsrief>0    and unadtr>w_vdsrief
079100101029     c                   eval      wnosfl = *on
079200101029     c                   endif
079300100907     c
079400120411     c* Parzializzo per numero ordine
079500120411     c                   if        vdsord>0
079600120411     c                   movel     vdsord        alford
079700120412     c                   if        alford<>%subst(unaemu:1:6)
079800120411     c                   eval      wnosfl = *on
079900120411     c                   endif
080000120411     c                   endif
080100120411     c
080200020319     C                   ENDSR
080300000000     C*================================================================
080400000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
080500000000     C*================================================================
080600000000     C     IRESFL        BEGSR
080700000000     C*
080800000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
080900930127     C*
081000930707     C                   MOVEL     UNAATB        VIDATB
081100930707     C                   Z-ADD     UNATIP        VIDTIP
081200930707     C                   Z-ADD     UNACOD        VIDCOD
081300930707     C                   MOVEL     UNAMAT        VIDMAT
081400930707     C                   MOVEL     UNAMAR        VIDMAR
081500120411     c                   clear                   vidord
081600120412     c                   if        %Subst(unaemu:1:6)>*zeros
081700120412     c                   eval      vidord=%int(%Subst(unaemu:1:6))
081800120411     c                   endif
081900120411     c
082000930707     C                   MOVEL     UNAMOD        VIDMOD
082100100906     C                   MOVEL     UNAdes        VIDdes
082200100906     c                   if        %Subst(unalsw:15:1)='S'
082300101027     c                   eval      vidrief='SI'
082400100906     c                   setoff                                       15
082500101027     c* Imposto la data da quando
082600101029     c                   clear                   viddalr
082700101027     c                   if        unadtr>0
082800101027     C     *iso          movel     UNADTR        dataiso
082900101027     c                   movel     dataiso       datadmy
083000101027     c                   movel     datadmy       viddalr
083100101027     c                   endif
083200101027     c
083300100906     c                   else
083400100906     c                   clear                   vidrief
083500101027     c                   clear                   viddalr
083600100906     c                   seton                                        15
083700100906     c                   endif
083800930707     C                   Z-ADD     UNAMAG        VIDMAG
083900100907     c**                 if        not *in94
084000971010     C                   Z-ADD     UNADSM        G08INV
084100971010     C                   Z-ADD     *ZEROS        G08DAT
084200971010     C                   MOVEL     '3'           G08ERR
084300971010     C                   CALL      'XSRDA8'
084400971010     C                   PARM                    WLBDA8
084500020319     C                   Z-ADD     G08DAT        VIDDat
084600160923
084700160923     c* se la data è minore di oggi, la metto in R.I.
084800160923     c                   if        unadsm<dateu  and unadsm>0
084900160923     c                   seton                                        10
085000160923     c                   else
085100160923     c                   setoff                                       10
085200160923     c                   endif
085300160923     c
085400100907     c**                 endif
085500100907     c**                 if        *in94
085600100907     C**                 Z-ADD     UNADSG        G08INV
085700100907     C**                 Z-ADD     *ZEROS        G08DAT
085800100907     C**                 MOVEL     '3'           G08ERR
085900100907     C**                 CALL      'XSRDA8'
086000100907     C**                 PARM                    WLBDA8
086100100907     C**                 Z-ADD     G08DAT        VIDDat
086200100907     c**                 endif
086300000000     C*
086400000000     C                   ENDSR
086500930202     C*================================================================
086600930202     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
086700930202     C*================================================================
086800930202     C     CHKPAR        BEGSR
086900020319
087000020319      * IMPOSTO FLAG EVIDENZAZIONE TIPO DI RICERCA
087100020319     c                   eval       *in90 = *off                                x tipo
087200020319     c                   eval       *in91 = *off                                x matricola
087300020319     c                   eval       *in92 = *off                                x magazzino
087400020319     c                   eval       *in93 = *off                                x scad.manutenzione
087500100906     c                   eval       *in94 = *off                                x modello
087600120411     c                   eval       *in95 = *off                                x num ordine
087700020319
087800020319      * IMPOSTO FLAG EVIDENZAZIONE ERRORI SUI PARAMETRI
087900020319     c                   eval       *in80 = *off                                tipo
088000020319     c                   eval       *in81 = *off                                tipo
088100020319     c                   eval       *in82 = *off                                matricola
088200020319     c                   eval       *in83 = *off                                magazzino
088300020319     c                   eval       *in84 = *off                                data sc.manut.dal
088400020319     c                   eval       *in85 = *off                                data sc.manut.al
088500100906     c                   eval       *in86 = *off                                modello
088600120411     c                   eval       *in87 = *off                                data rientro
088700020326
088800020326     c                   clear                   wtipri
088900020319
089000020319      * Se non è stata scelta una ricerca utilizzo la ricerca per tipo
089100020319     c                   if        vdpmat = *blanks and vdptip = *blanks and
089200020319     c                             vdpmag = *blanks and vdpti2 = *zeros  and
089300100906     c                             vddsmd = *zeros  and vddsma = *zeros  and
089400100906     c                             vdpmod=*blanks and vdpmat2=*blanks
089500020319     c                   eval      wtipri = 'TIP'
089600020319     c                   eval      *in90 = *on
089700020319     c                   endif
089800020319
089900020319      * ===> ACCETTO SOLO UN PARAMETRO DI INIZIO RICERCA
090000020319
090100020319      * Tipo unità / matricola
090200100906    1c                   if        vdptip <> *blanks
090300020319      * controllo tipo unità
090400020319     c                   exsr      chktip
090500020319     c                   eval      wtipri = 'TIP'
090600020319     c                   eval      *in90 = *on
090700100906    1c                   endif
090800020319
090900020319      * Matricola
091000100906    1c                   if        vdpmat <> *blanks and errl00 = *off and
091100020410     c                             vdptip = *blanks
091200020319     c                   eval      wtipri = 'MAT'
091300020319     c                   eval      *in91 = *on
091400100906    1c                   endif
091500100907      * ricerca x contenuto matricola
091600100906    1c                   if        errl00 = *off and vdpmat = *blanks and
091700020325     c                             vdpric = 'S'
091800020325     c                   eval      errl00 = *on
091900020325     c                   eval      dsmsms = msg(12)
092000100906     c                   leavesr
092100100906    1c                   endif
092200100906     c* non immettere in tipo
092300100906    1c                   if        vdptip<>*blanks  and vdpric='S'
092400100906     c                   eval      errl00 = *on
092500100906     c                   eval      dsmsms = msg(17)
092600100906     c                   leavesr
092700100906    1c                   endif
092800020319
092900020319      * Magazzino / tipo unità
093000100906    1c                   if        (vdpmag <> *blanks or vdpti2 > *zeros) and   --- 1 -->
093100020319     c                              errl00 = *off
093200100906    2c                   if        wtipri <> *blanks                            --- 2 -->
093300020319     c                   eval      errl00 = *on
093400020319     c                   eval      dsmsms = msg(6)
093500100906   x2c                   else                                                   <-- 2 -->
093600100907      * controllo magazzino obbligatorio se c'e' tipo
093700100906     c                   eval      ctrmag=vdpmag
093800020319     c                   exsr      chkmag
093900100906     c                   eval      vdpmag=ctrmag
094000100906     c
094100020319     c                   eval      wtipri = 'MAG'
094200020319     c                   eval      *in92 = *on
094300100906    2c                   endif                                                  <-- 2 ---
094400100906    1c                   endif                                                  <-- 1 ---
094500020319
094600020319      * Data scadenza manutenzione
094700100907    1c                   if        (vddsmd <> *zeros or vddsma <> *zeros) and   --- 1 -->
094800020319     c                              errl00 = *off
094900100907    2c                   if        wtipri <> *blanks                            --- 2 -->
095000020319     c                   eval      errl00 = *on
095100020319     c                   eval      dsmsms = msg(6)
095200100907   x2c                   else                                                   <-- 2 -->
095300020319      * controllo data 'dal'
095400100907    3c                   if        vddsmd = *zeros                              --- 3 -->
095500020319     c                   eval      errl00 = *on
095600020319     c                   eval      dsmsms = msg(9)
095700020319     c                   eval      *in84 = *on
095800100907    3c                   endif                                                  <-- 3 ---
095900020319     c                   clear                   w_vddsmd
096000020319     c                   clear                   wlbda8
096100020319     c                   z-add     vddsmd        g08dat
096200020319     c                   z-add     *zeros        g08inv
096300020319     c                   movel     '0'           g08err
096400020319     c                   call      'XSRDA8'
096500020319     c                   parm                    wlbda8
096600020319     c                   z-add     g08dat        vddsmd
096700020319     c                   z-add     g08inv        w_vddsmd
096800100907    3c                   if        g08err <> '0'                                --- 3 -->
096900020319     c                   eval      errl00 = *on
097000020319     c                   eval      *in84 = *on
097100020319     c                   eval      dsmsms = msg(9)
097200100907    3c                   endif                                                  <-- 3 ---
097300020319      * controllo data 'al'
097400100907    3c                   if        vddsma <> *zeros                             --- 3 -->
097500020319     c                   clear                   w_vddsma
097600020319     c                   clear                   wlbda8
097700020319     c                   z-add     vddsma        g08dat
097800020319     c                   z-add     *zeros        g08inv
097900020319     c                   movel     '0'           g08err
098000020319     c                   call      'XSRDA8'
098100020319     c                   parm                    wlbda8
098200020319     c                   z-add     g08dat        vddsma
098300020319     c                   z-add     g08inv        w_vddsma
098400100907    4c                   if        g08err <> '0'                                --- 4 -->
098500020319     c                   eval      errl00 = *on
098600020319     c                   eval      *in85 = *on
098700020319     c                   eval      dsmsms = msg(9)
098800100907    4c                   endif                                                  <-- 4 ---
098900100907    3c                   endif                                                  <-- 3 ---
099000020319     c                   eval      wtipri = 'DSM'
099100020319     c                   eval      *in93 = *on
099200100907    2c                   endif                                                  <-- 2 ---
099300100907    1c                   endif                                                  <-- 1 ---
099400100906     c
099500100906     c* Modello e matricola
099600100906    1c                   if        (vdpmod <> *blanks or vdpmat2<>*blanks) and   --- 1 -->
099700100906     c                              errl00 = *off
099800100906    2c                   if        wtipri <> *blanks                            --- 2 -->
099900100906     c                   eval      errl00 = *on
100000100906     c                   eval      dsmsms = msg(6)
100100100906   x2c                   else                                                   <-- 2 -->
100200100906     c     vdpmod        chain     unana08l
100300100906    3c                   if        not %found(unana08l)
100400100906     c                   eval      errl00 = *on
100500100906     c                   eval      *in86 = *on
100600100906     c                   eval      dsmsms = msg(15)
100700100906    3c                   endif                                                  <-- 4 ---
100800100906     c*
100900100906    3c                   if        errl00='0'
101000100906     c                   eval      wtipri = 'MOD'
101100100906     c                   eval      *in94 = *on
101200100906    3c                   endif                                                  <-- 4 ---
101300100906    2c                   endif                                                  <-- 4 ---
101400100906    1c                   endif                                                  <-- 4 ---
101500100907    c
101600100907      * Controllo parzializzazione magazzino
101700100907    1c                   if        vdsmag>0
101800100907     c                   eval      ctrmag=%editc(vdsmag:'X')
101900100907     c                   exsr      chkmag
102000100907    2c                   if        errl00='1'
102100100907     c                   leavesr
102200100907    2c                   endif
102300100907    1c                   endif
102400101029     c
102500101029     c* Controllo data rientro prima del
102600101029     c                   clear                   w_vdsrief
102700101029     c                   if        vdsrief>0
102800101029     c                   clear                   wlbda8
102900101029     c                   z-add     vdsrief       g08dat
103000101029     c                   z-add     *zeros        g08inv
103100101029     c                   movel     '0'           g08err
103200101029     c                   call      'XSRDA8'
103300101029     c                   parm                    wlbda8
103400101029     c                   z-add     g08dat        vdsrief
103500101029     c                   z-add     g08inv        w_vdsrief
103600101029    3c                   if        g08err <> '0'                                --- 3 -->
103700101029     c                   eval      errl00 = *on
103800101029     c                   eval      *in87 = *on
103900101029     c                   eval      dsmsms = msg(9)
104000101029    3c                   endif                                                  <-- 3 ---
104100101029    3c                   endif                                                  <-- 3 ---
104200020326
104300020326      * SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
104400020326      * CURSORE SUL CAMPO DEL CONTROL
104500020326     C     errl00        IFEQ      '1'
104600020326     C                   MOVEL     '0'           *IN43                          *NON EMETTO SFL
104700020326     C                   ELSE
104800020326     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
104900020326     C                   END
105000020319
105100930202     C                   ENDSR
105200930707     C*================================================================
105300930707     C*  CHKTIP - CONTROLLA IL TIPO DI UNITA
105400930707     C*================================================================
105500930707     C     CHKTIP        BEGSR
105600020318
105700020318      * CONTROLLO SE E' STATO INSERITO '?'
105800020319     C     '?'           SCAN      VDPTIP                                 99
105900020318
106000020318      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
106100020319     C                   if        *in99                                        --- 1 -->
106200020318     C                   clear                   TIBS02DS
106300020318     C                   movel     'R'           t02mod
106400020318     C                   movel     knsif         t02sif
106500020321     C                   movel     'UNT'         t02cod
106600020318     C                   call      'TIBS02R'
106700020318     C                   parm                    KPJBA
106800020318     C                   parm                    TIBS02DS
106900020318     C                   movel     T02ke1        vdptip
107000020318
107100020319     C                   endif                                                  <-- 1 ---
107200020318
107300020318      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
107400020318     C                   clear                   TIBS02DS
107500020318     C                   movel     'C'           T02mod
107600020318     C                   movel     knsif         t02sif
107700020321     C                   movel     'UNT'         t02cod
107800020318     C                   movel(p)  vdptip        T02ke1
107900020318     C                   call      'TIBS02R'
108000020318     C                   parm                    KPJBA
108100020318     C                   parm                    TIBS02DS
108200020318     C                   if        t02err <> *blanks
108300020319     C                   eval      errl00 = '1'
108400020319     C                   eval      *in80 = *on
108500020318     C                   movel     msg(7)        dsmsms
108600020318     C                   endif
108700020319
108800930707     C                   ENDSR
108900930707     C*================================================================
109000930707     C*  CHKMAG - CONTROLLA IL MAGAZZINO UBICAZIONE
109100930707     C*================================================================
109200930707     C     CHKMAG        BEGSR
109300930707     C*
109400930707     C* CONTROLLO SE E' STATO INSERITO '?'
109500100906     C     '?'           SCAN      ctrmag                                 99
109600930707     C*
109700930707     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
109800930707     C     *IN99         IFEQ      '1'                                          --- 1 -->
109900930707     C                   MOVEL     *BLANKS       §COD1             3
110000930707     C                   MOVE      *BLANKS       §DES1
110100941115     C                   CALL      'TNSD20R'
110200930707     C                   PARM                    §COD1
110300930707     C                   PARM                    §TIP              1
110400930707     C                   PARM                    §DES1            25
110500100906     C                   MOVEL     §COD1         ctrmag
110600930707     C                   END                                                    <-- 1 ---
110700930707     C*
110800930707     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
110900100906     C                   MOVE      ctrmag        ORGFIL
111000020529     C     KEYORG        CHAIN     AZORG21l                           99
111100930707     C     *IN99         IFEQ      '0'                                          --- 2 -->
111200100906     C                   MOVEL     ORGFIL        ctrmag
111300930707     C                   ELSE                                                   <-- 2 -->
111400020529     c     keyorg        chain     azorg01l
111500020529     c                   if        %found(azorg01l)
111600100906     c                   movel     orgfil        ctrmag
111700020529     c                   else
111800930707     C                   MOVEL     '1'           ERRL00
111900930707     C                   MOVEL     MSG(8)        DSMSMS
112000930707     C                   SETON                                        83
112100020529     c                   endif
112200930707     C                   END                                                    <-- 2 ---
112300020516      * l'utente deve essere abilitato al magazzino inserito
112400020516     c                   if        vdpmag <> '951'
112500100906     c                   movel     ctrmag        wmag
112600020523     c     wmag          lookup    skpoabi                                99
112700020516     c                   if        not *in99
112800020516     c                   eval      errl00 = *on
112900020516     c                   eval      dsmsms = msg(14)
113000020516     c                   eval      *in83 = *on
113100020516     c                   endif
113200020516     c                   endif
113300930707     C*
113400930707     C                   ENDSR
113500020516      *------------------------------------------------------------------------*
113600020516      *  CTRUTE - ROUTINE CONTROLLO UTENTE
113700020516      *------------------------------------------------------------------------*
113800020516     c     ctrute        begsr
113900020516
114000020516     c                   eval      *in76 = *off
114100020516
114200020516      * controllo se il profilo è abilitato
114300020516     c                   if        duterr = 'E'
114400020516     c                   eval      dsmsms = dutmsg
114500020516     c                   eval      *in76 = *on
114600020516     c                   endif
114700020516      * controllo le abilitazionei del profilo
114800020516     c                   if        *in76 = *off
114900020516     c                   movel     utefaf        dute01
115000020516     c                   if        §uteiun <> *blanks
115100020516     c                   eval      wabi = §uteiun
115200020516     c                   else
115300020516     c                   eval      wabi = uteaut
115400020516     c                   endif
115500020516     c                   clear                   tibs02ds
115600020516     c                   movel     'C'           t02mod
115700020516     c                   movel     knsif         t02sif
115800020516     c                   movel     'LAT'         t02cod
115900020516     c                   movel(p)  wabi          t02ke1
116000020516     c                   call      'TIBS02R'
116100020516     c                   parm                    kpjba
116200020516     c                   parm                    tibs02ds
116300020516     c                   movel     t02uni        dlat
116400020516     c                   if        §latabi = 'S'
116500020516     c                   eval      dsmsms= msg(13)
116600020516     c                   eval      *in76 = *on
116700020516     c                   endif
116800020516     c                   endif
116900020516
117000020516      * Utente ok richiamo pgm che ritorna i p.o. abilitati
117100020523     c                   if        *in76 = *off  and wabi <> 'AZ'
117200020516     c                   clear                   trul31ds
117300020516     c                   eval      i31abi = wabi
117400020516     c                   eval      i31cdi = dutdis
117500020516     c                   eval      i31car = dutare
117600020516     c                   eval      i31cpo = dutpou
117700020516     c                   call      'TRUL31R'
117800020516     c                   parm                    kpjba
117900020516     c                   parm                    trul31ds
118000020516     c                   endif
118100020517
118200090904      /free
118300090904
118400090904         SELECT;
118500090904
118600090904           // - Reperimento filiali gestibili per utente della LOGISTICA
118700090904           WHEN  DUTntw = c_Logistica;
118800090904
118900090904             setll  *loval  AZORG;
119000090904             read   AZORG;
119100090904
119200090904             DoW  Not  %eof(AZORG01L);
119300090904
119400090904               if  ORGfva = *blank;
119500090904
119600090904                 Og143 = ORGde3;
119700090904                 if  §OgNtw = DUTntw;
119800090904
119900090904                   ii += 1;
120000090904                   skPOabi(ii) = ORGfil;
120100090904
120200090904                 endif;
120300090904
120400090904               endif;
120500090904
120600090904               read   AZORG;
120700090904
120800090904             EndDo;
120900090904
121000090904      /end-free
121100090904
121200020529      * per abilitazione 'AZ' devo caricare tutti i p.o. di azorg21l
121300090904     c                   WHEN      wabi = 'AZ'
121400020529     c     *loval        setll     azorg21l
121500020517     c                   do        *hival
121600020529     c                   read      azorg21l
121700020517     c                   if        %eof
121800020517     c                   leave
121900020517     c                   endif
122000020517     c                   z-add     1             ii
122100020523     c     orgfil        lookup    skpoabi(ii)                            99
122200020517     c                   if        not *in99
122300020517     c                   z-add     1             ii
122400020523     c     *zeros        lookup    skpoabi(ii)                            99
122500020517     c                   if        *in99
122600020517     c                   movel     orgfil        skpoabi(ii)
122700020517     c                   endif
122800020517     c                   endif
122900020517     c                   enddo
123000020529      * e tutti i p.o. di azorg01l
123100020529     c     *loval        setll     azorg01l
123200020529     c                   do        *hival
123300020529     c                   read      azorg01l
123400020529     c                   if        %eof
123500020529     c                   leave
123600020529     c                   endif
123700020529     c                   z-add     1             ii
123800020529     c     orgfil        lookup    skpoabi(ii)                            99
123900020529     c                   if        not *in99
124000020529     c                   z-add     1             ii
124100020529     c     *zeros        lookup    skpoabi(ii)                            99
124200020529     c                   if        *in99
124300020529     c                   movel     orgfil        skpoabi(ii)
124400020529     c                   endif
124500020529     c                   endif
124600020529     c                   enddo
124700090904      *
124800020523      * se non è abilitazione 'AZ' passo la sk tornata dal TRUL31 nella sk di 500 elementi
124900090904     c                   OTHER
125000020523     c                   movea     skpo          skpoabi
125100020523     c                   z-add     1             ii
125200020523     c     951           lookup    skpoabi(ii)                            99
125300020523     c                   if        not *in99
125400020523     c                   z-add     1             ii
125500020523     c     *zeros        lookup    skpoabi(ii)                            99
125600020523     c                   if        *in99
125700021014     c                   movel     951           skpoabi(ii)
125800020523     c                   endif
125900020523     c                   endif
126000090904      *
126100090904     c                   ENDSL
126200020516
126300020516     c                   endsr
126400930201     C*================================================================
126500930201     C* LANPGM - LANCIA UN PROGRAMMA
126600930201     C*================================================================
126700930201     C     LANPGM        BEGSR
126800930201     C*
126900930201     C* IMPOSTO PARAMETRI PER IL PGM
127000930707     C                   MOVEL     '0'           DSUF03                         PREMUTO F03
127100930707     C                   MOVEL     '0'           DSUERR                         '1'=ERRORE
127200930707     C                   MOVEL     *BLANKS       DSUMSG                         MESSAGGIO
127300930707     C                   MOVE      WOPZ1         DSUOP1                         *OPZIONE LIV.PGM
127400020322     C                   Z-ADD     vidtip        DSUTIP                         TIPO
127500930707     C                   Z-ADD     VIDCOD        DSUCOD                         CODICE
127600930712     C                   Z-ADD     *ZEROS        DSUMAG                         MAGAZZINO
127700930712     C                   Z-ADD     *ZEROS        DSUPRG                         PROGRESSIVO MOV.
127800930906     C                   Z-ADD     *ZEROS        DSUDTM                         DATA MOVIMENTO
127900930201     C*
128000930201     C* LANCIO PROGRAMMA
128100940105     C                   MOVEL     DSUNIT        KPJBU
128200930312     C                   CALL      WPGM
128300940105     C                   PARM                    KPJBA
128400940105     C                   MOVEL     KPJBU         DSUNIT
128500930201     C*
128600930201     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
128700930707     C     DSUF03        IFEQ      '1'
128800930201     C                   MOVEL     '1'           F03L00
128900930201     C                   MOVEL     '1'           ERRL00
129000930201     C                   ELSE
129100930201     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
129200930707     C                   MOVEL     DSUMSG        DSMSMS
129300930201     C                   END
129400930201     C*
129500930201     C                   ENDSR
129600930312     C*================================================================
129700930312     C* CHKFOP - CONTROLLA TIPO GESTIONE E IMPOSTA FUNZ/OPZ
129800930312     C*================================================================
129900930312     C     CHKFOP        BEGSR
130000930312     C*
130100930312     C* CONTROLLO CHE VI SIA UNA OPZIONE DI PROCEDURA VALIDA IN INPUT
130200930312     C* ALTRIMENTI FORZO INTERROGAZIONE
130300940105     C     DSUOP0        IFNE      ' 2'                                         *GESTIONE
130400940105     C     DSUOP0        ANDNE     ' 5'                                         *INTERROGAZIONE
130500160923     C     DSUOP0        ANDNE     ' S'                                         *STORICIZZAZIONE
130600940105     C                   MOVE      ' 5'          DSUOP0
130700930312     C                   END
130800070509     C*
130900070509     C* SE IL PGM E' STATO LANCIATO IN SELEZIONE ABILITO SOLO LA "1"
131000070509     C     DSUOP0        IFEQ      ' 1'
131100070509     C                   MOVEL     OPD(1)        OPDRIG                         *INTERROGAZIONE
131200070509     C                   MOVEL     'S'           OPDATT
131300070509     C                   MOVEL     OPDRIG        OPD(1)
131400070509     C                   ENDIF
131500930312     C*
131600930312     C* SE L'UTENTE E' ENTRATO IN GESTIONE LO ABILITO A TUTTE LE FUNZIONI
131700940105     C     DSUOP0        IFEQ      ' 2'
131800930312     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
131900930312     C                   MOVEL     'S'           OPDATT
132000930312     C                   MOVEL     OPDRIG        OPD(2)
132100971015     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
132200971015     C                   MOVEL     'S'           OPDATT
132300971015     C                   MOVEL     OPDRIG        OPD(3)
132400930312     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
132500930312     C                   MOVEL     'S'           OPDATT
132600930312     C                   MOVEL     OPDRIG        OPD(4)
132700930312     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
132800930312     C                   MOVEL     'S'           OPDATT
132900930312     C                   MOVEL     OPDRIG        OPD(5)
133000930312     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
133100930312     C                   MOVEL     'S'           OPDATT
133200930312     C                   MOVEL     OPDRIG        OPD(7)
133300971015     C                   MOVEL     OPD(8)        OPDRIG                         *MOVIMENTI
133400971015     C                   MOVEL     'S'           OPDATT
133500971015     C                   MOVEL     OPDRIG        OPD(8)
133600160923     C                   MOVEL     OPD(9)        OPDRIG                         *RIENTRO
133700140128     C                   MOVEL     'S'           OPDATT
133800140128     C                   MOVEL     OPDRIG        OPD(9)
133900930312     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
134000930312     C                   MOVEL     'S'           FUDATT
134100930312     C                   MOVEL     FUDRIG        FUD(6)
134200930312     C                   END
134300930312     C*
134400930312     C* SE IL PGM E' STATO LANCIATO IN INTERROGAZIONE ABILITO SOLO QUELLA
134500940105     C     DSUOP0        IFEQ      ' 5'
134600930312     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
134700930312     C                   MOVEL     'S'           OPDATT
134800930312     C                   MOVEL     OPDRIG        OPD(5)
134900971015     C                   MOVEL     OPD(8)        OPDRIG                         *MOVIMENTI
135000971015     C                   MOVEL     'S'           OPDATT
135100971015     C                   MOVEL     OPDRIG        OPD(8)
135200930312     C                   END
135300160923     C* SE IL PGM E' STATO LANCIATO IN STORICIZZAZIONE ABILITO  QUELLA È INTERR
135400160923     C     DSUOP0        IFEQ      ' S'
135500160923     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
135600160923     C                   MOVEL     'S'           OPDATT
135700160923     C                   MOVEL     OPDRIG        OPD(5)
135800160923     C                   MOVEL     OPD(10)       OPDRIG                         *STORICIZZAZ
135900160923     C                   MOVEL     'S'           OPDATT
136000160923     C                   MOVEL     OPDRIG        OPD(8)
136100160923     C                   END
136200930312     C*
136300930312     C* SE SONO IN FILIALE NON CONCEDO CANCELLAZIONE,INSERIMENTO,RIPRISTINO
136400020709     c                   If        DutLpo <> 'S'
136500971015     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
136600971015     C                   MOVEL     'N'           OPDATT
136700971015     C                   MOVEL     OPDRIG        OPD(3)
136800930312     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
136900930312     C                   MOVEL     'N'           OPDATT
137000930312     C                   MOVEL     OPDRIG        OPD(4)
137100930312     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
137200930312     C                   MOVEL     'N'           OPDATT
137300930312     C                   MOVEL     OPDRIG        OPD(7)
137400930312     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
137500930312     C                   MOVEL     'N'           FUDATT
137600930312     C                   MOVEL     FUDRIG        FUD(6)
137700930312     C                   END
137800930312     C*
137900930312     C* IMPOSTO INDICE PER EMISSIONE OPZIONI A VIDEO
138000930312     C                   Z-ADD     0             OPZGRU            5 0
138100930312     C*
138200930312     C* GESTIONE OPZIONI A VIDEO
138300930312     C                   EXSR      OPZGES
138400930312     C*
138500930312     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
138600930312     C                   Z-ADD     0             FUNGRU            5 0
138700930312     C*
138800930312     C* GESTIONE TASTI FUNZIONALI A VIDEO
138900930312     C                   EXSR      FUNGES
139000930312     C*
139100930312     C                   ENDSR
139200000000     C*================================================================
139300000000     C*  RUTINZ - ROUTINE INIZIALE
139400000000     C*================================================================
139500000000     C     RUTINZ        BEGSR
139600000000     C*
139700930707     C* RICEVIMENTO PARAMETRI DALL'AZIONE (' 2'=GEST.,' 5'=INTERR.)
139800000000     C     *ENTRY        PLIST
139900930707     C                   PARM                    KPJBA
140000070509     c                   parm                    UNANAds
140100940105     C                   MOVEL     KPJBU         DSUNIT
140200070509      * - impostazione iniziale dei parametri di output per chiamante
140300070509     c                   eval      DSUf03 = *off                                PREMUTO F03
140400070509     c                   eval      DSUerr = *off                                '1'=ERRORE
140500070509     c                   eval      DSUmsg = *blanks                             MESSAGGIO
140600020516
140700020516     c     *dtaara       define    §azute        azuteds
140800020516     c     *dtaara       define    §datiute      ddatiute
140900020516
141000020516     c                   in(E)     *dtaara
141100020516     c                   if        %Error or rsut = *blanks
141200020516     c                   clear                   tibs34ds
141300020516     c                   call      'TIBS34R'
141400020516     c                   parm                    Tibs34Ds
141500020516     c                   in        *dtaara
141600020516     c                   endif
141700020709
141800020709      * controllo il profilo utente
141900020709     c                   exsr      ctrute
142000020709     c                   if        *in76
142100020709     c                   eval      sflult = 0
142200020709     c                   endif
142300020516
142400930204     C*
142500930204     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
142600930204     C     XMSGPA        PLIST
142700930204     C                   PARM                    DSMSMS
142800930204     C                   PARM                    DSMSPG
142900930204     C                   PARM                    DSMSKE
143000930204     C                   PARM                    DSMSER
143100020319
143200020319      * CHIAVE LETTURA UNANA02L
143300930707     C     KUNA02        KLIST
143400020319     C                   KFLD                    UNATIP
143500020319     C                   KFLD                    UNAMAT
143600140128     C     KEYUNA        KLIST
143700140128     c                   kfld                    vidtip
143800140128     C                   KFLD                    vidCOD
143900020319
144000020319      * CHIAVE LETTURA UNANA03L
144100930707     C     KUNA03        KLIST
144200930707     C                   KFLD                    UNAMAG
144300930707     C                   KFLD                    UNATIP
144400020319
144500020319      * CHIAVE LETTURA UNANA04L
144600930707     C     KUNA04        KLIST
144700930707     C                   KFLD                    UNADSM
144800020319
144900020319      * CHIAVE LETTURA UNANA07L
145000020319     C     KUNA07        KLIST
145100020319     C                   KFLD                    UNAMAT
145200020319     C                   KFLD                    UNATIP
145300100906     C     KUNA08        KLIST
145400100906     C                   KFLD                    UNAmod
145500100906     C                   KFLD                    UNAmat
145600020319
145700020529     C* CHIAVE LETTURA AZORG21l/azorg01l - COMPLETA
145800930707     C     KEYORG        KLIST
145900930707     C                   KFLD                    ORGFIL
146000930707     C*
146100000000     C*
146200930129     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
146300930204     C                   CALL      'XKEY'
146400930204     C                   PARM                    DSKEY
146500000000     C*
146600000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
146700000000     C     *LIKE         DEFINE    SFLNBR        SFLPAG
146800000000     C     *LIKE         DEFINE    SFLPAG        WFLPAG
146900000000     C     *LIKE         DEFINE    SFLNBR        SFLEOF
147000000000     C     *LIKE         DEFINE    SFLNBR        SFLERR
147100000000     C     *LIKE         DEFINE    SFLNBR        SFLULT
147200000000     C     *LIKE         DEFINE    SFLNBR        WFLULT
147300000000     C     *LIKE         DEFINE    SFLNBR        MSGULT
147400000000     C     *LIKE         DEFINE    SFLNBR        SFLRIG
147500000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
147600000000     C*
147700930127     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
147800000000     C*   -NOME PGM CHIAMANTE
147900000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
148000941115     C                   MOVEL     'TNSDU1R'     DSMSPG
148100941115     C                   MOVEL     'TNSDU1R'     MSGQUE
148200000000     C                   MOVEL     *BLANKS       MSGKEY
148300930127     C                   MOVEL     *BLANKS       DSMSMS
148400930204     C                   MOVEL     *BLANKS       DSMSKE
148500930204     C                   Z-ADD     0             DSMSER
148600000000     C*
148700930129     C* INIZIALIZZO SWITCH DI LIVELLO 0 (PGM)
148800930127     C                   MOVEL     '0'           F12L00            1
148900930127     C                   MOVEL     '0'           F03L00            1
149000930127     C                   MOVEL     '0'           FINL00            1
149100930127     C                   MOVEL     '0'           ERRL00            1
149200930128     C                   MOVEL     '0'           FLFINE            1
149300930127     C*
149400930312     C* IMPOSTO DEPOSITI PER CONFRONTI
149500930707     C                   MOVEL     *BLANKS       DEPTIP
149600930707     C                   MOVEL     *BLANKS       DEPMAT
149700020325     c                   clear                   depric
149800930707     C                   MOVEL     *BLANKS       DEPMAG
149900100907     C                   MOVEL     *BLANKS       DspMAG
150000100907     C                   MOVEL     *BLANKS       Dsprief
150100100907     C                   MOVEL     *BLANKS       depmod
150200100907     C                   MOVEL     *BLANKS       depmat2
150300020319     C                   Z-ADD     *ZEROS        DEPTI2
150400020319     C                   Z-ADD     *ZEROS        dedsmd
150500020319     C                   Z-ADD     *ZEROS        dedsma
150600101029     c                   clear                   depdes
150700101029     c                   clear                   depnot
150800930202     C*
150900941128     C* IMPOSTO TIPO DI RICERCA DI DEFAULT --> MAT
151000020319     C                   MOVEL     'TIP'         WTIPRI
151100020319     c                   eval      *in90 = *on
151200100924     c* se passata matricola eseguo ricerca
151300100927     c                   if        SDSprm > 1  and
151400100924     c                             UIMmat<>*blanks
151500100924     c                   movel     UIMmat        vdpmat
151600100924     c                   movel     UIMmat        depmat
151700100927     c                   clear                   unanaDS
151800100924     c                   exsr      chkpar
151900100924     c                   endif
152000000000     C*
152100000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
152200000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
152300020325     C                   Z-ADD     10            SFLPAG
152400930312     C*
152500930312     C* IMPOSTO LE OPZIONI E LE FUNZIONI A VIDEO
152600930312     C                   EXSR      CHKFOP
152700930225     C*
152800930225     C* IMPOSTO INTERROGAZIONE/GESTIONE NELLA TESTATA DELLA VIDEATA
152900070509     c                   select
153000070509     c                   when      DSUop0 = ' 1'
153100070509     C                   eval      VIDing = '   SELEZIONE    '
153200070509     c                   when      DSUop0 = ' 2'
153300930225     C                   MOVEL     '    GEST'    VIDING
153400930225     C                   MOVE      'IONE    '    VIDING
153500160923     c                   when      DSUop0 = ' S'
153600160923     C                   MOVEL     'STORICIZ'    VIDING
153700160923     C                   MOVE      'ZAZIONE '    VIDING
153800070509     c                   other
153900930225     C                   MOVEL     ' INTERRO'    VIDING
154000930225     C                   MOVE      'GAZIONE '    VIDING
154100070509     c                   endsl
154200160923
154300160923     c     *iso          movel     datasys       dateu             8 0
154400020325
154500000000     C                   ENDSR
154600020326
154700930312     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
154800930312     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
154900930312     C* LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
155000930312     C* STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
155100930312     C* NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
155200930312     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
155300930312     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
155400930312     C*
155500930312     C*================================================================
155600930312     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA
155700930312     C*          CARICARE
155800930312     C*================================================================
155900930312     C     POSSFL        BEGSR
156000930312     C*
156100930312     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
156200930312     C                   EXSR      CLRSFL
156300930312     C*
156400930312     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
156500930312     C                   EXSR      SETFIL
156600930312     C*
156700930312     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
156800930312     C     SFLEOF        IFEQ      0
156900020326     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
157000930312     C                   EXSR      CARSFL
157100930312     C                   END
157200930312     C*
157300930312     C                   ENDSR
157400000000     C*================================================================
157500000000     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
157600000000     C*================================================================
157700000000     C     ENTATT        BEGSR
157800000000     C*
157900000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
158000000000     C                   Z-ADD     0             OPZOK             1 0
158100000000     C*
158200000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
158300000000     C                   Z-ADD     0             SFLVAR            5 0
158400000000     C*
158500000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
158600000000     C                   Z-ADD     0             SFLERR
158700000000     C*
158800000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
158900000000     C                   Z-ADD     SFLULT        WFLULT
159000000000     C*
159100000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
159200941115     C                   READC     SDU1D05                                98
159300930202     C     *IN98         DOWEQ     '0'
159400000000     C*
159500000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
159600000000     C                   ADD       1             SFLVAR
159700000000     C*
159800000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
159900000000     C     OPZOK         IFEQ      0
160000000000     C*
160100000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
160200000000     C     OPZKEY        IFNE      *BLANKS
160300000000     C                   EXSR      OPZCON
160400000000     C                   END
160500000000     C*
160600000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
160700000000     C*            2. PULIZIA SCELTA IN SFL
160800000000     C     OPZOK         IFEQ      0
160900000000     C                   MOVEL     '0'           *IN41
161000000000     C                   MOVEL     *BLANKS       OPZKEY
161100000000     C                   ELSE
161200000000     C*
161300000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
161400000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
161500000000     C                   MOVEL     '1'           *IN41
161600000000     C                   Z-ADD     SFLULT        SFLERR
161700000000     C                   END
161800000000     C                   ELSE
161900000000     C*
162000000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
162100000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
162200000000     C                   MOVEL     '1'           *IN41
162300000000     C                   END                                                    OPZOK=0
162400000000     C*
162500000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
162600000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
162700000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
162800000000     C     OPZOK         IFNE      0
162900000000     C                   Z-ADD     SFLERR        SFLNBR
163000000000     C                   ELSE
163100000000     C                   Z-ADD     SFLULT        SFLNBR
163200000000     C                   END
163300000000     C*
163400000000     C* AGGIORNO RECORD SFL
163500101027     c                   if        vidrief='SI'
163600100906     c                   setoff                                       15
163700100906     c                   else
163800100906     c                   seton                                        15
163900100906     c                   endif
164000100906     c
164100941115     C                   UPDATE    SDU1D05
164200000000     C*
164300941115     C                   READC     SDU1D05                                98
164400000000     C                   END                                                    DO WHILE
164500000000     C*
164600000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
164700000000     C                   Z-ADD     WFLULT        SFLULT
164800000000     C*
164900000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
165000000000     C     SFLVAR        IFEQ      0
165100000000     C                   MOVEL     '1'           FINL00
165200000000     C                   END
165300000000     C*
165400000000     C                   ENDSR
165500000000     C*================================================================
165600000000     C* ELIMINAZIONE CONTENUTO SFL
165700000000     C*================================================================
165800000000     C     CLRSFL        BEGSR
165900000000     C*
166000000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
166100000000     C* (0=NON FINE FILE  1=FINE FILE)
166200000000     C                   Z-ADD     0             SFLEOF
166300000000     C*
166400000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
166500000000     C                   Z-ADD     0             SFLULT
166600000000     C*
166700000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
166800000000     C                   MOVEL     '1'           *IN40
166900941115     C                   WRITE     SDU1D06
167000000000     C                   MOVEL     '0'           *IN40
167100000000     C*
167200000000     C                   ENDSR
167300000000     C*================================================================
167400000000     C* CARSFL - CARICAMENTO SFL
167500000000     C*================================================================
167600000000     C     CARSFL        BEGSR
167700000000     C*
167800000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
167900000000     C     SFLULT        IFEQ      0
168000000000     C                   EXSR      SETFIL
168100000000     C                   END
168200000000     C*
168300000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
168400000000     C                   Z-ADD     SFLULT        WFLULT
168500000000     C*
168600000000     C* CICLO:
168700000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
168800000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
168900000000     C                   Z-ADD     1             SFLRIG
169000000000     C     SFLRIG        DOWLE     SFLPAG
169100000000     C     SFLEOF        ANDEQ     0
169200020319
169300020319     c                   eval      wnosfl = *off
169400020319
169500000000     C*
169600000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
169700000000     C                   MOVEL     *BLANKS       OPZKEY
169800020319
169900020326      * controllo se devo scrivere il record nel subfile
170000020319     c                   exsr      ctrsfl
170100020319
170200020319     c                   if        wnosfl = *off
170300000000     C*
170400000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
170500000000     C                   EXSR      IRESFL
170600000000     C*
170700000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
170800000000     C                   ADD       1             SFLULT
170900000000     C*
171000000000     C* SCRITTURA RECORD SFL
171100941115     C                   WRITE     SDU1D05
171200000000     C                   ADD       1             SFLRIG
171300020319
171400020319     c                   endif
171500000000     C*
171600000000     C* LETTURA FILE DA CARICARE
171700000000     C                   EXSR      LETFIL
171800000000     C                   END                                                    DO WHILE
171900000000     C*
172000000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
172100000000     C     WFLULT        IFNE      SFLULT
172200000000     C                   Z-ADD     WFLULT        SFLNBR
172300000000     C                   ADD       1             SFLNBR
172400000000     C                   END
172500000000     C*
172600000000     C                   ENDSR
172700000000     C*================================================================
172800000000     C* EMISSIONE SUBFILE A VIDEO
172900000000     C*================================================================
173000000000     C     OUTSFL        BEGSR
173100000000     C*
173200000000     C* WRITE RECORD TESTATA VIDEO
173300941115     C                   WRITE     SDU1D01
173400930205     C*
173500930205     C* VISUALIZZAZIONE MESSAGGIO
173600930205     C                   EXSR      SNDMSG
173700160923     C*
173800000000     C* WRITE RECORD OPZIONI VIDEO
173900941115     C                   WRITE     SDU1D02
174000000000     C*
174100000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
174200000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
174300000000     C     SFLEOF        IFEQ      0
174400000000     C                   MOVEL     '0'           *IN42
174500000000     C                   ELSE
174600000000     C*
174700000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
174800000000     C                   MOVEL     '1'           *IN42
174900000000     C                   END
175000000000     C*
175100000000     C* WRITE RECORD TASTI FUNZIONALI
175200941115     C                   WRITE     SDU1D09
175300000000     C*
175400000000     C* CASO DI SFL VUOTO
175500000000     C     SFLULT        IFLE      0
175600000000     C*
175700000000     C*    - WRITE RECORD VUOTO
175800020326     C                   MOVEL     '0'           *IN43                          *NON VISUALIZZA SFL
175900941115     C                   WRITE     SDU1D0A
176000020326     C                   WRITE     SDU1D06                                      *SFL CONTROL
176100020326     C                   READ      SDU1D06                                99
176200000000     C*
176300000000     C                   ELSE
176400000000     C*
176500000000     C* CASO DI SFL CON RECORDS
176600000000     C*
176700000000     C*    - WRITE RECORD CONTROLLO SFL
176800941115     C                   WRITE     SDU1D06
176900941115     C                   READ      SDU1D06                                99
177000000000     C                   END
177100000000     C*
177200000000     C                   ENDSR
177300000000     C*================================================================
177400000000     C* SNDMSG - MANDA MESSAGGIO
177500000000     C*================================================================
177600000000     C     SNDMSG        BEGSR
177700000000     C*
177800000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
177900000000     C                   MOVEL     '1'           *IN40
178000941115     C                   WRITE     SDU1DMC
178100000000     C                   MOVEL     '0'           *IN40
178200000000     C*
178300000000     C* VISUALIZZAZIONE MESSAGGIO
178400000000     C                   Z-ADD     0             MSGULT
178500930204     C                   CALL      'XMSG'        XMSGPA
178600930127     C     DSMSER        IFEQ      0
178700930127     C                   MOVEL     DSMSKE        MSGKEY
178800000000     C                   END
178900000000     C                   Z-ADD     1             MSGULT
179000941115     C                   WRITE     SDU1DMS                                      SFL MESS
179100941115     C                   WRITE     SDU1DMC                                      CONTR MESS
179200000000     C*
179300000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
179400930127     C                   MOVEL     *BLANKS       DSMSMS
179500000000     C*
179600000000     C                   ENDSR
179700000000     C*================================================================
179800000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
179900000000     C*================================================================
180000000000     C     FUNCON        BEGSR
180100000000     C*
180200000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
180300000000     C                   EXSR      FUNVAR
180400000000     C*
180500000000     C* PULISCO CAMPI DI LAVORO
180600000000     C                   Z-ADD     0             I                 5 0
180700000000     C                   Z-ADD     0             W                 5 0
180800000000     C*
180900000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
181000000000     C     FUNKEY        IFNE      *BLANKS
181100100922     C     1             DO        25            I
181200000000     C                   MOVEL     FUD(I)        FUDRIG
181300000000     C*
181400000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
181500000000     C     FUNKEY        IFEQ      FUDFUN
181600000000     C     FUDATT        ANDEQ     'S'
181700000000     C                   Z-ADD     I             W
181800000000     C                   END
181900000000     C                   END                                                    DO WHILE
182000000000     C*
182100000000     C* FUNZIONE NON ATTIVA (ERRORE)
182200000000     C     W             IFEQ      0
182300000000     C*
182400000000     C* EMISSIONE MESSAGGIO DI ERRORE
182500930127     C                   MOVEL     MSG(2)        DSMSMS
182600000000     C                   END                                                    W=0
182700000000     C                   END                                                    FUNKEY NE *BLANKS
182800000000     C*
182900000000     C* FUNZIONE ATTIVA (PROCEDI ...)
183000000000     C     W             IFNE      0
183100000000     C     FUNKEY        OREQ      *BLANKS
183200000000     C*
183300000000     C     FUNKEY        IFEQ      *BLANKS
183400000000     C                   EXSR      ENTGES
183500000000     C                   END
183600000000     C     FUNKEY        IFEQ      '03'
183700000000     C                   EXSR      F03GES
183800000000     C                   END
183900000000     C     FUNKEY        IFEQ      '05'
184000000000     C                   EXSR      F05GES
184100000000     C                   END
184200930126     C     FUNKEY        IFEQ      '06'
184300930126     C                   EXSR      F06GES
184400000000     C                   END
184500000000     C     FUNKEY        IFEQ      '12'
184600000000     C                   EXSR      F12GES
184700000000     C                   END
184800000000     C     FUNKEY        IFEQ      '17'
184900000000     C                   EXSR      F17GES
185000000000     C                   END
185100000000     C     FUNKEY        IFEQ      '18'
185200000000     C                   EXSR      F18GES
185300000000     C                   END
185400930129     C     FUNKEY        IFEQ      '21'
185500930129     C                   EXSR      F21GES
185600930129     C                   END
185700000000     C     FUNKEY        IFEQ      '22'
185800000000     C                   EXSR      F22GES
185900000000     C                   END
186000000000     C     FUNKEY        IFEQ      '23'
186100000000     C                   EXSR      F23GES
186200000000     C                   END
186300000000     C     FUNKEY        IFEQ      '24'
186400000000     C                   EXSR      F24GES
186500000000     C                   END
186600000000     C     FUNKEY        IFEQ      'RU'                                         ROLLUP
186700000000     C                   EXSR      RUPGES
186800000000     C                   END
186900000000     C     FUNKEY        IFEQ      'HP'                                         HELP
187000000000     C                   EXSR      HELGES
187100000000     C                   END
187200000000     C*
187300000000     C                   END
187400000000     C*
187500000000     C                   ENDSR
187600000000     C*================================================================
187700000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
187800000000     C*================================================================
187900000000     C     FUNVAR        BEGSR
188000000000     C*
188100000000     C* INIZIALIZZA VARIABILE -FUNKEY-
188200000000     C*
188300000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
188400000000     C     KEY           IFEQ      ENTER
188500000000     C                   MOVEL     *BLANKS       FUNKEY
188600000000     C                   END
188700000000     C     KEY           IFEQ      F1
188800000000     C                   MOVEL     '01'          FUNKEY
188900000000     C                   END
189000000000     C     KEY           IFEQ      F2
189100000000     C                   MOVEL     '02'          FUNKEY
189200000000     C                   END
189300000000     C     KEY           IFEQ      F3
189400000000     C                   MOVEL     '03'          FUNKEY
189500000000     C                   END
189600000000     C     KEY           IFEQ      F4
189700000000     C                   MOVEL     '04'          FUNKEY
189800000000     C                   END
189900000000     C     KEY           IFEQ      F5
190000000000     C                   MOVEL     '05'          FUNKEY
190100000000     C                   END
190200000000     C     KEY           IFEQ      F6
190300000000     C                   MOVEL     '06'          FUNKEY
190400000000     C                   END
190500000000     C     KEY           IFEQ      F7
190600000000     C                   MOVEL     '07'          FUNKEY
190700000000     C                   END
190800000000     C     KEY           IFEQ      F8
190900000000     C                   MOVEL     '08'          FUNKEY
191000000000     C                   END
191100000000     C     KEY           IFEQ      F9
191200000000     C                   MOVEL     '09'          FUNKEY
191300000000     C                   END
191400000000     C     KEY           IFEQ      F10
191500000000     C                   MOVEL     '10'          FUNKEY
191600000000     C                   END
191700000000     C     KEY           IFEQ      F11
191800000000     C                   MOVEL     '11'          FUNKEY
191900000000     C                   END
192000000000     C     KEY           IFEQ      F12
192100000000     C                   MOVEL     '12'          FUNKEY
192200000000     C                   END
192300000000     C     KEY           IFEQ      F13
192400000000     C                   MOVEL     '13'          FUNKEY
192500000000     C                   END
192600000000     C     KEY           IFEQ      F14
192700000000     C                   MOVEL     '14'          FUNKEY
192800000000     C                   END
192900000000     C     KEY           IFEQ      F15
193000000000     C                   MOVEL     '15'          FUNKEY
193100000000     C                   END
193200000000     C     KEY           IFEQ      F16
193300000000     C                   MOVEL     '16'          FUNKEY
193400000000     C                   END
193500000000     C     KEY           IFEQ      F17
193600000000     C                   MOVEL     '17'          FUNKEY
193700000000     C                   END
193800000000     C     KEY           IFEQ      F18
193900000000     C                   MOVEL     '18'          FUNKEY
194000000000     C                   END
194100000000     C     KEY           IFEQ      F19
194200000000     C                   MOVEL     '19'          FUNKEY
194300000000     C                   END
194400000000     C     KEY           IFEQ      F20
194500000000     C                   MOVEL     '20'          FUNKEY
194600000000     C                   END
194700000000     C     KEY           IFEQ      F21
194800000000     C                   MOVEL     '21'          FUNKEY
194900000000     C                   END
195000000000     C     KEY           IFEQ      F22
195100000000     C                   MOVEL     '22'          FUNKEY
195200000000     C                   END
195300000000     C     KEY           IFEQ      F23
195400000000     C                   MOVEL     '23'          FUNKEY
195500000000     C                   END
195600000000     C     KEY           IFEQ      F24
195700000000     C                   MOVEL     '24'          FUNKEY
195800000000     C                   END
195900000000     C     KEY           IFEQ      ROLLUP
196000000000     C                   MOVEL     'RU'          FUNKEY
196100000000     C                   END
196200000000     C     KEY           IFEQ      HELP
196300000000     C                   MOVEL     'HP'          FUNKEY
196400000000     C                   END
196500000000     C*
196600000000     C                   ENDSR
196700000000     C*================================================================
196800000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
196900000000     C*================================================================
197000000000     C     FUNGES        BEGSR
197100000000     C*
197200000000     C* PULISCO CAMPI DI LAVORO
197300000000     C                   Z-ADD     0             W                 5 0
197400000000     C*
197500000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
197600000000     C* E CARICO TABELLA FUNZIONI ATTIVE
197700000000     C     1             DO        24            I
197800000000     C                   MOVEL     FUD(I)        FUDRIG
197900000000     C*
198000000000     C* PULIZIA FUNZIONI PRECEDENTI
198100000000     C                   MOVEL     *BLANKS       FUV(I)
198200000000     C*
198300000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
198400000000     C* (SALTA F23 E F24)
198500000000     C     FUDATT        IFEQ      'S'
198600000000     C     FUDFUN        ANDNE     '23'
198700000000     C     FUDFUN        ANDNE     '24'
198800000000     C                   ADD       1             W
198900000000     C                   MOVEL     FUDDES        FUV(W)
199000000000     C                   END
199100000000     C                   END                                                    DO WHILE
199200000000     C*
199300000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
199400000000     C* ALTRIMENTI DISATTIVO F24
199500000000     C     1             DO        24            I
199600000000     C                   MOVEL     FUD(I)        FUDRIG
199700000000     C*
199800000000     C     FUDFUN        IFEQ      '24'
199900000000     C     W             IFGT      6
200000000000     C                   MOVEL     'S'           FUDATT
200100000000     C                   ELSE
200200000000     C                   MOVEL     *BLANKS       FUDATT
200300000000     C                   END                                                    W GT 8
200400000000     C                   MOVEL     FUDRIG        FUD(I)
200500000000     C                   END
200600000000     C                   END                                                    DO WHILE
200700000000     C*
200800000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
200900000000     C     W             DIV       6             I
201000000000     C                   MVR                     Z
201100000000     C*
201200000000     C     Z             IFNE      0
201300000000     C     I             ADD       1             Z
201400000000     C                   ELSE
201500000000     C                   Z-ADD     I             Z
201600000000     C                   END
201700000000     C*
201800000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
201900000000     C                   ADD       1             FUNGRU
202000000000     C     FUNGRU        IFGT      Z
202100000000     C     FUNGRU        ORLT      1
202200000000     C                   Z-ADD     1             FUNGRU
202300000000     C                   END
202400000000     C*
202500000000     C* PRIMO GRUPPO DI 6 FUNZIONI
202600000000     C     FUNGRU        IFEQ      1
202700000000     C                   MOVEL     FUV(1)        FUN001
202800000000     C                   MOVEL     FUV(2)        FUN002
202900000000     C                   MOVEL     FUV(3)        FUN003
203000000000     C                   MOVEL     FUV(4)        FUN004
203100000000     C                   MOVEL     FUV(5)        FUN005
203200000000     C                   MOVEL     FUV(6)        FUN006
203300000000     C                   END
203400000000     C*
203500000000     C* SECONDO GRUPPO DI 6 FUNZIONI
203600000000     C     FUNGRU        IFEQ      2
203700000000     C                   MOVEL     FUV(7)        FUN001
203800000000     C                   MOVEL     FUV(8)        FUN002
203900000000     C                   MOVEL     FUV(9)        FUN003
204000000000     C                   MOVEL     FUV(10)       FUN004
204100000000     C                   MOVEL     FUV(11)       FUN005
204200000000     C                   MOVEL     FUV(12)       FUN006
204300000000     C                   END
204400000000     C*
204500000000     C* TERZO GRUPPO DI 6 FUNZIONI
204600000000     C     FUNGRU        IFEQ      3
204700000000     C                   MOVEL     FUV(13)       FUN001
204800000000     C                   MOVEL     FUV(14)       FUN002
204900000000     C                   MOVEL     FUV(15)       FUN003
205000000000     C                   MOVEL     FUV(16)       FUN004
205100000000     C                   MOVEL     FUV(17)       FUN005
205200000000     C                   MOVEL     FUV(18)       FUN006
205300000000     C                   END
205400000000     C*
205500000000     C* QUARTO GRUPPO DI 6 FUNZIONI
205600000000     C     FUNGRU        IFEQ      4
205700000000     C                   MOVEL     FUV(19)       FUN001
205800000000     C                   MOVEL     FUV(20)       FUN002
205900000000     C                   MOVEL     FUV(21)       FUN003
206000000000     C                   MOVEL     FUV(22)       FUN004
206100000000     C                   MOVEL     *BLANKS       FUN005
206200000000     C                   MOVEL     *BLANKS       FUN006
206300000000     C                   END
206400000000     C*
206500000000     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
206600000000     C                   MOVEL     *BLANKS       FUN007
206700000000     C                   MOVEL     *BLANKS       FUN008
206800000000     C*
206900000000     C     1             DO        24            I
207000000000     C                   MOVEL     FUD(I)        FUDRIG
207100000000     C     FUDFUN        IFEQ      '23'
207200000000     C     FUDATT        ANDEQ     'S'
207300000000     C                   MOVEL     FUDDES        FUN007
207400000000     C                   END
207500000000     C     FUDFUN        IFEQ      '24'
207600000000     C     FUDATT        ANDEQ     'S'
207700000000     C                   MOVEL     FUDDES        FUN008
207800000000     C                   END
207900000000     C                   END                                                    DO
208000000000     C*
208100000000     C                   ENDSR
208200000000     C*================================================================
208300000000     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
208400000000     C*================================================================
208500000000     C     OPZGES        BEGSR
208600000000     C*
208700000000     C* PULISCO CAMPI DI LAVORO
208800000000     C                   Z-ADD     0             W                 5 0
208900000000     C*
209000000000     C* RICERCO QUANTE OPZIONI SONO ATTIVE
209100000000     C* E CARICO TABELLA OPZIONI ATTIVE
209200000000     C     1             DO        24            I
209300000000     C                   MOVEL     OPD(I)        OPDRIG
209400000000     C*
209500000000     C* PULIZIA OPZIONI PRECEDENTI
209600000000     C                   MOVEL     *BLANKS       OPV(I)
209700000000     C*
209800000000     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
209900000000     C     OPDATT        IFEQ      'S'
210000000000     C                   ADD       1             W
210100000000     C                   MOVEL     OPDDES        OPV(W)
210200000000     C                   END
210300000000     C                   END                                                    DO WHILE
210400000000     C*
210500000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
210600000000     C* ALTRIMENTI DISATTIVO F23
210700000000     C     1             DO        24            I
210800000000     C                   MOVEL     FUD(I)        FUDRIG
210900000000     C*
211000000000     C     FUDFUN        IFEQ      '23'
211100000000     C     W             IFGT      8
211200000000     C                   MOVEL     'S'           FUDATT
211300000000     C                   ELSE
211400000000     C                   MOVEL     *BLANKS       FUDATT
211500000000     C                   END                                                    W GT 8
211600000000     C                   MOVEL     FUDRIG        FUD(I)
211700000000     C                   END
211800000000     C                   END                                                    DO WHILE
211900000000     C*
212000000000     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
212100000000     C     W             DIV       8             I
212200000000     C                   MVR                     Z                 5 0
212300000000     C*
212400000000     C     Z             IFNE      0
212500000000     C     I             ADD       1             Z
212600000000     C                   ELSE
212700000000     C                   Z-ADD     I             Z
212800000000     C                   END
212900000000     C*
213000000000     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
213100000000     C                   ADD       1             OPZGRU
213200000000     C     OPZGRU        IFGT      Z
213300000000     C     OPZGRU        ORLT      1
213400000000     C                   Z-ADD     1             OPZGRU
213500000000     C                   END
213600000000     C*
213700000000     C* PRIMO GRUPPO DI 8 OPZIONI
213800000000     C     OPZGRU        IFEQ      1
213900000000     C                   MOVEL     OPV(1)        OPZ001
214000000000     C                   MOVEL     OPV(2)        OPZ002
214100000000     C                   MOVEL     OPV(3)        OPZ003
214200000000     C                   MOVEL     OPV(4)        OPZ004
214300000000     C                   MOVEL     OPV(5)        OPZ005
214400000000     C                   MOVEL     OPV(6)        OPZ006
214500000000     C                   MOVEL     OPV(7)        OPZ007
214600000000     C                   MOVEL     OPV(8)        OPZ008
214700000000     C                   END
214800000000     C*
214900000000     C* SECONDO GRUPPO DI 8 OPZIONI
215000000000     C     OPZGRU        IFEQ      2
215100000000     C                   MOVEL     OPV(9)        OPZ001
215200000000     C                   MOVEL     OPV(10)       OPZ002
215300000000     C                   MOVEL     OPV(11)       OPZ003
215400000000     C                   MOVEL     OPV(12)       OPZ004
215500000000     C                   MOVEL     OPV(13)       OPZ005
215600000000     C                   MOVEL     OPV(14)       OPZ006
215700000000     C                   MOVEL     OPV(15)       OPZ007
215800000000     C                   MOVEL     OPV(16)       OPZ008
215900000000     C                   END
216000000000     C*
216100000000     C* TERZO GRUPPO DI 8 OPZIONI
216200000000     C     OPZGRU        IFEQ      3
216300000000     C                   MOVEL     OPV(17)       OPZ001
216400000000     C                   MOVEL     OPV(18)       OPZ002
216500000000     C                   MOVEL     OPV(19)       OPZ003
216600000000     C                   MOVEL     OPV(20)       OPZ004
216700000000     C                   MOVEL     OPV(21)       OPZ005
216800000000     C                   MOVEL     OPV(22)       OPZ006
216900000000     C                   MOVEL     OPV(23)       OPZ007
217000000000     C                   MOVEL     OPV(24)       OPZ008
217100000000     C                   END
217200000000     C                   ENDSR
217300000000     C*================================================================
217400000000     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
217500000000     C*================================================================
217600000000     C     OPZCON        BEGSR
217700000000     C*
217800000000     C* PULISCO CAMPI DI LAVORO
217900000000     C                   Z-ADD     0             I                 5 0
218000000000     C                   Z-ADD     0             W                 5 0
218100000000     C*
218200000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
218300000000     C     1             DO        24            I
218400000000     C                   MOVEL     OPD(I)        OPDRIG
218500000000     C*
218600000000     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
218700000000     C     OPZKEY        IFEQ      OPDOPZ
218800000000     C     OPZKEY        OREQ      OPDOP1
218900000000     C*
219000000000     C     OPDATT        IFEQ      'S'
219100000000     C                   MOVEL     OPDOPZ        OPZKEY
219200000000     C                   Z-ADD     I             W
219300000000     C                   END
219400000000     C                   END
219500000000     C*
219600000000     C                   END                                                    DO WHILE
219700000000     C*
219800000000     C* OPZIONE NON ATTIVA (ERRORE)
219900000000     C     W             IFEQ      0
220000000000     C*
220100000000     C* OPZIONE NON OK
220200000000     C                   Z-ADD     1             OPZOK
220300000000     C*
220400000000     C* EMISSIONE MESSAGGIO DI ERRORE
220500930127     C                   MOVEL     MSG(1)        DSMSMS
220600000000     C                   ELSE
220700000000     C*
220800000000     C* OPZIONE ATTIVA (PROCEDI ...)
220900000000     C*
221000930129     C     OPZKEY        IFEQ      ' 1'
221100000000     C                   EXSR      O01GES
221200000000     C                   END
221300000000     C     OPZKEY        IFEQ      ' 2'
221400000000     C                   EXSR      O02GES
221500000000     C                   END
221600000000     C     OPZKEY        IFEQ      ' 3'
221700000000     C                   EXSR      O03GES
221800000000     C                   END
221900000000     C     OPZKEY        IFEQ      ' 4'
222000020709     c                   If        §UteCun = *Blanks
222100020320     c                   eval      dsmsms = msg(11)
222200020320     c                   z-add     1             opzok
222300020320     c                   else
222400000000     C                   EXSR      O04GES
222500020320     c                   endif
222600000000     C                   END
222700000000     C     OPZKEY        IFEQ      ' 5'
222800000000     C                   EXSR      O05GES
222900000000     C                   END
223000000000     C     OPZKEY        IFEQ      ' 6'
223100000000     C                   EXSR      O06GES
223200000000     C                   END
223300930128     C     OPZKEY        IFEQ      ' 7'
223400930128     C                   EXSR      O07GES
223500930128     C                   END
223600930201     C     OPZKEY        IFEQ      ' 8'
223700930201     C                   EXSR      O08GES
223800930201     C                   END
223900140128     C     OPZKEY        IFEQ      ' R'
224000140128     C                   EXSR      O0RGES
224100140128     C                   END
224200160923     C     OPZKEY        IFEQ      ' S'
224300160923     c                   If        §UteCun = *Blanks
224400160923     c                   eval      dsmsms = msg(18)
224500160923     c                   z-add     1             opzok
224600160923     c                   else
224700160923     C                   EXSR      O0SGES
224800160923     c                   endif
224900160923     C                   END
225000000000     C*
225100000000     C                   END                                                    OPZ.NON ATTIVA
225200000000     C*
225300000000     C                   ENDSR
225400000000     C*================================================================
225500000000     C* NOTE PER COMPILAZIONE SCHIERE
225600000000     C*================================================================
225700000000     C* -MSG-
225800000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
225900000000     C* (NON TOCCARE MSG,1 E MSG,2
226000000000     C*
226100000000     C* -CMD-
226200000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
226300000000     C*
226400000000     C* -OPD-
226500000000     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
226600000000     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
226700000000     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
226800000000     C*    ESEMPIO: OPZIONI -1 -
226900000000     C*                     - 1-  SONO LA STESSA COSA
227000000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
227100000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
227200000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
227300000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
227400000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
227500000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
227600000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
227700000000     C*
227800000000     C* -FUD-
227900000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
228000000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
228100000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
228200000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
228300000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
228400000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
228500000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
228600000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
228700000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
228800000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
228900000000     C*
229000000000     C*================================================================
229100000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
229200941115TNSDU1R- Opzione non gestita.                                                  1
229300941115TNSDU1R- Tasto funzionale non gestito.                                         2
229400160923TNSDU1R- Utente non abilitato a questa funzione                                3
229500941115TNSDU1R- Unità annullata, non gestibile.    Consentito solo Ripristino.        4
229600941115TNSDU1R- Unità non annullata, impossibile ripristinare.                        5
229700941115TNSDU1R- Accettato solo un parametro di inizio lista.                          6
229800100906TNSDU1R- Tipo unità inesitente                                                 7
229900941115TNSDU1R- Magazzino ubicazione non accettabile.                                 8
230000941115TNSDU1R- Data non accettabile.                                                 9
230100020319TNSDU1R- Matricola non accettabile.                                           10
230200160923TNSDU1R- Utente non abilitato all'annullamento delle unità.
230300020325TNSDU1R- Matricola obbligatoria se richiesta ricerca per contenuto            12
230400020516TNSDU1R- Profilo non abilitato alla funzione                                  13
230500020516TNSDU1R- Profilo non abilitato al magazzino richiesto                         14
230600100906TNSDU1R- Modello inesistente                                                  15
230700100906TNSDU1R- Modello / Matricola inesisnte                                        16
230800100906TNSDU1R- Se ricerca per contenuto Matricola, non indicare il tipo unità       17
230900160923TNSDU1R- Utente non abilitato alla storicizzazione delle unità                18
231000941115TNSDU1R- 19                                                                   19
231100941115TNSDU1R- 20                                                                   20
231200000000** ======== SCHIERA: CMD  (COMANDI)                        ================
231300000000WRKSPLF
231400000000
231500000000
231600000000
231700000000
231800000000** ======== SCHIERA: OPD  OPZIONI GESTITE NEL PGM (ATTIVA=S) ==========
231900930312 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
232000930312 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
232100971015 3=Duplica         -3 -     (S/N)=N                        opz. 03¬
232200930312 4=Cancellazione   -4 -     (S/N)=N                        opz. 04¬
232300930312 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
232400000000                   -6 -     (S/N)=N                        opz. 06¬
232500930312 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
232600971013 8=Movimenti       -8 -     (S/N)=N                        opz. 08¬
232700140128 R=S/N Rientro     -R -     (S/N)=N                        opz. 09¬
232800160923 S=Storicizzazione -S -     (S/N)=N                        opz. 10¬
232900000000                   -  -     (S/N)=N                        opz. 11¬
233000000000                   -  -     (S/N)=N                        opz. 12¬
233100000000                   -  -     (S/N)=N                        opz. 13¬
233200000000                   -  -     (S/N)=N                        opz. 14¬
233300000000                   -  -     (S/N)=N                        opz. 15¬
233400000000                   -  -     (S/N)=N                        opz. 16¬
233500000000                   -  -     (S/N)=N                        opz. 17¬
233600000000                   -  -     (S/N)=N                        opz. 18¬
233700000000                   -  -     (S/N)=N                        opz. 19¬
233800000000                   -  -     (S/N)=N                        opz. 20¬
233900000000                   -  -     (S/N)=N                        opz. 21¬
234000000000                   -  -     (S/N)=N                        opz. 22¬
234100000000                   -  -     (S/N)=N                        opz. 23¬
234200000000                   -  -     (S/N)=N                        opz. 24¬
234300000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
234400930129                   -        (S/N)=N                        funz.01¬
234500930129                   -        (S/N)=N                        funz.02¬
234600930129F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
234700930129                   -        (S/N)=N                        funz.04¬
234800971013F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
234900930312F06=Inserimento    -        (S/N)=N                        funz.06¬
235000000000                   -        (S/N)=N                        funz.07¬
235100000000                   -        (S/N)=N                        funz.08¬
235200100906F09=Altri Dati     -        (S/N)=S  <== S-FISSO           funz.09¬
235300100922                   -        (S/N)=N                        funz.10¬
235400100922                   -        (S/N)=N                        funz.11¬
235500930129F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
235600000000                   -        (S/N)=N                        funz.13¬
235700930129                   -        (S/N)=N                        funz.14¬
235800930129                   -        (S/N)=N                        funz.15¬
235900930126                   -        (S/N)=N                        funz.16¬
236000000000F17=Inizio dati    -        (S/N)=S  <== S-FISSO           funz.17¬
236100000000F18=Fine dati      -        (S/N)=S  <== S-FISSO           funz.18¬
236200930128                   -        (S/N)=N                        funz.19¬
236300930129                   -        (S/N)=N                        funz.20¬
236400971013F21=Stampa         -        (S/N)=S                        funz.21¬
236500940105F22=Gestione stampe-        (S/N)=N                        funz.22¬
236600000000F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
236700000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
236800100922 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.RU¬
