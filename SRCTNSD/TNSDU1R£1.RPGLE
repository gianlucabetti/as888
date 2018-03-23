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
046800000000     C*================================================================
046900000000     C*  O06GES - GESTIONE OPZIONE 6=STAMPA
047000000000     C*================================================================
047100000000     C     O06GES        BEGSR
047200000000     C                   ENDSR
047300930128     C*================================================================
047400930128     C*  O07GES - GESTIONE OPZIONE 7=RIPRISTINO
047500930128     C*================================================================
047600930128     C     O07GES        BEGSR
047700930128     C*
047800930128     C* SE IL RECORD NON E' ANNULLATO NON CONSENTO IL RIPRISTINO
047900930201     C     VIDATB        IFNE      'A'
048000930128     C                   MOVEL     '1'           ERRL00
048100930201     C                   MOVEL     MSG(5)        DSMSMS
048200930201     C                   END
048300930201     C*
048400930201     C     ERRL00        IFEQ      '0'                                          --- 1 -->
048500930128     C*
048600930201     C* LANCIO IL PGM PER IL RIPRISTINO DI UN ANNULLAMENTO
048700930201     C                   MOVEL     'O07'         WOPZ1
048800941115     C                   MOVEL     'TNSDU2R'     WPGM
048900930201     C                   EXSR      LANPGM
049000930128     C*
049100930128     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
049200930707     C                   Z-ADD     1             SWMODI
049300930128     C*
049400930201     C                   END                                                    <-- 1 ---
049500930128     C*
049600930128     C                   ENDSR
049700930201     C*================================================================
049800940105     C*  O08GES - GESTIONE OPZIONE 8=GESTIONE MOVIMENTI
049900930201     C*================================================================
050000930201     C     O08GES        BEGSR
050100930201     C*
050200930201     C* LANCIO IL PGM PER LA GESTIONE DELLE LOCALITA'
050300971013     C     ERRL00        IFEQ      '0'                                          --- 1 -->
050400930201     C                   MOVEL     'O08'         WOPZ1
050500941115     C                   MOVEL     'TNSDU4R'     WPGM
050600930201     C                   EXSR      LANPGM
050700930201     C                   END                                                    <-- 1 ---
050800930201     C*
050900930201     C                   ENDSR
051000140128     C*================================================================
051100140128     C*  O0RGES - GESTIONE OPZIONE R=si/no rietnro da filiale
051200140128     C*================================================================
051300140128     C     O0RGES        BEGSR
051400140128     C*
051500140128     C* Aggiorno l'anagrafica con reos si/no
051600140128     C     KEYUNA        CHAIN     UNANA01L
051700140128     c                   if        %found(unana01l)
051800140128     c* se c'e' reso lo tolgo e viceversa
051900140128     c                   if        %subst(unalsw:15:1)= 'S'
052000140128     c                   eval      %subst(unalsw:15:1)= ' '
052100140128     c                   clear                   unadtr
052200140128     c                   else
052300140128     c                   eval      %subst(unalsw:15:1)= 'S'
052400140128     c     *iso          movel     datasys       UNADTR
052500140128     c                   endif
052600140128     c                   update    unana1
052700140128     c                   endif
052800140128     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
052900140128     C                   Z-ADD     1             SWMODI
053000140128     C*
053100140128     C                   ENDSR
053200000000     C*================================================================
053300000000     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
053400000000     C*================================================================
053500000000     C     NEWPOS        BEGSR
053600000000     C*
053700000000     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAM.
053800000000     C*         1= RICHIESTO POSIZIONAM.
053900000000     C                   Z-ADD     0             SWPOSI            7 0
054000000000     C*
054100000000     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
054200000000     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
054300000000     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
054400000000     C                   Z-ADD     0             SWMODI            1 0
054500000000     C*
054600000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
054700020319     c                   if        vdpmat <> depmat or vdptip <> deptip or      --- 1 -->
054800020319     c                             vdpmag <> depmag or vdpti2 <> depti2 or
054900020319     c                             vddsmd <> dedsmd or vddsma <> dedsma or
055000100906     c                             vdpric <> depric or depmod <> vdpmod or
055100100906     c                             vdpmat2<> depmat2 or vdsmag<> dspmag or
055200101029     c                             vdpdes<>  depdes  or vdsrief<>dsprief or
055300120411     c                             vdpnot<>  depnot  or vdsord<> dspord
055400930202     C*
055500930202     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI DI PARTENZA
055600930202     C                   EXSR      CHKPAR
055700930202     C*
055800930202     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
055900930202     C     ERRL00        IFEQ      '0'                                          --- 2 -->
056000000000     C                   Z-ADD     1             SWPOSI
056100120411     c                   eval      dspord = vdsord
056200120411     c                   eval      depmat = vdpmat
056300020325     c                   eval      depric = vdpric
056400020319     c                   eval      deptip = vdptip
056500020319     c                   eval      depmag = vdpmag
056600020319     c                   eval      depti2 = vdpti2
056700020319     c                   eval      dedsmd = vddsmd
056800020319     c                   eval      dedsma = vddsma
056900100907     c                   eval      depmod = vdpmod
057000100907     c                   eval      depmat2= vdpmat2
057100100907     c                   eval      dspmag = vdsmag
057200100907     c                   eval      dsprief= vdsrief
057300100907     c                   eval      depdes= vdpdes
057400101029     c                   eval      depnot= vdpnot
057500930202     C                   END                                                    <-- 2 ---
057600930202     C*
057700000000     C                   END                                                    <-- 1 ---
057800000000     C*
057900000000     C                   ENDSR
058000000000     C*================================================================
058100000000     C* LETTURA RECORD FILE DA CARICARE
058200000000     C*================================================================
058300000000     C     LETFIL        BEGSR
058400020319
058500020319     c                   select
058600020319
058700020319     c                   when      wtipri = 'TIP'
058800020319     c                   read      unana02l                               99
058900020319
059000020319     c                   when      wtipri = 'MAT'
059100020319     c                   read      unana07l                               99
059200020319
059300020319     c                   when      wtipri = 'MAG'
059400020319     c                   read      unana03l                               99
059500020319
059600020319     c                   when      wtipri = 'DSM'
059700020319     c                   read      unana04l                               99
059800020319
059900100906     c                   when      wtipri = 'MOD'
060000100906     c                   read      unana08l                               99
060100020319
060200020319     c                   endsl
060300020319
060400000000     C*
060500930201     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
060600930707     C     *IN99         IFEQ      '1'
060700000000     C                   Z-ADD     1             SFLEOF
060800930707     C                   END
060900000000     C*
061000000000     C                   ENDSR
061100000000     C*================================================================
061200000000     C* SETFIL - SETLL DEL FILE DA CARICARE
061300000000     C*================================================================
061400000000     C     SETFIL        BEGSR
061500020325
061600020319
061700020319      * POSIZIONAMENTO DEL FILE DA CARICARE E PRIMA LETTURA
061800020319
061900020319     c                   select
062000020319
062100020319     c                   when      wtipri = 'TIP'
062200020319     c                   move      vdptip        unatip
062300020319     c                   move      vdpmat        unamat
062400020319     c     kuna02        setll     unana02l
062500020319     c                   if        not %found(unana02l)
062600020319     c                   eval      sfleof = 1
062700020319     c                   else
062800020319     c                   read      unana02l
062900020319     c                   if        %eof(unana02l)
063000020319     c                   eval      sfleof = 1
063100020319     c                   endif
063200020319     c                   endif
063300020319
063400020319     c                   when      wtipri = 'MAT'
063500020325      * no ricerca per contenuto
063600020325     c                   if        vdpric = *blanks
063700020319     c                   move      vdpmat        unamat
063800020319     c                   move      vdptip        unatip
063900020319     c     kuna07        setll     unana07l
064000020319     c                   if        not %found(unana07l)
064100020319     c                   eval      sfleof = 1
064200020319     c                   else
064300020319     c                   read      unana07l
064400020319     c                   if        %eof(unana07l)
064500020319     c                   eval      sfleof = 1
064600020319     c                   endif
064700020319     c                   endif
064800020325      * si ricerca per contenuto
064900020325     c                   else
065000020325     c     *loval        setll     unana07l
065100020325     c                   if        not %found(unana07l)
065200020325     c                   eval      sfleof = 1
065300020325     c                   else
065400020325     c                   read      unana07l
065500020325     c                   if        %eof(unana07l)
065600020325     c                   eval      sfleof = 1
065700020325     c                   endif
065800020325     c                   endif
065900020325     c                   endif
066000020319
066100020319     c                   when      wtipri = 'MAG'
066200020319     c                   move      vdpmag        unamag
066300020319     c                   move      vdpti2        unatip
066400020319     c     kuna03        setll     unana03l
066500020319     c                   if        not %found(unana03l)
066600020319     c                   eval      sfleof = 1
066700020319     c                   else
066800020319     c                   read      unana03l
066900020319     c                   if        %eof(unana03l)
067000020319     c                   eval      sfleof = 1
067100020319     c                   endif
067200020319     c                   endif
067300020319
067400020319     c                   when      wtipri = 'DSM'
067500020319     c                   z-add     w_vddsmd      unadsm
067600020319     c     kuna04        setll     unana04l
067700020319     c                   if        not %found(unana04l)
067800020319     c                   eval      sfleof = 1
067900020319     c                   else
068000020319     c                   read      unana04l
068100020319     c                   if        %eof(unana04l)
068200020319     c                   eval      sfleof = 1
068300020319     c                   endif
068400020319     c                   endif
068500020319
068600100906     c                   when      wtipri = 'MOD'
068700100906     c                   movel     vdpmod        unamod
068800100906     c                   movel     vdpmat        unamat
068900100906     c     kuna08        setll     unana08l
069000100906     c                   if        not %found(unana08l)
069100100906     c                   eval      sfleof = 1
069200100906     c                   else
069300100906     c                   read      unana08l
069400100906     c                   if        %eof(unana08l)
069500100906     c                   eval      sfleof = 1
069600100906     c                   endif
069700100906     c                   endif
069800100906     c
069900100906     c**                 when      wtipri = 'DSG'
070000100906     c**                 z-add     w_vddsgd      unadsg
070100100906     c**   kuna06        setll     unana06l
070200100906     c**                 if        not %found(unana06l)
070300100906     c**                 eval      sfleof = 1
070400100906     c**                 else
070500100906     c**                 read      unana06l
070600100906     c**                 if        %eof(unana06l)
070700100906     c**                 eval      sfleof = 1
070800100906     c**                 endif
070900100906     c**                 endif
071000020319
071100020319     c                   endsl
071200020319
071300000000     C                   ENDSR
071400020319      *================================================================
071500020319      * CTRSFL - CONTROLLO SE RECORD DA SCRIVERE NEL SUBFILE
071600020319      *================================================================
071700020319     C     CTRSFL        BEGSR
071800020319
071900020325      * record letto ok
072000020325     c                   if        wnosfl = *off
072100020516
072200020516     c                   if        unamag <> 951
072300020523     c     unamag        lookup    skpoabi                                99
072400020516     c                   if        not *in99
072500020516     c                   eval      wnosfl = *on
072600020516     c                   endif
072700020516     c                   else
072800020516     c                   movel     unaksc        wmag
072900020516     c     wmag          lookup    skpoabi                                99
073000020516     c                   if        not *in99
073100020516     c                   eval      wnosfl = *on
073200020516     c                   endif
073300020516     c                   endif
073400020516     c                   endif
073500020325
073600020325     c                   if        wtipri = 'MAT' and vdpric = 'S'
073700020516     c                             and wnosfl = *off
073800020325     c     ' '           checkr    vdpmat        xx
073900020325     c     vdpmat:xx     scan      unamat                                 99
074000020325     c                   if        not *in99
074100020325     c                   eval      wnosfl = *on
074200020325     c                   endif
074300020325     c                   endif
074400020325
074500020516     c                   if        wnosfl = *off
074600020319     c                   if        (wtipri = 'DSM' and w_vddsma > *zeros
074700100906     c                                             and unadsm > w_vddsma)
074800020319     c                   eval      wnosfl = *on
074900020319     c                   endif
075000020516     c                   endif
075100100907     c
075200101029     c* Ricerca per contenuto descrizione
075300100907     c                   if        vdpdes<>*blanks
075400100907     c     ' '           checkr    vdpdes        xx
075500100907     c     vdpdes:xx     scan      unades                                 99
075600100907     c                   if        not *in99
075700100907     c                   eval      wnosfl = *on
075800100907     c                   endif
075900100907     c                   endif
076000101029     c* Ricerca per contenuto note
076100101029     c                   if        vdpnot<>*blanks
076200101029     c     ' '           checkr    vdpnot        xx
076300101029     c     vdpnot:xx     scan      unanot                                 99
076400101029     c                   if        not *in99
076500101029     c                   eval      wnosfl = *on
076600101029     c                   endif
076700101029     c                   endif
076800100907     c* Parzializza magazzino
076900100907     c                   if        vdsmag>0 and unamag<>vdsmag
077000100907     c                   eval      wnosfl = *on
077100100907     c                   endif
077200100907     c* Parzializza unità da rendere
077300101029     c***                if        vdsrief='SI' and %subst(unalsw:15:1)<>'S'
077400101029     c                   if        vdsrief>0    and %subst(unalsw:15:1)<>'S'
077500100907     c                   eval      wnosfl = *on
077600100907     c                   endif
077700101029     c                   if        vdsrief>0    and unadtr>w_vdsrief
077800101029     c                   eval      wnosfl = *on
077900101029     c                   endif
078000100907     c
078100120411     c* Parzializzo per numero ordine
078200120411     c                   if        vdsord>0
078300120411     c                   movel     vdsord        alford
078400120412     c                   if        alford<>%subst(unaemu:1:6)
078500120411     c                   eval      wnosfl = *on
078600120411     c                   endif
078700120411     c                   endif
078800120411     c
078900020319     C                   ENDSR
079000000000     C*================================================================
079100000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
079200000000     C*================================================================
079300000000     C     IRESFL        BEGSR
079400000000     C*
079500000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
079600930127     C*
079700930707     C                   MOVEL     UNAATB        VIDATB
079800930707     C                   Z-ADD     UNATIP        VIDTIP
079900930707     C                   Z-ADD     UNACOD        VIDCOD
080000930707     C                   MOVEL     UNAMAT        VIDMAT
080100930707     C                   MOVEL     UNAMAR        VIDMAR
080200120411     c                   clear                   vidord
080300120412     c                   if        %Subst(unaemu:1:6)>*zeros
080400120412     c                   eval      vidord=%int(%Subst(unaemu:1:6))
080500120411     c                   endif
080600120411     c
080700930707     C                   MOVEL     UNAMOD        VIDMOD
080800100906     C                   MOVEL     UNAdes        VIDdes
080900100906     c                   if        %Subst(unalsw:15:1)='S'
081000101027     c                   eval      vidrief='SI'
081100100906     c                   setoff                                       15
081200101027     c* Imposto la data da quando
081300101029     c                   clear                   viddalr
081400101027     c                   if        unadtr>0
081500101027     C     *iso          movel     UNADTR        dataiso
081600101027     c                   movel     dataiso       datadmy
081700101027     c                   movel     datadmy       viddalr
081800101027     c                   endif
081900101027     c
082000100906     c                   else
082100100906     c                   clear                   vidrief
082200101027     c                   clear                   viddalr
082300100906     c                   seton                                        15
082400100906     c                   endif
082500930707     C                   Z-ADD     UNAMAG        VIDMAG
082600100907     c**                 if        not *in94
082700971010     C                   Z-ADD     UNADSM        G08INV
082800971010     C                   Z-ADD     *ZEROS        G08DAT
082900971010     C                   MOVEL     '3'           G08ERR
083000971010     C                   CALL      'XSRDA8'
083100971010     C                   PARM                    WLBDA8
083200020319     C                   Z-ADD     G08DAT        VIDDat
083300100907     c**                 endif
083400100907     c**                 if        *in94
083500100907     C**                 Z-ADD     UNADSG        G08INV
083600100907     C**                 Z-ADD     *ZEROS        G08DAT
083700100907     C**                 MOVEL     '3'           G08ERR
083800100907     C**                 CALL      'XSRDA8'
083900100907     C**                 PARM                    WLBDA8
084000100907     C**                 Z-ADD     G08DAT        VIDDat
084100100907     c**                 endif
084200000000     C*
084300000000     C                   ENDSR
084400930202     C*================================================================
084500930202     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
084600930202     C*================================================================
084700930202     C     CHKPAR        BEGSR
084800020319
084900020319      * IMPOSTO FLAG EVIDENZAZIONE TIPO DI RICERCA
085000020319     c                   eval       *in90 = *off                                x tipo
085100020319     c                   eval       *in91 = *off                                x matricola
085200020319     c                   eval       *in92 = *off                                x magazzino
085300020319     c                   eval       *in93 = *off                                x scad.manutenzione
085400100906     c                   eval       *in94 = *off                                x modello
085500120411     c                   eval       *in95 = *off                                x num ordine
085600020319
085700020319      * IMPOSTO FLAG EVIDENZAZIONE ERRORI SUI PARAMETRI
085800020319     c                   eval       *in80 = *off                                tipo
085900020319     c                   eval       *in81 = *off                                tipo
086000020319     c                   eval       *in82 = *off                                matricola
086100020319     c                   eval       *in83 = *off                                magazzino
086200020319     c                   eval       *in84 = *off                                data sc.manut.dal
086300020319     c                   eval       *in85 = *off                                data sc.manut.al
086400100906     c                   eval       *in86 = *off                                modello
086500120411     c                   eval       *in87 = *off                                data rientro
086600020326
086700020326     c                   clear                   wtipri
086800020319
086900020319      * Se non è stata scelta una ricerca utilizzo la ricerca per tipo
087000020319     c                   if        vdpmat = *blanks and vdptip = *blanks and
087100020319     c                             vdpmag = *blanks and vdpti2 = *zeros  and
087200100906     c                             vddsmd = *zeros  and vddsma = *zeros  and
087300100906     c                             vdpmod=*blanks and vdpmat2=*blanks
087400020319     c                   eval      wtipri = 'TIP'
087500020319     c                   eval      *in90 = *on
087600020319     c                   endif
087700020319
087800020319      * ===> ACCETTO SOLO UN PARAMETRO DI INIZIO RICERCA
087900020319
088000020319      * Tipo unità / matricola
088100100906    1c                   if        vdptip <> *blanks
088200020319      * controllo tipo unità
088300020319     c                   exsr      chktip
088400020319     c                   eval      wtipri = 'TIP'
088500020319     c                   eval      *in90 = *on
088600100906    1c                   endif
088700020319
088800020319      * Matricola
088900100906    1c                   if        vdpmat <> *blanks and errl00 = *off and
089000020410     c                             vdptip = *blanks
089100020319     c                   eval      wtipri = 'MAT'
089200020319     c                   eval      *in91 = *on
089300100906    1c                   endif
089400100907      * ricerca x contenuto matricola
089500100906    1c                   if        errl00 = *off and vdpmat = *blanks and
089600020325     c                             vdpric = 'S'
089700020325     c                   eval      errl00 = *on
089800020325     c                   eval      dsmsms = msg(12)
089900100906     c                   leavesr
090000100906    1c                   endif
090100100906     c* non immettere in tipo
090200100906    1c                   if        vdptip<>*blanks  and vdpric='S'
090300100906     c                   eval      errl00 = *on
090400100906     c                   eval      dsmsms = msg(17)
090500100906     c                   leavesr
090600100906    1c                   endif
090700020319
090800020319      * Magazzino / tipo unità
090900100906    1c                   if        (vdpmag <> *blanks or vdpti2 > *zeros) and   --- 1 -->
091000020319     c                              errl00 = *off
091100100906    2c                   if        wtipri <> *blanks                            --- 2 -->
091200020319     c                   eval      errl00 = *on
091300020319     c                   eval      dsmsms = msg(6)
091400100906   x2c                   else                                                   <-- 2 -->
091500100907      * controllo magazzino obbligatorio se c'e' tipo
091600100906     c                   eval      ctrmag=vdpmag
091700020319     c                   exsr      chkmag
091800100906     c                   eval      vdpmag=ctrmag
091900100906     c
092000020319     c                   eval      wtipri = 'MAG'
092100020319     c                   eval      *in92 = *on
092200100906    2c                   endif                                                  <-- 2 ---
092300100906    1c                   endif                                                  <-- 1 ---
092400020319
092500020319      * Data scadenza manutenzione
092600100907    1c                   if        (vddsmd <> *zeros or vddsma <> *zeros) and   --- 1 -->
092700020319     c                              errl00 = *off
092800100907    2c                   if        wtipri <> *blanks                            --- 2 -->
092900020319     c                   eval      errl00 = *on
093000020319     c                   eval      dsmsms = msg(6)
093100100907   x2c                   else                                                   <-- 2 -->
093200020319      * controllo data 'dal'
093300100907    3c                   if        vddsmd = *zeros                              --- 3 -->
093400020319     c                   eval      errl00 = *on
093500020319     c                   eval      dsmsms = msg(9)
093600020319     c                   eval      *in84 = *on
093700100907    3c                   endif                                                  <-- 3 ---
093800020319     c                   clear                   w_vddsmd
093900020319     c                   clear                   wlbda8
094000020319     c                   z-add     vddsmd        g08dat
094100020319     c                   z-add     *zeros        g08inv
094200020319     c                   movel     '0'           g08err
094300020319     c                   call      'XSRDA8'
094400020319     c                   parm                    wlbda8
094500020319     c                   z-add     g08dat        vddsmd
094600020319     c                   z-add     g08inv        w_vddsmd
094700100907    3c                   if        g08err <> '0'                                --- 3 -->
094800020319     c                   eval      errl00 = *on
094900020319     c                   eval      *in84 = *on
095000020319     c                   eval      dsmsms = msg(9)
095100100907    3c                   endif                                                  <-- 3 ---
095200020319      * controllo data 'al'
095300100907    3c                   if        vddsma <> *zeros                             --- 3 -->
095400020319     c                   clear                   w_vddsma
095500020319     c                   clear                   wlbda8
095600020319     c                   z-add     vddsma        g08dat
095700020319     c                   z-add     *zeros        g08inv
095800020319     c                   movel     '0'           g08err
095900020319     c                   call      'XSRDA8'
096000020319     c                   parm                    wlbda8
096100020319     c                   z-add     g08dat        vddsma
096200020319     c                   z-add     g08inv        w_vddsma
096300100907    4c                   if        g08err <> '0'                                --- 4 -->
096400020319     c                   eval      errl00 = *on
096500020319     c                   eval      *in85 = *on
096600020319     c                   eval      dsmsms = msg(9)
096700100907    4c                   endif                                                  <-- 4 ---
096800100907    3c                   endif                                                  <-- 3 ---
096900020319     c                   eval      wtipri = 'DSM'
097000020319     c                   eval      *in93 = *on
097100100907    2c                   endif                                                  <-- 2 ---
097200100907    1c                   endif                                                  <-- 1 ---
097300100906     c
097400100906     c* Modello e matricola
097500100906    1c                   if        (vdpmod <> *blanks or vdpmat2<>*blanks) and   --- 1 -->
097600100906     c                              errl00 = *off
097700100906    2c                   if        wtipri <> *blanks                            --- 2 -->
097800100906     c                   eval      errl00 = *on
097900100906     c                   eval      dsmsms = msg(6)
098000100906   x2c                   else                                                   <-- 2 -->
098100100906     c     vdpmod        chain     unana08l
098200100906    3c                   if        not %found(unana08l)
098300100906     c                   eval      errl00 = *on
098400100906     c                   eval      *in86 = *on
098500100906     c                   eval      dsmsms = msg(15)
098600100906    3c                   endif                                                  <-- 4 ---
098700100906     c*
098800100906    3c                   if        errl00='0'
098900100906     c                   eval      wtipri = 'MOD'
099000100906     c                   eval      *in94 = *on
099100100906    3c                   endif                                                  <-- 4 ---
099200100906    2c                   endif                                                  <-- 4 ---
099300100906    1c                   endif                                                  <-- 4 ---
099400100907    c
099500100907      * Controllo parzializzazione magazzino
099600100907    1c                   if        vdsmag>0
099700100907     c                   eval      ctrmag=%editc(vdsmag:'X')
099800100907     c                   exsr      chkmag
099900100907    2c                   if        errl00='1'
100000100907     c                   leavesr
100100100907    2c                   endif
100200100907    1c                   endif
100300101029     c
100400101029     c* Controllo data rientro prima del
100500101029     c                   clear                   w_vdsrief
100600101029     c                   if        vdsrief>0
100700101029     c                   clear                   wlbda8
100800101029     c                   z-add     vdsrief       g08dat
100900101029     c                   z-add     *zeros        g08inv
101000101029     c                   movel     '0'           g08err
101100101029     c                   call      'XSRDA8'
101200101029     c                   parm                    wlbda8
101300101029     c                   z-add     g08dat        vdsrief
101400101029     c                   z-add     g08inv        w_vdsrief
101500101029    3c                   if        g08err <> '0'                                --- 3 -->
101600101029     c                   eval      errl00 = *on
101700101029     c                   eval      *in87 = *on
101800101029     c                   eval      dsmsms = msg(9)
101900101029    3c                   endif                                                  <-- 3 ---
102000101029    3c                   endif                                                  <-- 3 ---
102100020326
102200020326      * SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
102300020326      * CURSORE SUL CAMPO DEL CONTROL
102400020326     C     errl00        IFEQ      '1'
102500020326     C                   MOVEL     '0'           *IN43                          *NON EMETTO SFL
102600020326     C                   ELSE
102700020326     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
102800020326     C                   END
102900020319
103000930202     C                   ENDSR
103100930707     C*================================================================
103200930707     C*  CHKTIP - CONTROLLA IL TIPO DI UNITA
103300930707     C*================================================================
103400930707     C     CHKTIP        BEGSR
103500020318
103600020318      * CONTROLLO SE E' STATO INSERITO '?'
103700020319     C     '?'           SCAN      VDPTIP                                 99
103800020318
103900020318      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
104000020319     C                   if        *in99                                        --- 1 -->
104100020318     C                   clear                   TIBS02DS
104200020318     C                   movel     'R'           t02mod
104300020318     C                   movel     knsif         t02sif
104400020321     C                   movel     'UNT'         t02cod
104500020318     C                   call      'TIBS02R'
104600020318     C                   parm                    KPJBA
104700020318     C                   parm                    TIBS02DS
104800020318     C                   movel     T02ke1        vdptip
104900020318
105000020319     C                   endif                                                  <-- 1 ---
105100020318
105200020318      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
105300020318     C                   clear                   TIBS02DS
105400020318     C                   movel     'C'           T02mod
105500020318     C                   movel     knsif         t02sif
105600020321     C                   movel     'UNT'         t02cod
105700020318     C                   movel(p)  vdptip        T02ke1
105800020318     C                   call      'TIBS02R'
105900020318     C                   parm                    KPJBA
106000020318     C                   parm                    TIBS02DS
106100020318     C                   if        t02err <> *blanks
106200020319     C                   eval      errl00 = '1'
106300020319     C                   eval      *in80 = *on
106400020318     C                   movel     msg(7)        dsmsms
106500020318     C                   endif
106600020319
106700930707     C                   ENDSR
106800930707     C*================================================================
106900930707     C*  CHKMAG - CONTROLLA IL MAGAZZINO UBICAZIONE
107000930707     C*================================================================
107100930707     C     CHKMAG        BEGSR
107200930707     C*
107300930707     C* CONTROLLO SE E' STATO INSERITO '?'
107400100906     C     '?'           SCAN      ctrmag                                 99
107500930707     C*
107600930707     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
107700930707     C     *IN99         IFEQ      '1'                                          --- 1 -->
107800930707     C                   MOVEL     *BLANKS       §COD1             3
107900930707     C                   MOVE      *BLANKS       §DES1
108000941115     C                   CALL      'TNSD20R'
108100930707     C                   PARM                    §COD1
108200930707     C                   PARM                    §TIP              1
108300930707     C                   PARM                    §DES1            25
108400100906     C                   MOVEL     §COD1         ctrmag
108500930707     C                   END                                                    <-- 1 ---
108600930707     C*
108700930707     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
108800100906     C                   MOVE      ctrmag        ORGFIL
108900020529     C     KEYORG        CHAIN     AZORG21l                           99
109000930707     C     *IN99         IFEQ      '0'                                          --- 2 -->
109100100906     C                   MOVEL     ORGFIL        ctrmag
109200930707     C                   ELSE                                                   <-- 2 -->
109300020529     c     keyorg        chain     azorg01l
109400020529     c                   if        %found(azorg01l)
109500100906     c                   movel     orgfil        ctrmag
109600020529     c                   else
109700930707     C                   MOVEL     '1'           ERRL00
109800930707     C                   MOVEL     MSG(8)        DSMSMS
109900930707     C                   SETON                                        83
110000020529     c                   endif
110100930707     C                   END                                                    <-- 2 ---
110200020516      * l'utente deve essere abilitato al magazzino inserito
110300020516     c                   if        vdpmag <> '951'
110400100906     c                   movel     ctrmag        wmag
110500020523     c     wmag          lookup    skpoabi                                99
110600020516     c                   if        not *in99
110700020516     c                   eval      errl00 = *on
110800020516     c                   eval      dsmsms = msg(14)
110900020516     c                   eval      *in83 = *on
111000020516     c                   endif
111100020516     c                   endif
111200930707     C*
111300930707     C                   ENDSR
111400020516      *------------------------------------------------------------------------*
111500020516      *  CTRUTE - ROUTINE CONTROLLO UTENTE
111600020516      *------------------------------------------------------------------------*
111700020516     c     ctrute        begsr
111800020516
111900020516     c                   eval      *in76 = *off
112000020516
112100020516      * controllo se il profilo è abilitato
112200020516     c                   if        duterr = 'E'
112300020516     c                   eval      dsmsms = dutmsg
112400020516     c                   eval      *in76 = *on
112500020516     c                   endif
112600020516      * controllo le abilitazionei del profilo
112700020516     c                   if        *in76 = *off
112800020516     c                   movel     utefaf        dute01
112900020516     c                   if        §uteiun <> *blanks
113000020516     c                   eval      wabi = §uteiun
113100020516     c                   else
113200020516     c                   eval      wabi = uteaut
113300020516     c                   endif
113400020516     c                   clear                   tibs02ds
113500020516     c                   movel     'C'           t02mod
113600020516     c                   movel     knsif         t02sif
113700020516     c                   movel     'LAT'         t02cod
113800020516     c                   movel(p)  wabi          t02ke1
113900020516     c                   call      'TIBS02R'
114000020516     c                   parm                    kpjba
114100020516     c                   parm                    tibs02ds
114200020516     c                   movel     t02uni        dlat
114300020516     c                   if        §latabi = 'S'
114400020516     c                   eval      dsmsms= msg(13)
114500020516     c                   eval      *in76 = *on
114600020516     c                   endif
114700020516     c                   endif
114800020516
114900020516      * Utente ok richiamo pgm che ritorna i p.o. abilitati
115000020523     c                   if        *in76 = *off  and wabi <> 'AZ'
115100020516     c                   clear                   trul31ds
115200020516     c                   eval      i31abi = wabi
115300020516     c                   eval      i31cdi = dutdis
115400020516     c                   eval      i31car = dutare
115500020516     c                   eval      i31cpo = dutpou
115600020516     c                   call      'TRUL31R'
115700020516     c                   parm                    kpjba
115800020516     c                   parm                    trul31ds
115900020516     c                   endif
116000020517
116100090904      /free
116200090904
116300090904         SELECT;
116400090904
116500090904           // - Reperimento filiali gestibili per utente della LOGISTICA
116600090904           WHEN  DUTntw = c_Logistica;
116700090904
116800090904             setll  *loval  AZORG;
116900090904             read   AZORG;
117000090904
117100090904             DoW  Not  %eof(AZORG01L);
117200090904
117300090904               if  ORGfva = *blank;
117400090904
117500090904                 Og143 = ORGde3;
117600090904                 if  §OgNtw = DUTntw;
117700090904
117800090904                   ii += 1;
117900090904                   skPOabi(ii) = ORGfil;
118000090904
118100090904                 endif;
118200090904
118300090904               endif;
118400090904
118500090904               read   AZORG;
118600090904
118700090904             EndDo;
118800090904
118900090904      /end-free
119000090904
119100020529      * per abilitazione 'AZ' devo caricare tutti i p.o. di azorg21l
119200090904     c                   WHEN      wabi = 'AZ'
119300020529     c     *loval        setll     azorg21l
119400020517     c                   do        *hival
119500020529     c                   read      azorg21l
119600020517     c                   if        %eof
119700020517     c                   leave
119800020517     c                   endif
119900020517     c                   z-add     1             ii
120000020523     c     orgfil        lookup    skpoabi(ii)                            99
120100020517     c                   if        not *in99
120200020517     c                   z-add     1             ii
120300020523     c     *zeros        lookup    skpoabi(ii)                            99
120400020517     c                   if        *in99
120500020517     c                   movel     orgfil        skpoabi(ii)
120600020517     c                   endif
120700020517     c                   endif
120800020517     c                   enddo
120900020529      * e tutti i p.o. di azorg01l
121000020529     c     *loval        setll     azorg01l
121100020529     c                   do        *hival
121200020529     c                   read      azorg01l
121300020529     c                   if        %eof
121400020529     c                   leave
121500020529     c                   endif
121600020529     c                   z-add     1             ii
121700020529     c     orgfil        lookup    skpoabi(ii)                            99
121800020529     c                   if        not *in99
121900020529     c                   z-add     1             ii
122000020529     c     *zeros        lookup    skpoabi(ii)                            99
122100020529     c                   if        *in99
122200020529     c                   movel     orgfil        skpoabi(ii)
122300020529     c                   endif
122400020529     c                   endif
122500020529     c                   enddo
122600090904      *
122700020523      * se non è abilitazione 'AZ' passo la sk tornata dal TRUL31 nella sk di 500 elementi
122800090904     c                   OTHER
122900020523     c                   movea     skpo          skpoabi
123000020523     c                   z-add     1             ii
123100020523     c     951           lookup    skpoabi(ii)                            99
123200020523     c                   if        not *in99
123300020523     c                   z-add     1             ii
123400020523     c     *zeros        lookup    skpoabi(ii)                            99
123500020523     c                   if        *in99
123600021014     c                   movel     951           skpoabi(ii)
123700020523     c                   endif
123800020523     c                   endif
123900090904      *
124000090904     c                   ENDSL
124100020516
124200020516     c                   endsr
124300930201     C*================================================================
124400930201     C* LANPGM - LANCIA UN PROGRAMMA
124500930201     C*================================================================
124600930201     C     LANPGM        BEGSR
124700930201     C*
124800930201     C* IMPOSTO PARAMETRI PER IL PGM
124900930707     C                   MOVEL     '0'           DSUF03                         PREMUTO F03
125000930707     C                   MOVEL     '0'           DSUERR                         '1'=ERRORE
125100930707     C                   MOVEL     *BLANKS       DSUMSG                         MESSAGGIO
125200930707     C                   MOVE      WOPZ1         DSUOP1                         *OPZIONE LIV.PGM
125300020322     C                   Z-ADD     vidtip        DSUTIP                         TIPO
125400930707     C                   Z-ADD     VIDCOD        DSUCOD                         CODICE
125500930712     C                   Z-ADD     *ZEROS        DSUMAG                         MAGAZZINO
125600930712     C                   Z-ADD     *ZEROS        DSUPRG                         PROGRESSIVO MOV.
125700930906     C                   Z-ADD     *ZEROS        DSUDTM                         DATA MOVIMENTO
125800930201     C*
125900930201     C* LANCIO PROGRAMMA
126000940105     C                   MOVEL     DSUNIT        KPJBU
126100930312     C                   CALL      WPGM
126200940105     C                   PARM                    KPJBA
126300940105     C                   MOVEL     KPJBU         DSUNIT
126400930201     C*
126500930201     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
126600930707     C     DSUF03        IFEQ      '1'
126700930201     C                   MOVEL     '1'           F03L00
126800930201     C                   MOVEL     '1'           ERRL00
126900930201     C                   ELSE
127000930201     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
127100930707     C                   MOVEL     DSUMSG        DSMSMS
127200930201     C                   END
127300930201     C*
127400930201     C                   ENDSR
127500930312     C*================================================================
127600930312     C* CHKFOP - CONTROLLA TIPO GESTIONE E IMPOSTA FUNZ/OPZ
127700930312     C*================================================================
127800930312     C     CHKFOP        BEGSR
127900930312     C*
128000930312     C* CONTROLLO CHE VI SIA UNA OPZIONE DI PROCEDURA VALIDA IN INPUT
128100930312     C* ALTRIMENTI FORZO INTERROGAZIONE
128200940105     C     DSUOP0        IFNE      ' 2'                                         *GESTIONE
128300940105     C     DSUOP0        ANDNE     ' 5'                                         *INTERROGAZIONE
128400070509     C     DSUOP0        ANDNE     ' 1'                                         *INTERROGAZIONE
128500940105     C                   MOVE      ' 5'          DSUOP0
128600930312     C                   END
128700070509     C*
128800070509     C* SE IL PGM E' STATO LANCIATO IN SELEZIONE ABILITO SOLO LA "1"
128900070509     C     DSUOP0        IFEQ      ' 1'
129000070509     C                   MOVEL     OPD(1)        OPDRIG                         *INTERROGAZIONE
129100070509     C                   MOVEL     'S'           OPDATT
129200070509     C                   MOVEL     OPDRIG        OPD(1)
129300070509     C                   ENDIF
129400930312     C*
129500930312     C* SE L'UTENTE E' ENTRATO IN GESTIONE LO ABILITO A TUTTE LE FUNZIONI
129600940105     C     DSUOP0        IFEQ      ' 2'
129700930312     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
129800930312     C                   MOVEL     'S'           OPDATT
129900930312     C                   MOVEL     OPDRIG        OPD(2)
130000971015     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
130100971015     C                   MOVEL     'S'           OPDATT
130200971015     C                   MOVEL     OPDRIG        OPD(3)
130300930312     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
130400930312     C                   MOVEL     'S'           OPDATT
130500930312     C                   MOVEL     OPDRIG        OPD(4)
130600930312     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
130700930312     C                   MOVEL     'S'           OPDATT
130800930312     C                   MOVEL     OPDRIG        OPD(5)
130900930312     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
131000930312     C                   MOVEL     'S'           OPDATT
131100930312     C                   MOVEL     OPDRIG        OPD(7)
131200971015     C                   MOVEL     OPD(8)        OPDRIG                         *MOVIMENTI
131300971015     C                   MOVEL     'S'           OPDATT
131400971015     C                   MOVEL     OPDRIG        OPD(8)
131500140128     C                   MOVEL     OPD(9)        OPDRIG                         *RIPRISTINO
131600140128     C                   MOVEL     'S'           OPDATT
131700140128     C                   MOVEL     OPDRIG        OPD(9)
131800930312     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
131900930312     C                   MOVEL     'S'           FUDATT
132000930312     C                   MOVEL     FUDRIG        FUD(6)
132100930312     C                   END
132200930312     C*
132300930312     C* SE IL PGM E' STATO LANCIATO IN INTERROGAZIONE ABILITO SOLO QUELLA
132400940105     C     DSUOP0        IFEQ      ' 5'
132500930312     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
132600930312     C                   MOVEL     'S'           OPDATT
132700930312     C                   MOVEL     OPDRIG        OPD(5)
132800971015     C                   MOVEL     OPD(8)        OPDRIG                         *MOVIMENTI
132900971015     C                   MOVEL     'S'           OPDATT
133000971015     C                   MOVEL     OPDRIG        OPD(8)
133100930312     C                   END
133200930312     C*
133300930312     C* SE SONO IN FILIALE NON CONCEDO CANCELLAZIONE,INSERIMENTO,RIPRISTINO
133400020709     c                   If        DutLpo <> 'S'
133500971015     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
133600971015     C                   MOVEL     'N'           OPDATT
133700971015     C                   MOVEL     OPDRIG        OPD(3)
133800930312     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
133900930312     C                   MOVEL     'N'           OPDATT
134000930312     C                   MOVEL     OPDRIG        OPD(4)
134100930312     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
134200930312     C                   MOVEL     'N'           OPDATT
134300930312     C                   MOVEL     OPDRIG        OPD(7)
134400930312     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
134500930312     C                   MOVEL     'N'           FUDATT
134600930312     C                   MOVEL     FUDRIG        FUD(6)
134700930312     C                   END
134800930312     C*
134900930312     C* IMPOSTO INDICE PER EMISSIONE OPZIONI A VIDEO
135000930312     C                   Z-ADD     0             OPZGRU            5 0
135100930312     C*
135200930312     C* GESTIONE OPZIONI A VIDEO
135300930312     C                   EXSR      OPZGES
135400930312     C*
135500930312     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
135600930312     C                   Z-ADD     0             FUNGRU            5 0
135700930312     C*
135800930312     C* GESTIONE TASTI FUNZIONALI A VIDEO
135900930312     C                   EXSR      FUNGES
136000930312     C*
136100930312     C                   ENDSR
136200000000     C*================================================================
136300000000     C*  RUTINZ - ROUTINE INIZIALE
136400000000     C*================================================================
136500000000     C     RUTINZ        BEGSR
136600000000     C*
136700930707     C* RICEVIMENTO PARAMETRI DALL'AZIONE (' 2'=GEST.,' 5'=INTERR.)
136800000000     C     *ENTRY        PLIST
136900930707     C                   PARM                    KPJBA
137000070509     c                   parm                    UNANAds
137100940105     C                   MOVEL     KPJBU         DSUNIT
137200070509      * - impostazione iniziale dei parametri di output per chiamante
137300070509     c                   eval      DSUf03 = *off                                PREMUTO F03
137400070509     c                   eval      DSUerr = *off                                '1'=ERRORE
137500070509     c                   eval      DSUmsg = *blanks                             MESSAGGIO
137600100927     c**
137700100927     c**                 if        SDSprm > 1
137800100927     c* Se richiamo per selezione pulisco al DS
137900100924     c* altrimenti vedo se il campo MATRICOLA è pieno per la ricerca
138000100927     c**                 if        dsuop0=' 1'
138100100927     c**                 clear                   UNANAds
138200100927     c**                 endif
138300100927     c**                 endif
138400020516
138500020516     c     *dtaara       define    §azute        azuteds
138600020516     c     *dtaara       define    §datiute      ddatiute
138700020516
138800020516     c                   in(E)     *dtaara
138900020516     c                   if        %Error or rsut = *blanks
139000020516     c                   clear                   tibs34ds
139100020516     c                   call      'TIBS34R'
139200020516     c                   parm                    Tibs34Ds
139300020516     c                   in        *dtaara
139400020516     c                   endif
139500020709
139600020709      * controllo il profilo utente
139700020709     c                   exsr      ctrute
139800020709     c                   if        *in76
139900020709     c                   eval      sflult = 0
140000020709     c                   endif
140100020516
140200930204     C*
140300930204     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
140400930204     C     XMSGPA        PLIST
140500930204     C                   PARM                    DSMSMS
140600930204     C                   PARM                    DSMSPG
140700930204     C                   PARM                    DSMSKE
140800930204     C                   PARM                    DSMSER
140900020319
141000020319      * CHIAVE LETTURA UNANA02L
141100930707     C     KUNA02        KLIST
141200020319     C                   KFLD                    UNATIP
141300020319     C                   KFLD                    UNAMAT
141400140128     C     KEYUNA        KLIST
141500140128     c                   kfld                    vidtip
141600140128     C                   KFLD                    vidCOD
141700020319
141800020319      * CHIAVE LETTURA UNANA03L
141900930707     C     KUNA03        KLIST
142000930707     C                   KFLD                    UNAMAG
142100930707     C                   KFLD                    UNATIP
142200020319
142300020319      * CHIAVE LETTURA UNANA04L
142400930707     C     KUNA04        KLIST
142500930707     C                   KFLD                    UNADSM
142600020319
142700020319      * CHIAVE LETTURA UNANA07L
142800020319     C     KUNA07        KLIST
142900020319     C                   KFLD                    UNAMAT
143000020319     C                   KFLD                    UNATIP
143100100906     C     KUNA08        KLIST
143200100906     C                   KFLD                    UNAmod
143300100906     C                   KFLD                    UNAmat
143400020319
143500020529     C* CHIAVE LETTURA AZORG21l/azorg01l - COMPLETA
143600930707     C     KEYORG        KLIST
143700930707     C                   KFLD                    ORGFIL
143800930707     C*
143900000000     C*
144000930129     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
144100930204     C                   CALL      'XKEY'
144200930204     C                   PARM                    DSKEY
144300000000     C*
144400000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
144500000000     C     *LIKE         DEFINE    SFLNBR        SFLPAG
144600000000     C     *LIKE         DEFINE    SFLPAG        WFLPAG
144700000000     C     *LIKE         DEFINE    SFLNBR        SFLEOF
144800000000     C     *LIKE         DEFINE    SFLNBR        SFLERR
144900000000     C     *LIKE         DEFINE    SFLNBR        SFLULT
145000000000     C     *LIKE         DEFINE    SFLNBR        WFLULT
145100000000     C     *LIKE         DEFINE    SFLNBR        MSGULT
145200000000     C     *LIKE         DEFINE    SFLNBR        SFLRIG
145300000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
145400000000     C*
145500930127     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
145600000000     C*   -NOME PGM CHIAMANTE
145700000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
145800941115     C                   MOVEL     'TNSDU1R'     DSMSPG
145900941115     C                   MOVEL     'TNSDU1R'     MSGQUE
146000000000     C                   MOVEL     *BLANKS       MSGKEY
146100930127     C                   MOVEL     *BLANKS       DSMSMS
146200930204     C                   MOVEL     *BLANKS       DSMSKE
146300930204     C                   Z-ADD     0             DSMSER
146400000000     C*
146500930129     C* INIZIALIZZO SWITCH DI LIVELLO 0 (PGM)
146600930127     C                   MOVEL     '0'           F12L00            1
146700930127     C                   MOVEL     '0'           F03L00            1
146800930127     C                   MOVEL     '0'           FINL00            1
146900930127     C                   MOVEL     '0'           ERRL00            1
147000930128     C                   MOVEL     '0'           FLFINE            1
147100930127     C*
147200930312     C* IMPOSTO DEPOSITI PER CONFRONTI
147300930707     C                   MOVEL     *BLANKS       DEPTIP
147400930707     C                   MOVEL     *BLANKS       DEPMAT
147500020325     c                   clear                   depric
147600930707     C                   MOVEL     *BLANKS       DEPMAG
147700100907     C                   MOVEL     *BLANKS       DspMAG
147800100907     C                   MOVEL     *BLANKS       Dsprief
147900100907     C                   MOVEL     *BLANKS       depmod
148000100907     C                   MOVEL     *BLANKS       depmat2
148100020319     C                   Z-ADD     *ZEROS        DEPTI2
148200020319     C                   Z-ADD     *ZEROS        dedsmd
148300020319     C                   Z-ADD     *ZEROS        dedsma
148400101029     c                   clear                   depdes
148500101029     c                   clear                   depnot
148600930202     C*
148700941128     C* IMPOSTO TIPO DI RICERCA DI DEFAULT --> MAT
148800020319     C                   MOVEL     'TIP'         WTIPRI
148900020319     c                   eval      *in90 = *on
149000100924     c* se passata matricola eseguo ricerca
149100100927     c                   if        SDSprm > 1  and
149200100924     c                             UIMmat<>*blanks
149300100924     c                   movel     UIMmat        vdpmat
149400100924     c                   movel     UIMmat        depmat
149500100927     c                   clear                   unanaDS
149600100924     c                   exsr      chkpar
149700100924     c                   endif
149800000000     C*
149900000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
150000000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
150100020325     C                   Z-ADD     10            SFLPAG
150200930312     C*
150300930312     C* IMPOSTO LE OPZIONI E LE FUNZIONI A VIDEO
150400930312     C                   EXSR      CHKFOP
150500930225     C*
150600930225     C* IMPOSTO INTERROGAZIONE/GESTIONE NELLA TESTATA DELLA VIDEATA
150700070509     c                   select
150800070509     c                   when      DSUop0 = ' 1'
150900070509     C                   eval      VIDing = '   SELEZIONE    '
151000070509     c                   when      DSUop0 = ' 2'
151100930225     C                   MOVEL     '    GEST'    VIDING
151200930225     C                   MOVE      'IONE    '    VIDING
151300070509     c                   other
151400930225     C                   MOVEL     ' INTERRO'    VIDING
151500930225     C                   MOVE      'GAZIONE '    VIDING
151600070509     c                   endsl
151700020325
151800000000     C                   ENDSR
151900020326
152000930312     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
152100930312     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
152200930312     C* LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
152300930312     C* STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
152400930312     C* NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
152500930312     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
152600930312     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
152700930312     C*
152800930312     C*================================================================
152900930312     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA
153000930312     C*          CARICARE
153100930312     C*================================================================
153200930312     C     POSSFL        BEGSR
153300930312     C*
153400930312     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
153500930312     C                   EXSR      CLRSFL
153600930312     C*
153700930312     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
153800930312     C                   EXSR      SETFIL
153900930312     C*
154000930312     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
154100930312     C     SFLEOF        IFEQ      0
154200020326     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
154300930312     C                   EXSR      CARSFL
154400930312     C                   END
154500930312     C*
154600930312     C                   ENDSR
154700000000     C*================================================================
154800000000     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
154900000000     C*================================================================
155000000000     C     ENTATT        BEGSR
155100000000     C*
155200000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
155300000000     C                   Z-ADD     0             OPZOK             1 0
155400000000     C*
155500000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
155600000000     C                   Z-ADD     0             SFLVAR            5 0
155700000000     C*
155800000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
155900000000     C                   Z-ADD     0             SFLERR
156000000000     C*
156100000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
156200000000     C                   Z-ADD     SFLULT        WFLULT
156300000000     C*
156400000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
156500941115     C                   READC     SDU1D05                                98
156600930202     C     *IN98         DOWEQ     '0'
156700000000     C*
156800000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
156900000000     C                   ADD       1             SFLVAR
157000000000     C*
157100000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
157200000000     C     OPZOK         IFEQ      0
157300000000     C*
157400000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
157500000000     C     OPZKEY        IFNE      *BLANKS
157600000000     C                   EXSR      OPZCON
157700000000     C                   END
157800000000     C*
157900000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
158000000000     C*            2. PULIZIA SCELTA IN SFL
158100000000     C     OPZOK         IFEQ      0
158200000000     C                   MOVEL     '0'           *IN41
158300000000     C                   MOVEL     *BLANKS       OPZKEY
158400000000     C                   ELSE
158500000000     C*
158600000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
158700000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
158800000000     C                   MOVEL     '1'           *IN41
158900000000     C                   Z-ADD     SFLULT        SFLERR
159000000000     C                   END
159100000000     C                   ELSE
159200000000     C*
159300000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
159400000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
159500000000     C                   MOVEL     '1'           *IN41
159600000000     C                   END                                                    OPZOK=0
159700000000     C*
159800000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
159900000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
160000000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
160100000000     C     OPZOK         IFNE      0
160200000000     C                   Z-ADD     SFLERR        SFLNBR
160300000000     C                   ELSE
160400000000     C                   Z-ADD     SFLULT        SFLNBR
160500000000     C                   END
160600000000     C*
160700000000     C* AGGIORNO RECORD SFL
160800101027     c                   if        vidrief='SI'
160900100906     c                   setoff                                       15
161000100906     c                   else
161100100906     c                   seton                                        15
161200100906     c                   endif
161300100906     c
161400941115     C                   UPDATE    SDU1D05
161500000000     C*
161600941115     C                   READC     SDU1D05                                98
161700000000     C                   END                                                    DO WHILE
161800000000     C*
161900000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
162000000000     C                   Z-ADD     WFLULT        SFLULT
162100000000     C*
162200000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
162300000000     C     SFLVAR        IFEQ      0
162400000000     C                   MOVEL     '1'           FINL00
162500000000     C                   END
162600000000     C*
162700000000     C                   ENDSR
162800000000     C*================================================================
162900000000     C* ELIMINAZIONE CONTENUTO SFL
163000000000     C*================================================================
163100000000     C     CLRSFL        BEGSR
163200000000     C*
163300000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
163400000000     C* (0=NON FINE FILE  1=FINE FILE)
163500000000     C                   Z-ADD     0             SFLEOF
163600000000     C*
163700000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
163800000000     C                   Z-ADD     0             SFLULT
163900000000     C*
164000000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
164100000000     C                   MOVEL     '1'           *IN40
164200941115     C                   WRITE     SDU1D06
164300000000     C                   MOVEL     '0'           *IN40
164400000000     C*
164500000000     C                   ENDSR
164600000000     C*================================================================
164700000000     C* CARSFL - CARICAMENTO SFL
164800000000     C*================================================================
164900000000     C     CARSFL        BEGSR
165000000000     C*
165100000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
165200000000     C     SFLULT        IFEQ      0
165300000000     C                   EXSR      SETFIL
165400000000     C                   END
165500000000     C*
165600000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
165700000000     C                   Z-ADD     SFLULT        WFLULT
165800000000     C*
165900000000     C* CICLO:
166000000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
166100000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
166200000000     C                   Z-ADD     1             SFLRIG
166300000000     C     SFLRIG        DOWLE     SFLPAG
166400000000     C     SFLEOF        ANDEQ     0
166500020319
166600020319     c                   eval      wnosfl = *off
166700020319
166800000000     C*
166900000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
167000000000     C                   MOVEL     *BLANKS       OPZKEY
167100020319
167200020326      * controllo se devo scrivere il record nel subfile
167300020319     c                   exsr      ctrsfl
167400020319
167500020319     c                   if        wnosfl = *off
167600000000     C*
167700000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
167800000000     C                   EXSR      IRESFL
167900000000     C*
168000000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
168100000000     C                   ADD       1             SFLULT
168200000000     C*
168300000000     C* SCRITTURA RECORD SFL
168400941115     C                   WRITE     SDU1D05
168500000000     C                   ADD       1             SFLRIG
168600020319
168700020319     c                   endif
168800000000     C*
168900000000     C* LETTURA FILE DA CARICARE
169000000000     C                   EXSR      LETFIL
169100000000     C                   END                                                    DO WHILE
169200000000     C*
169300000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
169400000000     C     WFLULT        IFNE      SFLULT
169500000000     C                   Z-ADD     WFLULT        SFLNBR
169600000000     C                   ADD       1             SFLNBR
169700000000     C                   END
169800000000     C*
169900000000     C                   ENDSR
170000000000     C*================================================================
170100000000     C* EMISSIONE SUBFILE A VIDEO
170200000000     C*================================================================
170300000000     C     OUTSFL        BEGSR
170400000000     C*
170500000000     C* WRITE RECORD TESTATA VIDEO
170600941115     C                   WRITE     SDU1D01
170700930205     C*
170800930205     C* VISUALIZZAZIONE MESSAGGIO
170900930205     C                   EXSR      SNDMSG
171000000000     C*
171100000000     C* WRITE RECORD OPZIONI VIDEO
171200941115     C                   WRITE     SDU1D02
171300000000     C*
171400000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
171500000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
171600000000     C     SFLEOF        IFEQ      0
171700000000     C                   MOVEL     '0'           *IN42
171800000000     C                   ELSE
171900000000     C*
172000000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
172100000000     C                   MOVEL     '1'           *IN42
172200000000     C                   END
172300000000     C*
172400000000     C* WRITE RECORD TASTI FUNZIONALI
172500941115     C                   WRITE     SDU1D09
172600000000     C*
172700000000     C* CASO DI SFL VUOTO
172800000000     C     SFLULT        IFLE      0
172900000000     C*
173000000000     C*    - WRITE RECORD VUOTO
173100020326     C                   MOVEL     '0'           *IN43                          *NON VISUALIZZA SFL
173200941115     C                   WRITE     SDU1D0A
173300020326     C                   WRITE     SDU1D06                                      *SFL CONTROL
173400020326     C                   READ      SDU1D06                                99
173500000000     C*
173600000000     C                   ELSE
173700000000     C*
173800000000     C* CASO DI SFL CON RECORDS
173900000000     C*
174000000000     C*    - WRITE RECORD CONTROLLO SFL
174100941115     C                   WRITE     SDU1D06
174200941115     C                   READ      SDU1D06                                99
174300000000     C                   END
174400000000     C*
174500000000     C                   ENDSR
174600000000     C*================================================================
174700000000     C* SNDMSG - MANDA MESSAGGIO
174800000000     C*================================================================
174900000000     C     SNDMSG        BEGSR
175000000000     C*
175100000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
175200000000     C                   MOVEL     '1'           *IN40
175300941115     C                   WRITE     SDU1DMC
175400000000     C                   MOVEL     '0'           *IN40
175500000000     C*
175600000000     C* VISUALIZZAZIONE MESSAGGIO
175700000000     C                   Z-ADD     0             MSGULT
175800930204     C                   CALL      'XMSG'        XMSGPA
175900930127     C     DSMSER        IFEQ      0
176000930127     C                   MOVEL     DSMSKE        MSGKEY
176100000000     C                   END
176200000000     C                   Z-ADD     1             MSGULT
176300941115     C                   WRITE     SDU1DMS                                      SFL MESS
176400941115     C                   WRITE     SDU1DMC                                      CONTR MESS
176500000000     C*
176600000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
176700930127     C                   MOVEL     *BLANKS       DSMSMS
176800000000     C*
176900000000     C                   ENDSR
177000000000     C*================================================================
177100000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
177200000000     C*================================================================
177300000000     C     FUNCON        BEGSR
177400000000     C*
177500000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
177600000000     C                   EXSR      FUNVAR
177700000000     C*
177800000000     C* PULISCO CAMPI DI LAVORO
177900000000     C                   Z-ADD     0             I                 5 0
178000000000     C                   Z-ADD     0             W                 5 0
178100000000     C*
178200000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
178300000000     C     FUNKEY        IFNE      *BLANKS
178400100922     C     1             DO        25            I
178500000000     C                   MOVEL     FUD(I)        FUDRIG
178600000000     C*
178700000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
178800000000     C     FUNKEY        IFEQ      FUDFUN
178900000000     C     FUDATT        ANDEQ     'S'
179000000000     C                   Z-ADD     I             W
179100000000     C                   END
179200000000     C                   END                                                    DO WHILE
179300000000     C*
179400000000     C* FUNZIONE NON ATTIVA (ERRORE)
179500000000     C     W             IFEQ      0
179600000000     C*
179700000000     C* EMISSIONE MESSAGGIO DI ERRORE
179800930127     C                   MOVEL     MSG(2)        DSMSMS
179900000000     C                   END                                                    W=0
180000000000     C                   END                                                    FUNKEY NE *BLANKS
180100000000     C*
180200000000     C* FUNZIONE ATTIVA (PROCEDI ...)
180300000000     C     W             IFNE      0
180400000000     C     FUNKEY        OREQ      *BLANKS
180500000000     C*
180600000000     C     FUNKEY        IFEQ      *BLANKS
180700000000     C                   EXSR      ENTGES
180800000000     C                   END
180900000000     C     FUNKEY        IFEQ      '03'
181000000000     C                   EXSR      F03GES
181100000000     C                   END
181200000000     C     FUNKEY        IFEQ      '05'
181300000000     C                   EXSR      F05GES
181400000000     C                   END
181500930126     C     FUNKEY        IFEQ      '06'
181600930126     C                   EXSR      F06GES
181700000000     C                   END
181800000000     C     FUNKEY        IFEQ      '12'
181900000000     C                   EXSR      F12GES
182000000000     C                   END
182100000000     C     FUNKEY        IFEQ      '17'
182200000000     C                   EXSR      F17GES
182300000000     C                   END
182400000000     C     FUNKEY        IFEQ      '18'
182500000000     C                   EXSR      F18GES
182600000000     C                   END
182700930129     C     FUNKEY        IFEQ      '21'
182800930129     C                   EXSR      F21GES
182900930129     C                   END
183000000000     C     FUNKEY        IFEQ      '22'
183100000000     C                   EXSR      F22GES
183200000000     C                   END
183300000000     C     FUNKEY        IFEQ      '23'
183400000000     C                   EXSR      F23GES
183500000000     C                   END
183600000000     C     FUNKEY        IFEQ      '24'
183700000000     C                   EXSR      F24GES
183800000000     C                   END
183900000000     C     FUNKEY        IFEQ      'RU'                                         ROLLUP
184000000000     C                   EXSR      RUPGES
184100000000     C                   END
184200000000     C     FUNKEY        IFEQ      'HP'                                         HELP
184300000000     C                   EXSR      HELGES
184400000000     C                   END
184500000000     C*
184600000000     C                   END
184700000000     C*
184800000000     C                   ENDSR
184900000000     C*================================================================
185000000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
185100000000     C*================================================================
185200000000     C     FUNVAR        BEGSR
185300000000     C*
185400000000     C* INIZIALIZZA VARIABILE -FUNKEY-
185500000000     C*
185600000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
185700000000     C     KEY           IFEQ      ENTER
185800000000     C                   MOVEL     *BLANKS       FUNKEY
185900000000     C                   END
186000000000     C     KEY           IFEQ      F1
186100000000     C                   MOVEL     '01'          FUNKEY
186200000000     C                   END
186300000000     C     KEY           IFEQ      F2
186400000000     C                   MOVEL     '02'          FUNKEY
186500000000     C                   END
186600000000     C     KEY           IFEQ      F3
186700000000     C                   MOVEL     '03'          FUNKEY
186800000000     C                   END
186900000000     C     KEY           IFEQ      F4
187000000000     C                   MOVEL     '04'          FUNKEY
187100000000     C                   END
187200000000     C     KEY           IFEQ      F5
187300000000     C                   MOVEL     '05'          FUNKEY
187400000000     C                   END
187500000000     C     KEY           IFEQ      F6
187600000000     C                   MOVEL     '06'          FUNKEY
187700000000     C                   END
187800000000     C     KEY           IFEQ      F7
187900000000     C                   MOVEL     '07'          FUNKEY
188000000000     C                   END
188100000000     C     KEY           IFEQ      F8
188200000000     C                   MOVEL     '08'          FUNKEY
188300000000     C                   END
188400000000     C     KEY           IFEQ      F9
188500000000     C                   MOVEL     '09'          FUNKEY
188600000000     C                   END
188700000000     C     KEY           IFEQ      F10
188800000000     C                   MOVEL     '10'          FUNKEY
188900000000     C                   END
189000000000     C     KEY           IFEQ      F11
189100000000     C                   MOVEL     '11'          FUNKEY
189200000000     C                   END
189300000000     C     KEY           IFEQ      F12
189400000000     C                   MOVEL     '12'          FUNKEY
189500000000     C                   END
189600000000     C     KEY           IFEQ      F13
189700000000     C                   MOVEL     '13'          FUNKEY
189800000000     C                   END
189900000000     C     KEY           IFEQ      F14
190000000000     C                   MOVEL     '14'          FUNKEY
190100000000     C                   END
190200000000     C     KEY           IFEQ      F15
190300000000     C                   MOVEL     '15'          FUNKEY
190400000000     C                   END
190500000000     C     KEY           IFEQ      F16
190600000000     C                   MOVEL     '16'          FUNKEY
190700000000     C                   END
190800000000     C     KEY           IFEQ      F17
190900000000     C                   MOVEL     '17'          FUNKEY
191000000000     C                   END
191100000000     C     KEY           IFEQ      F18
191200000000     C                   MOVEL     '18'          FUNKEY
191300000000     C                   END
191400000000     C     KEY           IFEQ      F19
191500000000     C                   MOVEL     '19'          FUNKEY
191600000000     C                   END
191700000000     C     KEY           IFEQ      F20
191800000000     C                   MOVEL     '20'          FUNKEY
191900000000     C                   END
192000000000     C     KEY           IFEQ      F21
192100000000     C                   MOVEL     '21'          FUNKEY
192200000000     C                   END
192300000000     C     KEY           IFEQ      F22
192400000000     C                   MOVEL     '22'          FUNKEY
192500000000     C                   END
192600000000     C     KEY           IFEQ      F23
192700000000     C                   MOVEL     '23'          FUNKEY
192800000000     C                   END
192900000000     C     KEY           IFEQ      F24
193000000000     C                   MOVEL     '24'          FUNKEY
193100000000     C                   END
193200000000     C     KEY           IFEQ      ROLLUP
193300000000     C                   MOVEL     'RU'          FUNKEY
193400000000     C                   END
193500000000     C     KEY           IFEQ      HELP
193600000000     C                   MOVEL     'HP'          FUNKEY
193700000000     C                   END
193800000000     C*
193900000000     C                   ENDSR
194000000000     C*================================================================
194100000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
194200000000     C*================================================================
194300000000     C     FUNGES        BEGSR
194400000000     C*
194500000000     C* PULISCO CAMPI DI LAVORO
194600000000     C                   Z-ADD     0             W                 5 0
194700000000     C*
194800000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
194900000000     C* E CARICO TABELLA FUNZIONI ATTIVE
195000000000     C     1             DO        24            I
195100000000     C                   MOVEL     FUD(I)        FUDRIG
195200000000     C*
195300000000     C* PULIZIA FUNZIONI PRECEDENTI
195400000000     C                   MOVEL     *BLANKS       FUV(I)
195500000000     C*
195600000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
195700000000     C* (SALTA F23 E F24)
195800000000     C     FUDATT        IFEQ      'S'
195900000000     C     FUDFUN        ANDNE     '23'
196000000000     C     FUDFUN        ANDNE     '24'
196100000000     C                   ADD       1             W
196200000000     C                   MOVEL     FUDDES        FUV(W)
196300000000     C                   END
196400000000     C                   END                                                    DO WHILE
196500000000     C*
196600000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
196700000000     C* ALTRIMENTI DISATTIVO F24
196800000000     C     1             DO        24            I
196900000000     C                   MOVEL     FUD(I)        FUDRIG
197000000000     C*
197100000000     C     FUDFUN        IFEQ      '24'
197200000000     C     W             IFGT      6
197300000000     C                   MOVEL     'S'           FUDATT
197400000000     C                   ELSE
197500000000     C                   MOVEL     *BLANKS       FUDATT
197600000000     C                   END                                                    W GT 8
197700000000     C                   MOVEL     FUDRIG        FUD(I)
197800000000     C                   END
197900000000     C                   END                                                    DO WHILE
198000000000     C*
198100000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
198200000000     C     W             DIV       6             I
198300000000     C                   MVR                     Z
198400000000     C*
198500000000     C     Z             IFNE      0
198600000000     C     I             ADD       1             Z
198700000000     C                   ELSE
198800000000     C                   Z-ADD     I             Z
198900000000     C                   END
199000000000     C*
199100000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
199200000000     C                   ADD       1             FUNGRU
199300000000     C     FUNGRU        IFGT      Z
199400000000     C     FUNGRU        ORLT      1
199500000000     C                   Z-ADD     1             FUNGRU
199600000000     C                   END
199700000000     C*
199800000000     C* PRIMO GRUPPO DI 6 FUNZIONI
199900000000     C     FUNGRU        IFEQ      1
200000000000     C                   MOVEL     FUV(1)        FUN001
200100000000     C                   MOVEL     FUV(2)        FUN002
200200000000     C                   MOVEL     FUV(3)        FUN003
200300000000     C                   MOVEL     FUV(4)        FUN004
200400000000     C                   MOVEL     FUV(5)        FUN005
200500000000     C                   MOVEL     FUV(6)        FUN006
200600000000     C                   END
200700000000     C*
200800000000     C* SECONDO GRUPPO DI 6 FUNZIONI
200900000000     C     FUNGRU        IFEQ      2
201000000000     C                   MOVEL     FUV(7)        FUN001
201100000000     C                   MOVEL     FUV(8)        FUN002
201200000000     C                   MOVEL     FUV(9)        FUN003
201300000000     C                   MOVEL     FUV(10)       FUN004
201400000000     C                   MOVEL     FUV(11)       FUN005
201500000000     C                   MOVEL     FUV(12)       FUN006
201600000000     C                   END
201700000000     C*
201800000000     C* TERZO GRUPPO DI 6 FUNZIONI
201900000000     C     FUNGRU        IFEQ      3
202000000000     C                   MOVEL     FUV(13)       FUN001
202100000000     C                   MOVEL     FUV(14)       FUN002
202200000000     C                   MOVEL     FUV(15)       FUN003
202300000000     C                   MOVEL     FUV(16)       FUN004
202400000000     C                   MOVEL     FUV(17)       FUN005
202500000000     C                   MOVEL     FUV(18)       FUN006
202600000000     C                   END
202700000000     C*
202800000000     C* QUARTO GRUPPO DI 6 FUNZIONI
202900000000     C     FUNGRU        IFEQ      4
203000000000     C                   MOVEL     FUV(19)       FUN001
203100000000     C                   MOVEL     FUV(20)       FUN002
203200000000     C                   MOVEL     FUV(21)       FUN003
203300000000     C                   MOVEL     FUV(22)       FUN004
203400000000     C                   MOVEL     *BLANKS       FUN005
203500000000     C                   MOVEL     *BLANKS       FUN006
203600000000     C                   END
203700000000     C*
203800000000     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
203900000000     C                   MOVEL     *BLANKS       FUN007
204000000000     C                   MOVEL     *BLANKS       FUN008
204100000000     C*
204200000000     C     1             DO        24            I
204300000000     C                   MOVEL     FUD(I)        FUDRIG
204400000000     C     FUDFUN        IFEQ      '23'
204500000000     C     FUDATT        ANDEQ     'S'
204600000000     C                   MOVEL     FUDDES        FUN007
204700000000     C                   END
204800000000     C     FUDFUN        IFEQ      '24'
204900000000     C     FUDATT        ANDEQ     'S'
205000000000     C                   MOVEL     FUDDES        FUN008
205100000000     C                   END
205200000000     C                   END                                                    DO
205300000000     C*
205400000000     C                   ENDSR
205500000000     C*================================================================
205600000000     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
205700000000     C*================================================================
205800000000     C     OPZGES        BEGSR
205900000000     C*
206000000000     C* PULISCO CAMPI DI LAVORO
206100000000     C                   Z-ADD     0             W                 5 0
206200000000     C*
206300000000     C* RICERCO QUANTE OPZIONI SONO ATTIVE
206400000000     C* E CARICO TABELLA OPZIONI ATTIVE
206500000000     C     1             DO        24            I
206600000000     C                   MOVEL     OPD(I)        OPDRIG
206700000000     C*
206800000000     C* PULIZIA OPZIONI PRECEDENTI
206900000000     C                   MOVEL     *BLANKS       OPV(I)
207000000000     C*
207100000000     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
207200000000     C     OPDATT        IFEQ      'S'
207300000000     C                   ADD       1             W
207400000000     C                   MOVEL     OPDDES        OPV(W)
207500000000     C                   END
207600000000     C                   END                                                    DO WHILE
207700000000     C*
207800000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
207900000000     C* ALTRIMENTI DISATTIVO F23
208000000000     C     1             DO        24            I
208100000000     C                   MOVEL     FUD(I)        FUDRIG
208200000000     C*
208300000000     C     FUDFUN        IFEQ      '23'
208400000000     C     W             IFGT      8
208500000000     C                   MOVEL     'S'           FUDATT
208600000000     C                   ELSE
208700000000     C                   MOVEL     *BLANKS       FUDATT
208800000000     C                   END                                                    W GT 8
208900000000     C                   MOVEL     FUDRIG        FUD(I)
209000000000     C                   END
209100000000     C                   END                                                    DO WHILE
209200000000     C*
209300000000     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
209400000000     C     W             DIV       8             I
209500000000     C                   MVR                     Z                 5 0
209600000000     C*
209700000000     C     Z             IFNE      0
209800000000     C     I             ADD       1             Z
209900000000     C                   ELSE
210000000000     C                   Z-ADD     I             Z
210100000000     C                   END
210200000000     C*
210300000000     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
210400000000     C                   ADD       1             OPZGRU
210500000000     C     OPZGRU        IFGT      Z
210600000000     C     OPZGRU        ORLT      1
210700000000     C                   Z-ADD     1             OPZGRU
210800000000     C                   END
210900000000     C*
211000000000     C* PRIMO GRUPPO DI 8 OPZIONI
211100000000     C     OPZGRU        IFEQ      1
211200000000     C                   MOVEL     OPV(1)        OPZ001
211300000000     C                   MOVEL     OPV(2)        OPZ002
211400000000     C                   MOVEL     OPV(3)        OPZ003
211500000000     C                   MOVEL     OPV(4)        OPZ004
211600000000     C                   MOVEL     OPV(5)        OPZ005
211700000000     C                   MOVEL     OPV(6)        OPZ006
211800000000     C                   MOVEL     OPV(7)        OPZ007
211900000000     C                   MOVEL     OPV(8)        OPZ008
212000000000     C                   END
212100000000     C*
212200000000     C* SECONDO GRUPPO DI 8 OPZIONI
212300000000     C     OPZGRU        IFEQ      2
212400000000     C                   MOVEL     OPV(9)        OPZ001
212500000000     C                   MOVEL     OPV(10)       OPZ002
212600000000     C                   MOVEL     OPV(11)       OPZ003
212700000000     C                   MOVEL     OPV(12)       OPZ004
212800000000     C                   MOVEL     OPV(13)       OPZ005
212900000000     C                   MOVEL     OPV(14)       OPZ006
213000000000     C                   MOVEL     OPV(15)       OPZ007
213100000000     C                   MOVEL     OPV(16)       OPZ008
213200000000     C                   END
213300000000     C*
213400000000     C* TERZO GRUPPO DI 8 OPZIONI
213500000000     C     OPZGRU        IFEQ      3
213600000000     C                   MOVEL     OPV(17)       OPZ001
213700000000     C                   MOVEL     OPV(18)       OPZ002
213800000000     C                   MOVEL     OPV(19)       OPZ003
213900000000     C                   MOVEL     OPV(20)       OPZ004
214000000000     C                   MOVEL     OPV(21)       OPZ005
214100000000     C                   MOVEL     OPV(22)       OPZ006
214200000000     C                   MOVEL     OPV(23)       OPZ007
214300000000     C                   MOVEL     OPV(24)       OPZ008
214400000000     C                   END
214500000000     C                   ENDSR
214600000000     C*================================================================
214700000000     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
214800000000     C*================================================================
214900000000     C     OPZCON        BEGSR
215000000000     C*
215100000000     C* PULISCO CAMPI DI LAVORO
215200000000     C                   Z-ADD     0             I                 5 0
215300000000     C                   Z-ADD     0             W                 5 0
215400000000     C*
215500000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
215600000000     C     1             DO        24            I
215700000000     C                   MOVEL     OPD(I)        OPDRIG
215800000000     C*
215900000000     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
216000000000     C     OPZKEY        IFEQ      OPDOPZ
216100000000     C     OPZKEY        OREQ      OPDOP1
216200000000     C*
216300000000     C     OPDATT        IFEQ      'S'
216400000000     C                   MOVEL     OPDOPZ        OPZKEY
216500000000     C                   Z-ADD     I             W
216600000000     C                   END
216700000000     C                   END
216800000000     C*
216900000000     C                   END                                                    DO WHILE
217000000000     C*
217100000000     C* OPZIONE NON ATTIVA (ERRORE)
217200000000     C     W             IFEQ      0
217300000000     C*
217400000000     C* OPZIONE NON OK
217500000000     C                   Z-ADD     1             OPZOK
217600000000     C*
217700000000     C* EMISSIONE MESSAGGIO DI ERRORE
217800930127     C                   MOVEL     MSG(1)        DSMSMS
217900000000     C                   ELSE
218000000000     C*
218100000000     C* OPZIONE ATTIVA (PROCEDI ...)
218200000000     C*
218300930129     C     OPZKEY        IFEQ      ' 1'
218400000000     C                   EXSR      O01GES
218500000000     C                   END
218600000000     C     OPZKEY        IFEQ      ' 2'
218700000000     C                   EXSR      O02GES
218800000000     C                   END
218900000000     C     OPZKEY        IFEQ      ' 3'
219000000000     C                   EXSR      O03GES
219100000000     C                   END
219200000000     C     OPZKEY        IFEQ      ' 4'
219300020709     c                   If        §UteCun = *Blanks
219400020320     c                   eval      dsmsms = msg(11)
219500020320     c                   z-add     1             opzok
219600020320     c                   else
219700000000     C                   EXSR      O04GES
219800020320     c                   endif
219900000000     C                   END
220000000000     C     OPZKEY        IFEQ      ' 5'
220100000000     C                   EXSR      O05GES
220200000000     C                   END
220300000000     C     OPZKEY        IFEQ      ' 6'
220400000000     C                   EXSR      O06GES
220500000000     C                   END
220600930128     C     OPZKEY        IFEQ      ' 7'
220700930128     C                   EXSR      O07GES
220800930128     C                   END
220900930201     C     OPZKEY        IFEQ      ' 8'
221000930201     C                   EXSR      O08GES
221100930201     C                   END
221200140128     C     OPZKEY        IFEQ      ' R'
221300140128     C                   EXSR      O0RGES
221400140128     C                   END
221500000000     C*
221600000000     C                   END                                                    OPZ.NON ATTIVA
221700000000     C*
221800000000     C                   ENDSR
221900000000     C*================================================================
222000000000     C* NOTE PER COMPILAZIONE SCHIERE
222100000000     C*================================================================
222200000000     C* -MSG-
222300000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
222400000000     C* (NON TOCCARE MSG,1 E MSG,2
222500000000     C*
222600000000     C* -CMD-
222700000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
222800000000     C*
222900000000     C* -OPD-
223000000000     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
223100000000     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
223200000000     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
223300000000     C*    ESEMPIO: OPZIONI -1 -
223400000000     C*                     - 1-  SONO LA STESSA COSA
223500000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
223600000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
223700000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
223800000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
223900000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
224000000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
224100000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
224200000000     C*
224300000000     C* -FUD-
224400000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
224500000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
224600000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
224700000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
224800000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
224900000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
225000000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
225100000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
225200000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
225300000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
225400000000     C*
225500000000     C*================================================================
225600000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
225700941115TNSDU1R- Opzione non gestita.                                                  1
225800941115TNSDU1R- Tasto funzionale non gestito.                                         2
225900941115TNSDU1R- Non abilitato alla gestione di questa Unità.                          3
226000941115TNSDU1R- Unità annullata, non gestibile.    Consentito solo Ripristino.        4
226100941115TNSDU1R- Unità non annullata, impossibile ripristinare.                        5
226200941115TNSDU1R- Accettato solo un parametro di inizio lista.                          6
226300100906TNSDU1R- Tipo unità inesitente                                                 7
226400941115TNSDU1R- Magazzino ubicazione non accettabile.                                 8
226500941115TNSDU1R- Data non accettabile.                                                 9
226600020319TNSDU1R- Matricola non accettabile.                                           10
226700020320TNSDU1R- Non abilitato all'annullamento delle unità.                          11
226800020325TNSDU1R- Matricola obbligatoria se richiesta ricerca per contenuto            12
226900020516TNSDU1R- Profilo non abilitato alla funzione                                  13
227000020516TNSDU1R- Profilo non abilitato al magazzino richiesto                         14
227100100906TNSDU1R- Modello inesistente                                                  15
227200100906TNSDU1R- Modello / Matricola inesisnte                                        16
227300100906TNSDU1R- Se ricerca per contenuto Matricola, non indicare il tipo unità       17
227400941115TNSDU1R- 18                                                                   18
227500941115TNSDU1R- 19                                                                   19
227600941115TNSDU1R- 20                                                                   20
227700000000** ======== SCHIERA: CMD  (COMANDI)                        ================
227800000000WRKSPLF
227900000000
228000000000
228100000000
228200000000
228300000000** ======== SCHIERA: OPD  OPZIONI GESTITE NEL PGM (ATTIVA=S) ==========
228400930312 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
228500930312 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
228600971015 3=Duplica         -3 -     (S/N)=N                        opz. 03¬
228700930312 4=Cancellazione   -4 -     (S/N)=N                        opz. 04¬
228800930312 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
228900000000                   -6 -     (S/N)=N                        opz. 06¬
229000930312 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
229100971013 8=Movimenti       -8 -     (S/N)=N                        opz. 08¬
229200140128 R=S/N Rientro     -R -     (S/N)=N                        opz. 09¬
229300000000                   -  -     (S/N)=N                        opz. 10¬
229400000000                   -  -     (S/N)=N                        opz. 11¬
229500000000                   -  -     (S/N)=N                        opz. 12¬
229600000000                   -  -     (S/N)=N                        opz. 13¬
229700000000                   -  -     (S/N)=N                        opz. 14¬
229800000000                   -  -     (S/N)=N                        opz. 15¬
229900000000                   -  -     (S/N)=N                        opz. 16¬
230000000000                   -  -     (S/N)=N                        opz. 17¬
230100000000                   -  -     (S/N)=N                        opz. 18¬
230200000000                   -  -     (S/N)=N                        opz. 19¬
230300000000                   -  -     (S/N)=N                        opz. 20¬
230400000000                   -  -     (S/N)=N                        opz. 21¬
230500000000                   -  -     (S/N)=N                        opz. 22¬
230600000000                   -  -     (S/N)=N                        opz. 23¬
230700000000                   -  -     (S/N)=N                        opz. 24¬
230800000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
230900930129                   -        (S/N)=N                        funz.01¬
231000930129                   -        (S/N)=N                        funz.02¬
231100930129F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
231200930129                   -        (S/N)=N                        funz.04¬
231300971013F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
231400930312F06=Inserimento    -        (S/N)=N                        funz.06¬
231500000000                   -        (S/N)=N                        funz.07¬
231600000000                   -        (S/N)=N                        funz.08¬
231700100906F09=Altri Dati     -        (S/N)=S  <== S-FISSO           funz.09¬
231800100922                   -        (S/N)=N                        funz.10¬
231900100922                   -        (S/N)=N                        funz.11¬
232000930129F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
232100000000                   -        (S/N)=N                        funz.13¬
232200930129                   -        (S/N)=N                        funz.14¬
232300930129                   -        (S/N)=N                        funz.15¬
232400930126                   -        (S/N)=N                        funz.16¬
232500000000F17=Inizio dati    -        (S/N)=S  <== S-FISSO           funz.17¬
232600000000F18=Fine dati      -        (S/N)=S  <== S-FISSO           funz.18¬
232700930128                   -        (S/N)=N                        funz.19¬
232800930129                   -        (S/N)=N                        funz.20¬
232900971013F21=Stampa         -        (S/N)=S                        funz.21¬
233000940105F22=Gestione stampe-        (S/N)=N                        funz.22¬
233100000000F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
233200000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
233300100922 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.RU¬
