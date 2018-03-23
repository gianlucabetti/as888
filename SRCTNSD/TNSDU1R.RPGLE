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
002400170116     FUNANA13L  IF   E           K DISK
002500170116     F                                     RENAME(UNANA000:UNANA13)
002600020529     FAZORG21l  IF   E           K DISK
002700020529     fazorg01l  if   e           k disk
002800090904
002900090904       //--------------------------------------------------------------
003000090904       // COSTANTI
003100090904       //--------------------------------------------------------------
003200090904
003300090904     d c_Logistica     c                   const('LOG')
003400020319
003500020319      *------------------------------------------------------------------------*
003600020319      * SCHIERE
003700020319      *------------------------------------------------------------------------*
003800020319      * MESSAGGI
003900930127     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
004000020319      * COMANDI
004100000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
004200020319      * DESCRIZIONI OPZIONI
004300000000     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
004400020319      * DESCRIZIONI TASTI FUNZIONALI
004500100922     D FUD             S             80    DIM(25) CTDATA PERRCD(1)
004600020319      * TABELLA OPZIONI DA CARICARE A VIDEO
004700000000     D OPV             S             19    DIM(24)
004800020319      * TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
004900000000     D FUV             S             19    DIM(24)
005000020523
005100020523     d  skpoabi        s              3  0 dim(500)
005200020319
005300020319      *------------------------------------------------------------------------*
005400020319      * INPUT
005500020319      *------------------------------------------------------------------------*
005600020319
005700120411     d depmat          s                   like(vdpmat)
005800170116     d depcod          s                   like(vdpcod)
005900020325     d depric          s                   like(vdpric)
006000020319     d depmag          s                   like(vdpmag)
006100020319     d deptip          s                   like(vdptip)
006200020319     d depti2          s                   like(vdpti2)
006300020319     d dedsmd          s                   like(vddsmd)
006400020319     d dedsma          s                   like(vddsma)
006500100906     d depmod          s                   like(vdpmod)
006600100906     d depmat2         s                   like(vdpmat2)
006700100906     d dspmag          s                   like(vdsmag)
006800120411     d dspord          s                   like(vdsord)
006900100906     d dsprief         s                   like(vdsrief)
007000100907     d depdes          s                   like(vdpdes)
007100101029     d depnot          s                   like(vdpnot)
007200120412     d alford          s              6
007300120411     d wtipri          s              3
007400020325     d wfil            s              3
007500020325     d wksc            s              3  0
007600020319     d wnosfl          s              1    inz('0')
007700020410     d wprimo          s              1    inz('0')
007800020319     d w_vddsmd        s                   like(vddsmd)
007900020319     d w_vddsma        s                   like(vddsma)
008000101029     d w_vdsrief       s                   like(vdsrief)
008100020325     d xx              s              3  0
008200020517     d ii              s              3  0
008300020516     d wabi            s                   like(uteaut)
008400020523     d wmag            s              3  0
008500100906     d ctrmag          s                   like(vdpmag)
008600101027     d Dataiso         s               d   datfmt(*iso)
008700101027     d Datadmy         s               d   datfmt(*dmy)
008800140128     d Datasys         s               d   datfmt(*iso) inz(*sys)
008900020319
009000020319      * RIDEFINIZIONE SCHIERA -OPD-
009100000000     D OPDDS           DS
009200000000     D  OPDOPZ                 1      2
009300000000     D  OPDDE1                 3     19
009400000000     D  OPDOP1                21     22
009500000000     D  OPDFI0                23     34
009600000000     D  OPDATT                35     35
009700000000     D  OPDFI1                36     80
009800000000     D  OPDDES                 1     19
009900000000     D  OPDRIG                 1     80
010000020319      * RIDEFINIZIONE SCHIERA -FUD-
010100000000     D FUDDS           DS
010200000000     D  FUDDE1                 1      1
010300000000     D  FUDFUN                 2      3
010400000000     D  FUDDE2                 4     19
010500000000     D  FUDFI0                20     34
010600000000     D  FUDATT                35     35
010700000000     D  FUDFI1                36     80
010800000000     D  FUDDES                 1     19
010900000000     D  FUDRIG                 1     80
011000020319      * PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
011100930127     D DSMSG         E DS
011200020319      *------------------
011300020319      * DS "XSRDA8" - CONTROLLA DATA (8)
011400020319      *------------------
011500971010     D WLBDA8          DS
011600971010     D  G08DAT                 1      8  0
011700971010     D  G08INV                 9     16  0
011800971010     D  G08ERR                17     17
011900971010     D  G08TGI                18     22  0
012000020319      * PARAMETRI PER LANCIO ALTRI PGM DELLA PROCEDURA
012100930707     D DSUNIT        E DS
012200020319      * DS PER GESTIONE TASTI FUNZIONALI
012300930127     D DSKEY         E DS
012400000000     D DEVDS1          DS
012500000000     D  KEY                  369    369
012600020319      * ARCHITETTURA
012700930312     D KPJBA         E DS
012800070509      * Altri parametri ricevuti (da restituire)
012900070509     d UNANAds       e ds                  extname(UNANA00F)
013000070509     d                                     prefix(UIM:3)
013100020318
013200020318     d tibs02ds      e ds
013300020516
013400020516     d ddatiute      e ds
013500020516     d azuteds       e ds                  extname(azute00f)
013600020516     d tibs34ds      e ds                  inz
013700020516     d dute01        e ds
013800020516     d dlat          e ds
013900020516     d trul31ds      e ds
014000020523     d  skpo                  10    759  0 dim(250)
014100090904      *
014200090904     d Og143         e ds                  inz
014300070509
014400070509     d Status         sds
014500070509     d   SDSprm          *parms
014600020325
014700971010     C*------------------------------------------------------------------------*
014800971010     C* MAIN LINE
014900971010     C*------------------------------------------------------------------------*
015000000000     C* MAIN DEL PGM
015100000000     C                   EXSR      RUTINZ
015200000000     C*
015300930126     C* SE CONTROLLI PRELIMINARI NON OK VADO A FINE PGM
015400930126     C     ERRL00        IFNE      '1'                                          --- 1 -->
015500000000     C*
015600000000     C* CARICA IL SFL DAI PARAMETRI IN INPUT AL PROGRAMMA
015700000000     C                   EXSR      POSSFL
015800000000     C*
015900000000     C* CICLO FINO A CHE:
016000000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
016100000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
016200930707     C*  -FINE PGM PER ENTER SENZA SCELTA
016300000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
016400000000     C     F12L00        OREQ      '1'
016500020404     C**!!!FINL00        OREQ      '1'
016600000000     C*
016700000000     C* AZZERA FLAG DI ERROE
016800000000     C                   MOVEL     '0'           ERRL00
016900020410
017000020410      * posiziono il cursore sulla matricola se prima volta
017100020410     c                   if        wprimo = *off
017200020410     c                   eval      *in88 = *on
017300020410     c                   eval      wprimo = *on
017400020410     c                   else
017500020410     c                   eval      *in88 = *off
017600020410     c                   endif
017700020516
017800020516      * profilo non abilitato
017900020516     c                   if        *in76
018000020516     c                   eval      f03l00 = *on
018100020516     c                   endif
018200000000     C*
018300000000     C* EMETTE SFL A VIDEO
018400000000     C                   EXSR      OUTSFL
018500000000     C*
018600000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI -ROUTINE FUNCON
018700000000     C                   EXSR      FUNCON
018800000000     C*
018900000000     C                   END                                                    <-- 2 ---
019000000000     C*
019100000000     C                   END                                                    <-- 1 --
019200000000     C*
019300940105     C                   MOVEL     DSUNIT        KPJBU
019400000000     C                   SETON                                        LR
019500000000     C*================================================================
019600930126     C* F03GES -GEST.TASTO FUNZ. F03 ===> FINE
019700000000     C*================================================================
019800000000     C     F03GES        BEGSR
019900000000     C*
020000000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PGM 00)
020100000000     C                   MOVEL     '1'           F03L00
020200000000     C*
020300000000     C                   ENDSR
020400000000     C*================================================================
020500930129     C* F05GES -GEST.TASTO FUNZ. F05 ===> RIVISUALIZZAZIONE
020600000000     C*================================================================
020700000000     C     F05GES        BEGSR
020800000000     C*
020900000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
021000000000     C                   EXSR      POSSFL
021100000000     C*
021200000000     C                   ENDSR
021300000000     C*================================================================
021400931116     C* F06GES -GEST.TASTO FUNZ. F06 ===> INSERIMENTO
021500000000     C*================================================================
021600930126     C     F06GES        BEGSR
021700930201     C*
021800930201     C* INSERIMENTO CONSENTITO SOLO IN SEDE
021900020709     c                   If        DutLpo <> 'S'
022000930201     C                   MOVEL     '1'           ERRL00
022100930201     C                   MOVEL     MSG(3)        DSMSMS
022200930201     C                   END
022300930201     C*
022400930201     C     ERRL00        IFEQ      '0'                                          --- 1 -->
022500930127     C*
022600941115     C* LANCIO IL PGM PER L'INSERIMENTO DEL RECORD (TNSDU2R)
022700930201     C                   MOVEL     'F06'         WOPZ1             3
022800941115     C                   MOVEL     'TNSDU2R'     WPGM              8
022900930201     C                   EXSR      LANPGM
023000930128     C*
023100930128     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
023200930128     C                   EXSR      POSSFL
023300930201     C*
023400930201     C                   END                                                    <-- 1 ---
023500930127     C*
023600000000     C                   ENDSR
023700000000     C*================================================================
023800930129     C* F12GES -GEST.TASTO FUNZ. F12 ===> RITORNO
023900000000     C*================================================================
024000000000     C     F12GES        BEGSR
024100000000     C*
024200000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12 (A LIVELLO PGM 00)
024300000000     C                   MOVEL     '1'           F12L00
024400000000     C*
024500000000     C                   ENDSR
024600000000     C*================================================================
024700930126     C* F17GES -GEST.TASTO FUNZ. F17 ===> INIZIO DATI
024800000000     C*================================================================
024900000000     C     F17GES        BEGSR
025000000000     C*
025100000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
025200000000     C                   Z-ADD     1             SFLNBR
025300000000     C*
025400000000     C                   ENDSR
025500000000     C*================================================================
025600930126     C* F18GES -GEST.TASTO FUNZ. F18 ===> FINE DATI
025700000000     C*================================================================
025800000000     C     F18GES        BEGSR
025900000000     C*
026000000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
026100000000     C                   Z-ADD     SFLULT        SFLNBR
026200000000     C*
026300000000     C                   ENDSR
026400930128     C*================================================================
026500930128     C* F21GES -GEST.TASTO FUNZ. F21 ===> STAMPA ELENCO
026600930128     C*================================================================
026700930128     C     F21GES        BEGSR
026800930128     C*
026900930707     C* LANCIA PROGRAMMA DI STAMPA DELLE UNITA
027000940105     C                   MOVEL     DSUNIT        KPJBU
027100941115     C                   CALL      'TNSDU6R'
027200930209     C                   PARM                    KPJBA
027300940105     C                   MOVEL     KPJBU         DSUNIT
027400940105     C*
027500940105     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
027600940105     C     DSUF03        IFEQ      '1'
027700940105     C                   MOVEL     '1'           F03L00
027800940105     C                   MOVEL     '1'           ERRL00
027900940105     C                   ENDIF
028000930128     C*
028100930128     C                   ENDSR
028200000000     C*================================================================
028300930126     C* F22GES -GEST.TASTO FUNZ. F22 ===> STAMPE
028400000000     C*================================================================
028500000000     C     F22GES        BEGSR
028600000000     C*
028700000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
028800000000     C                   Z-ADD     80            LENGH            15 5
028900000000     C                   CALL      'QCAEXEC'
029000000000     C                   PARM                    CMD(1)
029100000000     C                   PARM                    LENGH
029200000000     C*
029300000000     C                   ENDSR
029400000000     C*================================================================
029500930126     C* F23GES -GEST.TASTO FUNZ. F23 ===> ALTRE OPZIONI
029600000000     C*================================================================
029700000000     C     F23GES        BEGSR
029800000000     C*
029900000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
030000000000     C                   EXSR      OPZGES
030100000000     C*
030200000000     C                   ENDSR
030300000000     C*================================================================
030400930126     C* F24GES -GEST.TASTO FUNZ. F24 ===> ALTRI TASTI
030500000000     C*================================================================
030600000000     C     F24GES        BEGSR
030700000000     C*
030800000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
030900000000     C                   EXSR      FUNGES
031000000000     C*
031100000000     C                   ENDSR
031200000000     C*================================================================
031300930126     C* RUPGES -GEST.TASTO FUNZ. ROLLUP ===> PAGINA SUCCESSIVA
031400000000     C*================================================================
031500000000     C     RUPGES        BEGSR
031600000000     C*
031700000000     C* CARICA IL SFL DA FILE "FISICO"
031800000000     C                   EXSR      CARSFL
031900000000     C*
032000000000     C                   ENDSR
032100000000     C*================================================================
032200930126     C* HELGES - GEST.TASTO FUNZ. HELP ===> AIUTO
032300000000     C*================================================================
032400000000     C     HELGES        BEGSR
032500000000     C*
032600000000     C                   ENDSR
032700000000     C*================================================================
032800930126     C* ENTGES - GEST.TASTO FUNZ. ENTER ===> INVIO
032900000000     C*================================================================
033000000000     C     ENTGES        BEGSR
033100000000     C*
033200000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
033300000000     C                   EXSR      NEWPOS
033400000000     C*
033500000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
033600000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
033700000000     C*
033800000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
033900000000     C     SWPOSI        IFNE      0                                            --- 2 -->
034000000000     C                   EXSR      POSSFL
034100000000     C*
034200000000     C                   ELSE                                                   <-- 2 -->
034300000000     C*
034400000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
034500000000     C     SFLULT        IFNE      0                                            --- 3 -->
034600000000     C                   EXSR      ENTATT
034700000000     C*
034800000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
034900000000     C     SWMODI        IFNE      0                                            --- 4 -->
035000000000     C                   EXSR      POSSFL
035100000000     C                   END                                                    <-- 4 ---
035200000000     C*
035300000000     C                   END                                                    <-- 3 ---
035400000000     C                   END                                                    <-- 2 ---
035500000000     C                   END                                                    <-- 1 ---
035600000000     C*
035700000000     C                   ENDSR
035800000000     C*================================================================
035900930126     C*  O01GES - GESTIONE OPZIONE 1=SCELTA
036000000000     C*================================================================
036100000000     C     O01GES        BEGSR
036200070509      *
036300070509if  1c                   IF        SDSprm <= 1
036400070509     c                   eval      DSUtip  = VIDtip
036500070509     c                   eval      DSUcod  = VIDcod
036600070509     c                   eval      DSUmag  = VIDmag
036700070509     c                   clear                   DSUprg
036800070509     c                   clear                   DSUdtm
036900070509     c                   clear                   DSUtmo
037000070509x   1c                   ELSE
037100070509     c                   clear                   UNANAds
037200070509     c                   eval      UIMtip  = VIDtip
037300070509     c                   eval      UIMcod  = VIDcod
037400070509     c                   eval      UIMmar  = VIDmar
037500070509     c                   eval      UIMmod  = VIDmod
037600070509     c                   eval      UIMmat  = VIDmat
037700070509     c                   eval      UIMmag  = VIDmag
037800070509     c***                eval      UIMdsm  = VIDdsm
037900070509     c                   clear                   WLBda8
038000070509     c                   eval      G08dat  = UIMdsm
038100070509     c                   call      'XSRDA8'
038200070509     c                   parm                    WLBda8
038300070509     c                   eval      UIMdsm  = G08inv
038400070509e   1c                   ENDIF
038500070509      *
038600070509     c                   eval      F12l00  = *on
038700070509     c                   eval      ERRl00  = *on
038800070509      *
038900000000     C                   ENDSR
039000000000     C*================================================================
039100000000     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
039200000000     C*================================================================
039300000000     C     O02GES        BEGSR
039400930128     C*
039500930128     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA MODIFICA
039600930201     C     VIDATB        IFEQ      'A'
039700930128     C                   MOVEL     '1'           ERRL00
039800930201     C                   MOVEL     MSG(4)        DSMSMS
039900930201     C                   END
040000930201     C*
040100930201     C     ERRL00        IFEQ      '0'                                          --- 1 -->
040200000000     C*
040300930707     C* LANCIO IL PGM PER LA MODIFICA DEL DETTAGLIO DELL'ANAGRAFICA
040400930201     C                   MOVEL     'O02'         WOPZ1
040500941115     C                   MOVEL     'TNSDU2R'     WPGM
040600930201     C                   EXSR      LANPGM
040700000000     C*
040800930127     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
040900930707     C                   Z-ADD     1             SWMODI
041000930128     C*
041100930201     C                   END                                                    <-- 1 ---
041200000000     C*
041300000000     C                   ENDSR
041400000000     C*================================================================
041500971015     C* O03GES - GESTIONE OPZIONE 3=DUPLICA
041600000000     C*================================================================
041700000000     C     O03GES        BEGSR
041800971015     C*
041900971015     C* DUPLICA CONSENTITA SOLO IN SEDE
042000020709     c                   If        DutLpo <> 'S'
042100971015     C                   MOVEL     '1'           ERRL00
042200971015     C                   MOVEL     MSG(3)        DSMSMS
042300971015E   1C                   ENDIF
042400971015     C*
042500971015     C* LANCIA IL PGM PER L'INSERIMENTO
042600971015IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
042700971015     C                   MOVEL     'O03'         WOPZ1
042800971015     C                   MOVEL     'TNSDU2R'     WPGM
042900971015     C                   EXSR      LANPGM
043000971015     C*
043100971015     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
043200971015     C                   Z-ADD     1             SWMODI
043300971015E   1C                   ENDIF
043400971015     C*
043500000000     C                   ENDSR
043600000000     C*================================================================
043700000000     C*  O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE
043800000000     C*================================================================
043900000000     C     O04GES        BEGSR
044000930128     C*
044100930128     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
044200930201     C     VIDATB        IFEQ      'A'
044300930128     C                   MOVEL     '1'           ERRL00
044400930201     C                   MOVEL     MSG(4)        DSMSMS
044500930201     C                   END
044600930128     C*
044700930201     C     ERRL00        IFEQ      '0'                                          --- 1 -->
044800000000     C*
044900930707     C* LANCIO IL PGM DI GESTIONE DELLE UNITA
045000930201     C                   MOVEL     'O04'         WOPZ1
045100941115     C                   MOVEL     'TNSDU2R'     WPGM
045200930201     C                   EXSR      LANPGM
045300000000     C*
045400930127     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
045500930707     C                   Z-ADD     1             SWMODI
045600930128     C*
045700930201     C                   END                                                    <-- 1 ---
045800000000     C*
045900000000     C                   ENDSR
046000000000     C*================================================================
046100000000     C*  O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
046200000000     C*================================================================
046300000000     C     O05GES        BEGSR
046400000000     C*
046500000000     C* LANCIO ROUTINE PER LA VISUALIZZAZIONE DEL DETTAGLIO DELLA RIGA
046600930201     C                   MOVEL     'O05'         WOPZ1
046700941115     C                   MOVEL     'TNSDU2R'     WPGM
046800930201     C                   EXSR      LANPGM
046900000000     C*
047000000000     C                   ENDSR
047100160923     C*================================================================
047200160923     C*  O0SGES - GESTIONE OPZIONE S=Storicizzazione
047300160923     C*================================================================
047400160923     C     O0SGES        BEGSR
047500160923     C*
047600160923     C* LANCIO ROUTINE PER LA VISUALIZZAZIONE DEL DETTAGLIO DELLA RIGA
047700160923     C                   MOVEL     'O0S'         WOPZ1
047800160923     C                   MOVEL     'TNSDU2R'     WPGM
047900160923     C                   EXSR      LANPGM
048000160926     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
048100160926     C                   Z-ADD     1             SWMODI
048200160923     C*
048300160923     C                   ENDSR
048400000000     C*================================================================
048500000000     C*  O06GES - GESTIONE OPZIONE 6=STAMPA
048600000000     C*================================================================
048700000000     C     O06GES        BEGSR
048800000000     C                   ENDSR
048900930128     C*================================================================
049000930128     C*  O07GES - GESTIONE OPZIONE 7=RIPRISTINO
049100930128     C*================================================================
049200930128     C     O07GES        BEGSR
049300930128     C*
049400930128     C* SE IL RECORD NON E' ANNULLATO NON CONSENTO IL RIPRISTINO
049500930201     C     VIDATB        IFNE      'A'
049600930128     C                   MOVEL     '1'           ERRL00
049700930201     C                   MOVEL     MSG(5)        DSMSMS
049800930201     C                   END
049900930201     C*
050000930201     C     ERRL00        IFEQ      '0'                                          --- 1 -->
050100930128     C*
050200930201     C* LANCIO IL PGM PER IL RIPRISTINO DI UN ANNULLAMENTO
050300930201     C                   MOVEL     'O07'         WOPZ1
050400941115     C                   MOVEL     'TNSDU2R'     WPGM
050500930201     C                   EXSR      LANPGM
050600930128     C*
050700930128     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
050800930707     C                   Z-ADD     1             SWMODI
050900930128     C*
051000930201     C                   END                                                    <-- 1 ---
051100930128     C*
051200930128     C                   ENDSR
051300930201     C*================================================================
051400940105     C*  O08GES - GESTIONE OPZIONE 8=GESTIONE MOVIMENTI
051500930201     C*================================================================
051600930201     C     O08GES        BEGSR
051700930201     C*
051800930201     C* LANCIO IL PGM PER LA GESTIONE DELLE LOCALITA'
051900971013     C     ERRL00        IFEQ      '0'                                          --- 1 -->
052000930201     C                   MOVEL     'O08'         WOPZ1
052100941115     C                   MOVEL     'TNSDU4R'     WPGM
052200930201     C                   EXSR      LANPGM
052300930201     C                   END                                                    <-- 1 ---
052400930201     C*
052500930201     C                   ENDSR
052600140128     C*================================================================
052700140128     C*  O0RGES - GESTIONE OPZIONE R=si/no rietnro da filiale
052800140128     C*================================================================
052900140128     C     O0RGES        BEGSR
053000140128     C*
053100140128     C* Aggiorno l'anagrafica con reos si/no
053200140128     C     KEYUNA        CHAIN     UNANA01L
053300140128     c                   if        %found(unana01l)
053400140128     c* se c'e' reso lo tolgo e viceversa
053500140128     c                   if        %subst(unalsw:15:1)= 'S'
053600140128     c                   eval      %subst(unalsw:15:1)= ' '
053700140128     c                   clear                   unadtr
053800140128     c                   else
053900140128     c                   eval      %subst(unalsw:15:1)= 'S'
054000140128     c     *iso          movel     datasys       UNADTR
054100140128     c                   endif
054200140128     c                   update    unana1
054300140128     c                   endif
054400140128     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
054500140128     C                   Z-ADD     1             SWMODI
054600140128     C*
054700140128     C                   ENDSR
054800000000     C*================================================================
054900000000     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
055000000000     C*================================================================
055100000000     C     NEWPOS        BEGSR
055200000000     C*
055300000000     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAM.
055400000000     C*         1= RICHIESTO POSIZIONAM.
055500000000     C                   Z-ADD     0             SWPOSI            7 0
055600000000     C*
055700000000     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
055800000000     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
055900000000     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
056000000000     C                   Z-ADD     0             SWMODI            1 0
056100000000     C*
056200000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
056300020319     c                   if        vdpmat <> depmat or vdptip <> deptip or      --- 1 -->
056400020319     c                             vdpmag <> depmag or vdpti2 <> depti2 or
056500020319     c                             vddsmd <> dedsmd or vddsma <> dedsma or
056600100906     c                             vdpric <> depric or depmod <> vdpmod or
056700100906     c                             vdpmat2<> depmat2 or vdsmag<> dspmag or
056800170116     c                             vdpdes<>  depdes  or vdsrief<>dsprief or
056900170116     c                             vdpnot<>  depnot  or vdsord<> dspord  or
057000170116     c                             vdpcod<>  depcod
057100930202     C*
057200930202     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI DI PARTENZA
057300930202     C                   EXSR      CHKPAR
057400930202     C*
057500930202     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
057600930202     C     ERRL00        IFEQ      '0'                                          --- 2 -->
057700000000     C                   Z-ADD     1             SWPOSI
057800170116     c                   eval      dspord = vdsord
057900120411     c                   eval      depmat = vdpmat
058000020325     c                   eval      depric = vdpric
058100020319     c                   eval      deptip = vdptip
058200020319     c                   eval      depmag = vdpmag
058300020319     c                   eval      depti2 = vdpti2
058400020319     c                   eval      dedsmd = vddsmd
058500020319     c                   eval      dedsma = vddsma
058600100907     c                   eval      depmod = vdpmod
058700100907     c                   eval      depmat2= vdpmat2
058800100907     c                   eval      dspmag = vdsmag
058900100907     c                   eval      dsprief= vdsrief
059000100907     c                   eval      depdes= vdpdes
059100101029     c                   eval      depnot= vdpnot
059200170116     c                   eval      depcod= vdpcod
059300930202     C                   END                                                    <-- 2 ---
059400930202     C*
059500000000     C                   END                                                    <-- 1 ---
059600000000     C*
059700000000     C                   ENDSR
059800000000     C*================================================================
059900000000     C* LETTURA RECORD FILE DA CARICARE
060000000000     C*================================================================
060100000000     C     LETFIL        BEGSR
060200020319
060300020319     c                   select
060400020319
060500020319     c                   when      wtipri = 'TIP'
060600020319     c                   read      unana02l                               99
060700020319
060800170116     c                   when      wtipri = 'COD'
060900170116     c                   read      unana13l                               99
061000170116
061100020319     c                   when      wtipri = 'MAT'
061200020319     c                   read      unana07l                               99
061300020319
061400020319     c                   when      wtipri = 'MAG'
061500020319     c                   read      unana03l                               99
061600020319
061700020319     c                   when      wtipri = 'DSM'
061800020319     c                   read      unana04l                               99
061900020319
062000100906     c                   when      wtipri = 'MOD'
062100100906     c                   read      unana08l                               99
062200020319
062300020319     c                   endsl
062400020319
062500000000     C*
062600930201     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
062700930707     C     *IN99         IFEQ      '1'
062800000000     C                   Z-ADD     1             SFLEOF
062900930707     C                   END
063000000000     C*
063100000000     C                   ENDSR
063200000000     C*================================================================
063300000000     C* SETFIL - SETLL DEL FILE DA CARICARE
063400000000     C*================================================================
063500000000     C     SETFIL        BEGSR
063600020325
063700020319
063800020319      * POSIZIONAMENTO DEL FILE DA CARICARE E PRIMA LETTURA
063900020319
064000020319     c                   select
064100020319
064200020319     c                   when      wtipri = 'TIP'
064300020319     c                   move      vdptip        unatip
064400020319     c                   move      vdpmat        unamat
064500020319     c     kuna02        setll     unana02l
064600020319     c                   if        not %found(unana02l)
064700020319     c                   eval      sfleof = 1
064800020319     c                   else
064900020319     c                   read      unana02l
065000020319     c                   if        %eof(unana02l)
065100020319     c                   eval      sfleof = 1
065200020319     c                   endif
065300020319     c                   endif
065400020319
065500170116     c                   when      wtipri = 'COD'
065600170116     c                   move      vdpcod        unacod
065700170116     c                   move      vdptip        unatip
065800170116     c     kuna13        setll     unana13l
065900170116     c                   if        not %found(unana13l)
066000170116     c                   eval      sfleof = 1
066100170116     c                   else
066200170116     c                   read      unana13l
066300170116     c                   if        %eof(unana13l)
066400170116     c                   eval      sfleof = 1
066500170116     c                   endif
066600170116     c                   endif
066700020319     c                   when      wtipri = 'MAT'
066800020325      * no ricerca per contenuto
066900020325     c                   if        vdpric = *blanks
067000020319     c                   move      vdpmat        unamat
067100020319     c                   move      vdptip        unatip
067200020319     c     kuna07        setll     unana07l
067300020319     c                   if        not %found(unana07l)
067400020319     c                   eval      sfleof = 1
067500020319     c                   else
067600020319     c                   read      unana07l
067700020319     c                   if        %eof(unana07l)
067800020319     c                   eval      sfleof = 1
067900020319     c                   endif
068000020319     c                   endif
068100020325      * si ricerca per contenuto
068200020325     c                   else
068300020325     c     *loval        setll     unana07l
068400020325     c                   if        not %found(unana07l)
068500020325     c                   eval      sfleof = 1
068600020325     c                   else
068700020325     c                   read      unana07l
068800020325     c                   if        %eof(unana07l)
068900020325     c                   eval      sfleof = 1
069000020325     c                   endif
069100020325     c                   endif
069200020325     c                   endif
069300020319
069400020319     c                   when      wtipri = 'MAG'
069500020319     c                   move      vdpmag        unamag
069600020319     c                   move      vdpti2        unatip
069700020319     c     kuna03        setll     unana03l
069800020319     c                   if        not %found(unana03l)
069900020319     c                   eval      sfleof = 1
070000020319     c                   else
070100020319     c                   read      unana03l
070200020319     c                   if        %eof(unana03l)
070300020319     c                   eval      sfleof = 1
070400020319     c                   endif
070500020319     c                   endif
070600020319
070700020319     c                   when      wtipri = 'DSM'
070800020319     c                   z-add     w_vddsmd      unadsm
070900020319     c     kuna04        setll     unana04l
071000020319     c                   if        not %found(unana04l)
071100020319     c                   eval      sfleof = 1
071200020319     c                   else
071300020319     c                   read      unana04l
071400020319     c                   if        %eof(unana04l)
071500020319     c                   eval      sfleof = 1
071600020319     c                   endif
071700020319     c                   endif
071800020319
071900100906     c                   when      wtipri = 'MOD'
072000100906     c                   movel     vdpmod        unamod
072100100906     c                   movel     vdpmat        unamat
072200100906     c     kuna08        setll     unana08l
072300100906     c                   if        not %found(unana08l)
072400100906     c                   eval      sfleof = 1
072500100906     c                   else
072600100906     c                   read      unana08l
072700100906     c                   if        %eof(unana08l)
072800100906     c                   eval      sfleof = 1
072900100906     c                   endif
073000100906     c                   endif
073100100906     c
073200100906     c**                 when      wtipri = 'DSG'
073300100906     c**                 z-add     w_vddsgd      unadsg
073400100906     c**   kuna06        setll     unana06l
073500100906     c**                 if        not %found(unana06l)
073600100906     c**                 eval      sfleof = 1
073700100906     c**                 else
073800100906     c**                 read      unana06l
073900100906     c**                 if        %eof(unana06l)
074000100906     c**                 eval      sfleof = 1
074100100906     c**                 endif
074200100906     c**                 endif
074300020319
074400020319     c                   endsl
074500020319
074600000000     C                   ENDSR
074700020319      *================================================================
074800020319      * CTRSFL - CONTROLLO SE RECORD DA SCRIVERE NEL SUBFILE
074900020319      *================================================================
075000020319     C     CTRSFL        BEGSR
075100020319
075200020325      * record letto ok
075300020325     c                   if        wnosfl = *off
075400020516
075500020516     c                   if        unamag <> 951
075600020523     c     unamag        lookup    skpoabi                                99
075700020516     c                   if        not *in99
075800020516     c                   eval      wnosfl = *on
075900020516     c                   endif
076000020516     c                   else
076100020516     c                   movel     unaksc        wmag
076200020516     c     wmag          lookup    skpoabi                                99
076300020516     c                   if        not *in99
076400020516     c                   eval      wnosfl = *on
076500020516     c                   endif
076600020516     c                   endif
076700020516     c                   endif
076800020325
076900020325     c                   if        wtipri = 'MAT' and vdpric = 'S'
077000020516     c                             and wnosfl = *off
077100020325     c     ' '           checkr    vdpmat        xx
077200020325     c     vdpmat:xx     scan      unamat                                 99
077300020325     c                   if        not *in99
077400020325     c                   eval      wnosfl = *on
077500020325     c                   endif
077600020325     c                   endif
077700020325
077800020516     c                   if        wnosfl = *off
077900020319     c                   if        (wtipri = 'DSM' and w_vddsma > *zeros
078000100906     c                                             and unadsm > w_vddsma)
078100020319     c                   eval      wnosfl = *on
078200020319     c                   endif
078300020516     c                   endif
078400100907     c
078500101029     c* Ricerca per contenuto descrizione
078600100907     c                   if        vdpdes<>*blanks
078700100907     c     ' '           checkr    vdpdes        xx
078800100907     c     vdpdes:xx     scan      unades                                 99
078900100907     c                   if        not *in99
079000100907     c                   eval      wnosfl = *on
079100100907     c                   endif
079200100907     c                   endif
079300101029     c* Ricerca per contenuto note
079400101029     c                   if        vdpnot<>*blanks
079500101029     c     ' '           checkr    vdpnot        xx
079600101029     c     vdpnot:xx     scan      unanot                                 99
079700101029     c                   if        not *in99
079800101029     c                   eval      wnosfl = *on
079900101029     c                   endif
080000101029     c                   endif
080100100907     c* Parzializza magazzino
080200100907     c                   if        vdsmag>0 and unamag<>vdsmag
080300100907     c                   eval      wnosfl = *on
080400100907     c                   endif
080500100907     c* Parzializza unità da rendere
080600101029     c***                if        vdsrief='SI' and %subst(unalsw:15:1)<>'S'
080700101029     c                   if        vdsrief>0    and %subst(unalsw:15:1)<>'S'
080800100907     c                   eval      wnosfl = *on
080900100907     c                   endif
081000101029     c                   if        vdsrief>0    and unadtr>w_vdsrief
081100101029     c                   eval      wnosfl = *on
081200101029     c                   endif
081300100907     c
081400120411     c* Parzializzo per numero ordine
081500120411     c                   if        vdsord>0
081600120411     c                   movel     vdsord        alford
081700120412     c                   if        alford<>%subst(unaemu:1:6)
081800120411     c                   eval      wnosfl = *on
081900120411     c                   endif
082000120411     c                   endif
082100120411     c
082200020319     C                   ENDSR
082300000000     C*================================================================
082400000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
082500000000     C*================================================================
082600000000     C     IRESFL        BEGSR
082700000000     C*
082800000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
082900930127     C*
083000930707     C                   MOVEL     UNAATB        VIDATB
083100930707     C                   Z-ADD     UNATIP        VIDTIP
083200930707     C                   Z-ADD     UNACOD        VIDCOD
083300930707     C                   MOVEL     UNAMAT        VIDMAT
083400930707     C                   MOVEL     UNAMAR        VIDMAR
083500120411     c                   clear                   vidord
083600120412     c                   if        %Subst(unaemu:1:6)>*zeros
083700120412     c                   eval      vidord=%int(%Subst(unaemu:1:6))
083800120411     c                   endif
083900120411     c
084000930707     C                   MOVEL     UNAMOD        VIDMOD
084100100906     C                   MOVEL     UNAdes        VIDdes
084200100906     c                   if        %Subst(unalsw:15:1)='S'
084300101027     c                   eval      vidrief='SI'
084400100906     c                   setoff                                       15
084500101027     c* Imposto la data da quando
084600101029     c                   clear                   viddalr
084700101027     c                   if        unadtr>0
084800101027     C     *iso          movel     UNADTR        dataiso
084900101027     c                   movel     dataiso       datadmy
085000101027     c                   movel     datadmy       viddalr
085100101027     c                   endif
085200101027     c
085300100906     c                   else
085400100906     c                   clear                   vidrief
085500101027     c                   clear                   viddalr
085600100906     c                   seton                                        15
085700100906     c                   endif
085800930707     C                   Z-ADD     UNAMAG        VIDMAG
085900100907     c**                 if        not *in94
086000971010     C                   Z-ADD     UNADSM        G08INV
086100971010     C                   Z-ADD     *ZEROS        G08DAT
086200971010     C                   MOVEL     '3'           G08ERR
086300971010     C                   CALL      'XSRDA8'
086400971010     C                   PARM                    WLBDA8
086500020319     C                   Z-ADD     G08DAT        VIDDat
086600160923
086700160923     c* se la data è minore di oggi, la metto in R.I.
086800160923     c                   if        unadsm<dateu  and unadsm>0
086900160923     c                   seton                                        10
087000160923     c                   else
087100160923     c                   setoff                                       10
087200160923     c                   endif
087300160923     c
087400100907     c**                 endif
087500100907     c**                 if        *in94
087600100907     C**                 Z-ADD     UNADSG        G08INV
087700100907     C**                 Z-ADD     *ZEROS        G08DAT
087800100907     C**                 MOVEL     '3'           G08ERR
087900100907     C**                 CALL      'XSRDA8'
088000100907     C**                 PARM                    WLBDA8
088100100907     C**                 Z-ADD     G08DAT        VIDDat
088200100907     c**                 endif
088300000000     C*
088400000000     C                   ENDSR
088500930202     C*================================================================
088600930202     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
088700930202     C*================================================================
088800930202     C     CHKPAR        BEGSR
088900020319
089000020319      * IMPOSTO FLAG EVIDENZAZIONE TIPO DI RICERCA
089100020319     c                   eval       *in90 = *off                                x tipo
089200020319     c                   eval       *in91 = *off                                x matricola
089300020319     c                   eval       *in92 = *off                                x magazzino
089400020319     c                   eval       *in93 = *off                                x scad.manutenzione
089500100906     c                   eval       *in94 = *off                                x modello
089600120411     c                   eval       *in95 = *off                                x num ordine
089700170116     c                   eval       *in96 = *off                                x codice
089800020319
089900020319      * IMPOSTO FLAG EVIDENZAZIONE ERRORI SUI PARAMETRI
090000020319     c                   eval       *in80 = *off                                tipo
090100020319     c                   eval       *in81 = *off                                tipo
090200020319     c                   eval       *in82 = *off                                matricola
090300020319     c                   eval       *in83 = *off                                magazzino
090400020319     c                   eval       *in84 = *off                                data sc.manut.dal
090500020319     c                   eval       *in85 = *off                                data sc.manut.al
090600100906     c                   eval       *in86 = *off                                modello
090700120411     c                   eval       *in87 = *off                                data rientro
090800020326
090900020326     c                   clear                   wtipri
091000020319
091100020319      * Se non è stata scelta una ricerca utilizzo la ricerca per tipo
091200020319     c                   if        vdpmat = *blanks and vdptip = *blanks and
091300020319     c                             vdpmag = *blanks and vdpti2 = *zeros  and
091400100906     c                             vddsmd = *zeros  and vddsma = *zeros  and
091500170116     c                             vdpmod=*blanks and vdpmat2=*blanks    and
091600170116     c                             vdpcod= *zeros
091700020319     c                   eval      wtipri = 'TIP'
091800020319     c                   eval      *in90 = *on
091900020319     c                   endif
092000020319
092100020319      * ===> ACCETTO SOLO UN PARAMETRO DI INIZIO RICERCA
092200020319
092300020319      * Tipo unità / matricola
092400100906    1c                   if        vdptip <> *blanks
092500020319      * controllo tipo unità
092600020319     c                   exsr      chktip
092700020319     c                   eval      wtipri = 'TIP'
092800020319     c                   eval      *in90 = *on
092900100906    1c                   endif
093000020319
093100020319      * Matricola
093200100906    1c                   if        vdpmat <> *blanks and errl00 = *off and
093300020410     c                             vdptip = *blanks
093400020319     c                   eval      wtipri = 'MAT'
093500020319     c                   eval      *in91 = *on
093600100906    1c                   endif
093700170116      * codice
093800170116     c                   if        vdpcod>*zeros
093900170116     c                   eval      wtipri = 'COD'
094000170116     c                   eval      *in96 = *on
094100170116    1c                   endif
094200170116      * non è possibile immettere sia il codice che la matricola
094300170116     c                   if        vdpmat <> *blanks and vdpcod> *zeros
094400170116     c                   eval      errl00 = *on
094500170116     c                   eval      dsmsms = msg(6)
094600170116     c                   leavesr
094700170116    1c                   endif
094800100907      * ricerca x contenuto matricola
094900100906    1c                   if        errl00 = *off and vdpmat = *blanks and
095000020325     c                             vdpric = 'S'
095100020325     c                   eval      errl00 = *on
095200020325     c                   eval      dsmsms = msg(12)
095300100906     c                   leavesr
095400100906    1c                   endif
095500100906     c* non immettere in tipo
095600100906    1c                   if        vdptip<>*blanks  and vdpric='S'
095700100906     c                   eval      errl00 = *on
095800100906     c                   eval      dsmsms = msg(17)
095900100906     c                   leavesr
096000100906    1c                   endif
096100020319
096200020319      * Magazzino / tipo unità
096300100906    1c                   if        (vdpmag <> *blanks or vdpti2 > *zeros) and   --- 1 -->
096400020319     c                              errl00 = *off
096500100906    2c                   if        wtipri <> *blanks                            --- 2 -->
096600020319     c                   eval      errl00 = *on
096700020319     c                   eval      dsmsms = msg(6)
096800100906   x2c                   else                                                   <-- 2 -->
096900100907      * controllo magazzino obbligatorio se c'e' tipo
097000100906     c                   eval      ctrmag=vdpmag
097100020319     c                   exsr      chkmag
097200100906     c                   eval      vdpmag=ctrmag
097300100906     c
097400020319     c                   eval      wtipri = 'MAG'
097500020319     c                   eval      *in92 = *on
097600100906    2c                   endif                                                  <-- 2 ---
097700100906    1c                   endif                                                  <-- 1 ---
097800020319
097900020319      * Data scadenza manutenzione
098000100907    1c                   if        (vddsmd <> *zeros or vddsma <> *zeros) and   --- 1 -->
098100020319     c                              errl00 = *off
098200100907    2c                   if        wtipri <> *blanks                            --- 2 -->
098300020319     c                   eval      errl00 = *on
098400020319     c                   eval      dsmsms = msg(6)
098500100907   x2c                   else                                                   <-- 2 -->
098600020319      * controllo data 'dal'
098700100907    3c                   if        vddsmd = *zeros                              --- 3 -->
098800020319     c                   eval      errl00 = *on
098900020319     c                   eval      dsmsms = msg(9)
099000020319     c                   eval      *in84 = *on
099100100907    3c                   endif                                                  <-- 3 ---
099200020319     c                   clear                   w_vddsmd
099300020319     c                   clear                   wlbda8
099400020319     c                   z-add     vddsmd        g08dat
099500020319     c                   z-add     *zeros        g08inv
099600020319     c                   movel     '0'           g08err
099700020319     c                   call      'XSRDA8'
099800020319     c                   parm                    wlbda8
099900020319     c                   z-add     g08dat        vddsmd
100000020319     c                   z-add     g08inv        w_vddsmd
100100100907    3c                   if        g08err <> '0'                                --- 3 -->
100200020319     c                   eval      errl00 = *on
100300020319     c                   eval      *in84 = *on
100400020319     c                   eval      dsmsms = msg(9)
100500100907    3c                   endif                                                  <-- 3 ---
100600020319      * controllo data 'al'
100700100907    3c                   if        vddsma <> *zeros                             --- 3 -->
100800020319     c                   clear                   w_vddsma
100900020319     c                   clear                   wlbda8
101000020319     c                   z-add     vddsma        g08dat
101100020319     c                   z-add     *zeros        g08inv
101200020319     c                   movel     '0'           g08err
101300020319     c                   call      'XSRDA8'
101400020319     c                   parm                    wlbda8
101500020319     c                   z-add     g08dat        vddsma
101600020319     c                   z-add     g08inv        w_vddsma
101700100907    4c                   if        g08err <> '0'                                --- 4 -->
101800020319     c                   eval      errl00 = *on
101900020319     c                   eval      *in85 = *on
102000020319     c                   eval      dsmsms = msg(9)
102100100907    4c                   endif                                                  <-- 4 ---
102200100907    3c                   endif                                                  <-- 3 ---
102300020319     c                   eval      wtipri = 'DSM'
102400020319     c                   eval      *in93 = *on
102500100907    2c                   endif                                                  <-- 2 ---
102600100907    1c                   endif                                                  <-- 1 ---
102700100906     c
102800100906     c* Modello e matricola
102900100906    1c                   if        (vdpmod <> *blanks or vdpmat2<>*blanks) and   --- 1 -->
103000100906     c                              errl00 = *off
103100100906    2c                   if        wtipri <> *blanks                            --- 2 -->
103200100906     c                   eval      errl00 = *on
103300100906     c                   eval      dsmsms = msg(6)
103400100906   x2c                   else                                                   <-- 2 -->
103500100906     c     vdpmod        chain     unana08l
103600100906    3c                   if        not %found(unana08l)
103700100906     c                   eval      errl00 = *on
103800100906     c                   eval      *in86 = *on
103900100906     c                   eval      dsmsms = msg(15)
104000100906    3c                   endif                                                  <-- 4 ---
104100100906     c*
104200100906    3c                   if        errl00='0'
104300100906     c                   eval      wtipri = 'MOD'
104400100906     c                   eval      *in94 = *on
104500100906    3c                   endif                                                  <-- 4 ---
104600100906    2c                   endif                                                  <-- 4 ---
104700100906    1c                   endif                                                  <-- 4 ---
104800100907    c
104900100907      * Controllo parzializzazione magazzino
105000100907    1c                   if        vdsmag>0
105100100907     c                   eval      ctrmag=%editc(vdsmag:'X')
105200100907     c                   exsr      chkmag
105300100907    2c                   if        errl00='1'
105400100907     c                   leavesr
105500100907    2c                   endif
105600100907    1c                   endif
105700101029     c
105800101029     c* Controllo data rientro prima del
105900101029     c                   clear                   w_vdsrief
106000101029     c                   if        vdsrief>0
106100101029     c                   clear                   wlbda8
106200101029     c                   z-add     vdsrief       g08dat
106300101029     c                   z-add     *zeros        g08inv
106400101029     c                   movel     '0'           g08err
106500101029     c                   call      'XSRDA8'
106600101029     c                   parm                    wlbda8
106700101029     c                   z-add     g08dat        vdsrief
106800101029     c                   z-add     g08inv        w_vdsrief
106900101029    3c                   if        g08err <> '0'                                --- 3 -->
107000101029     c                   eval      errl00 = *on
107100101029     c                   eval      *in87 = *on
107200101029     c                   eval      dsmsms = msg(9)
107300101029    3c                   endif                                                  <-- 3 ---
107400101029    3c                   endif                                                  <-- 3 ---
107500020326
107600020326      * SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
107700020326      * CURSORE SUL CAMPO DEL CONTROL
107800020326     C     errl00        IFEQ      '1'
107900020326     C                   MOVEL     '0'           *IN43                          *NON EMETTO SFL
108000020326     C                   ELSE
108100020326     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
108200020326     C                   END
108300020319
108400930202     C                   ENDSR
108500930707     C*================================================================
108600930707     C*  CHKTIP - CONTROLLA IL TIPO DI UNITA
108700930707     C*================================================================
108800930707     C     CHKTIP        BEGSR
108900020318
109000020318      * CONTROLLO SE E' STATO INSERITO '?'
109100020319     C     '?'           SCAN      VDPTIP                                 99
109200020318
109300020318      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
109400020319     C                   if        *in99                                        --- 1 -->
109500020318     C                   clear                   TIBS02DS
109600020318     C                   movel     'R'           t02mod
109700020318     C                   movel     knsif         t02sif
109800020321     C                   movel     'UNT'         t02cod
109900020318     C                   call      'TIBS02R'
110000020318     C                   parm                    KPJBA
110100020318     C                   parm                    TIBS02DS
110200020318     C                   movel     T02ke1        vdptip
110300020318
110400020319     C                   endif                                                  <-- 1 ---
110500020318
110600020318      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
110700020318     C                   clear                   TIBS02DS
110800020318     C                   movel     'C'           T02mod
110900020318     C                   movel     knsif         t02sif
111000020321     C                   movel     'UNT'         t02cod
111100020318     C                   movel(p)  vdptip        T02ke1
111200020318     C                   call      'TIBS02R'
111300020318     C                   parm                    KPJBA
111400020318     C                   parm                    TIBS02DS
111500020318     C                   if        t02err <> *blanks
111600020319     C                   eval      errl00 = '1'
111700020319     C                   eval      *in80 = *on
111800020318     C                   movel     msg(7)        dsmsms
111900020318     C                   endif
112000020319
112100930707     C                   ENDSR
112200930707     C*================================================================
112300930707     C*  CHKMAG - CONTROLLA IL MAGAZZINO UBICAZIONE
112400930707     C*================================================================
112500930707     C     CHKMAG        BEGSR
112600930707     C*
112700930707     C* CONTROLLO SE E' STATO INSERITO '?'
112800100906     C     '?'           SCAN      ctrmag                                 99
112900930707     C*
113000930707     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
113100930707     C     *IN99         IFEQ      '1'                                          --- 1 -->
113200930707     C                   MOVEL     *BLANKS       §COD1             3
113300930707     C                   MOVE      *BLANKS       §DES1
113400941115     C                   CALL      'TNSD20R'
113500930707     C                   PARM                    §COD1
113600930707     C                   PARM                    §TIP              1
113700930707     C                   PARM                    §DES1            25
113800100906     C                   MOVEL     §COD1         ctrmag
113900930707     C                   END                                                    <-- 1 ---
114000930707     C*
114100930707     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
114200100906     C                   MOVE      ctrmag        ORGFIL
114300020529     C     KEYORG        CHAIN     AZORG21l                           99
114400930707     C     *IN99         IFEQ      '0'                                          --- 2 -->
114500100906     C                   MOVEL     ORGFIL        ctrmag
114600930707     C                   ELSE                                                   <-- 2 -->
114700020529     c     keyorg        chain     azorg01l
114800020529     c                   if        %found(azorg01l)
114900100906     c                   movel     orgfil        ctrmag
115000020529     c                   else
115100930707     C                   MOVEL     '1'           ERRL00
115200930707     C                   MOVEL     MSG(8)        DSMSMS
115300930707     C                   SETON                                        83
115400020529     c                   endif
115500930707     C                   END                                                    <-- 2 ---
115600020516      * l'utente deve essere abilitato al magazzino inserito
115700020516     c                   if        vdpmag <> '951'
115800100906     c                   movel     ctrmag        wmag
115900020523     c     wmag          lookup    skpoabi                                99
116000020516     c                   if        not *in99
116100020516     c                   eval      errl00 = *on
116200020516     c                   eval      dsmsms = msg(14)
116300020516     c                   eval      *in83 = *on
116400020516     c                   endif
116500020516     c                   endif
116600930707     C*
116700930707     C                   ENDSR
116800020516      *------------------------------------------------------------------------*
116900020516      *  CTRUTE - ROUTINE CONTROLLO UTENTE
117000020516      *------------------------------------------------------------------------*
117100020516     c     ctrute        begsr
117200020516
117300020516     c                   eval      *in76 = *off
117400020516
117500020516      * controllo se il profilo è abilitato
117600020516     c                   if        duterr = 'E'
117700020516     c                   eval      dsmsms = dutmsg
117800020516     c                   eval      *in76 = *on
117900020516     c                   endif
118000020516      * controllo le abilitazionei del profilo
118100020516     c                   if        *in76 = *off
118200020516     c                   movel     utefaf        dute01
118300020516     c                   if        §uteiun <> *blanks
118400020516     c                   eval      wabi = §uteiun
118500020516     c                   else
118600020516     c                   eval      wabi = uteaut
118700020516     c                   endif
118800020516     c                   clear                   tibs02ds
118900020516     c                   movel     'C'           t02mod
119000020516     c                   movel     knsif         t02sif
119100020516     c                   movel     'LAT'         t02cod
119200020516     c                   movel(p)  wabi          t02ke1
119300020516     c                   call      'TIBS02R'
119400020516     c                   parm                    kpjba
119500020516     c                   parm                    tibs02ds
119600020516     c                   movel     t02uni        dlat
119700020516     c                   if        §latabi = 'S'
119800020516     c                   eval      dsmsms= msg(13)
119900020516     c                   eval      *in76 = *on
120000020516     c                   endif
120100020516     c                   endif
120200020516
120300020516      * Utente ok richiamo pgm che ritorna i p.o. abilitati
120400020523     c                   if        *in76 = *off  and wabi <> 'AZ'
120500020516     c                   clear                   trul31ds
120600020516     c                   eval      i31abi = wabi
120700020516     c                   eval      i31cdi = dutdis
120800020516     c                   eval      i31car = dutare
120900020516     c                   eval      i31cpo = dutpou
121000020516     c                   call      'TRUL31R'
121100020516     c                   parm                    kpjba
121200020516     c                   parm                    trul31ds
121300020516     c                   endif
121400020517
121500090904      /free
121600090904
121700090904         SELECT;
121800090904
121900090904           // - Reperimento filiali gestibili per utente della LOGISTICA
122000090904           WHEN  DUTntw = c_Logistica;
122100090904
122200090904             setll  *loval  AZORG;
122300090904             read   AZORG;
122400090904
122500090904             DoW  Not  %eof(AZORG01L);
122600090904
122700090904               if  ORGfva = *blank;
122800090904
122900090904                 Og143 = ORGde3;
123000090904                 if  §OgNtw = DUTntw;
123100090904
123200090904                   ii += 1;
123300090904                   skPOabi(ii) = ORGfil;
123400090904
123500090904                 endif;
123600090904
123700090904               endif;
123800090904
123900090904               read   AZORG;
124000090904
124100090904             EndDo;
124200090904
124300090904      /end-free
124400090904
124500020529      * per abilitazione 'AZ' devo caricare tutti i p.o. di azorg21l
124600090904     c                   WHEN      wabi = 'AZ'
124700020529     c     *loval        setll     azorg21l
124800020517     c                   do        *hival
124900020529     c                   read      azorg21l
125000020517     c                   if        %eof
125100020517     c                   leave
125200020517     c                   endif
125300020517     c                   z-add     1             ii
125400020523     c     orgfil        lookup    skpoabi(ii)                            99
125500020517     c                   if        not *in99
125600020517     c                   z-add     1             ii
125700020523     c     *zeros        lookup    skpoabi(ii)                            99
125800020517     c                   if        *in99
125900020517     c                   movel     orgfil        skpoabi(ii)
126000020517     c                   endif
126100020517     c                   endif
126200020517     c                   enddo
126300020529      * e tutti i p.o. di azorg01l
126400020529     c     *loval        setll     azorg01l
126500020529     c                   do        *hival
126600020529     c                   read      azorg01l
126700020529     c                   if        %eof
126800020529     c                   leave
126900020529     c                   endif
127000020529     c                   z-add     1             ii
127100020529     c     orgfil        lookup    skpoabi(ii)                            99
127200020529     c                   if        not *in99
127300020529     c                   z-add     1             ii
127400020529     c     *zeros        lookup    skpoabi(ii)                            99
127500020529     c                   if        *in99
127600020529     c                   movel     orgfil        skpoabi(ii)
127700020529     c                   endif
127800020529     c                   endif
127900020529     c                   enddo
128000090904      *
128100020523      * se non è abilitazione 'AZ' passo la sk tornata dal TRUL31 nella sk di 500 elementi
128200090904     c                   OTHER
128300020523     c                   movea     skpo          skpoabi
128400020523     c                   z-add     1             ii
128500020523     c     951           lookup    skpoabi(ii)                            99
128600020523     c                   if        not *in99
128700020523     c                   z-add     1             ii
128800020523     c     *zeros        lookup    skpoabi(ii)                            99
128900020523     c                   if        *in99
129000021014     c                   movel     951           skpoabi(ii)
129100020523     c                   endif
129200020523     c                   endif
129300090904      *
129400090904     c                   ENDSL
129500020516
129600020516     c                   endsr
129700930201     C*================================================================
129800930201     C* LANPGM - LANCIA UN PROGRAMMA
129900930201     C*================================================================
130000930201     C     LANPGM        BEGSR
130100930201     C*
130200930201     C* IMPOSTO PARAMETRI PER IL PGM
130300930707     C                   MOVEL     '0'           DSUF03                         PREMUTO F03
130400930707     C                   MOVEL     '0'           DSUERR                         '1'=ERRORE
130500930707     C                   MOVEL     *BLANKS       DSUMSG                         MESSAGGIO
130600930707     C                   MOVE      WOPZ1         DSUOP1                         *OPZIONE LIV.PGM
130700020322     C                   Z-ADD     vidtip        DSUTIP                         TIPO
130800930707     C                   Z-ADD     VIDCOD        DSUCOD                         CODICE
130900930712     C                   Z-ADD     *ZEROS        DSUMAG                         MAGAZZINO
131000930712     C                   Z-ADD     *ZEROS        DSUPRG                         PROGRESSIVO MOV.
131100930906     C                   Z-ADD     *ZEROS        DSUDTM                         DATA MOVIMENTO
131200930201     C*
131300930201     C* LANCIO PROGRAMMA
131400940105     C                   MOVEL     DSUNIT        KPJBU
131500930312     C                   CALL      WPGM
131600940105     C                   PARM                    KPJBA
131700940105     C                   MOVEL     KPJBU         DSUNIT
131800930201     C*
131900930201     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
132000930707     C     DSUF03        IFEQ      '1'
132100930201     C                   MOVEL     '1'           F03L00
132200930201     C                   MOVEL     '1'           ERRL00
132300930201     C                   ELSE
132400930201     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
132500930707     C                   MOVEL     DSUMSG        DSMSMS
132600930201     C                   END
132700930201     C*
132800930201     C                   ENDSR
132900930312     C*================================================================
133000930312     C* CHKFOP - CONTROLLA TIPO GESTIONE E IMPOSTA FUNZ/OPZ
133100930312     C*================================================================
133200930312     C     CHKFOP        BEGSR
133300930312     C*
133400930312     C* CONTROLLO CHE VI SIA UNA OPZIONE DI PROCEDURA VALIDA IN INPUT
133500930312     C* ALTRIMENTI FORZO INTERROGAZIONE
133600940105     C     DSUOP0        IFNE      ' 2'                                         *GESTIONE
133700940105     C     DSUOP0        ANDNE     ' 5'                                         *INTERROGAZIONE
133800170314     C     DSUOP0        ANDNE     ' 1'                                         *scelta
133900170314     C     DSUOP0        ANDNE     ' S'                                         *STORICIZZAZIONE
134000940105     C                   MOVE      ' 5'          DSUOP0
134100930312     C                   END
134200070509     C*
134300070509     C* SE IL PGM E' STATO LANCIATO IN SELEZIONE ABILITO SOLO LA "1"
134400070509     C     DSUOP0        IFEQ      ' 1'
134500070509     C                   MOVEL     OPD(1)        OPDRIG                         *INTERROGAZIONE
134600070509     C                   MOVEL     'S'           OPDATT
134700070509     C                   MOVEL     OPDRIG        OPD(1)
134800070509     C                   ENDIF
134900930312     C*
135000930312     C* SE L'UTENTE E' ENTRATO IN GESTIONE LO ABILITO A TUTTE LE FUNZIONI
135100940105     C     DSUOP0        IFEQ      ' 2'
135200930312     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
135300930312     C                   MOVEL     'S'           OPDATT
135400930312     C                   MOVEL     OPDRIG        OPD(2)
135500971015     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
135600971015     C                   MOVEL     'S'           OPDATT
135700971015     C                   MOVEL     OPDRIG        OPD(3)
135800930312     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
135900930312     C                   MOVEL     'S'           OPDATT
136000930312     C                   MOVEL     OPDRIG        OPD(4)
136100930312     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
136200930312     C                   MOVEL     'S'           OPDATT
136300930312     C                   MOVEL     OPDRIG        OPD(5)
136400930312     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
136500930312     C                   MOVEL     'S'           OPDATT
136600930312     C                   MOVEL     OPDRIG        OPD(7)
136700971015     C                   MOVEL     OPD(8)        OPDRIG                         *MOVIMENTI
136800971015     C                   MOVEL     'S'           OPDATT
136900971015     C                   MOVEL     OPDRIG        OPD(8)
137000160923     C                   MOVEL     OPD(9)        OPDRIG                         *RIENTRO
137100140128     C                   MOVEL     'S'           OPDATT
137200140128     C                   MOVEL     OPDRIG        OPD(9)
137300930312     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
137400930312     C                   MOVEL     'S'           FUDATT
137500930312     C                   MOVEL     FUDRIG        FUD(6)
137600930312     C                   END
137700930312     C*
137800930312     C* SE IL PGM E' STATO LANCIATO IN INTERROGAZIONE ABILITO SOLO QUELLA
137900940105     C     DSUOP0        IFEQ      ' 5'
138000930312     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
138100930312     C                   MOVEL     'S'           OPDATT
138200930312     C                   MOVEL     OPDRIG        OPD(5)
138300971015     C                   MOVEL     OPD(8)        OPDRIG                         *MOVIMENTI
138400971015     C                   MOVEL     'S'           OPDATT
138500971015     C                   MOVEL     OPDRIG        OPD(8)
138600930312     C                   END
138700160923     C* SE IL PGM E' STATO LANCIATO IN STORICIZZAZIONE ABILITO  QUELLA È INTERR
138800160923     C     DSUOP0        IFEQ      ' S'
138900160923     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
139000160923     C                   MOVEL     'S'           OPDATT
139100160923     C                   MOVEL     OPDRIG        OPD(5)
139200160923     C                   MOVEL     OPD(10)       OPDRIG                         *STORICIZZAZ
139300160923     C                   MOVEL     'S'           OPDATT
139400160923     C                   MOVEL     OPDRIG        OPD(8)
139500160923     C                   END
139600930312     C*
139700930312     C* SE SONO IN FILIALE NON CONCEDO CANCELLAZIONE,INSERIMENTO,RIPRISTINO
139800020709     c                   If        DutLpo <> 'S'
139900971015     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
140000971015     C                   MOVEL     'N'           OPDATT
140100971015     C                   MOVEL     OPDRIG        OPD(3)
140200930312     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
140300930312     C                   MOVEL     'N'           OPDATT
140400930312     C                   MOVEL     OPDRIG        OPD(4)
140500930312     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
140600930312     C                   MOVEL     'N'           OPDATT
140700930312     C                   MOVEL     OPDRIG        OPD(7)
140800930312     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
140900930312     C                   MOVEL     'N'           FUDATT
141000930312     C                   MOVEL     FUDRIG        FUD(6)
141100930312     C                   END
141200930312     C*
141300930312     C* IMPOSTO INDICE PER EMISSIONE OPZIONI A VIDEO
141400930312     C                   Z-ADD     0             OPZGRU            5 0
141500930312     C*
141600930312     C* GESTIONE OPZIONI A VIDEO
141700930312     C                   EXSR      OPZGES
141800930312     C*
141900930312     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
142000930312     C                   Z-ADD     0             FUNGRU            5 0
142100930312     C*
142200930312     C* GESTIONE TASTI FUNZIONALI A VIDEO
142300930312     C                   EXSR      FUNGES
142400930312     C*
142500930312     C                   ENDSR
142600000000     C*================================================================
142700000000     C*  RUTINZ - ROUTINE INIZIALE
142800000000     C*================================================================
142900000000     C     RUTINZ        BEGSR
143000000000     C*
143100930707     C* RICEVIMENTO PARAMETRI DALL'AZIONE (' 2'=GEST.,' 5'=INTERR.)
143200000000     C     *ENTRY        PLIST
143300930707     C                   PARM                    KPJBA
143400070509     c                   parm                    UNANAds
143500940105     C                   MOVEL     KPJBU         DSUNIT
143600070509      * - impostazione iniziale dei parametri di output per chiamante
143700070509     c                   eval      DSUf03 = *off                                PREMUTO F03
143800070509     c                   eval      DSUerr = *off                                '1'=ERRORE
143900070509     c                   eval      DSUmsg = *blanks                             MESSAGGIO
144000020516
144100020516     c     *dtaara       define    §azute        azuteds
144200020516     c     *dtaara       define    §datiute      ddatiute
144300020516
144400020516     c                   in(E)     *dtaara
144500020516     c                   if        %Error or rsut = *blanks
144600020516     c                   clear                   tibs34ds
144700020516     c                   call      'TIBS34R'
144800020516     c                   parm                    Tibs34Ds
144900020516     c                   in        *dtaara
145000020516     c                   endif
145100020709
145200020709      * controllo il profilo utente
145300020709     c                   exsr      ctrute
145400020709     c                   if        *in76
145500020709     c                   eval      sflult = 0
145600020709     c                   endif
145700020516
145800930204     C*
145900930204     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
146000930204     C     XMSGPA        PLIST
146100930204     C                   PARM                    DSMSMS
146200930204     C                   PARM                    DSMSPG
146300930204     C                   PARM                    DSMSKE
146400930204     C                   PARM                    DSMSER
146500020319
146600020319      * CHIAVE LETTURA UNANA02L
146700930707     C     KUNA02        KLIST
146800020319     C                   KFLD                    UNATIP
146900020319     C                   KFLD                    UNAMAT
147000140128     C     KEYUNA        KLIST
147100140128     c                   kfld                    vidtip
147200140128     C                   KFLD                    vidCOD
147300020319
147400020319      * CHIAVE LETTURA UNANA03L
147500930707     C     KUNA03        KLIST
147600930707     C                   KFLD                    UNAMAG
147700930707     C                   KFLD                    UNATIP
147800170116     c
147900170116     C     KUNA13        KLIST
148000170116     C                   KFLD                    UNAcod
148100170116     C                   KFLD                    UNATIP
148200020319
148300020319      * CHIAVE LETTURA UNANA04L
148400930707     C     KUNA04        KLIST
148500930707     C                   KFLD                    UNADSM
148600020319
148700020319      * CHIAVE LETTURA UNANA07L
148800020319     C     KUNA07        KLIST
148900020319     C                   KFLD                    UNAMAT
149000020319     C                   KFLD                    UNATIP
149100100906     C     KUNA08        KLIST
149200100906     C                   KFLD                    UNAmod
149300100906     C                   KFLD                    UNAmat
149400020319
149500020529     C* CHIAVE LETTURA AZORG21l/azorg01l - COMPLETA
149600930707     C     KEYORG        KLIST
149700930707     C                   KFLD                    ORGFIL
149800930707     C*
149900000000     C*
150000930129     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
150100930204     C                   CALL      'XKEY'
150200930204     C                   PARM                    DSKEY
150300000000     C*
150400000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
150500000000     C     *LIKE         DEFINE    SFLNBR        SFLPAG
150600000000     C     *LIKE         DEFINE    SFLPAG        WFLPAG
150700000000     C     *LIKE         DEFINE    SFLNBR        SFLEOF
150800000000     C     *LIKE         DEFINE    SFLNBR        SFLERR
150900000000     C     *LIKE         DEFINE    SFLNBR        SFLULT
151000000000     C     *LIKE         DEFINE    SFLNBR        WFLULT
151100000000     C     *LIKE         DEFINE    SFLNBR        MSGULT
151200000000     C     *LIKE         DEFINE    SFLNBR        SFLRIG
151300000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
151400000000     C*
151500930127     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
151600000000     C*   -NOME PGM CHIAMANTE
151700000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
151800941115     C                   MOVEL     'TNSDU1R'     DSMSPG
151900941115     C                   MOVEL     'TNSDU1R'     MSGQUE
152000000000     C                   MOVEL     *BLANKS       MSGKEY
152100930127     C                   MOVEL     *BLANKS       DSMSMS
152200930204     C                   MOVEL     *BLANKS       DSMSKE
152300930204     C                   Z-ADD     0             DSMSER
152400000000     C*
152500930129     C* INIZIALIZZO SWITCH DI LIVELLO 0 (PGM)
152600930127     C                   MOVEL     '0'           F12L00            1
152700930127     C                   MOVEL     '0'           F03L00            1
152800930127     C                   MOVEL     '0'           FINL00            1
152900930127     C                   MOVEL     '0'           ERRL00            1
153000930128     C                   MOVEL     '0'           FLFINE            1
153100930127     C*
153200930312     C* IMPOSTO DEPOSITI PER CONFRONTI
153300930707     C                   MOVEL     *BLANKS       DEPTIP
153400930707     C                   MOVEL     *BLANKS       DEPMAT
153500020325     c                   clear                   depric
153600930707     C                   MOVEL     *BLANKS       DEPMAG
153700100907     C                   MOVEL     *BLANKS       DspMAG
153800100907     C                   MOVEL     *BLANKS       Dsprief
153900100907     C                   MOVEL     *BLANKS       depmod
154000100907     C                   MOVEL     *BLANKS       depmat2
154100020319     C                   Z-ADD     *ZEROS        DEPTI2
154200020319     C                   Z-ADD     *ZEROS        dedsmd
154300020319     C                   Z-ADD     *ZEROS        dedsma
154400101029     c                   clear                   depdes
154500101029     c                   clear                   depnot
154600930202     C*
154700941128     C* IMPOSTO TIPO DI RICERCA DI DEFAULT --> MAT
154800020319     C                   MOVEL     'TIP'         WTIPRI
154900020319     c                   eval      *in90 = *on
155000100924     c* se passata matricola eseguo ricerca
155100100927     c                   if        SDSprm > 1  and
155200100924     c                             UIMmat<>*blanks
155300100924     c                   movel     UIMmat        vdpmat
155400100924     c                   movel     UIMmat        depmat
155500100927     c                   clear                   unanaDS
155600100924     c                   exsr      chkpar
155700100924     c                   endif
155800000000     C*
155900000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
156000000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
156100170116     C                   Z-ADD     14            SFLPAG
156200930312     C*
156300930312     C* IMPOSTO LE OPZIONI E LE FUNZIONI A VIDEO
156400930312     C                   EXSR      CHKFOP
156500930225     C*
156600930225     C* IMPOSTO INTERROGAZIONE/GESTIONE NELLA TESTATA DELLA VIDEATA
156700070509     c                   select
156800070509     c                   when      DSUop0 = ' 1'
156900070509     C                   eval      VIDing = '   SELEZIONE    '
157000070509     c                   when      DSUop0 = ' 2'
157100930225     C                   MOVEL     '    GEST'    VIDING
157200930225     C                   MOVE      'IONE    '    VIDING
157300160923     c                   when      DSUop0 = ' S'
157400160923     C                   MOVEL     'STORICIZ'    VIDING
157500160923     C                   MOVE      'ZAZIONE '    VIDING
157600070509     c                   other
157700930225     C                   MOVEL     ' INTERRO'    VIDING
157800930225     C                   MOVE      'GAZIONE '    VIDING
157900070509     c                   endsl
158000160923
158100160923     c     *iso          movel     datasys       dateu             8 0
158200020325
158300000000     C                   ENDSR
158400020326
158500930312     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
158600930312     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
158700930312     C* LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
158800930312     C* STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
158900930312     C* NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
159000930312     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
159100930312     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
159200930312     C*
159300930312     C*================================================================
159400930312     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA
159500930312     C*          CARICARE
159600930312     C*================================================================
159700930312     C     POSSFL        BEGSR
159800930312     C*
159900930312     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
160000930312     C                   EXSR      CLRSFL
160100930312     C*
160200930312     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
160300930312     C                   EXSR      SETFIL
160400930312     C*
160500930312     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
160600930312     C     SFLEOF        IFEQ      0
160700020326     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
160800930312     C                   EXSR      CARSFL
160900930312     C                   END
161000930312     C*
161100930312     C                   ENDSR
161200000000     C*================================================================
161300000000     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
161400000000     C*================================================================
161500000000     C     ENTATT        BEGSR
161600000000     C*
161700000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
161800000000     C                   Z-ADD     0             OPZOK             1 0
161900000000     C*
162000000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
162100000000     C                   Z-ADD     0             SFLVAR            5 0
162200000000     C*
162300000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
162400000000     C                   Z-ADD     0             SFLERR
162500000000     C*
162600000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
162700000000     C                   Z-ADD     SFLULT        WFLULT
162800000000     C*
162900000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
163000941115     C                   READC     SDU1D05                                98
163100930202     C     *IN98         DOWEQ     '0'
163200000000     C*
163300000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
163400000000     C                   ADD       1             SFLVAR
163500000000     C*
163600000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
163700000000     C     OPZOK         IFEQ      0
163800000000     C*
163900000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
164000000000     C     OPZKEY        IFNE      *BLANKS
164100000000     C                   EXSR      OPZCON
164200000000     C                   END
164300000000     C*
164400000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
164500000000     C*            2. PULIZIA SCELTA IN SFL
164600000000     C     OPZOK         IFEQ      0
164700000000     C                   MOVEL     '0'           *IN41
164800000000     C                   MOVEL     *BLANKS       OPZKEY
164900000000     C                   ELSE
165000000000     C*
165100000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
165200000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
165300000000     C                   MOVEL     '1'           *IN41
165400000000     C                   Z-ADD     SFLULT        SFLERR
165500000000     C                   END
165600000000     C                   ELSE
165700000000     C*
165800000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
165900000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
166000000000     C                   MOVEL     '1'           *IN41
166100000000     C                   END                                                    OPZOK=0
166200000000     C*
166300000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
166400000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
166500000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
166600000000     C     OPZOK         IFNE      0
166700000000     C                   Z-ADD     SFLERR        SFLNBR
166800000000     C                   ELSE
166900000000     C                   Z-ADD     SFLULT        SFLNBR
167000000000     C                   END
167100000000     C*
167200000000     C* AGGIORNO RECORD SFL
167300101027     c                   if        vidrief='SI'
167400100906     c                   setoff                                       15
167500100906     c                   else
167600100906     c                   seton                                        15
167700100906     c                   endif
167800100906     c
167900941115     C                   UPDATE    SDU1D05
168000000000     C*
168100941115     C                   READC     SDU1D05                                98
168200000000     C                   END                                                    DO WHILE
168300000000     C*
168400000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
168500000000     C                   Z-ADD     WFLULT        SFLULT
168600000000     C*
168700000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
168800000000     C     SFLVAR        IFEQ      0
168900000000     C                   MOVEL     '1'           FINL00
169000000000     C                   END
169100000000     C*
169200000000     C                   ENDSR
169300000000     C*================================================================
169400000000     C* ELIMINAZIONE CONTENUTO SFL
169500000000     C*================================================================
169600000000     C     CLRSFL        BEGSR
169700000000     C*
169800000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
169900000000     C* (0=NON FINE FILE  1=FINE FILE)
170000000000     C                   Z-ADD     0             SFLEOF
170100000000     C*
170200000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
170300000000     C                   Z-ADD     0             SFLULT
170400000000     C*
170500000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
170600000000     C                   MOVEL     '1'           *IN40
170700941115     C                   WRITE     SDU1D06
170800000000     C                   MOVEL     '0'           *IN40
170900000000     C*
171000000000     C                   ENDSR
171100000000     C*================================================================
171200000000     C* CARSFL - CARICAMENTO SFL
171300000000     C*================================================================
171400000000     C     CARSFL        BEGSR
171500000000     C*
171600000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
171700000000     C     SFLULT        IFEQ      0
171800000000     C                   EXSR      SETFIL
171900000000     C                   END
172000000000     C*
172100000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
172200000000     C                   Z-ADD     SFLULT        WFLULT
172300000000     C*
172400000000     C* CICLO:
172500000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
172600000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
172700000000     C                   Z-ADD     1             SFLRIG
172800000000     C     SFLRIG        DOWLE     SFLPAG
172900000000     C     SFLEOF        ANDEQ     0
173000020319
173100020319     c                   eval      wnosfl = *off
173200020319
173300000000     C*
173400000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
173500000000     C                   MOVEL     *BLANKS       OPZKEY
173600020319
173700020326      * controllo se devo scrivere il record nel subfile
173800020319     c                   exsr      ctrsfl
173900020319
174000020319     c                   if        wnosfl = *off
174100000000     C*
174200000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
174300000000     C                   EXSR      IRESFL
174400000000     C*
174500000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
174600000000     C                   ADD       1             SFLULT
174700000000     C*
174800000000     C* SCRITTURA RECORD SFL
174900941115     C                   WRITE     SDU1D05
175000000000     C                   ADD       1             SFLRIG
175100020319
175200020319     c                   endif
175300000000     C*
175400000000     C* LETTURA FILE DA CARICARE
175500000000     C                   EXSR      LETFIL
175600000000     C                   END                                                    DO WHILE
175700000000     C*
175800000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
175900000000     C     WFLULT        IFNE      SFLULT
176000000000     C                   Z-ADD     WFLULT        SFLNBR
176100000000     C                   ADD       1             SFLNBR
176200000000     C                   END
176300000000     C*
176400000000     C                   ENDSR
176500000000     C*================================================================
176600000000     C* EMISSIONE SUBFILE A VIDEO
176700000000     C*================================================================
176800000000     C     OUTSFL        BEGSR
176900000000     C*
177000000000     C* WRITE RECORD TESTATA VIDEO
177100941115     C                   WRITE     SDU1D01
177200930205     C*
177300930205     C* VISUALIZZAZIONE MESSAGGIO
177400930205     C                   EXSR      SNDMSG
177500160923     C*
177600000000     C* WRITE RECORD OPZIONI VIDEO
177700941115     C                   WRITE     SDU1D02
177800000000     C*
177900000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
178000000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
178100000000     C     SFLEOF        IFEQ      0
178200000000     C                   MOVEL     '0'           *IN42
178300000000     C                   ELSE
178400000000     C*
178500000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
178600000000     C                   MOVEL     '1'           *IN42
178700000000     C                   END
178800000000     C*
178900000000     C* WRITE RECORD TASTI FUNZIONALI
179000941115     C                   WRITE     SDU1D09
179100000000     C*
179200000000     C* CASO DI SFL VUOTO
179300000000     C     SFLULT        IFLE      0
179400000000     C*
179500000000     C*    - WRITE RECORD VUOTO
179600020326     C                   MOVEL     '0'           *IN43                          *NON VISUALIZZA SFL
179700941115     C                   WRITE     SDU1D0A
179800020326     C                   WRITE     SDU1D06                                      *SFL CONTROL
179900020326     C                   READ      SDU1D06                                99
180000000000     C*
180100000000     C                   ELSE
180200000000     C*
180300000000     C* CASO DI SFL CON RECORDS
180400000000     C*
180500000000     C*    - WRITE RECORD CONTROLLO SFL
180600941115     C                   WRITE     SDU1D06
180700941115     C                   READ      SDU1D06                                99
180800000000     C                   END
180900000000     C*
181000000000     C                   ENDSR
181100000000     C*================================================================
181200000000     C* SNDMSG - MANDA MESSAGGIO
181300000000     C*================================================================
181400000000     C     SNDMSG        BEGSR
181500000000     C*
181600000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
181700000000     C                   MOVEL     '1'           *IN40
181800941115     C                   WRITE     SDU1DMC
181900000000     C                   MOVEL     '0'           *IN40
182000000000     C*
182100000000     C* VISUALIZZAZIONE MESSAGGIO
182200000000     C                   Z-ADD     0             MSGULT
182300930204     C                   CALL      'XMSG'        XMSGPA
182400930127     C     DSMSER        IFEQ      0
182500930127     C                   MOVEL     DSMSKE        MSGKEY
182600000000     C                   END
182700000000     C                   Z-ADD     1             MSGULT
182800941115     C                   WRITE     SDU1DMS                                      SFL MESS
182900941115     C                   WRITE     SDU1DMC                                      CONTR MESS
183000000000     C*
183100000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
183200930127     C                   MOVEL     *BLANKS       DSMSMS
183300000000     C*
183400000000     C                   ENDSR
183500000000     C*================================================================
183600000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
183700000000     C*================================================================
183800000000     C     FUNCON        BEGSR
183900000000     C*
184000000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
184100000000     C                   EXSR      FUNVAR
184200000000     C*
184300000000     C* PULISCO CAMPI DI LAVORO
184400000000     C                   Z-ADD     0             I                 5 0
184500000000     C                   Z-ADD     0             W                 5 0
184600000000     C*
184700000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
184800000000     C     FUNKEY        IFNE      *BLANKS
184900100922     C     1             DO        25            I
185000000000     C                   MOVEL     FUD(I)        FUDRIG
185100000000     C*
185200000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
185300000000     C     FUNKEY        IFEQ      FUDFUN
185400000000     C     FUDATT        ANDEQ     'S'
185500000000     C                   Z-ADD     I             W
185600000000     C                   END
185700000000     C                   END                                                    DO WHILE
185800000000     C*
185900000000     C* FUNZIONE NON ATTIVA (ERRORE)
186000000000     C     W             IFEQ      0
186100000000     C*
186200000000     C* EMISSIONE MESSAGGIO DI ERRORE
186300930127     C                   MOVEL     MSG(2)        DSMSMS
186400000000     C                   END                                                    W=0
186500000000     C                   END                                                    FUNKEY NE *BLANKS
186600000000     C*
186700000000     C* FUNZIONE ATTIVA (PROCEDI ...)
186800000000     C     W             IFNE      0
186900000000     C     FUNKEY        OREQ      *BLANKS
187000000000     C*
187100000000     C     FUNKEY        IFEQ      *BLANKS
187200000000     C                   EXSR      ENTGES
187300000000     C                   END
187400000000     C     FUNKEY        IFEQ      '03'
187500000000     C                   EXSR      F03GES
187600000000     C                   END
187700000000     C     FUNKEY        IFEQ      '05'
187800000000     C                   EXSR      F05GES
187900000000     C                   END
188000930126     C     FUNKEY        IFEQ      '06'
188100930126     C                   EXSR      F06GES
188200000000     C                   END
188300000000     C     FUNKEY        IFEQ      '12'
188400000000     C                   EXSR      F12GES
188500000000     C                   END
188600000000     C     FUNKEY        IFEQ      '17'
188700000000     C                   EXSR      F17GES
188800000000     C                   END
188900000000     C     FUNKEY        IFEQ      '18'
189000000000     C                   EXSR      F18GES
189100000000     C                   END
189200930129     C     FUNKEY        IFEQ      '21'
189300930129     C                   EXSR      F21GES
189400930129     C                   END
189500000000     C     FUNKEY        IFEQ      '22'
189600000000     C                   EXSR      F22GES
189700000000     C                   END
189800000000     C     FUNKEY        IFEQ      '23'
189900000000     C                   EXSR      F23GES
190000000000     C                   END
190100000000     C     FUNKEY        IFEQ      '24'
190200000000     C                   EXSR      F24GES
190300000000     C                   END
190400000000     C     FUNKEY        IFEQ      'RU'                                         ROLLUP
190500000000     C                   EXSR      RUPGES
190600000000     C                   END
190700000000     C     FUNKEY        IFEQ      'HP'                                         HELP
190800000000     C                   EXSR      HELGES
190900000000     C                   END
191000000000     C*
191100000000     C                   END
191200000000     C*
191300000000     C                   ENDSR
191400000000     C*================================================================
191500000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
191600000000     C*================================================================
191700000000     C     FUNVAR        BEGSR
191800000000     C*
191900000000     C* INIZIALIZZA VARIABILE -FUNKEY-
192000000000     C*
192100000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
192200000000     C     KEY           IFEQ      ENTER
192300000000     C                   MOVEL     *BLANKS       FUNKEY
192400000000     C                   END
192500000000     C     KEY           IFEQ      F1
192600000000     C                   MOVEL     '01'          FUNKEY
192700000000     C                   END
192800000000     C     KEY           IFEQ      F2
192900000000     C                   MOVEL     '02'          FUNKEY
193000000000     C                   END
193100000000     C     KEY           IFEQ      F3
193200000000     C                   MOVEL     '03'          FUNKEY
193300000000     C                   END
193400000000     C     KEY           IFEQ      F4
193500000000     C                   MOVEL     '04'          FUNKEY
193600000000     C                   END
193700000000     C     KEY           IFEQ      F5
193800000000     C                   MOVEL     '05'          FUNKEY
193900000000     C                   END
194000000000     C     KEY           IFEQ      F6
194100000000     C                   MOVEL     '06'          FUNKEY
194200000000     C                   END
194300000000     C     KEY           IFEQ      F7
194400000000     C                   MOVEL     '07'          FUNKEY
194500000000     C                   END
194600000000     C     KEY           IFEQ      F8
194700000000     C                   MOVEL     '08'          FUNKEY
194800000000     C                   END
194900000000     C     KEY           IFEQ      F9
195000000000     C                   MOVEL     '09'          FUNKEY
195100000000     C                   END
195200000000     C     KEY           IFEQ      F10
195300000000     C                   MOVEL     '10'          FUNKEY
195400000000     C                   END
195500000000     C     KEY           IFEQ      F11
195600000000     C                   MOVEL     '11'          FUNKEY
195700000000     C                   END
195800000000     C     KEY           IFEQ      F12
195900000000     C                   MOVEL     '12'          FUNKEY
196000000000     C                   END
196100000000     C     KEY           IFEQ      F13
196200000000     C                   MOVEL     '13'          FUNKEY
196300000000     C                   END
196400000000     C     KEY           IFEQ      F14
196500000000     C                   MOVEL     '14'          FUNKEY
196600000000     C                   END
196700000000     C     KEY           IFEQ      F15
196800000000     C                   MOVEL     '15'          FUNKEY
196900000000     C                   END
197000000000     C     KEY           IFEQ      F16
197100000000     C                   MOVEL     '16'          FUNKEY
197200000000     C                   END
197300000000     C     KEY           IFEQ      F17
197400000000     C                   MOVEL     '17'          FUNKEY
197500000000     C                   END
197600000000     C     KEY           IFEQ      F18
197700000000     C                   MOVEL     '18'          FUNKEY
197800000000     C                   END
197900000000     C     KEY           IFEQ      F19
198000000000     C                   MOVEL     '19'          FUNKEY
198100000000     C                   END
198200000000     C     KEY           IFEQ      F20
198300000000     C                   MOVEL     '20'          FUNKEY
198400000000     C                   END
198500000000     C     KEY           IFEQ      F21
198600000000     C                   MOVEL     '21'          FUNKEY
198700000000     C                   END
198800000000     C     KEY           IFEQ      F22
198900000000     C                   MOVEL     '22'          FUNKEY
199000000000     C                   END
199100000000     C     KEY           IFEQ      F23
199200000000     C                   MOVEL     '23'          FUNKEY
199300000000     C                   END
199400000000     C     KEY           IFEQ      F24
199500000000     C                   MOVEL     '24'          FUNKEY
199600000000     C                   END
199700000000     C     KEY           IFEQ      ROLLUP
199800000000     C                   MOVEL     'RU'          FUNKEY
199900000000     C                   END
200000000000     C     KEY           IFEQ      HELP
200100000000     C                   MOVEL     'HP'          FUNKEY
200200000000     C                   END
200300000000     C*
200400000000     C                   ENDSR
200500000000     C*================================================================
200600000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
200700000000     C*================================================================
200800000000     C     FUNGES        BEGSR
200900000000     C*
201000000000     C* PULISCO CAMPI DI LAVORO
201100000000     C                   Z-ADD     0             W                 5 0
201200000000     C*
201300000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
201400000000     C* E CARICO TABELLA FUNZIONI ATTIVE
201500000000     C     1             DO        24            I
201600000000     C                   MOVEL     FUD(I)        FUDRIG
201700000000     C*
201800000000     C* PULIZIA FUNZIONI PRECEDENTI
201900000000     C                   MOVEL     *BLANKS       FUV(I)
202000000000     C*
202100000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
202200000000     C* (SALTA F23 E F24)
202300000000     C     FUDATT        IFEQ      'S'
202400000000     C     FUDFUN        ANDNE     '23'
202500000000     C     FUDFUN        ANDNE     '24'
202600000000     C                   ADD       1             W
202700000000     C                   MOVEL     FUDDES        FUV(W)
202800000000     C                   END
202900000000     C                   END                                                    DO WHILE
203000000000     C*
203100000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
203200000000     C* ALTRIMENTI DISATTIVO F24
203300000000     C     1             DO        24            I
203400000000     C                   MOVEL     FUD(I)        FUDRIG
203500000000     C*
203600000000     C     FUDFUN        IFEQ      '24'
203700000000     C     W             IFGT      6
203800000000     C                   MOVEL     'S'           FUDATT
203900000000     C                   ELSE
204000000000     C                   MOVEL     *BLANKS       FUDATT
204100000000     C                   END                                                    W GT 8
204200000000     C                   MOVEL     FUDRIG        FUD(I)
204300000000     C                   END
204400000000     C                   END                                                    DO WHILE
204500000000     C*
204600000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
204700000000     C     W             DIV       6             I
204800000000     C                   MVR                     Z
204900000000     C*
205000000000     C     Z             IFNE      0
205100000000     C     I             ADD       1             Z
205200000000     C                   ELSE
205300000000     C                   Z-ADD     I             Z
205400000000     C                   END
205500000000     C*
205600000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
205700000000     C                   ADD       1             FUNGRU
205800000000     C     FUNGRU        IFGT      Z
205900000000     C     FUNGRU        ORLT      1
206000000000     C                   Z-ADD     1             FUNGRU
206100000000     C                   END
206200000000     C*
206300000000     C* PRIMO GRUPPO DI 6 FUNZIONI
206400000000     C     FUNGRU        IFEQ      1
206500000000     C                   MOVEL     FUV(1)        FUN001
206600000000     C                   MOVEL     FUV(2)        FUN002
206700000000     C                   MOVEL     FUV(3)        FUN003
206800000000     C                   MOVEL     FUV(4)        FUN004
206900000000     C                   MOVEL     FUV(5)        FUN005
207000000000     C                   MOVEL     FUV(6)        FUN006
207100000000     C                   END
207200000000     C*
207300000000     C* SECONDO GRUPPO DI 6 FUNZIONI
207400000000     C     FUNGRU        IFEQ      2
207500000000     C                   MOVEL     FUV(7)        FUN001
207600000000     C                   MOVEL     FUV(8)        FUN002
207700000000     C                   MOVEL     FUV(9)        FUN003
207800000000     C                   MOVEL     FUV(10)       FUN004
207900000000     C                   MOVEL     FUV(11)       FUN005
208000000000     C                   MOVEL     FUV(12)       FUN006
208100000000     C                   END
208200000000     C*
208300000000     C* TERZO GRUPPO DI 6 FUNZIONI
208400000000     C     FUNGRU        IFEQ      3
208500000000     C                   MOVEL     FUV(13)       FUN001
208600000000     C                   MOVEL     FUV(14)       FUN002
208700000000     C                   MOVEL     FUV(15)       FUN003
208800000000     C                   MOVEL     FUV(16)       FUN004
208900000000     C                   MOVEL     FUV(17)       FUN005
209000000000     C                   MOVEL     FUV(18)       FUN006
209100000000     C                   END
209200000000     C*
209300000000     C* QUARTO GRUPPO DI 6 FUNZIONI
209400000000     C     FUNGRU        IFEQ      4
209500000000     C                   MOVEL     FUV(19)       FUN001
209600000000     C                   MOVEL     FUV(20)       FUN002
209700000000     C                   MOVEL     FUV(21)       FUN003
209800000000     C                   MOVEL     FUV(22)       FUN004
209900000000     C                   MOVEL     *BLANKS       FUN005
210000000000     C                   MOVEL     *BLANKS       FUN006
210100000000     C                   END
210200000000     C*
210300000000     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
210400000000     C                   MOVEL     *BLANKS       FUN007
210500000000     C                   MOVEL     *BLANKS       FUN008
210600000000     C*
210700000000     C     1             DO        24            I
210800000000     C                   MOVEL     FUD(I)        FUDRIG
210900000000     C     FUDFUN        IFEQ      '23'
211000000000     C     FUDATT        ANDEQ     'S'
211100000000     C                   MOVEL     FUDDES        FUN007
211200000000     C                   END
211300000000     C     FUDFUN        IFEQ      '24'
211400000000     C     FUDATT        ANDEQ     'S'
211500000000     C                   MOVEL     FUDDES        FUN008
211600000000     C                   END
211700000000     C                   END                                                    DO
211800000000     C*
211900000000     C                   ENDSR
212000000000     C*================================================================
212100000000     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
212200000000     C*================================================================
212300000000     C     OPZGES        BEGSR
212400000000     C*
212500000000     C* PULISCO CAMPI DI LAVORO
212600000000     C                   Z-ADD     0             W                 5 0
212700000000     C*
212800000000     C* RICERCO QUANTE OPZIONI SONO ATTIVE
212900000000     C* E CARICO TABELLA OPZIONI ATTIVE
213000000000     C     1             DO        24            I
213100000000     C                   MOVEL     OPD(I)        OPDRIG
213200000000     C*
213300000000     C* PULIZIA OPZIONI PRECEDENTI
213400000000     C                   MOVEL     *BLANKS       OPV(I)
213500000000     C*
213600000000     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
213700000000     C     OPDATT        IFEQ      'S'
213800000000     C                   ADD       1             W
213900000000     C                   MOVEL     OPDDES        OPV(W)
214000000000     C                   END
214100000000     C                   END                                                    DO WHILE
214200000000     C*
214300000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
214400000000     C* ALTRIMENTI DISATTIVO F23
214500000000     C     1             DO        24            I
214600000000     C                   MOVEL     FUD(I)        FUDRIG
214700000000     C*
214800000000     C     FUDFUN        IFEQ      '23'
214900000000     C     W             IFGT      8
215000000000     C                   MOVEL     'S'           FUDATT
215100000000     C                   ELSE
215200000000     C                   MOVEL     *BLANKS       FUDATT
215300000000     C                   END                                                    W GT 8
215400000000     C                   MOVEL     FUDRIG        FUD(I)
215500000000     C                   END
215600000000     C                   END                                                    DO WHILE
215700000000     C*
215800000000     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
215900000000     C     W             DIV       8             I
216000000000     C                   MVR                     Z                 5 0
216100000000     C*
216200000000     C     Z             IFNE      0
216300000000     C     I             ADD       1             Z
216400000000     C                   ELSE
216500000000     C                   Z-ADD     I             Z
216600000000     C                   END
216700000000     C*
216800000000     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
216900000000     C                   ADD       1             OPZGRU
217000000000     C     OPZGRU        IFGT      Z
217100000000     C     OPZGRU        ORLT      1
217200000000     C                   Z-ADD     1             OPZGRU
217300000000     C                   END
217400000000     C*
217500000000     C* PRIMO GRUPPO DI 8 OPZIONI
217600000000     C     OPZGRU        IFEQ      1
217700000000     C                   MOVEL     OPV(1)        OPZ001
217800000000     C                   MOVEL     OPV(2)        OPZ002
217900000000     C                   MOVEL     OPV(3)        OPZ003
218000000000     C                   MOVEL     OPV(4)        OPZ004
218100000000     C                   MOVEL     OPV(5)        OPZ005
218200000000     C                   MOVEL     OPV(6)        OPZ006
218300000000     C                   MOVEL     OPV(7)        OPZ007
218400000000     C                   MOVEL     OPV(8)        OPZ008
218500000000     C                   END
218600000000     C*
218700000000     C* SECONDO GRUPPO DI 8 OPZIONI
218800000000     C     OPZGRU        IFEQ      2
218900000000     C                   MOVEL     OPV(9)        OPZ001
219000000000     C                   MOVEL     OPV(10)       OPZ002
219100000000     C                   MOVEL     OPV(11)       OPZ003
219200000000     C                   MOVEL     OPV(12)       OPZ004
219300000000     C                   MOVEL     OPV(13)       OPZ005
219400000000     C                   MOVEL     OPV(14)       OPZ006
219500000000     C                   MOVEL     OPV(15)       OPZ007
219600000000     C                   MOVEL     OPV(16)       OPZ008
219700000000     C                   END
219800000000     C*
219900000000     C* TERZO GRUPPO DI 8 OPZIONI
220000000000     C     OPZGRU        IFEQ      3
220100000000     C                   MOVEL     OPV(17)       OPZ001
220200000000     C                   MOVEL     OPV(18)       OPZ002
220300000000     C                   MOVEL     OPV(19)       OPZ003
220400000000     C                   MOVEL     OPV(20)       OPZ004
220500000000     C                   MOVEL     OPV(21)       OPZ005
220600000000     C                   MOVEL     OPV(22)       OPZ006
220700000000     C                   MOVEL     OPV(23)       OPZ007
220800000000     C                   MOVEL     OPV(24)       OPZ008
220900000000     C                   END
221000000000     C                   ENDSR
221100000000     C*================================================================
221200000000     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
221300000000     C*================================================================
221400000000     C     OPZCON        BEGSR
221500000000     C*
221600000000     C* PULISCO CAMPI DI LAVORO
221700000000     C                   Z-ADD     0             I                 5 0
221800000000     C                   Z-ADD     0             W                 5 0
221900000000     C*
222000000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
222100000000     C     1             DO        24            I
222200000000     C                   MOVEL     OPD(I)        OPDRIG
222300000000     C*
222400000000     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
222500000000     C     OPZKEY        IFEQ      OPDOPZ
222600000000     C     OPZKEY        OREQ      OPDOP1
222700000000     C*
222800000000     C     OPDATT        IFEQ      'S'
222900000000     C                   MOVEL     OPDOPZ        OPZKEY
223000000000     C                   Z-ADD     I             W
223100000000     C                   END
223200000000     C                   END
223300000000     C*
223400000000     C                   END                                                    DO WHILE
223500000000     C*
223600000000     C* OPZIONE NON ATTIVA (ERRORE)
223700000000     C     W             IFEQ      0
223800000000     C*
223900000000     C* OPZIONE NON OK
224000000000     C                   Z-ADD     1             OPZOK
224100000000     C*
224200000000     C* EMISSIONE MESSAGGIO DI ERRORE
224300930127     C                   MOVEL     MSG(1)        DSMSMS
224400000000     C                   ELSE
224500000000     C*
224600000000     C* OPZIONE ATTIVA (PROCEDI ...)
224700000000     C*
224800930129     C     OPZKEY        IFEQ      ' 1'
224900000000     C                   EXSR      O01GES
225000000000     C                   END
225100000000     C     OPZKEY        IFEQ      ' 2'
225200000000     C                   EXSR      O02GES
225300000000     C                   END
225400000000     C     OPZKEY        IFEQ      ' 3'
225500000000     C                   EXSR      O03GES
225600000000     C                   END
225700000000     C     OPZKEY        IFEQ      ' 4'
225800020709     c                   If        §UteCun = *Blanks
225900020320     c                   eval      dsmsms = msg(11)
226000020320     c                   z-add     1             opzok
226100020320     c                   else
226200000000     C                   EXSR      O04GES
226300020320     c                   endif
226400000000     C                   END
226500000000     C     OPZKEY        IFEQ      ' 5'
226600000000     C                   EXSR      O05GES
226700000000     C                   END
226800000000     C     OPZKEY        IFEQ      ' 6'
226900000000     C                   EXSR      O06GES
227000000000     C                   END
227100930128     C     OPZKEY        IFEQ      ' 7'
227200930128     C                   EXSR      O07GES
227300930128     C                   END
227400930201     C     OPZKEY        IFEQ      ' 8'
227500930201     C                   EXSR      O08GES
227600930201     C                   END
227700140128     C     OPZKEY        IFEQ      ' R'
227800140128     C                   EXSR      O0RGES
227900140128     C                   END
228000160923     C     OPZKEY        IFEQ      ' S'
228100160923     c                   If        §UteCun = *Blanks
228200160923     c                   eval      dsmsms = msg(18)
228300160923     c                   z-add     1             opzok
228400160923     c                   else
228500160923     C                   EXSR      O0SGES
228600160923     c                   endif
228700160923     C                   END
228800000000     C*
228900000000     C                   END                                                    OPZ.NON ATTIVA
229000000000     C*
229100000000     C                   ENDSR
229200000000     C*================================================================
229300000000     C* NOTE PER COMPILAZIONE SCHIERE
229400000000     C*================================================================
229500000000     C* -MSG-
229600000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
229700000000     C* (NON TOCCARE MSG,1 E MSG,2
229800000000     C*
229900000000     C* -CMD-
230000000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
230100000000     C*
230200000000     C* -OPD-
230300000000     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
230400000000     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
230500000000     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
230600000000     C*    ESEMPIO: OPZIONI -1 -
230700000000     C*                     - 1-  SONO LA STESSA COSA
230800000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
230900000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
231000000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
231100000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
231200000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
231300000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
231400000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
231500000000     C*
231600000000     C* -FUD-
231700000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
231800000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
231900000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
232000000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
232100000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
232200000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
232300000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
232400000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
232500000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
232600000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
232700000000     C*
232800000000     C*================================================================
232900000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
233000941115TNSDU1R- Opzione non gestita.                                                  1
233100941115TNSDU1R- Tasto funzionale non gestito.                                         2
233200160923TNSDU1R- Utente non abilitato a questa funzione                                3
233300941115TNSDU1R- Unità annullata, non gestibile.    Consentito solo Ripristino.        4
233400941115TNSDU1R- Unità non annullata, impossibile ripristinare.                        5
233500941115TNSDU1R- Accettato solo un parametro di inizio lista.                          6
233600100906TNSDU1R- Tipo unità inesitente                                                 7
233700941115TNSDU1R- Magazzino ubicazione non accettabile.                                 8
233800941115TNSDU1R- Data non accettabile.                                                 9
233900020319TNSDU1R- Matricola non accettabile.                                           10
234000160923TNSDU1R- Utente non abilitato all'annullamento delle unità.
234100020325TNSDU1R- Matricola obbligatoria se richiesta ricerca per contenuto            12
234200020516TNSDU1R- Profilo non abilitato alla funzione                                  13
234300020516TNSDU1R- Profilo non abilitato al magazzino richiesto                         14
234400100906TNSDU1R- Modello inesistente                                                  15
234500100906TNSDU1R- Modello / Matricola inesisnte                                        16
234600100906TNSDU1R- Se ricerca per contenuto Matricola, non indicare il tipo unità       17
234700160923TNSDU1R- Utente non abilitato alla storicizzazione delle unità                18
234800170116TNSDU1R- Se immesso il codice per ricerca, immettere anche il tipo            19
234900170116TNSDU1R-
235000000000** ======== SCHIERA: CMD  (COMANDI)                        ================
235100000000WRKSPLF
235200000000
235300000000
235400000000
235500000000
235600000000** ======== SCHIERA: OPD  OPZIONI GESTITE NEL PGM (ATTIVA=S) ==========
235700930312 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
235800930312 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
235900971015 3=Duplica         -3 -     (S/N)=N                        opz. 03¬
236000930312 4=Cancellazione   -4 -     (S/N)=N                        opz. 04¬
236100930312 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
236200000000                   -6 -     (S/N)=N                        opz. 06¬
236300930312 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
236400971013 8=Movimenti       -8 -     (S/N)=N                        opz. 08¬
236500140128 R=S/N Rientro     -R -     (S/N)=N                        opz. 09¬
236600160923 S=Storicizzazione -S -     (S/N)=N                        opz. 10¬
236700000000                   -  -     (S/N)=N                        opz. 11¬
236800000000                   -  -     (S/N)=N                        opz. 12¬
236900000000                   -  -     (S/N)=N                        opz. 13¬
237000000000                   -  -     (S/N)=N                        opz. 14¬
237100000000                   -  -     (S/N)=N                        opz. 15¬
237200000000                   -  -     (S/N)=N                        opz. 16¬
237300000000                   -  -     (S/N)=N                        opz. 17¬
237400000000                   -  -     (S/N)=N                        opz. 18¬
237500000000                   -  -     (S/N)=N                        opz. 19¬
237600000000                   -  -     (S/N)=N                        opz. 20¬
237700000000                   -  -     (S/N)=N                        opz. 21¬
237800000000                   -  -     (S/N)=N                        opz. 22¬
237900000000                   -  -     (S/N)=N                        opz. 23¬
238000000000                   -  -     (S/N)=N                        opz. 24¬
238100000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
238200930129                   -        (S/N)=N                        funz.01¬
238300930129                   -        (S/N)=N                        funz.02¬
238400930129F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
238500930129                   -        (S/N)=N                        funz.04¬
238600971013F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
238700930312F06=Inserimento    -        (S/N)=N                        funz.06¬
238800000000                   -        (S/N)=N                        funz.07¬
238900000000                   -        (S/N)=N                        funz.08¬
239000170116F09=Altri Dati     -        (S/N)=N  <== S-FISSO           funz.09¬
239100100922                   -        (S/N)=N                        funz.10¬
239200100922                   -        (S/N)=N                        funz.11¬
239300930129F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
239400000000                   -        (S/N)=N                        funz.13¬
239500930129                   -        (S/N)=N                        funz.14¬
239600930129                   -        (S/N)=N                        funz.15¬
239700930126                   -        (S/N)=N                        funz.16¬
239800000000F17=Inizio dati    -        (S/N)=S  <== S-FISSO           funz.17¬
239900000000F18=Fine dati      -        (S/N)=S  <== S-FISSO           funz.18¬
240000930128                   -        (S/N)=N                        funz.19¬
240100930129                   -        (S/N)=N                        funz.20¬
240200971013F21=Stampa         -        (S/N)=S                        funz.21¬
240300940105F22=Gestione stampe-        (S/N)=N                        funz.22¬
240400000000F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
240500000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
240600100922 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.RU¬
