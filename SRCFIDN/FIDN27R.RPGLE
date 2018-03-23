000100980506      *------------------------------------------------------------------------*
000200981126      * RICEZIONE  DOCUMENTI CA  DA INVIARE ALLA CONSULDANNI                   *
000300980506      *------------------------------------------------------------------------*
000400990520      *
000500990520      * Oltre alla gestione della ricezione dei documenti richiama i pgm di
000600990520      * sollecito sia ai clienti che alla filiale
000700020125      *
000800020211      * Se c.a. con flag di Franchigia richiamo il pgm di stampa progetto
000900020211      * di liquidazione se ricevuti tutti i documenti
001000990520      *
001100050713      * Gestione dell'affidamento delle pratiche legate ad eventi fortuiti
001200131011      * alla gestione dell'AIG
001300050713      *
001400980803      ****************************************************************
001500981118      *  FASI C.A. FISSATE A PROGRAMMA
001600980803      ****************************************************************
001700981229      *    600 = 1° Ricezione       documenti       (WFARI)
001800981119      *    605 = 1° Sollecito invio documenti       (WFASO)
001900981118      *    660 = Stampa copia Avviso Danni          (WFAAD)
002000020125      *    700 = Stampa progetto di liquidazione    (WFLIQ)
002100990610      ****************************************************************
002200990610      *  allegato tipo testo fisso a programma
002300990610      ****************************************************************
002400990610      *    A12 = 1° Sollecito       documenti       (WTESTO)
002500980803      ***************************************************************
002600980506     H DECEDIT('0,') DATEDIT(*DMY.)
002700980506      *--------------------------------------------
002800981118     Ffidn27D   CF   E             WORKSTN sfile(fi27s01:nrr1)
002900981119     F                                     sfile(fi27s02:nrr2)
003000981120     Ffndct01l  uf   E           k DISK
003100981120     Ffndcc01l  uf a E           k DISK
003200981120     Ffndcl01l  uf a E           k DISK
003300981120     Ffndcf01l  if A E           k DISK
003400990412     Ffndpt01l  if   E           k DISK
003500020125     ffndcs01l  if a e           k disk
003600050706     Ffndet01l  if   E           k DISK
003700990809     fTITAS30C  IF   E           K DISK
003800981022     Fazorg01l  if   E           k DISK
003900981119     Ftabel00f  if   E           k DISK
004000981119     Ftntbe01l  if   E           k DISK
004100981119     Ftntbe02l  if   E           k DISK    rename(tntbe000:tntbe2)
004200981022      ****************************************************************
004300981022      *  RIEPILOGO INDICATORI
004400981022      ***************************************************************
004500981217      * 05   - Cliente Benieficiaro è la RIVALSA
004600990310      * 08   - Cliente Estero documenti in lingua
004700990412      * 09   - Cliente con mandato assicurativo
004800981120      * 10   - emissione e richiesta data/numero documento/importo
004900981120      * 11   - visualizzo data sollecito
005000020225      * 13   - Emissione campo VTCTXT - DSPATR(HI RI)
005100981118      * 20/22- UTILI PER LA GESTIONE DEL 1° SUBFILE
005200981118      * 60/62- UTILI PER LA GESTIONE DEL 2° SUBFILE
005300981022      * 28   - ERRORE GENERICO
005400981022      * 30/31- COMODO
005500990310      * 40/43- Errori video
005600981229      * 88   - documenti già presenti in FNDCC
005700990520      * 98   - Ricezione parziale dei documenti in filiale
005800981120      * 99   - Ricezione parziale dei documenti
005900981022      *--------------------------------------------
006000981119      *   S C H I E R E
006100010910     D MSG             S             78    DIM(7) CTDATA PERRCD(1)              MSG VIDEO
006200981119     D cd              S              2    DIM(250)                             codice documenti
006300020327     D*S2F             S              1    DIM(50)                              tipo comunicaz. giac
006400981022     D                SDS
006500981022     D  VTCPGM                 1     10
006600981022      *
006700081223     D Fidnbeds      E DS
006800981022     D Fidn00ds      E DS
006900981022     d  dsnumca                6     19
007000981022     D DSDN10        E DS                  EXTNAME(FIDN10DS)
007100981022      * Ds passaggio parametri a pgm ricerca tabelle
007200981022     D DSbs02        E DS                  EXTNAME(tibs02ds)
007300981124     D FIDN05DS      E DS
007400990412     D FIDN30DS      E DS
007500110727     D FIDN40DS      E DS                  INZ
007600981124     D DS_fndfa      E DS                  EXTNAME(fndfa00f)
007700981119     D DCDD          E DS
007800981119     D DTAD          E DS
007900981230     D DSTB          E DS
008000981230     D DDCT01        E DS
008100020125     d ddcf01        e ds
008200981126     D DSCV          E DS
008300020327     D*DS2F          E DS
008400010910     D DGEDDN        E DS                  inz
008500020125     d dstd          e ds
008600981022
008700981022      *--------------------------------------------
008800981022     D KPJBA         E DS
008900981022     D CNCR80        E DS
009000981022     D UT§DSE0F      E DS
009100011017     D XDECDS        E DS                  inz
009200011017     D   OCDESI      E                     inz(*OFF)
009300981022      *--------------------------------------------
009400990308     D* schiera tipi anomalie
009500990308     D WTA             S              1    DIM(3)
009600990308      *
009700990310     D ksc             S                   LIKE(dctksc)
009800981022     D dateu           s              8  0
009900010910     D w002a           s              2
010000981022     D W0140           S             14  0
010100981120     D WORA            S              6  0
010200981022     D WDTGIO          S              8  0
010300981022     D vtcksm          S              7  0
010400981022     D vtcksd          S              7  0
010500981126     D §des            S             30
010600981126     D w0033           s              3  3
010700081223     D w0040           s              4  0
010800981119     D nrr1            S              4  0
010900981119     D maxim           S              4  0
011000981119     D e               S              3  0
011100981119     D nrr2            S              4  0
011200981229     D dcc             s              1
011300981230     D Wdctlnp         s              3  0
011400990310     D estero          s              1
011500990310     D W001a           s              1
011600020328     D wclsflo7        S              1
011700990309     d w§dctport       S                   like(§dctport)
011800031211     D w§STDlpf        s                   like(§STDlpf1) inz(*loval)
011900981119     D Wdrz            s                   like(dccdrz)
012000981229     D Wcdd            s                   like(dcccdd)
012100981119     D §cod            S                   LIKE(TBLcod)
012200981119     D §key            S                   LIKE(TBLkey)
012300981119     D Klin            S                   LIKE(tbelin)
012400981119     D Ksif            S                   LIKE(tbesif)
012500981119     D Kcot            S                   LIKE(tbecod)
012600981119     D Kke1            S                   LIKE(tbeke1)
012700981119     D Kke2            S                   LIKE(tbeke2)
012800981022     D Kaac            S                   LIKE(dctaac)
012900981022     D Kfil            S                   LIKE(V1Cfil)
013000981022     D Knca            S                   LIKE(V1Cnca)
013100081223     d Ktrc            s                   like(dcstrc)
013200981120     d kdat            s                   like(dcfdfc)                         * data fase ca
013300981120     d                                     inz(99999999)
013400981120     d kora            s                   like(dcfhfc)                         * ora fase ca
013500981120     d                                     inz(9999)
013600981022     d Wdatac          s              8  0
013700981022     d Wdaspe          s              8  0
013800981229     d Wfari           s                   like(i00fca)
013900981229     d                                     inz(600)
014000981119     d Wfaso           s                   like(i00fca)
014100981120     d                                     inz(605)
014200981119     d Wfaad           s                   like(i00fca)
014300981119     d                                     inz(660)
014400020125     d wfliq           s                   like(i00fca)
014500020125     d                                     inz(700)
014600990610     d Wtesto          s              3    inz('A12')
014700981120     d ultsol          s                   like(dcffca)                         * ultimo sollecito
014800990520     d savtpb          s                   like(i00tpb)
014900981022      *
015000010215     d Wdatada         s              8  0 inz(19400101)
015100010215     d Wdataa          s              8  0 inz(20391231)
015200020125
015300020125     d Ktpd            s                   like(dcstpd)
015400020125     d Knkt            s                   like(dcsnkt)
015500020125     d Kstd            s                   like(dcsstd)
015600020125     d Kdim            s                   like(dcsdim)
015700020125     d Khim            s                   like(dcshim)
015800020125     d Knks            s                   like(dcsnks)
015900020125     d prgnot          s                   like(dcspno)   inz(999)              progressivo note
016000020125     d wokfra          s              1    inz(*off)
016100110727     d weexpartner     s              1    inz(*off)
016200120508     d wPFIEEX         s               n   inz(*off)                            PF IMPORT EEX
016300010215      *
016400981022     D WLBDAT          DS                  INZ
016500981022     D  G02DAT                 1      8  0
016600981022     D  G02INV                 9     16  0
016700981022     D  G02ERR                17     17
016800981022     D  G02TGI                18     22  0
016900981217      *  titolo videata (lunghezza massima 34)
017000981217     D TIT_1           C                   CONST('***    RICEZIONE  DOCUMENTI  -
017100981217     D                                       ***')
017200020222      *  sotto-titolo (avvertimento)
017300020225     D TIT_2           C                   CONST(' PRATICA IN   F R A N C H I G-
017400020225     D                                      I A ')
017500981217      *
017600981022      *
017700981022      * ---------------------------------------------------------
017800981022      *
017900981022      * EMISSIONE PRIMA VIDEATA
018000981022    1C                   do        *hival
018100010913     C                   if        C1RCD = 0
018200010913     C                   Z-ADD     1             C1RCD
018300010913     C                   endif
018400981119     c                   write     fi27z01
018500981119     C                   EXFMT     FI27c01
018600981119     c*
018700981119     c                   setoff                                       404142
018800981126     c                   setoff                                       2843
018900981119     c                   clear                   v1cmsg
019000981022      *
019100981022      * F3=FINE
019200981119    2C                   if        *inkc = *on
019300981022     c                   eval      o00f03 = 'S'
019400981022     c                   leave
019500981119    2c                   endif
019600981022      * F12=PRECEDENTE
019700981119    2C                   if        *inkl = *on
019800981022     c                   eval      o00f12 = 'S'
019900981022     c                   leave
020000981119    2c                   endif
020100990412      * F09=Visualizza il Mandato Assicurativo
020200990412    2C                   if        *inkI = *on
020300990412     C*
020400990412      * imposto DS per richiamo pgm di visualizzazione mandato
020500990412     c                   clear                   fidn30ds
020600990412     c                   z-add     dptksc        I30ksc
020700990412     c                   z-add     dptctr        I30ctr
020800990412     c                   z-add     dptprg        I30prg
020900990412     c                   movel     dptftc        I30ftc
021000990412     c                   movel     dptfvm        I30fvm
021100990412     c                   z-add     dptvlm        I30vlm
021200990412     c                   movel     dptvvm        I30vvm
021300990412      *
021400990412     c                   call      'FIDN30R'
021500990412     c                   parm                    KPJBA
021600990412     c                   parm                    FIDN30DS
021700990412      *
021800981119     c                   iter
021900981119    2c                   endif
022000981119      * controllo del subfile
022100981119     c   20              exsr      SUB_CTRS01
022200981119     c   28              iter
022300990520      * F11=Ricerca documenti
022400990520    2C                   if        *inkK = *on
022500990520     c                   exsr      SUB_DOCUM
022600990520     c                   iter
022700990520    2c                   endif
022800990520      * F4=Note Stampa
022900990520    2C                   if        *inkd = *on
023000990520     c                   exsr      SUB_NOTEST
023100990520     c                   iter
023200990520    2c                   endif
023300990520      * F18=Note
023400990520    2C                   if        *inks = *on
023500990520     c                   exsr      SUB_NOTE
023600990520     c                   iter
023700990520    2c                   endif
023800990520      * F14=Interrogazione C.A.
023900990520     C     *INKN         IFEQ      *ON
024000990520     c                   exsr      SUB_F14
024100990520     C                   iter
024200990520     c                   ENDIF
024300981119     c* F06= CONFERMA
024400981119    2C                   if        *inkf = *on
024500981120     c                   exsr      SUB_CONFE
024600981022     c                   leave
024700981119    2c                   endif
024800981022      *
024900981022    1c                   enddo                                                  *fine ciclo KC *on
025000981022      *
025100981022     c                   clear                   dsdn10
025200981022     c                   movel     'C'           i10tla
025300981022     c                   movel     dsdn10        kpjbu
025400981022     c                   call      'FIDN10R'
025500981022     c                   parm                    kpjba
025600981022      *
025700981029     c                   clear                   dsbs02
025800981029     c                   movel     'C'           t02tla
025900981029     c                   call      'TIBS02R'
026000981029     c                   parm                    kpjba
026100981029     c                   parm                    dsbs02
026200981119     c*
026300981119     c                   movel     fidn00ds      kpjbu
026400981119      *
026500981022     C                   EVAL      *INLR = *ON
026600981119     C*-------------------------------------------------------------------------*
026700981119     c* SUB_CTRS01 Routine di Controllo dati del subfile
026800981119     C*-------------------------------------------------------------------------*
026900981119     c     SUB_CTRS01    BEGSR
027000981119     C*
027100981119     c* leggo il subfile se nrr1 > 0
027200981119     c                   z-add     1             nrr1
027300981119    1c                   do        *hival
027400981119     c*
027500981119     c     nrr1          chain     fi27s01                            30
027600981119     c*
027700981119    2c                   if        *in30
027800981119     c                   leave
027900981119    2c                   endif
028000981119     c*
028100981119     c* controllo se la data ricezione diversa da zero
028200981119    2c                   if        v1sdrz > *zeros
028300981119     C                   z-add     v1sdrz        g02dat
028400981119     C                   eval      g02err = *blanks
028500981119     C                   call      'XSRDA8'
028600981119     C                   parm                    wlbdat
028700010215    3C                   if        g02err = '1'  or  G02inv < Wdatada
028800010215    3C                                           or  G02inv > Wdataa
028900981119     C                   seton                                        2840
029000981119     C                   movel     msg(1)        v1cmsg
029100981119   x3C                   else
029200981119     C                   z-add     g02dat        V1sdrz
029300981119    3C                   endif
029400981119    2C                   endif
029500981119     c*
029600981119    2c                   if        not *in28
029700981119     c* controllo altri campi se il tipo documento li richiede
029800981119    3c                   if        v1hi10 = 'S'
029900981119     c* controllo data documento se è diversa da zero
030000981119    4c                   if        v1sddc > *zeros
030100981119     C                   z-add     v1sddc        g02dat
030200981119     C                   eval      g02err = *blanks
030300981119     C                   call      'XSRDA8'
030400981119     C                   parm                    wlbdat
030500010215    3C                   if        g02err = '1'  or  G02inv < Wdatada
030600010215    3C                                           or  G02inv > Wdataa
030700981119     C                   seton                                        2841
030800981119     C                   movel     msg(2)        v1cmsg
030900981119   x5C                   else
031000981119     C                   z-add     g02dat        V1sddc
031100981119    5C                   endif
031200981119    4C                   endif
031300981119     c* controllo la validità della valuta
031400011017    4c                   if        not *in28
031500981126     C                   exsr      ctrvlt
031600011017    4c                   endif
031700981119     c*
031800981119    3c                   endif
031900981119     c*
032000981119    2c                   endif
032100981119     c*
032200981119      * se v1hi10  è = 'S' accendo il 10
032300981119     c                   eval      *in10 = (v1hi10 = 'S')
032400981119     c* aggiorno il subfile
032500981119     c                   update    fi27s01
032600981119     c*
032700981119     c* se c'è errore emetto il subfile
032800981119    2c                   if        *in28
032900010913     C                   Z-ADD     NRR1          C1RCD
033000981119     c                   leave
033100981119     c                   else
033200010913     C                   clear                   C1RCD
033300981119     c                   add       1             nrr1
033400981119    2c                   endif
033500981119     c*
033600981119    1c                   enddo
033700981119     c*
033800981119     c                   endsr
033900981126      *---------------------------------------------------------------------------------------------
034000981126      * routine controllo validità codice valuta
034100981126      *---------------------------------------------------------------------------------------------
034200981126     c     CTRVLT        begsr
034300981126      * Controllo la divisa
034400981126     C                   MOVEL     'CV'          §COD
034500981126      * RICERCA
034600981126     C     '?'           SCAN      v1svpr                                 30
034700981126      *
034800981126     C                   if        *IN30 = *ON
034900981126     c                   clear                   §key
035000981126     c                   clear                   §des
035100981126      *
035200981126     C                   CALL      'X§TABER'
035300981126     C                   PARM                    CODUT
035400981126     C                   PARM                    §COD
035500981126     C                   PARM                    §KEY
035600981126     C                   PARM                    §DES
035700981126      *
035800981126     C                   MOVEL     §KEY          v1svpr
035900981126     C                   MOVEL     §des          DSCV
036000981126     C                   GOTO      ENDVLT
036100981126     C                   endif
036200981126      *
036300981126      * CONTROLLO DIVISA
036400981126     C                   MOVEL(P)  v1svpr        §KEY
036500981126     C     KTAB          CHAIN     TABEL00f                           30
036600981126      * ERRORE
036700981126     C                   if        *IN30 = *ON    OR
036800981126     C                              ( *IN30 = *OFF  and  TBLFLG <> *blanks)
036900981126     C                   MOVEL     MSG(3)        V1CMSG
037000981126     C                   eval      *IN28 = *ON
037100981126     C                   eval      *IN42 = *ON
037200981126     C                   GOTO      ENDVLT
037300981126     C                   endif
037400981126      *
037500981126     C                   MOVEL     TBLUNI        DSCV
037600010905     c* Inutile gestire li cambio:
037700010905     c*  la valuta non potrà MAI essere diversa dalla divisa corrente
037800010905     c                   z-add     *zeros        v1hcmb
037900981126      *
038000981126      * VEDO SE AMMESSI IMPORTI DECIMALI NELL'IMPORTO
038100981126     C                   IF        §CVFDC <> 'S'
038200981126     C                   MOVE      v1sipr        W0033
038300981126     C                   if        W0033 <> 0
038400981126     C                   MOVEL     MSG(4)        V1CMSG
038500981126     C                   eval      *IN28 = *ON
038600981126     C                   eval      *IN43 = *ON
038700981126     C                   GOTO      ENDVLT
038800981126     C                   endif
038900010905     C                   else
039000010910      * controllo numero massimo di decimali impostabili per la valuta
039100010910     C     V1Sipr        casNE     0             srCTRDEC
039200010905     C                   ENDCS
039300010910     C                   IF        *in28
039400011005     C                   eval      *IN43 = *ON
039500010910     C                   MOVEL     MSG(7)        V1CMSG
039600010910     c                   move      §cvDEC        w002a
039700010914     c                   eval      %subst(V1CMSG:29:2) = w002a
039800010910     C                   GOTO      ENDVLT
039900010910     C                   ENDIF
040000981126     C                   ENDIF
040100981126      *
040200981126     C     endvlt        ENDSR
040300981119      *-------------------------------------------------------------------------*
040400981119      * SUB_DOCUM  Routine Caricamento SUBFILE scelta e creazione documenti
040500981119      *-------------------------------------------------------------------------*
040600981119      *
040700981119     C     SUB_DOCUM     BEGSR
040800981119      *
040900981119      * routine di caricamento del subfile
041000981119     c                   exsr      cardoc
041100981119      * routine di gestione subfile
041200981119     c   60              exsr      gesdoc
041300981119      *
041400981119     c                   endsr
041500981119      *-------------------------------------------------------------------------*
041600981119      * CARDOC     Routine Caricamento SUBFILE scelta e creazione documenti
041700981119      *-------------------------------------------------------------------------*
041800981119      *
041900981119     C     CARDOC        BEGSR
042000981119      *
042100981119      * pulisco il subfile
042200981119     c                   setoff                                       6061
042300981119     c                   write     fi27c02
042400981119     c                   seton                                        6061
042500981119     c                   z-add     *zeros        nrr2
042600981119     C*
042700981119     c* definisco i campi del control
042800981119     c                   move      *blanks       kcot
042900981119     c                   move      *blanks       klin
043000981119     c                   move      knsif         ksif
043100981119     c                   movel     *zeros        kke1
043200981119     c                   move      'CDD'         kke1
043300981119     c                   move      *blanks       kke2
043400981119     C     Ktbe1         chain     tntbe01l                           31
043500981119     c* se non l'ho trovato cerco senza il sistema informativo specifico
043600981119     c                   if        *in31 = *on
043700981119     c                   move      *blanks       ksif
043800981119     C     Ktbe1         chain     tntbe01l                           31
043900981119     c                   endif
044000981119     c*
044100981119     c* imposto la chiave a parte il sistema informativo che rimane uguale all'ultima
044200981119     c* chain fatta
044300981119     c                   move      'CDD'         kcot
044400981119     c                   move      *blank        klin
044500981119     C*
044600981119     C*-------- DETTAGLIO TABELLA -------------------------*
044700981119     C*                        SISTEMA INFORMATIVO  VALORIZZATO come da testata tabella
044800981119     C     Ktbe2         SETLL     tntbe02l                                     *
044900981119     C*
045000981229     C                   DO        *hival
045100981229     C     Ktbe2         READE     tntbe02l                               30
045200981229     c* fine file vado a fine
045300981229     c                   if        *in30
045400981229     c                   leave
045500981229     c                   endif
045600981229     c*
045700981229     c                   if        tbeatb <> ' '
045800981229     c                   iter
045900981229     c                   endif
046000981229     c*
046100981119     c* controllo se esiste già come documento
046200981119     c                   movel     tbeke1        wcdd
046300981119     c     wcdd          lookup    cd                                     90
046400981119     c* se non esiste
046500981119     c                   if        not *in90
046600981229     C* controllo se è un documento che si può richiedere
046700981119     c                   movel     tbeuni        DCDD
046800981229     c* se sono rivalsa (05 acceso) deve essere richiesto o facoltativo
046900981229     c                   if        *in05 and §cddrdsr = 'N'
047000981229     c                   iter
047100981229     c                   endif
047200981229     c* se non c'è rivalsa (05 spento) deve essere richiesto o facoltativo
047300981229     c                   if        not *in05 and §cddrdnr = 'N'
047400981229     c                   iter
047500981229     c                   endif
047600990308     c*
047700990308     c* verifico se il documento non è valido per particolari anomalie
047800990308     c                   if        §cddvtad  <> *blanks
047900990308     c                   movea     §cddvtad      wta
048000990308     c     §tadragr      lookup    wta                                    33
048100990308     c* per anomalia non trovata controllo se esiste un importo limite di validità
048200990308     c                   if        not *in33 and §cddlimr > 0
048300990308     c                   eval      *in33 = (dctipv < §cddlimr)
048400990308     c                   endif
048500990308     c*
048600990308     c                   if        *in33
048700990308     c                   iter
048800990308     c                   endif
048900990308     c*
049000990308     c                   endif
049100981229     c*
049200981119     c                   movel     §CDDdesc      v2sdes
049300981119     c                   movel     tbeke1        v2scdd
049400981119     c* controllo quali sono i dati previsti per questo documento
049500981119     c                   movel     §cddpnum      v2hpnu
049600981119     c                   movel     §cddpdat      v2hpda
049700981119     c                   movel     §cddpimp      v2hpim
049800981119     c*
049900981119     c                   clear                   v2sopz
050000981119     c* scrivo
050100981119     C                   ADD       1             nrr2                           *
050200981119     C                   WRITE     fi27s02                                      *
050300981119     c                   endif
050400981119     c**
050500981119     C                   ENDdo                                                  *
050600981119     c*
050700981119     c                   eval      *in60 = (nrr2 > 0)
050800981119     c                   endsr
050900981119     c*---------------------------------------------------------------------------
051000981119     c* GESDOC    Routine gestione subfile
051100981119     c*---------------------------------------------------------------------------
051200981119     c     gesdoc        begsr
051300981119      *
051400981119    1C                   do        *hival
051500981119     c                   write     fi27z02
051600981119     C                   EXFMT     FI27c02
051700981119     c*
051800981119      * F12=PRECEDENTE
051900981119    2C                   if        *inkl = *on
052000981119     c                   leave
052100981119    2c                   endif
052200981119      * F10=Inserimento di un nuovo documento
052300981119    2C                   if        *inkJ = *on
052400981119     c                   call      'TNTB02R'
052500981119     c                   parm                    kpjba
052600981119     c* ricarico il subfile
052700981119     c                   exsr      cardoc
052800981119     c* riemetto il subfile
052900981119     c                   iter
053000981119    2c                   endif
053100981119      * G E S T I O N E    S U B F I L E
053200981119    2c                   dow       *in31 = *off
053300981119     c                   readc     fi27s02                                31
053400981119     c* selezionato documento
053500981119    3c                   if        *in31 = *off and v2sopz = '1'
053600981119     c* scrivo nel subfile dei documenti da richiedere
053700990609     c                   clear                   fi27s01
053800981119     c                   movel     v2sdes        v1sdes
053900981119     c                   movel     v2scdd        v1scdd
054000981119     c* controllo quali sono i dati previsti per questo documento
054100981119    4c                   if        v2hpnu  = 'S'
054200981119     c                   movel     'S'           v1hi10
054300010913     c                   movel     §gedDBA       V1Svpr
054400981119     c                   seton                                        10
054500981119     c                   else
054600981119     c                   clear                   v1hi10
054700010913     c                   clear                   V1Svpr
054800981119     c                   setoff                                       10
054900981119    4c                   endif
055000990520     c*
055100990520     c                   eval      v1hsol = v2hsol
055200981119     c* scrivo
055300981119     C     maxim         ADD       1             nrr1                           *
055400981119     C                   ADD       1             maxim                          *
055500981119     C                   WRITE     fi27s01                                      *
055600981119     c* carico la schiera con i documenti già presenti nel subfile
055700981119     c                   add       1             e
055800981119     c                   movea     v1scdd        cd(e)
055900981119     c                   seton                                        31
056000990521      *
056100990521     c                   clear                   fi27s01
056200981119    3c                   endif
056300981119    2c                   enddo                                                  *fine ciclo KC *on
056400981119      *
056500981119     c                   if        *in31 and v2sopz = '1'
056600981119     c                   leave
056700981119     c                   endif
056800981119     c*
056900981119    1c                   enddo                                                  *fine ciclo KC *on
057000981119     c
057100981119     c                   endsr
057200990225      **********************************************************************
057300990225      * NOTEST = NOTE TESTO
057400990225      **********************************************************************
057500990225     C     SUB_NOTEST    BEGSR
057600990225      *
057700990225     C                   clear                   DSDN10
057800990225     c                   movel     'D'           i10trc
057900990225     c                   movel     'T'           i10std
058000990225     c                   movel     'C'           i10tpd
058100990225     c                   z-add     v1cdca        i10dta
058200990225     c                   movel     'V'           i10flm
058300990225     c                   movel(P)  dsnumca       I10nkt
058400990610     c                   movel(P)  wtesto        I10nks
058500990225      *
058600990225     c                   movel(P)  dsdn10        kpjbu
058700990520     C                   CALL      'FIDN10R'
058800990225     C                   PARM                    kpjba
058900990225      *
059000990225     C                   ENDSR
059100981119      *-------------------------------------------------------------------------*
059200981022      *   ROUTINE SCRITTURA NOTE
059300981119      *-------------------------------------------------------------------------*
059400981022     C     SUB_NOTE      BEGSR
059500981022     c* valorizzo la DS di gestione note/descrizioni e chiamo il PGM FIDN10R
059600981022     C                   CLEAR                   DSDN10
059700981022     c                   move      v1cdca        i10dta
059800981022     c                   movel     'N'           i10trc
059900981022     c                   movel     'C'           i10tpd
060000981022     c                   movel     i00fca        i10nks
060100981022     c                   movel     dsnumca       i10nkt
060200981022     c                   movel     'M'           i10flm
060300981022     c                   movel(P)  dsdn10        kpjbu
060400990520     C                   CALL      'FIDN10R'
060500981022     C                   PARM                    kpjba
060600981022     c
060700981022     c                   endsr
060800981119     C*-------------------------------------------------------------------------*
060900981119     c* SUBCAR    Routine di Caricamento videata iniziale
061000981119     C*-------------------------------------------------------------------------*
061100981119     c     SUBCAR        BEGSR
061200981119     C*
061300990521     c                   setoff                                       2021
061400990521     c                   write     fi27c01
061500990521     c                   seton                                        2021
061600990521     c*
061700981119     c                   move      i00fil        v1cfil
061800981119      * valorizzo il capo anno e numero C.A.
061900981119     c                   z-add     i00nca        v1cnca
062000981119      * aggancio la C.A. per recuperare i dati anagrafici della persona
062100981119     c                   z-add     i00aac        Kaac
062200981119     c                   z-add     i00fil        Kfil
062300981119     c                   z-add     i00nca        Knca
062400981119     C     kdct          CHAIN     fndct000                           31
062500981230     c*
062600981230     c                   movel     dctflo        ddct01
062700981230     c                   movel     §dctlnpc      Wdctlnp
062800020222      *
062900020225     c                   eval      *in13 = *off
063000020222     c                   clear                   VTCtxt
063100020222     c                   if        §DCTpfra = 'F'
063200020222     c                   movel     TIT_2         VTCtxt
063300020225     c                   eval      *in13 = *on
063400020222     c                   endif
063500981230     c*
063600981119      * calcolo data apertura C.A.
063700981119     C                   eval      wdatac = (DCTaac * 10000) + DCTmgc
063800981119     c*
063900981119     C                   CLEAR                   WLBDAT
064000981119     C                   Z-ADD     wdatac        G02inv
064100981119     C                   movel     '3'           G02err
064200981119     C                   CALL      'XSRDA8'
064300981119     C                   PARM                    WLBDAT
064400981119     c                   z-add     g02dat        v1cdca
064500990310      * decodifico il tipo anomalia
064600990310     c                   clear                   dsbs02
064700990310     c                   clear                   dtad
064800990310     C                   MOVEL     'C'           t02mod
064900990310     C                   MOVEL     knsif         t02sif
065000990310     C                   MOVEL     'TAD'         t02cod
065100990310     C                   MOVEL(P)  dcttad        t02ke1
065200990310      *
065300990310     C                   CALL      'TIBS02R'
065400990310     C                   PARM                    KPJBA
065500990310     C                   PARM                    dsbs02
065600990310      *
065700990310    2C                   if        t02err = *BLANKS
065800990310     C                   MOVEL     T02UNI        DTAD
065900990310     c                   movel     §taddesc      v1dtad
066000990310     C                   MOVEL(P)  dcttad        v1ctad
066100990310    2C                   endif
066200990310      * numero spedizione
066300990310     c                   movel     dctlnp        v1clnp
066400990310     c                   movel     dctnrs        v1cnrs
066500990310     c                   movel     dctnsp        v1cnsp
066600990310      * decodifico la Linea di arrivo spedizione
066700990310     c                   move      i00lna        v1clna
066800990310     c     i00lna        chain     azorg                              31
066900990310     c  n31              movel     orgdes        v1dlna
067000990310     c
067100990412      *
067200990412      * Controllo se esiste il file relativo il Mandato Assicurativo ed abilito F9 per
067300990412      * visualizzarlo
067400990412     c     kdct          chain     fndpt01l                           09
067500990412     c  n09              eval      *in09 = (dptatb <> ' ' )
067600081223      * recupero la data spedizione
067700990809     c     kbol01        chain     TITAS30c                           30
067800040322      * recupero data spedizione
067900040322     c                   eval      Wdaspe = (I00aas * 10000) + TASmgs
068000040322     c                   clear                   WLBdat
068100040322     c                   z-add     Wdaspe        G02inv
068200040322     c                   movel     '3'           G02err
068300040322     c                   call      'XSRDA8'
068400040322     c                   parm                    WLBdat
068500040322     c                   z-add     G02dat        V1Cdsp
068600081223      * cerco il beneficiario
068700081223     c                   clear                   fidnbeds
068800081223      *
068900081223     c                   movel     'B'           IBEmod
069000081223     c                   z-add     dctaac        IBEaac
069100081223     c                   z-add     dctfil        IBEfil
069200081223     c                   z-add     dctnca        IBEnca
069300081223     c                   z-add     dctaas        IBEaas
069400081223     c                   z-add     dctlnp        IBElnp
069500081223     c                   z-add     dctnrs        IBEnrs
069600081223     c                   z-add     dctnsp        IBEnsp
069700081223     c                   z-add     dctksc        IBEksc
069800081223     c                   movel     dctptr        IBEptr
069900081223     c                   movel     dctflo        IBEflo
070000081223     c                   movel     'S'           IBEtpb
070100081223     c                   If        %subst(knsif:7:1) = 'P'
070200081223     c                   eval      IBEsif = 'P'
070300081223     c                   EndIf
070400081223      *
070500081223     c                   call      'FIDNBER'
070600081223     c                   parm                    fidnbeds
070700081223      *
070800081223     c                   If        oberag <> *blanks
070900081223      *
071000081223     c                   movel     oberag        v1crsc
071100081223     c                   movel     obecap        v1ccac
071200081223     c                   movel     obeloc        v1cloc
071300081223     c                   movel     obeprv        v1cprc
071400081223     c                   movel     obetel        v1ctel
071500081223     c                   movel     obefax        v1cfax
071600081223     c                   movel     obenaz        v1cnac
071700081223     c                   movel     obeksc        v1cksc
071800081229     c                   if        oberiv = 'S'
071900081229     c                   seton                                        05
072000081229     c                   else
072100081229     c                   setoff                                       05
072200081229     c                   endif
072300081223     c                   endif
072400990310     c*
072500990310     c* controllo se cliente estero e devo stampare in inglese
072600990310     c*
072700990310      * Imposto se devo stampare in Inglese
072800990310     c                   clear                   ESTERO
072900081223     c                   clear                   wclsflo7
073000990520     c                   eval      *in08 = *OFF
073100081223     c                   IF        obeing <> *blanks
073200081223     c                   eval      wclsflo7 = obeing
073300020327     c                   eval      *in08 = *on
073400020327     C                   movel     msg(6)        v1cmsg
073500020327     c                   seton                                        28
073600990520     c                   ENDIF
073700020328     c                   eval      estero = wclsflo7
073800990202     c*
073900981120     C* controllo se l'ultima fase eseguita è un sollecito oppure no
074000981120     c     kdcf          setgt     fndcf01l
074100981120     c     kdct          readpe    fndcf01l                               32
074200981124     c* se il record esiste
074300981126     c                   dow       not *in32
074400981126     c                   if        dcffca >= wfaso and
074500981126     c                             dctfca = dcffca
074600981120     c* Data sollecito
074700981120     C                   CLEAR                   WLBDAT
074800981120     C                   Z-ADD     dcfdfc        G02inv
074900981120     C                   movel     '3'           G02err
075000981120     C                   CALL      'XSRDA8'
075100981120     C                   PARM                    WLBDAT
075200981120     c                   z-add     g02dat        v1cdus
075300981120     c* accendo l'11 che visualizza la data
075400981120     c                   seton                                        11
075500981120     c* salvo l'ultima fase sollecito perchè in fase di conferma guiderà la stampa dei sollecito
075600981126     c* e l'aggiornamento di fase nella testata CA
075700981120     c                   movel     dcffca        ultsol
075800981126     c                   leave
075900981120     c*
076000981120     c                   endif
076100981126     c*
076200981126     c     kdct          readpe    fndcf01l                               32
076300981126     c                   enddo
076400020125
076500020211      * controllo se la c.a.ha le caratteristiche x il flag Franchigia
076600020125      * wokfra *on
076700020125      * - il flag non è impostato
076800031211      * - L'anno della spedizione è oltre il 2001 (w0040=2002)
076900020125      * - L'importo da assicurare in bolla è 0
077000020125      * - Il risarcimento è calcolato sulla base della legge 450 o su CMR
077100031211     c                   eval      w0040 = §STDdlpf1 / 10000
077200051122     c                   if        §dctpfra = *blanks  and
077300051122     c                             dctaas >= w0040
077400051122     c                   eval      wokfra = *on
077500051122     c                   endif
077600110727
077700110727      *  Verifico se spedizione EuroExpress legata a partner ai quali chiediamo rivalsa
077800110727     c                   eval      Weexpartner = *off
077900110727     c                   If        §dcteuro = 'X' and §dcttisp = 'E'
078000110727     c                   clear                   fidn40ds
078100110727     c                   eval      I40dsp = Wdaspe
078200110727     c                   eval      I40fil = dctlna
078300110727     c                   call      'FIDN40R'
078400110727     c                   parm                    fidn40ds
078500110727     c                   If        o40cod <> *zeros
078600110727     c                   eval      Weexpartner = *on
078700110727     c                   endif
078800110727     c                   endif
078900110727
079000020125
079100981119     c* caricamento  del subfile con i documenti già richiesti per questa CA e se non ci sono
079200981119     c* perchè è la prima volta carico nel subfile i soli documenti obbligatori
079300981119      *
079400981229     c*
079500981229     c                   setoff                                       88
079600981229     c*
079700981229     c     kdct          setll     fndcc01l                               88
079800981229      *
079900981229      * recupero i documenti obbligatori dalla tabella documenti danni se 1° volta
080000981229      * che richiedo questi documenti
080100981229      *
080200981229     c                   if        not *in88
080300981229     c                   exsr      subtbe
080400981229     c                   endif
080500981229     c* se ho dei documenti ma sono nella fase di 1° ricezione documenti (600) vuol dire che
080600981229     c* è stato fatto un cambio di beneficiario (RIVALSA aggiunta o tolta)
080700981229     c* quindi oltre ai documenti già esistenti mostro anche quelli obbligatori per
080800981229     c* la situazione.
080900981229      *
081000981229     c                   eval      dcc = ' '
081100981229     c                   if        *in88 and i00fca = WFARI
081200981229      * leggo la tabella e verifico per ogni documento se esiste in fndcc
081300981229     c                   eval      dcc = 'Y'
081400981229     c                   exsr      subtbe
081500981229     c                   endif
081600981229     c*
081700981229      * recupero i dati relativi ai documenti già richiesti se non sono in 1° ricezione
081800981229     c                   if        *in88 and i00fca > WFARI
081900981119     c                   do        *hival
082000981120     c     kdct          reade(n)  fndcc01l                               31
082100981229     c                   if        *in31
082200981119     c                   leave
082300981119     c                   endif
082400990715     c* preparo i dati del subfile se record non annullato
082500990715     c                   if        dccatb = ' '
082600981229     c                   exsr      subdcc
082700981229     c*
082800981119     c                   add       1             nrr1
082900981119     c                   write     fi27s01
083000981119     c* carico la schiera con i documenti già presenti nel subfile
083100981119     c                   add       1             e
083200981119     c                   movea     v1scdd        cd(e)
083300981119     c*
083400981119     c                   clear                   fi27s01
083500990715     c*
083600990715     c                   endif
083700981119     c*
083800981119     c                   enddo
083900981229      *
084000981229     c                   endif
084100981119      *
084200981119     c* controllo se scritto almeno un record nel subfile
084300981119     c                   eval      *in20 = (nrr1 > 0)
084400981119     c* memorizzo il numero massimo di record per aggiungere  altri documenti da richiedere
084500981119     c                   z-add     nrr1          maxim
084600120508
084700120508
084800120508      *?C.A. su Bolla in Porto Franco Import EEX
084900120508     c                   IF        §DCTport = 'F' and §DCTtisp = 'I' and
085000120508     c                             §DCTeuro = 'X'
085100120508     c                   eval      wPFIEEX = *on
085200120508     c                   ELSE
085300120508     c                   eval      wPFIEEX = *off
085400120508     c                   ENDIF
085500981119     c*
085600981119     c                   endsr
085700020125      *-------------------------------------------------------------------------*
085800020125      * Sub_chkdpt Routine di controllo mandato assicurativo
085900020125      *-------------------------------------------------------------------------*
086000020125     c     sub_chkdpt    begsr
086100020125
086200020125     c                   eval      wokfra = *on
086300020125
086400020211     c     kdct          chain     fndpt01l
086500020211     c                   if        %found(fndpt01l)  and
086600020211     c                             dptatb <> 'A' and
086700060315     c                             (dptftc='C' or dptftc='8' or dptftc='d')
086800020125     c                   eval      wokfra = *off
086900020125     c                   endif
087000020125
087100020125     c                   endsr
087200981119     C*-------------------------------------------------------------------------*
087300981119     c* SUBTBE    Routine di Ricerca documenti obbligatori
087400981119     C*-------------------------------------------------------------------------*
087500981119     c     SUBTBE        BEGSR
087600981119     C*
087700981119     c* definisco i campi del control
087800981119     c                   move      *blanks       kcot
087900981119     c                   move      *blanks       klin
088000990310     c*  08              movel     estero        klin
088100981119     c                   move      knsif         ksif
088200981119     c                   movel     *zeros        kke1
088300981119     c                   move      'CDD'         kke1
088400981119     c                   move      *blanks       kke2
088500981119     C     Ktbe1         chain     tntbe01l                           31
088600981119     c* se non l'ho trovato cerco senza il sistema informativo specifico
088700981119     c                   if        *in31 = *on
088800981119     c                   move      *blanks       ksif
088900981119     C     Ktbe1         chain     tntbe01l                           31
089000981119     c                   endif
089100981119     c*
089200981119     c* imposto la chiave a parte il sistema informativo che rimane uguale all'ultima
089300981119     c* chain fatta
089400981119     c                   move      'CDD'         kcot
089500981119     c                   move      *blank        klin
089600990310     c*  08              movel     estero        klin
089700981119     C*
089800981119     C*-------- DETTAGLIO TABELLA -------------------------*
089900981119     C*                        SISTEMA INFORMATIVO  VALORIZZATO come da testata tabella
090000981119     C     Ktbe2         SETLL     tntbe02l                                     *
090100981119     C*
090200981229     C                   DO        *HIVAL
090300981229     C     Ktbe2         READE     tntbe02l                               30
090400981229     c* se fine file esco dal DO
090500981229     c                   if        *in30
090600981229     c                   leave
090700981229     c                   endif
090800981229     c* se record annullato leggo il successivo
090900981229     c                   if        tbeatb <> ' '
091000981229     c                   iter
091100981229     c                   endif
091200981229     c* controllo se è un documento già  richiesto / ricevuto
091300981229     c*
091400981229     c                   if        dcc = 'Y'
091500981229     c                   movel     tbeke1        wcdd
091600981229      * aggancio file documenti
091700981229     c     kdcc          chain     fndcc01l                           31
091800981229      *
091900981229     c                   if        not *in31
092000981229      * preparo i dati
092100981229     c                   exsr      subdcc
092200981229      *
092300981229     c                   endif
092400981229      *
092500981229     c                   endif
092600981229     c*
092700981229     c                   if        dcc = ' ' or (dcc = 'Y' and *in31)
092800981229      *
092900981229     C* controllo se è un documento da richiedere
093000981119     c                   movel     tbeuni        DCDD
093100981229     c* se c'è rivalsa (05 acceso) e il documento non è obbligatorio leggo il successivo
093200981229     c                   if        §cddrdsr <>'S' and *in05
093300981229     c                   iter
093400981229     c                   endif
093500981229     c* se non c'è rivalsa controllo l'altro flag  (05 spento)
093600981229     c                   if        §cddrdnr <>'S' and not *in05
093700981229     c                   iter
093800981229     c                   endif
093900990308     c*
094000990308     c* verifico se il documento non è valido per particolari anomalie
094100990308     c                   if        §cddvtad <> *blanks
094200990308     c                   movea     §cddvtad      wta
094300990308     c     §tadragr      lookup    wta                                    33
094400990308     c* per anomalia non trovata controllo se esiste un importo limite di validità
094500990308     c                   if        not *in33 and §cddlimr > 0
094600990308     c                   eval      *in33 = (dctipv < §cddlimr)
094700990308     c                   endif
094800990308     c*
094900990308     c                   if        *in33
095000990308     c                   iter
095100990308     c                   endif
095200990308     c*
095300990308     c                   endif
095400990305     c
095500981229     c*
095600981119     c                   movel     §CDDdesc      v1sdes
095700981119     c                   movel     tbeke1        v1scdd
095800981119     c* controllo quali sono i dati previsti per questo documento
095900981119     c                   if        §cddpnum = 'S'
096000981119     c                   movel     'S'           v1hi10
096100010913     c                   movel     §gedDBA       V1Svpr
096200981119     c                   seton                                        10
096300981119     c                   else
096400981119     c                   clear                   v1hi10
096500010913     c                   clear                   V1Svpr
096600981119     c                   setoff                                       10
096700981119     c                   endif
096800990520     c*
096900990520     c                   eval      v1hsol = §cddsopo
097000981229     c*
097100981229     c                   endif
097200981119     c* scrivo
097300981119     C                   ADD       1             nrr1                           *
097400981119     C                   WRITE     fi27s01                                      *
097500981119     c* carico la schiera con i documenti già presenti nel subfile
097600981119     c                   add       1             e
097700981119     c                   movea     v1scdd        cd(e)
097800981119     c*
097900990521     c                   clear                   fi27s01
098000981119     c**
098100981119     C                   ENDdo                                                  *
098200981119     c*
098300981119     c                   endsr
098400981229      *-------------------------------------------------------------------------*
098500981229      * SUBDCC    Routine Caricamento documenti
098600981229      *-------------------------------------------------------------------------*
098700981229      *
098800981229     C     SUBDCC        BEGSR
098900981229      *
099000981229     c* Data ricezione
099100981229     C                   CLEAR                   WLBDAT
099200981229     C                   Z-ADD     dccdrz        G02inv
099300981229     C                   movel     '3'           G02err
099400981229     C                   CALL      'XSRDA8'
099500981229     C                   PARM                    WLBDAT
099600981229     c                   z-add     g02dat        v1sdrz
099700981229     c* Codice documento
099800981229     c                   move      dcccdd        v1scdd
099900981229     c* Gestione decodifica e campi supplementari
100000981229     c                   exsr      subcdd
100100981229     c*
100200981229     c                   ENDSR
100300981229     c*
100400981119      *-------------------------------------------------------------------------*
100500981119      * SUBCDD    Routine Caricamento documenti
100600981119      *-------------------------------------------------------------------------*
100700981119      *
100800981118     C     SUBCDD        BEGSR
100900981118      *
101000981118     c                   setoff                                       10
101100981118     c                   clear                   v1hi10
101200981118     c*
101300981118     c                   clear                   dsbs02
101400981118     c                   clear                   dcdd
101500981118     C                   MOVEL     'C'           t02mod
101600981118     C                   MOVEL     knsif         t02sif
101700981118     C                   MOVEL     'CDD'         t02cod
101800981118     C                   MOVEL(P)  v1scdd        t02ke1
101900981118      *
102000981118     C                   CALL      'TIBS02R'
102100981118     C                   PARM                    KPJBA
102200981118     C                   PARM                    dsbs02
102300981118      *
102400981118    1C                   if        t02err = *BLANKS
102500981118     C                   MOVEL     T02UNI        DCDD
102600981118     c                   movel     §CDDdesc      v1sdes
102700981118     c* controllo quali sono i dati previsti per questo documento
102800981118     c                   if        §cddpnum = 'S'
102900981118     c                   movel     'S'           v1hi10
103000981118     c                   seton                                        10
103100981118     c                   else
103200981118     c                   clear                   v1hi10
103300981118     c                   setoff                                       10
103400981118     c                   endif
103500990520     c* recupero a chi devo sollecitare il documento mai pervenuto
103600990520     c                   eval      v1hsol = §cddsopo
103700990520     c*
103800981118    1C                   endif
103900981118      * recupero i dati dalk file fndcl01l
104000981118     c                   if        *in10
104100981120     c     kdct          chain(n)  fndcl01l                           30
104200981118     c                   if        not *in30
104300981118      * dta documento
104400981118     C                   CLEAR                   WLBDAT
104500981118     C                   Z-ADD     dclddc        G02inv
104600981118     C                   movel     '3'           G02err
104700981118     C                   CALL      'XSRDA8'
104800981118     C                   PARM                    WLBDAT
104900981118     c                   z-add     g02dat        v1sddc
105000981118      * numero documento
105100981118     c                   z-add     dclndc        v1sndc
105200981118      * importo + valuta
105300981118     c                   z-add     dclipr        v1sipr
105400981201     c                   move      dclvpr        v1svpr
105500981229     c                   move      dclcpr        v1hcmb
105600981118      *
105700981118    1C                   endif
105800981118      *
105900981118    1C                   endif
106000010911      *
106100010911      * Visualizzazione del codice divisa/valuta per gli importi in I/O
106200010912      *  (anche se 0) - mal che vada è DSPATR(ND) !
106300010913     c                   if        *in10            and
106400010913     c                             V1Svpr = *blanks
106500010911     c                   movel     §gedDBA       V1Svpr
106600010911     c                   endif
106700981118      *
106800981118     c                   endsr
106900981120     C*-------------------------------------------------------------------------*
107000981120     c* SUB_CONFE  Routine CONFERMA RICEZIONE DOCUMENTI
107100981120     C*-------------------------------------------------------------------------*
107200981120     c     SUB_CONFE     BEGSR
107300981120     C*
107400981120     c* lettura di ogni record del subfile
107500981120     c                   clear                   nrr1
107600981120    1c                   do        *hival
107700981120     c*
107800981120     c                   add       1             nrr1
107900981120     c     nrr1          chain     fi27s01                            30
108000981120     c*
108100981120    2c                   if        *in30
108200981120     c                   leave
108300981120    2c                   endif
108400981120     c* controllo se record già esistente in FNDCC
108500981229     c                   movel     v1scdd        wcdd
108600981120     c     kdcc          chain     fndcc01l                           50
108700981120     c* se esiste già controllo se è stato escluso
108800981120    2c                   if        not *in50  and v1sopz = 'E'
108900990715     c* annullo il record
109000990715     c                   eval      dccatb = 'A'
109100990715     c                   clear                   dccftr
109200990715     c                   clear                   dccdtr
109300990715     c                   update    fndcc000
109400990715     c*
109500981120     c* controllo se era un documento per il quale erano previsti l'inserimento di dati
109600981120    3c                   if        v1hi10 = 'S'
109700981120     c* pulisco dal file fndcl quello che era stato memorizzato
109800981120     c     kdct          chain     fndcl01l                           31
109900981120    4c                   if        not *in31
110000981120     c                   clear                   dclipr
110100981120     c                   clear                   dclvpr
110200981229     c                   clear                   dclcpr
110300981120     c                   clear                   dclndc
110400981120     c                   clear                   dclddc
110500981120     c                   update    fndcl000
110600981120    4c                   endif
110700981120    3c                   endif
110800981120     c                   iter
110900981120    2c                   endif
111000981202     c* se il documento non è escluso
111100981202     c                   if        v1sopz <> 'E'
111200981120     c* valorizzo i campi del file fndcc/fndcl
111300981202    2c                   if        *in50
111400981120     c                   z-add     kaac          dccaac
111500981120     c                   z-add     kfil          dccfil
111600981120     c                   z-add     knca          dccnca
111700981120    2c                   endif
111800990715     c                   clear                   dccatb
111900981120     c                   clear                   dccftr
112000981120     c                   clear                   dccdtr
112100981120     c                   move      v1scdd        dcccdd                         * codice documento
112200981120    2c                   if        v1sdrz > *zeros                              * data ricezione
112300981120     C                   z-add     v1sdrz        g02dat                         *
112400981120     c                   movel     *blanks       g02err                         *
112500981120     C                   call      'XSRDA8'                                     *
112600981120     C                   parm                    wlbdat                         *
112700981120     C                   move      g02inv        dccdrz                         *
112800981120     c                   else                                                   * se data = a 0
112900981120     c                   clear                   dccdrz
113000990520      * controllo se il documento che manca è da sollecitare alla filiale
113100990520     c                   if        v1hsol = 'S'                                 * sol filiale
113200990520     c                   seton                                        98        *
113300990520     c                   else                                                   *
113400981120     c                   seton                                        99        * doc non ricevuto
113500990520     c                   endif                                                  *
113600990520     c
113700981120    2C                   endif                                                  *
113800981202     c* solo se non è stato escluso
113900981120     c   50              write     fndcc000
114000981120     c  n50              update    fndcc000
114100981120     c* controllo se documento che prevee la richiesta di dati supplementari
114200981120    2c                   if        v1hi10 = 'S'
114300981120     c* aggancio il file fndcl ed aggiorno o scrivo se il file non eiste
114400981120     c     kdct          chain     fndcl01l                           31
114500981120    3c                   if        *in31
114600981120     c                   clear                   fndcl000
114700981120     c                   z-add     kaac          dclaac
114800981120     c                   z-add     kfil          dclfil
114900981120     c                   z-add     knca          dclnca
115000981120    3c                   endif
115100981120     c*
115200020125
115300020125      * controllo se emettere o no la videata di aggiornamento pratica in franchigia
115400020211      * - La c.a.corrisponde ai requisiti
115500020125      * - Inserimento della richiesta di risarcimento
115600020125      * - Importo richiesto <= §stdlpf
115700031211      * - Data della spedizione >= 2002/01/01 (§STDdlpf1)
115800031217      * (prima imposto l'importo "limite" da testare in base alla data
115900031217      *  spedizione)
116000031211     c                   reset                   w§STDlpf
116100031211if  3c                   if            wOKfra =  *on
116200031211     c                             and DCLipr =  *zeros
116300031211     c                             and V1Sipr >  0
116400031211     c                             and wDaSpe >= §STDdlpf1
116500031211sel 4c                   select
116600031211w   4c                   when      wDaSpe  >= §STDdlpf2
116700031211     c                   eval      w§STDlpf = §STDlpf2
116800031211w   4c                   when      wDaSpe  >= §STDdlpf1
116900031211     c                   eval      w§STDlpf = §STDlpf1
117000031211e   4c                   endsl
117100031217     c                   if        V1Sipr  <= w§STDlpf
117200020125     c                   exsr      sub_pfra
117300020125     c                   endif
117400031217e   3c                   endif
117500020125
117600981120     c                   clear                   dclftr
117700981120     c                   clear                   dcldtr
117800981120     c* valorizzo
117900981120     c                   z-add     v1sndc        dclndc                         * numero documento
118000981120     c                   z-add     v1sipr        dclipr                         * importo
118100010907     c                   IF        DCLipr <> 0
118200981120     c                   move      v1svpr        dclvpr                         * valuta
118300010905     c                   move      v1hcmb        dclcpr                         * cambio
118400010907     c                   else
118500010907     c                   clear                   dclvpr                         * valuta
118600010907     c                   clear                   dclcpr                         * cambio
118700010907     c                   ENDIF
118800981120    3c                   if        v1sddc > *zeros                              * data documento
118900981120     C                   z-add     v1sddc        g02dat                         *
119000981120     c                   movel     *blanks       g02err                         *
119100981120     C                   call      'XSRDA8'                                     *
119200981120     C                   parm                    wlbdat                         *
119300981120     C                   move      g02inv        dclddc                         *
119400981120    3C                   endif                                                  *
119500981120     c  n31              update    fndcl000
119600981120     c   31              write     fndcl000
119700981120    2c                   endif
119800981120     c*
119900981202     c                   endif                                                  * documento non escl
120000981120    1c                   enddo
120100981120     c*
120200981120     c* gestione fase
120300981120     c                   exsr      SUB_FASE
120400030704     c* memorizzo le note
120500030704     C                   CLEAR                   DSDN10
120600030704     c                   z-add     v1cdca        i10dta
120700030704     C                   movel     'C'           I10FLM
120800030704     c                   movel(P)  dsnumca       i10nkt
120900030704     c                   movel     i00fca        i10nks
121000030704     c                   movel     'N'           i10trc
121100030704     c                   movel(P)  dsdn10        kpjbu
121200030704     C                   CALL      'FIDN10R'
121300030704     C                   PARM                    kpjba
121400030704      *
121500981120     c*
121600981120     c                   endsr
121700020125      *-------------------------------------------------------------------------*
121800020125      * Sub_pfra   Routine richiesta aggiornamento pratica in Franchigia
121900020125      *-------------------------------------------------------------------------*
122000020125     c     sub_pfra      begsr
122100020125
122200020125     c                   do        *hival
122300020125     c                   exfmt     fi27w01
122400020125      * F12 esco
122500020125     c                   if        *inkl = *on
122600020125     c                   leave
122700020125     c                   endif
122800020125      * F6 di conferma
122900020125     c                   if        *inkf
123000020125      * Sì franchigia
123100020125     c                   if        v1pfra = 'SI'
123200020125     c                   eval      §dctpfra = 'F'
123300020125     c                   endif
123400020125     c                   leave
123500020125     c                   endif
123600020125     c                   enddo
123700020125
123800020125      * se ok franchigia scrivo le note
123900020125     c                   if        §dctpfra = 'F'
124000020125     c                   exsr      sub_notefra
124100020125     c                   endif
124200020125
124300020125     c                   endsr
124400020125      *-------------------------------------------------------------------------*
124500020125      * Sub_notefra   Routine scrittura note x immissione franchigia
124600020125      *-------------------------------------------------------------------------*
124700020125     c     sub_notefra   begsr
124800020125
124900020125     c                   clear                   dcspno
125000020125
125100020125     c                   eval      ktpd='C'
125200020125     c                   eval      knkt=dsnumca
125300020125     c                   eval      kstd=' '
125400020125     c                   eval      kdim=dateu
125500020125     c                   movel     wora          khim
125600020125     c                   movel     i00fca        knks
125700020125     c                   eval      ktrc='N'
125800020125
125900020422      * Cerco l'ultimo progressivo riga relativo alla fase attuale CA
126000020422      *  se esiste
126100020125     c     kdcs          setgt     fndcs01l
126200020125     c     kdcsp         readpe    fndcs01l
126300020125
126400020125     c                   eval      dcstpd = ktpd
126500020125     c                   eval      dcsnkt = knkt
126600020125     c                   eval      dcsstd = kstd
126700020125     c                   eval      dcsdim = kdim
126800020125     c                   eval      dcshim = khim
126900020125     c                   eval      dcsnks = knks
127000020125     c                   eval      dcstrc = ktrc
127100020125     c                   eval      dcsatb = ' '
127200020125
127300020125     c                   move      knmus         dcspru
127400020125     c                   if        simfel <> *zeros
127500020125     c                   movel     simfel        dcspos
127600020125     c                   else
127700020125     c                   eval      dcspos = 046
127800020125     c                   endif
127900020125     c                   clear                   dcsstn
128000020125     c                   clear                   dcsft1
128100020125     c                   clear                   dcsdt1
128200020125
128300020125      * Scrivo 2 note xchè su una non ci sto con tutta la frase
128400020125     c                   add       1             dcspno
128500020125     c                   clear                   dcsnot
128600020205     c                   eval      dcsnot = 'Ins. Automatico Pratica Franchigia'
128700020125     c                   write     fndcs000
128800020125
128900020125     c                   endsr
129000050706      *-------------------------------------------------------------------------*
129100131011      * Sub_noteaff   Routine scrittura note x affidamento pratica ad AIG
129200050706      *-------------------------------------------------------------------------*
129300050706     c     sub_noteaff   begsr
129400050706
129500050706     c                   clear                   dcspno
129600050706
129700050706     c                   eval      ktpd='C'
129800050706     c                   eval      knkt=dsnumca
129900050706     c                   eval      kstd=' '
130000050706     c                   eval      kdim=dateu
130100050706     c                   movel     wora          khim
130200050706     c                   movel     i00fca        knks
130300050706     c                   eval      ktrc='N'
130400050706
130500050706      * Cerco l'ultimo progressivo riga relativo alla fase attuale CA
130600050706      *  se esiste
130700050706     c     kdcs          setgt     fndcs01l
130800050706     c     kdcsp         readpe    fndcs01l
130900050706
131000050706     c                   eval      dcstpd = ktpd
131100050706     c                   eval      dcsnkt = knkt
131200050706     c                   eval      dcsstd = kstd
131300050706     c                   eval      dcsdim = kdim
131400050706     c                   eval      dcshim = khim
131500050706     c                   eval      dcsnks = knks
131600050706     c                   eval      dcstrc = ktrc
131700050706     c                   eval      dcsatb = ' '
131800050706
131900050706     c                   move      knmus         dcspru
132000050706     c                   if        simfel <> *zeros
132100050706     c                   movel     simfel        dcspos
132200050706     c                   else
132300050706     c                   eval      dcspos = 046
132400050706     c                   endif
132500050706     c                   clear                   dcsstn
132600050706     c                   clear                   dcsft1
132700050706     c                   clear                   dcsdt1
132800050706
132900050714      *
133000050706     c                   add       1             dcspno
133100050706     c                   clear                   dcsnot
133200131011     c                   eval      dcsnot = 'Pratica affidata ad AIG'
133300050706     c                   write     fndcs000
133400050706
133500050706     c                   endsr
133600981120     C*-------------------------------------------------------------------------*
133700981120     c* SUB_FASE   Routine SCRITTURA FASE E GESTIONE SOLLECITI
133800981120     C*-------------------------------------------------------------------------*
133900981120     c     SUB_FASE      BEGSR
134000981120      *
134100020422     C* aggiorno la fase nello storico fase   la fase attuale
134200020422      *   (600)/(610)/(620)/(635)
134300981120      *
134400981120     C                   movel     i00aac        Dcfaac
134500981120     C                   movel     i00fil        Dcffil
134600981120     C                   z-add     i00nca        Dcfnca
134700981120      *
134800981120     C                   movel     i00fca        Dcffca
134900981120     C                   movel     dctptr        Dcfptr
135000981120     C                   z-add     dateu         Dcfdfc
135100981120     C                   movel     wora          Dcfhfc
135200981120     C                   movel     i00fgs        Dcffev
135300981120     C                   movel     knmus         Dcfpru
135400981120      *
135500020125     c**!!!              clear                   dcflet
135600981120     c                   clear                   dcfftr
135700981120     c                   clear                   dcfdtr
135800020125
135900020125      * Imposto il flag di pratica assicurativa in Franchigia
136000020125     c                   eval      §dcfpfra = §dctpfra
136100020125     c                   movel     ddcf01        dcflet
136200020125
136300981120      *
136400981120     c                   WRITE     fndcf000
136500981124      *
136600981124     c* aggiorno  testata CA
136700020422     c* ricerca del punto operativo abilitato alla gestione della fase
136800020422     c*  successiva
136900981124     c                   exsr      SUB_FASU
137000020422     c* dopo il secondo sollecito non aggiorno + la fase di ricezione
137100020422     c*  documenti sulla CA
137200981126     c                   if        i00fca > ultsol
137300981126     c                   else
137400981124     c                   movel     i00fca        dctfca
137500981126     c                   endif
137600981222     c                   clear                   dctft1
137700981222     c                   clear                   dctft2
137800050715
137900050715      * se ricezione totale dei documenti
138000050715    1c                   IF        not *in99 and not *in98 and
138100050715      * se ca con data spedizione maggiore il 17042005 verifico se legata ad evento fortuito
138200131011      * per forzare la CA all'AIG
138300050715     c                             Wdaspe > 20050417 and dctnev > 0
138400050715      * aggancio il file eventi
138500050715     c     kdet          chain     Fndet01l
138600050715    2c                   If        %found(fndet01l)
138700050715      * verifico se evento fortuito in base al tipo anomalia
138800050715     c                   clear                   dsbs02
138900050715     c                   movel     'C'           t02mod
139000050715     c                   movel     knsif         t02sif
139100050715     C                   MOVEL(P)  dettad        t02ke1
139200050715     C                   movel     'TAD'         t02cod
139300050715      *
139400050715     C                   CALL      'TIBS02R'
139500050715     c                   parm                    KPJBA
139600050715     c                   parm                    dsbs02
139700050715      *
139800050715    3c                   if        t02err =  *blanks
139900050715     c                   movel     t02uni        dtad
140000050715    3c                   endif
140100131011      *  FORTUITO  Affido ad AIG
140200050715    3c                   If        §tadevfo = 'S'
140300050715     c                   eval      §dctaffi = 'A'
140400131011      * scrivo note relative all'affidamento ad AIG
140500050715     c                   exsr      SUB_noteaff
140600050715    3c                   endif
140700050715
140800050715    2c                   endif
140900050715
141000050715    1c                   endif
141100020125
141200020125     c                   movel     ddct01        dctflo
141300020422
141400021120      * Se C.A. aperta su spedizioni in PF, export DPD
141500021120      *       o aperta su spedizioni export FedEx
141600110727      *       o aperta su spedizioni export EuroExpress con Partner rivalsa
141700021120      *       comunque con responsabilità partner
141800021120      *       ed in fase 660 (vedi ricezione totale)
141900020422      * imposto il campo DCTPER = 999,99
142000110727    1c                   if          ((§DCTport = 'F'
142100020422     c                             and §DCTtisp = 'E'
142200110727     c                             and §DCTEURO = 'D')
142300110727     c                             or (§DCTtisp = 'E'
142400110727     c                             and §DCTeuro = 'F')
142500110727     c                             or (§DCTtisp = 'E'
142600110727     c                             and §DCTeuro = 'X'
142700120413     c                             and §DCTport = 'F'
142800110727     c                             and Weexpartner = *on))
142900020422     c                             and DCTres   = 'P'
143000020509     c                             and not *in99 and not *in98
143100020422     c                   move      *all'9'       DCTper
143200020422   -1c                   endif
143300020125
143400981124     c                   update    fndct000
143500981124      *
143600981120     c
143700990520     c* R I C E Z I O N E    P A R Z I A L E  (99 = *on / 98= *on)
143800981120      *
143900990520      * Sollecito A CLIENTE
144000981120     c                   if        *in99
144100981120      * se ultima fase è  maggiore della fase 1° sollecito non faccio stampe
144200981120      * se ultima fase è  minore   della fase 1° sollecito stampo il 1° sollecito
144300981120      * fin quando non stampo il secondo sollecito
144400981123     c                   if        ultsol <= wfaso
144500981120      *
144600981120     c                   movel     wfaso         i00fca
144700981120     c                   movel     fidn00ds      kpjbu
144800981120     c                   call      'FIDN28R'
144900981120     c                   parm                    kpjba
145000981120     c                   endif
145100990520      *
145200990520     c                   Endif
145300990520      *
145400990520      * Sollecito A FILIALE
145500990520     c                   if        *in98
145600990520      *
145700990520     c                   movel     wfaso         i00fca
145800990520     c                   movel     i00tpb        savtpb
145900990520     c                   movel     'F'           i00tpb
146000990520      *
146100990520     c                   movel     fidn00ds      kpjbu
146200990520     c                   call      'FIDN28R'
146300990520     c                   parm                    kpjba
146400990520     c                   movel     savtpb        i00tpb
146500990520     c                   endif
146600990520     c* R I C E Z I O N E    T O T A L E   (99 = *off , 98 = *off)
146700981120      *
146800990520     c                   IF        not *in99 and not *in98
146900120508
147000120508      *?se C.A. su Bolla in Porto Franco Import EEX non devo stampare avviso danno
147100120508     c                   IF        not wPFIEEX
147200981120     c*
147300981120     c* richiamo il programma che ristampa l'avviso danno ed aggiorna la CA per la fase
147400981120     c* 660
147500981120     c                   movel     wfaad         i00fca
147600981120     c                   movel     fidn00ds      kpjbu
147700981126     c                   call      'FIDN35R'
147800981120     c                   parm                    kpjba
147900120508     c                   ENDIF
148000020422
148100021120      * Se C.A. aperta su spedizioni in PF, export DPD
148200021120      *       o aperta su spedizioni export FedEx
148300110727      *       o aperta su spedizioni export EuroExpress
148400021120      *       comunque con responsabilità partner
148500060207      *             (e con flag Franchigia NON + COME DA RICHIESTA DI MELCHIONI DI OGGI)
148600020422      * emetto a video un messaggio (unica possibilità di uscire: F3)
148700110727    1c                   dow         ((§DCTport = 'F'
148800020422     c                             and §DCTtisp = 'E'
148900110727     c                             and §DCTEURO = 'D')
149000110727     c                             or (§DCTtisp = 'E'
149100021120     c                             and §DCTeuro = 'F')
149200110727     c                             or (§DCTtisp = 'E'
149300110727     c                             and §DCTeuro = 'X'
149400110727     c                             and Weexpartner = *on))
149500020422     c                             and DCTres   = 'P'
149600120413     c                             and §DCTport = 'F'
149700060207     c*******                      and §DCTpfra = 'F'
149800021120    2c                   select
149900021120     c                   when          §DCTport = 'F'
150000021120     c                             and §DCTtisp = 'E'
150100021120     c                             and §DCTEURO = 'D'
150200021120     c                   eval      V1Euro = 'DPD.'
150300021120     c                   when          §DCTtisp = 'E'
150400021120     c                             and §DCTeuro = 'F'
150500021120     c                   eval      V1Euro = 'FedEx.'
150600110727     c                   when          §DCTtisp = 'E'
150700110727     c                             and §DCTeuro = 'X'
150800110727     c                             and Weexpartner = *on
150900110727     c                   eval      V1Euro = 'EuroExpress'
151000021120   -2c                   endsl
151100020422     c                   exfmt     fi27w02
151200020422      * F3 esco
151300020422    2c                   if        *inkc = *on
151400020422     c                   leave
151500020422   -2c                   endif
151600020422   -1c                   enddo
151700050706
151800131011      * Se C.A. affidata ad AIG
151900050706      * emetto a video un messaggio (unica possibilità di uscire: F3)
152000050706    1c                   dow          §DCTaffi = 'A'
152100050706     c                   exfmt     fi27w03
152200050706      * F3 esco
152300050706    2c                   if        *inkc = *on
152400050706     c                   leave
152500050706   -2c                   endif
152600050706   -1c                   enddo
152700020125
152800020125      * Se c/a con flag franchigia eseguo subito la stampa progetto di liquidazione
152900020125      * per la fase 700
153000050513      * Da maggio del 2005 eseguiamo subito il progetto di Liquidazione in quanto non
153100050513      * inviamo più i documenti alla Consuldanni
153200131011      * a meno che non sia affidata ad AIG per evento fortuito
153300050513     c*********          if        §dctpfra = 'F'
153400051124      * se CA in franchigia  oppure
153500131011      *    CA non affidata ad AIG chiamo il programma di gestione progetto di liquidazione
153600051124     c                   If        §dctaffi = ' ' or §DCTpfra = 'F'
153700020125     c                   movel     wfliq         i00fca
153800020125     c                   movel     fidn00ds      kpjbu
153900130920     c                   call      'FIDN29R'
154000020125     c                   parm                    kpjba
154100050513     c*********          endif
154200050706     c                   endif
154300981120     c*
154400981120     c                   endif
154500981120     c*
154600981120     c                   endsr
154700981124      *****************************************************************
154800981124      * RICAVO FASE SUCCESSIVA
154900981124      *****************************************************************
155000981124     C     SUB_FASU      BEGSR
155100981124      *
155200981124     c                   CLEAR                   FIDN05DS
155300981124     c                   movel     'F'           I05MOD
155400981124     c                   z-add     I00fca        I05fca
155500981124     c                   movel     'P'           I05fpr
155600981124     c                   movel     dctptr        I05tpc
155700981124     c                   movel     'O'           I05ffs
155800981124     c                   movel     ' '           I05fpe
155900981124     c                   z-add     DATEU         i05dta
156000981124     c                   move      dcttad        i05tad
156100981125      * Valorizzo numero CA
156200981125     c                   z-add     DCTaac        i05aac
156300981125     c                   z-add     DCTfil        i05fil
156400981125     c                   z-add     DCTnca        i05nca
156500981124
156600981124     C                   CALL      'FIDN05R'
156700981124     c                   PARM                    KPJBA
156800981124     c                   PARM                    FIDN05DS
156900981124      *
157000981124     c                   movel     O05REC        DS_FNDFA
157100981124      *
157200981124     c                   SELECT
157300981124     c                   WHEN      DFAgfs = 'P'
157400981230     c                   z-add     WDCTlnp       dctgfc
157500981124     c                   WHEN      DFAgfs = 'S'
157600981124     c                   z-add     46            dctgfc
157700981124     c                   WHEN      DFAgfs = 'A'
157800981124     c                   z-add     DCTlna        dctgfc
157900981124     c                   OTHER
158000981124     c                   z-add     DCTfil        dctgfc
158100981124     c                   ENDSL
158200981124      *
158300981124     c                   ENDSR
158400990519      ****************************************************************
158500990519      * F14 = INTERROGAZIONE C.A.
158600990519      ****************************************************************
158700990519     C     SUB_F14       BEGSR
158800990519      *
158900990519      *
159000990519     c                   movel     'I'           I00mod
159100990519      *
159200990519     c                   movel     FIDN00DS      kpjbu
159300990519      *
159400990519     c                   call      'FIDN01R'
159500990519     c                   parm                    kpjba
159600990519     c                   movel     'G'           I00mod
159700990519      *
159800990519     c                   endsr
159900010905
160000010905      *****************************************************************
160100010910      * VERIFICA NUMERO DECIMALI IMPOSTATI
160200010905      *****************************************************************
160300010910     C     srCTRDEC      BEGSR
160400010905      *
160500011017     C                   reset                   XDECDS
160600010905      *
160700011017     C                   z-add     V1Sipr        ICDnum
160800011017     C                   z-add     §cvDEC        ICDnrd
160900010905      *
161000011017     C                   call      'XDEC'
161100011017     C                   parm                    XDECDS
161200010905      *
161300011017     C                   IF        OCDesi  <> *off
161400010910     C                   eval      *IN28 = *ON
161500010905     C                   ENDIF
161600010905      *
161700010905     C                   ENDSR
161800010905
161900981119      **************************************************************************
162000981022      *   ROUTINE INIZIALE
162100981119      **************************************************************************
162200981022     C     *INZSR        BEGSR
162300981022      *
162400981022     C     *ENTRY        PLIST
162500981022     C                   PARM                    KPJBA
162600981022     C                   MOVEL     KPJBU         fidn00ds
162700981022      *
162800981022     C                   Z-ADD     1             CODUT
162900981022     C                   CALL      'X§PARUT'
163000981022     C                   PARM                    UT§DSE0F
163100981022     C                   MOVEL     RAGUT         RSUT
163200981022     C                   MOVEL     REC80         CNCR80
163300981119     C* ACCESSO   TABEL
163400981119     C     KTAB          KLIST
163500981119     C                   KFLD                    CODUT
163600981119     C                   KFLD                    §COD
163700981119     C                   KFLD                    §KEY
163800990310     C     KTABE         KLIST
163900990310     C                   KFLD                    CODUT
164000990310     C                   KFLD                    §COD
164100981022      * accesso tntbe
164200981119     C     KTbe1         KLIST                                                  *
164300981119     C                   KFLD                    kcot                           *
164400981119     C                   KFLD                    kke1                           *
164500981119     C                   KFLD                    kke2                           *
164600981119     C                   KFLD                    klin                           *
164700981119     C                   KFLD                    ksif                           *
164800981119     c*
164900981119     C     KTbe2         KLIST                                                  *
165000981119     C                   KFLD                    kcot                           *
165100981119     C                   KFLD                    klin                           *
165200981119     C                   KFLD                    ksif                           *
165300981022      * accesso fndct01l
165400981022     C     Kdct          klist
165500981022     C                   KFLD                    kaac
165600981022     C                   KFLD                    kfil
165700981022     C                   KFLD                    knca
165800981120      * accesso fndcf01l
165900981120     C     Kdcf          klist
166000981120     C                   KFLD                    kaac
166100981120     C                   KFLD                    kfil
166200981120     C                   KFLD                    knca
166300981120     C                   KFLD                    kdat
166400981120     C                   KFLD                    kora
166500981120      * accesso fndcc01l
166600981120     C     Kdcc          klist
166700981120     C                   KFLD                    kaac
166800981120     C                   KFLD                    kfil
166900981120     C                   KFLD                    knca
167000981229     c                   kfld                    wcdd
167100050706      * accesso fndet01l
167200050706     C     Kdet          klist
167300050706     C                   KFLD                    dctaae
167400050706     C                   KFLD                    dctnev
167500020125      * accesso archivio note
167600020125     c     kdcsp         klist
167700020125     c                   kfld                    ktpd
167800020125     c                   kfld                    knkt
167900020125     c                   kfld                    kstd
168000020125     c                   kfld                    kdim
168100020125     c                   kfld                    khim
168200020125     C                   kfld                    knks
168300020125     C                   kfld                    ktrc
168400020125
168500020125     c     kdcs          klist
168600020125     c                   kfld                    ktpd
168700020125     c                   kfld                    knkt
168800020125     c                   kfld                    kstd
168900020125     c                   kfld                    kdim
169000020125     c                   kfld                    khim
169100020125     c                   kfld                    knks
169200020125     c                   kfld                    ktrc
169300020125     c                   kfld                    prgnot
169400981022      * accesso archivio bolle
169500981022     c     kbol01        klist
169600981022     c                   kfld                    i00aas
169700981022     c                   kfld                    i00lnp
169800981022     c                   kfld                    i00nrs
169900981022     c                   kfld                    i00nsp
170000981022      *
170100981027     C                   TIME                    W0140
170200981027      *
170300981022     C                   MOVE      W0140         WDTGIO
170400981120     C                   MOVEL     W0140         WORA
170500981022      * UDATE IN AAAAMMGG
170600981022     C                   Z-ADD     WDTGIO        G02DAT
170700981022     C                   MOVEL     *BLANK        G02ERR
170800981022     C                   CALL      'XSRDA8'
170900981022     C                   PARM                    WLBDAT
171000981022     C                   MOVEL     G02INV        DATEU
171100981211      * titolo
171200981211      *
171300981217     c                   clear                   vcttit
171400981217     c                   movel     TIT_1         vcttit
171500990310      *
171600990310      *  Carico tabella 2F
171700020327     C**!!!              clear                   xx
171800020327     C**!!!              MOVE      '2F'          §COD
171900020327     C**!!!ktabe         SETLL     TABEL
172000990310      *
172100020327     C**!!!              DO        *HIVAL
172200020327     C**!!!ktabe         READE     TABEL                                  31
172300020327     C**!31              leave
172400020327     c**!!!              movel     tbluni        DS2F
172500020327     c**!!!              IF        §2Ftxt =  'E'
172600020327     C**!!!              add       1             xx
172700020327     C**!!!              movel     TBLKEY        S2F(xx)
172800020327     c**!!!              ENDIF
172900020327     c**!!!              ENDDO
173000010905      *
173100010905      * aggancio tabella GED
173200010910     c                   clear                   DGEDDN
173300010905     c                   clear                   dsbs02
173400010905     C                   MOVEL     'C'           t02mod
173500010905     C                   MOVEL     knsif         t02sif
173600010905     C                   MOVEL     'GED'         t02cod
173700010905     C                   MOVEL(P)  'DANNI'       t02ke1
173800010905      *
173900010905     C                   CALL      'TIBS02R'
174000010905     C                   PARM                    KPJBA
174100010905     C                   PARM                    DSBS02
174200010905      *
174300010905     C                   if        t02err = *BLANKS
174400010910     C                   MOVEL     T02UNI        DGEDDN
174500010905     C                   endif
174600020125      *
174700020125      * aggancio tabella STD
174800020125     c                   clear                   dstd
174900020125     c                   clear                   dsbs02
175000020125     C                   MOVEL     'C'           t02mod
175100020125     C                   MOVEL     knsif         t02sif
175200020125     C                   MOVEL     'STD'         t02cod
175300020125     C                   MOVEL(P)  1             t02ke1
175400020125     C                   MOVEL(P)  §gedDBA       t02ke2
175500020125      *
175600020125     C                   CALL      'TIBS02R'
175700020125     C                   PARM                    KPJBA
175800020125     C                   PARM                    DSBS02
175900020125      *
176000020125     C                   if        t02err = *BLANKS
176100020125     C                   MOVEL     T02UNI        dstd
176200020125     C                   endif
176300990310      *
176400990305      *
176500981119      * C A R I C A M E N T O      P R I M A    V I D E A T A
176600981119     c                   exsr      subcar
176700981119     c*
176800981022     C                   ENDSR
176900981119** MSG L78
177000981119Data Ricezione errata                                                           1
177100981119Data Documento errata                                                           2
177200981119Valuta errata                                                                   3
177300981126Non ammessi importi con decimali per la divisa indicata                         4
177400010911LIBERO - ex Se immessa la divisa immettere anche l'importo o eliminare entrambi 5
177500990310CLIENTE ESTERO fare attenzione alle note di stampa ed ai documenti              6
177600010910Ammessi importi con massimo __ decimali per la divisa indicata                  7
