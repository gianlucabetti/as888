000100070924      *---------------------------------------------------------------*
000200070924      * GESTIONE TABELLA CAUSALI EVENTI                               *
000300070924      *---------------------------------------------------------------*
000400070924     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500070924      *
000600070924      * ARCHIVI-------------------------------------------------------*
000700070924      *
000800070924     fTABEL00F  if   e           k disk
000900070924     fTRTB2AD   cf   e             workstn sfile(TB2AS01:S01nrr)
001000070924      *
001100070924      * COSTANTI -----------------------------------------------------*
001200070924      *
001300070924     d C_tab           c                   const('2A')
001400070924     d C_opzG          c                   const('2=Modifica  3=Copia -
001500070924     d                                      4=Annullamento/Ripristino -
001600070924     d                                      5=Visualizzazione')
001700070924     d C_opzS          c                   const('1=Scelta')
001800070924      *
001900070924      * SCHIERE ------------------------------------------------------*
002000070924      *
002100070924     d $Msg            s             78    dim(01) ctdata perrcd(1)
002200070924     d $Opz            s              2  0 dim(10)
002300070924      *
002400070924      * STRUTTURE DATI -----------------------------------------------*
002500070924      *
002600070924      * - Parametri ricevuti
002700070924     d KPJBA         e ds
002800070924     d TRTB2Ads      e ds                  inz
002900070924      *
003000070924      * - Parametri x Controllo profilo utenti
003100070924     d TIBS34ds      e ds                  inz
003200070924      * - Ds di riferimento al file esterno AZUTE00F
003300070924     d §AZUTE        e ds                  extname(AZUTE00F)
003400070924     d                                     dtaara
003500070924      * - Ds per dati organigramma
003600070924     d §DatiUte      e ds                  extname(dDatiUte)
003700070924     d                                     dtaara
003800070924      *
003900070924      * - Tab. "2A" = Causali Eventi
004000070924     d ds2A          e ds                  inz
004100070924      *
004200070924      *
004300070924     d Status         sds
004400070924     d  SDSpgm           *proc
004500070924      *
004600070924      * VARIABILI ----------------------------------------------------*
004700070924      *
004800070924      * - Flags booleani
004900070924     d $Fine           s              1    inz(*off)
005000070924     d $InzS01         s              1    inz(*on)
005100070924      * - Indici di schiera
005200070924     d xx              s              2  0
005300070924      * - Campi per gestione subfile
005400070924     d S01nrr          s                   like(C01rcd)  inz
005500070924      * - Campi di comodo
005600070924     d SAVvc1cod       s                   like(VC1cod)  inz
005700070926     d SAVvc1des       s                   like(VC1des)  inz
005800070924      *
005900070924      * DATI PER ORDINAMENTO SUBFILE ---------------------------------*
006000070924      *
006100070927      *****************************************************************
006200070924      * Constants
006300070924      *
006400070927      *  MaxKey - Maximum number of key fields allowed by this program
006500070924      *
006600070927      *****************************************************************
006700070924     d MaxKey          c                    1
006800070924     d Ascendente      c                   1
006900070924     d Carattere       c                   6
007000070924     d Put             c                   1
007100070924     d EndPut          c                   2
007200070924     d Get             c                   3
007300070927      *****************************************************************
007400070924      * Standalone fields
007500070924      *
007600070927      *  S01nrr     - Relative record number for subfile
007700070927      *  SizeList   - Size of list
007800070927      *  ReturnSize - Size of list returned by sort API
007900070927      *****************************************************************
008000070924     d NotUsed         s             16a   inz
008100070924     d ReturnSize      s              9b 0 inz
008200070924     d SizeList        s              9b 0 inz
008300070924     d RrnLast         s              5i 0 inz
008400070924     d WrkSort         s              1    inz(*off)
008500070927      *****************************************************************
008600070924      * Data Structures
008700070927      *  SflRcd     - The entire subfile record to pass to the sort
008800070927      *  QLGSCB     - The sort request block for the QLGSORT API
008900070927      *  QLGSCB00   - The sort request block for the QLGSRTIO API
009000070927      *  QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
009100070927      *  USEC      - Error structure for the QLGSORT API
009200070927      *****************************************************************
009300070924     d SflRcd          ds                  inz
009400070924     d  VS1opz                             inz
009500070924     d  VS1cod                             inz
009600070924     d  VS1des                             inz
009700070924     d  Selected                      1a   inz
009800070924      /COPY QSYSINC/QRPGLESRC,QLGSORT
009900070924     d QLGKL                         16    dim(MaxKey)
010000070924     d  QLGSP00                       9b 0 overlay(QLGKL:00001)
010100070924     d  QLGSS00                       9b 0 overlay(QLGKL:00005)
010200070924     d  QLGDT00                       9b 0 overlay(QLGKL:00009)
010300070924     d  QLGSO00                       9b 0 overlay(QLGKL:00013)
010400070924      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
010500070924      /COPY QSYSINC/QRPGLESRC,QUSEC
010600070924      *
010700070924      * PROCEDURE ----------------------------------------------------*
010800070924      *
010900070924     d TIBS34R         pr                  extpgm('TIBS34R')
011000070924     d  TIBS34ds                           likeds(TIBS34ds)
011100070924      *
011200070924     d Qlgsort_pr      pr                  extpgm('QLGSORT')
011300070924     d  pr_QLGSCB                          like(Qlgscb)
011400070924     d  pr_NotUsed1                        like(NotUsed)
011500070924     d  pr_NotUsed2                        like(NotUsed)
011600070924     d  pr_SizeList                        like(SizeList)
011700070924     d  pr_ReturnSize                      like(ReturnSize)
011800070924     d  pr_QUSEC                           like(Qusec)
011900070924     d                                     options(*varsize)
012000070924     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
012100070924     d  pr_QLGSCB00                        like(QLGSCB00)
012200070924     d  pr_SflRcd                          like(SflRcd)
012300070924     d  pr_NotUsed1                        like(NotUsed)
012400070924     d  pr_SizeList                        like(SizeList)
012500070924     d  pr_NotUsed2                        like(NotUsed)
012600070924     d  pr_QUSEC                           like(Qusec)
012700070924     d                                     options(*varsize)
012800070924     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
012900070924     d  pr_QLGSCB00                        like(QLGSCB00)
013000070924     d  pr_NotUsed1                        like(NotUsed)
013100070924     d  pr_SflRcd                          like(SflRcd)
013200070924     d  pr_SizeList                        like(SizeList)
013300070924     d  pr_NotUsed2                        like(NotUsed)
013400070924     d  pr_QUSEC                           like(Qusec)
013500070924     d                                     options(*varsize)
013600070924      *
013700070924      * KEY-LIST  ----------------------------------------------------*
013800070924      *
013900070924     d KDStabel      e ds                  extname(TABEL00F:*key) inz
014000070924
014100070927      *===============================================================*
014200070924      * RIEPILOGO INDICATORI                                          *
014300070924      *---------------------------------------------------------------*
014400070926      * 28 - Visualizzazione messaggio di errore                      *
014500070927      * 30 - *ON = Pulizia del subfile  / *OFF = SflDsp    + SflClr   *
014600070927      * 31 - *ON = NO emissione subfile / *OFF = SflDspCtl + SflClr   *
014700070926      * 32 - SflEnd                                                   *
014800070926      * 33 - Record modificato nel subfile (SflNxtChg)                *
014900070927      * 40 - *ON = Ricerca/Scelta       / *OFF = Manutenzione         *
015000070927      * 41 - *ON = Sfl ordinato x Descr / *OFF = Sfl ordinato x Cod.  *
015100070927      * 50 - Opzione errata nel subfile                               *
015200070927      *===============================================================*
015300070924
015400070924     c     *entry        plist
015500070924     c                   parm                    kpjba
015600070924
015700070924      /free
015800070924
015900070924       exsr sr_Parm;
016000070924
016100070924       dow $Fine = *off;
016200070926         exsr sr_GesS01;
016300070924       enddo;
016400070924
016500070924       exsr sr_Uscita;
016600070924
016700070924       // ------------------------------------------------------------*
016800070924       // routine iniziale                                            *
016900070924       // ------------------------------------------------------------*
017000070926       begsr *inzsr;
017100070926       //    ¯¯¯¯¯¯¯
017200070924
017300070926        in(e) §azute;
017400070926        if not %error;
017500070926          in(e) §datiute;
017600070926        endif;
017700070926        if %error or rsut = *blanks;
017800070926          tibs34r(tibs34ds);
017900070926          in §azute;
018000070926          in §datiute;
018100070926        endif;
018200070924
018300070926       endsr;
018400070924
018500070924       // ------------------------------------------------------------*
018600070924       // elaborazione parametri ricevuti                             *
018700070924       // ------------------------------------------------------------*
018800070924       begsr sr_Parm;
018900070926       //    ¯¯¯¯¯¯¯¯
019000070924
019100070924        TRTB2Ads = kpjbu;
019200070924        TB2Auni = *blanks;
019300070924        TB2Afxx = *off;
019400070924        TB2Aerr = *off;
019500070924
019600070924        clear $Opz;
019700070924        select;
019800070924       // ricerca e scelta
019900070924         when TB2Aopz = '1';
020000070927          *in40 = *on;
020100070924          VC1opz = C_opzS;
020200070924          $Opz(1) = 01;
020300070924       // gestione
020400070924         when  TB2Aopz = *blanks;
020500070927          *in40 = *off;
020600070924          VC1opz = C_opzG;
020700070924          $Opz(1) = 02;
020800070924          $Opz(2) = 03;
020900070924          $Opz(3) = 04;
021000070924          $Opz(4) = 05;
021100070924       // altrimenti
021200070924         other;
021300070924       // esco
021400070924          TB2Aerr = *on;
021500070924          exsr sr_Uscita;
021600070924         endsl;
021700070924
021800070926        PgmName = SDSpgm;
021900070927
022000070927       // intestazione videata
022100070927        clear VT1txt;
022200070927        select;
022300070927         when TB2Atip = 'G';
022400070927          VT1txt = '* CAUSALI APERTURA GIACENZA *?';
022500070927         when TB2Atip = 'S';
022600070927          VT1txt = '* Tab. "2A" - CAUSALI EVENTI *';
022700070927         when TB2Atip = 'R';
022800070927          VT1txt = '* Tab. "2A" - CAUSALI EVENTI *';
022900070927         when TB2Atip = 'P';
023000070927          VT1txt = '* Tab. "2A" - CAUSALI EVENTI *';
023100070927         when TB2Atip = 'N';
023200070927          VT1txt = '* Tab. "2A" - CAUSALI EVENTI *';
023300070927         when TB2Atip = '§';
023400070927          VT1txt = '* Tab. "2A" - CAUSALI EVENTI *';
023500070927         other;
023600070927          VT1txt = '* Tab. "2A" - CAUSALI EVENTI *';
023700070927        endsl;
023800070924
023900070924       endsr;
024000070924
024100070924       // ------------------------------------------------------------*
024200070924       // gestione subfile                                            *
024300070924       // ------------------------------------------------------------*
024400070924       begsr sr_GesS01;
024500070926       //    ¯¯¯¯¯¯¯¯¯¯
024600070924
024700070924       // caricamento subfile
024800070926        if $InzS01 = *on;
024900070926          exsr sr_InzS01;
025000070926          $InzS01 = *off;
025100070926        endif;
025200070924
025300070926        if C01csr > *zeros;
025400070926          C01rcd = C01csr;
025500070926        else;
025600070926          C01rcd = 1;
025700070926        endif;
025800070924
025900070924       // se non so quale pagina visualizzare forzo pagina 1
026000070926        if C01rcd < 1;
026100070926          C01rcd = 1;
026200070926        endif;
026300070924
026400070924       // Emissione del subfile
026500070926        write TB2AT01;
026600070926        write TB2AZ01;
026700070926        if RrnLast <= *zeros;
026800070926          write TB2AD00;
026900070926        endif;
027000070926        exfmt TB2AC01;
027100070924
027200070924       // reset indicatore generico di errore
027300070926        *in28 = *off;
027400070926        *in90 = *off;
027500070924       // pulisco il campo messaggi
027600070926        clear VC1msg;
027700070924
027800070924       // controllo tasti funzionali del subfile
027900070926        select;
028000070924
028100070924       // F3=Fine
028200070926         when *inKC;
028300070926          TB2Afxx = '1';
028400070926          $Fine = *on;
028500070924
028600070924       // F5=Refresh
028700070926         when *inKE;
028800070926          $InzS01 = *on;
028900070927
029000070927       // F10=Immissione
029100070927         when *inKJ;
029200070924
029300070924       // F11=Ordinamento
029400070926         when *inKK;
029500070926          clear VC1cod;
029600070926          clear VC1des;
029700070926          clear SAVvc1cod;
029800070926          clear SAVvc1des;
029900070926          $InzS01 = *on;
030000070926          if WrkSort = *off;
030100070926            WrkSort = *on;
030200070926          else;
030300070926            WrkSort = *off;
030400070926          endif;
030500070927
030600070927       // subfile vuoto
030700070927         when RrnLast <= *zeros;
030800070927          leavesr;
030900070924
031000070924       // in tutti gli altri casi
031100070926         other;
031200070927         // controllo ed elaborazione scelte su subfile
031300070926          exsr sr_OpzS01;
031400070926          if *in28;
031500070926            leavesr;
031600070926          endif;
031700070927         // verifica richiesta di nuovo posizionamento
031800070926          if NOT *in41 and VC1cod <> SAVvc1cod;
031900070926            SAVvc1cod = VC1cod;
032000070926            $InzS01 = *on;
032100070926          endif;
032200070926          if *in41 and VC1des <> SAVvc1des;
032300070926            SAVvc1des = VC1des;
032400070926            $InzS01 = *on;
032500070926          endif;
032600070926        endsl;
032700070924
032800070926       endsr;
032900070924
033000070924       // ------------------------------------------------------------*
033100070926       // inizializzazione / caricamento subfile                      *
033200070924       // ------------------------------------------------------------*
033300070926       begsr sr_InzS01;
033400070926       //    ¯¯¯¯¯¯¯¯¯¯
033500070924
033600070926       // inizializzazione del subfile
033700070926        RrnLast = *zeros;
033800070926        S01nrr = *zeros;
033900070927        *in30 = *on;
034000070927        *in31 = *on;
034100070926        write TB2AC01;
034200070927        *in31 = *off;
034300070927        *in33 = *off;
034400070926
034500070926       // intestazione videata
034600070926        clear VC1txt;
034700070927       // if TB2Atip <> *blanks;
034800070927       //   VC1txt = 'Visualizzate SOLO causali con "Utilizzo Chiusura +
034900070927       //             Distinta" = ' + TB2Atip;
035000070927       // endif;
035100070924
035200070926       // impostazione chiave di posizionamento e lettura file
035300070926        TBLkut = 1;
035400070926        TBLcod = C_tab;
035500070926        if WrkSort = *off;
035600070926          TBLkey = VC1cod;
035700070926        else;
035800070926          clear TBLkey;
035900070926        endif;
036000070924
036100070924       // posizionamento sul file
036200070924       // se è stato scelto il posizionamento
036300070926        if VC1cod <> *blanks;
036400070926          C01csr = *zeros;
036500070926          setll (TBLkut:TBLcod:TBLkey) TABEL00F;
036600070924       // altrimenti
036700070926        else;
036800070926          setll (TBLkut:TBLcod) TABEL00F;
036900070926        endif;
037000070924
037100070924       // fino a che non è fine file
037200070926        DOU %eof(TABEL00F);
037300070924
037400070926       // lettura file
037500070926          reade (TBLkut:TBLcod) TABEL00F;
037600070924
037700070924       // fine file esco
037800070926          if %eof(TABEL00F);
037900070926            leave;
038000070926          endif;
038100070924
038200070926          ds2A = TBLuni;
038300070926
038400070924       // selezione record
038500070926          select;
038600070926          // se "ricerca/scelta" non carico i records annullati
038700070927           when *in40 and TBLflg <> *blanks;
038800070926            iter;
038900070926          // selezione per "utilizzo chiusura distinta"
039000081029          // 29//10--> ES non si deve usare TIP ma FTC per escludere
039100081029          //           l'uso dei codici di lasciato avviso
039200081029           when TB2Atip <> *blanks and TB2Atip <> §2Aftc;
039300070926            iter;
039400070926          // selezione descrizione in ordinamento x descrizione
039500070926           when WrkSort = *on and VC1des <> *blanks and
039600070926               %subst(§2Ades:1:%len(%trimr(VC1des))) < %trimr(VC1des);
039700070926            iter;
039800070926          endsl;
039900070924
040000070926       // scrittura subfile
040100070926          exsr sr_WrtS01;
040200070924
040300070926        ENDDO;
040400070924
040500070924       // fine caricamento subfile
040600070926        RrnLast = S01nrr;
040700070924
040800070927       // visualizzazione subfile SOLO SE ci sono dati
040900070927        *in30 = (S01nrr <= *zeros);
041000070924       // indicatore di sflend se scritto almeno un record
041100070927        *in32 = (S01nrr >  *zeros);
041200070924
041300070924       // posizionamento iniziale sul 1° record del sfl
041400070927        if S01nrr > *zeros;
041500070927          C01csr = 1;
041600070927        else;
041700070927          clear C01csr;
041800070927          clear C01rcd;
041900070927        endif;
042000070924
042100070926       // disattivazione opzione di caricamento subfile
042200070926        $InzS01 = *off;
042300070926
042400070926       // richiesta di posizionamento per codice o per descrizione
042500070926        *in41  = (WrkSort = *on);
042600070926
042700070926       // ordinamento dei dati selezionati per descrizione
042800070926       // SE  richiesta una selezione
042900070926        if WrkSort = *on;
043000070926          exsr sr_SortS01;
043100070926        endif;
043200070924
043300070924       endsr;
043400070924
043500070924       // ------------------------------------------------------------*
043600070924       // scrittura singolo record nel subfile                        *
043700070924       // ------------------------------------------------------------*
043800070924       begsr sr_WrtS01;
043900070926       //    ¯¯¯¯¯¯¯¯¯¯
044000070924
044100070924       // se non raggiunto limite massimo di caricamento
044200070924        if S01nrr < 9999;
044300070924       // imposto campi di subfile
044400070926          exsr sr_RieS01;
044500070924       // imposto numeratore righe e numero relativo di record
044600070926          S01nrr = S01nrr + 1;
044700070924       // scrivo subfile
044800070926          write TB2AS01;
044900070924        endif;
045000070924
045100070924       endsr;
045200070924
045300070924       // ------------------------------------------------------------*
045400070924       // impostazione campi del subfile                              *
045500070924       // ------------------------------------------------------------*
045600070924       begsr sr_RieS01;
045700070926       //    ¯¯¯¯¯¯¯¯¯¯
045800070924
045900070924        clear TB2AS01;
046000070924
046100070924        VS1cod = TBLkey;
046200070924        VS1des = §2Ades;
046300070924        if TBLflg <> *blanks;
046400070927          %subst(VS1des:%len(VS1des)-3) = '?A?';
046500070924        endif;
046600070924
046700070924       endsr;
046800070924
046900070924       // ------------------------------------------------------------*
047000070924       // gestione opzioni nel subfile                                *
047100070924       // ------------------------------------------------------------*
047200070924       begsr sr_OpzS01;
047300070926       //    ¯¯¯¯¯¯¯¯¯¯
047400070924
047500070926        *in50 = *off;
047600070924
047700070924       // lettura record variati nel subfile
047800070924        readc TB2AS01;
047900070924
048000070924       // se non trovo esco
048100070924        if %eof(TRTB2AD);
048200070926          leavesr;
048300070924        endif;
048400070924
048500070924        DOW NOT %eof(TRTB2AD);
048600070924
048700070924       // se è stata immessa un'opzione
048800070926          if VS1opz <> *zeros;
048900070924       // controllo se opzione valida
049000070926            xx = %lookup(VS1opz:$Opz);
049100070926            if xx = *zeros;
049200070926              C01rcd = S01nrr;
049300070926              C01csr = S01nrr;
049400070926              VC1msg = $Msg(01);
049500070926              *in28  = *on;
049600070926              *in33  = *on;
049700070926              *in50  = *on;
049800070926              update TB2AS01;
049900070926              leavesr;
050000070926            endif;
050100070924
050200070926            select;
050300070924       // ritorno dati selezionati
050400070926             when VS1opz = 1;
050500070926              TBLkut = 1;
050600070926              TBLcod = C_tab;
050700070926              TBLkey = VS1cod;
050800070926              chain %kds(KDStabel) TABEL;
050900070926              TB2Acod = VS1cod;
051000070926              if %found(TABEL00F);
051100070926                TB2Auni = TBLuni;
051200070926              endif;
051300070926              $Fine = *on;
051400070926              leavesr;
051500070927       // manutenzione
051600070927             when VS1opz = 2;
051700070927       // copia
051800070927             when VS1opz = 3;
051900070927       // annullamento
052000070927             when VS1opz = 4;
052100070927       // visualizzazione
052200070927             when VS1opz = 5;
052300070926            endsl;
052400070924
052500070924       // gestione del formato video
052600070926            C01rcd = S01nrr;
052700070926            C01csr = S01nrr;
052800070926            clear VS1opz;
052900070926            update TB2AS01;
053000070926          endif;
053100070924
053200070926        readc TB2AS01;
053300070924
053400070924        ENDDO;
053500070924
053600070924       endsr;
053700070924
053800070924       // ------------------------------------------------------------*
053900070926       // This subroutine sortsfl the subfile records.                *
054000070924       // ------------------------------------------------------------*
054100070924       begsr sr_SortS01;
054200070926       //    ¯¯¯¯¯¯¯¯¯¯¯
054300070927
054400070926        C01rcd = 1;
054500070924
054600070924       // Initialize the key fields to sort on. There is one extra
054700070924       // field not in the subfile -- Selected -- that is added to
054800070924       // the record. This field is used to place selected records
054900070924       // in front of nonselected records.
055000070926        clear QLGKL;
055100070926        clear QLGSKL;
055200070926        clear QLGSCB;
055300070926        clear QLGSCB00;
055400070924
055500070924       // Gestione della scelta ordinamento.
055600070926        SELECT;
055700070926
055800070926       // Ordinamento per codice.
055900070926         WHEN WrkSort = *off;
056000070926       // 1° campo chiave.
056100070926          QLGNBRK  = 1;
056200070926       // Unico campo ordinamento: il codice è
056300070926       //   a posizione 2, 3 byte, char, ascending.
056400070926          QLGSP    = 2;
056500070926          QLGSS    = %size(VS1cod);
056600070926          QLGDT    = Carattere;
056700070926          QLGSO    = Ascendente;
056800070926          QLGKL(1) = QLGSKL;
056900070924
057000070924       // Ordinamento per descrizione.
057100070926        WHEN WrkSort = *on;
057200070924       // 1° campo chiave.
057300070926          QLGNBRK  = 1;
057400070924       // Unico campo ordinamento: la descrizione è
057500070924       //   a posizone  5, 65 byte, char, ascending.
057600070926          QLGSP    = 5;
057700070926          QLGSS    = %size(VS1des);
057800070926          QLGDT    = Carattere;
057900070926          QLGSO    = Ascendente;
058000070926          QLGKL(1) = QLGSKL;
058100070924
058200070926        ENDSL;
058300070924
058400070924       // Load other sort parameters.
058500070926        QLGLB   = 80 + 16 * MaxKey;
058600070926        QLGRL   = %size(SflRcd) - 1;
058700070926        QLGRT   = 8;
058800070926        QLGOKL  = 80;
058900070926        QLGLKE  = 16;
059000070926        QLGLSS  = 290;
059100070924       // Initialize Sort I/O API fields.
059200070926        QLGRL00 = QLGRL;
059300070926        QLGRC00 = 1;
059400070926        clear QUSEI;
059500070926        QUSBPRV = %size(QUSEC);
059600070924       // First step - Initialize the sort routine.
059700070926        QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
059800070924       // Next step - Write records to I/O routine.
059900070926        QLGRT00 = Put;
060000070924
060100070926        for xx = 1 to RrnLast;
060200070926          chain xx TB2AS01;
060300070926          // Solo le righe con Selected = 'Y' sono riordinate,
060400070926          // quindi per fare un ordinamento di tutte le righe
060500070926          // metto 'Y' sempre.
060600070926          Selected = 'Y';
060700070926          clear QUSEI;
060800070926          QUSBPRV = %size(QUSEC);
060900070926          QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
061000070926        endfor;
061100070924
061200070924       // Next step - Signal end of input, clear subfile for reload.
061300070926        QLGRT00 = EndPut;
061400070926        clear QUSEI;
061500070926        QUSBPRV = %size(QUSEC);
061600070924
061700070926        QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
061800070924
061900070924       // inizializzo il subfile
062000070926        S01nrr = *zeros;
062100070927        *in30 = *on;
062200070927        *in31 = *on;
062300070926        write TB2AC01;
062400070927        *in31 = *off;
062500070927        *in33 = *off;
062600070924
062700070924       // Final step - Write the records back to the subfile.
062800070926        QLGRT00 = Get;
062900070926        for xx = 1 to RrnLast;
063000070926          clear QUSEI;
063100070926          QUSBPRV = %size(QUSEC);
063200070926          QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
063300070926          S01nrr = xx;
063400070926          write TB2AS01;
063500070926        endfor;
063600070924
063700070927       // visualizzazione subfile SOLO SE ci sono dati
063800070927        *in30 = (S01nrr <= *zeros);
063900070927       // indicatore di sflend se scritto almeno un record
064000070927        *in32 = (S01nrr >  *zeros);
064100070927
064200070927       // posizionamento iniziale sul 1° record del sfl
064300070927        if S01nrr > *zeros;
064400070927          C01csr = 1;
064500070927        else;
064600070927          clear C01csr;
064700070927          clear C01rcd;
064800070927        endif;
064900070924
065000070924       endsr;
065100070924
065200070924       // ------------------------------------------------------------*
065300070924       // uscita dal programma                                        *
065400070924       // ------------------------------------------------------------*
065500070926       begsr sr_Uscita;
065600070926       //    ¯¯¯¯¯¯¯¯¯¯
065700070924
065800070926        kpjbu = TRTB2Ads;
065900070924
066000070926        *inLR = *on;
066100070926        return;
066200070924
066300070926       endsr;
066400070924
066500070924      /end-free
066600070924
066700070924** - $MSG -------------------------------------------------------------------*
066800070924Opzione errata                                                                 1
