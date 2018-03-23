000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300041015     ffiqdt03l  uf   e           k disk
000400041015     ffnfvv01l  if   e           k disk
000500041015     ffiftd03l  if   e           k disk
000600041015     ffnarb01l  if   e           k disk
000700041020     ffnlbl01l  if   e           k disk
000800051114     ffiar901l  if   e           k disk
000900041022     ffiar601l  if   e           k disk
001000041015     ffnart01l  if   e           k disk
001100070123     ffnbrv01l  if   e           k disk
001200041015     ftabel00f  if   e           k disk
001300041020     ftntbe01l  if   e           k disk
001400041015     ffiqdc00f  o    e           k disk
001500110301     FFIar501L  IF   E           K DISK
001600030522      *--------------------------------------------------------------------
001700020626     d kpjba         e ds
001800110308     d dqdtflo       e ds
001900030321     d ds3a          e ds
002000110308     D dar5ban       E DS                  inz
002100110308     D dar5bnb       E DS                  inz
002200041020     d dcra          e ds
002300030321     d i               s              3  0 inz
002400030321     d sk3a            s              2    dim(50)
002500041015     d vuoto           s              2
002600041020     D dataiso         S               d   datfmt(*iso)
002700041020     D datalimite      S              8  0
002800041020     D*
002900041020     D WLBDAT          DS
003000041020     D  G02DAT                 1      8  0
003100041020     D  G02INV                 9     16  0
003200041020     D  G02ERR                17     17
003300041020     D  G02TGI                18     22  0
003400000607      *--------------------------------------------------------------------
003500041015      *loop sui record di qdt ancora da trattare
003600041015     c     *loval        setll     fiqdt03l
003700041015     c                   do        *hival
003800041015     c                   read      fiqdt03l
003900041020      *CHIUDE x EoF oppure data maggiore rispetto alla data limite impostata
004000041020      * UDATE - xx giorni da tabella CRA(consegne)
004100041020
004200041020     c                   if        %Eof(fiqdt03l) or qdtddc > datalimite
004300041020     c                   leave
004400041020     c                   endif
004500030414      *
004600041015      * A rottura di distinta
004700041015     c                   z-add     qdtnfv        wnfv
004800041015     c                   z-add     qdtfgs        wfgs
004900041015     c     kfvv          chain     fnfvv01l
005000050209     c                   if        %found(fnfvv01l)
005100050209     c                   if        fvvfcf = 'S'
005200041019      * ora e data aggiornamento consegne
005300041019     c                   z-add     dateu8        QDTDAECO
005400041019     c                   z-add     hhmm          QDTORACO
005500041015     C                   exsr      Cal_dis
005600041020      * aggiorna fiqdt letto ad inizio loop
005700110308     c                   move      wnban         §qdtnbdar
005800110308     c                   movel     dqdtflo       qdtflo
005900041020     c                   update    fiqdt000
006000050209     c                   else
006100050209      * se il rekord di fvv è aperto ma annullato aggiorno fiqdt come elaborato
006200050209     c                   if        fvvatb <> *blank
006300050209     c                   z-add     dateu8        QDTDAECO
006400050209     c                   z-add     hhmm          QDTORACO
006500050310     c                   move      *all'9'       QDTddc
006600050209      * aggiorna fiqdt letto ad inizio loop
006700050209     c                   update    fiqdt000
006800050209     c                   endif
006900050209     c                   endif
007000041015     c                   endif
007100030414      *
007200041015     c                   enddo
007300030414      *                 * ------------------ *
007400030414     c                   seton                                        lr
007500030414     c*==================================================================*
007600041018      *  Calcola i valori da riportare per distinta
007700030414     c*==================================================================*
007800041015     c     Cal_dis       begsr
007900110407      * ------------------ *
008000110407      * -    Azzera il totalizzatore x singola Distinta
008100110407     c                   clear                   wnban
008200030415      * ------------------ *
008300041015     c                   z-add     qdtnfv        wndc
008400041015      * leggo il dettaglio dei conteggi per valorizzare il rekord        .
008500041015      * principale e reperire i dati di consegna
008600041015     c     kftd          setll     fiftd03l
008700041015     c                   do        *hival
008800041015     c     kftd          reade     fiftd03l
008900041015     c                   if        %eof(fiftd03l)
009000041015     c                   leave
009100041015     c                   endif
009200041015      * leggo la bolla in arrivo per le informazioni generali sulla bolla
009300041015      * AFFIDATI
009400041015     c     karb          chain     fnarb01l
009500041015     c                   if        %found(fnarb01l)
009600041018      * Verifico se è una bolla in assegnato per eventuali dati          .
009700041018      * di fatturazione
009800041018     c                   clear                   Bolla_ass         1
0099000410182    c                   if        arbcbo <> *blank
010000041018     c     arbcbo        lookup    sk3a                                   91
010100041018     c   91              move      'S'           Bolla_ass
0102000410182-   c                   endif
010300041102      * testo su tabella 3A se la bolla è di recupero nel caso non deve
010400041102      * sommare le quantità ma come per i solo incassi mantenere solo
010500041102      * i valori economici
010600041102     c                   movel     arbcbo        tblkey
010700041102     c                   clear                   ds3a
010800041102     c     ktab2         chain     tabel00f
010900041102     c                   if        %found(tabel00f) and tblflg = *blank
011000041102     c                   movel     tbluni        ds3a
011100041102     c                   endif
011200041018      *
011300041018     c                   if        bolla_ass = 'S'
011400041018     c                   exsr      lett_ar6
011500041018     c                   endif
011600110301      * routine per calcolo dei bancali rileva bancali  B + O
011700110301     c                   clear                   nban              2 0
011800110301     c                   exsr      srnban
011900041018
012000041018      * Verifico se la bolla ha dei legami devo leggere l'originale per
012100041018      * reperire le informazioni relative al C/A e al dettaglio colli
012200041020     c     kbol          chain     fnlbl01l
012300041025     c                   if        %found(fnlbl01l) and lbllan = lbllap
012400041018      * con legame reperisco dati da bolla originale
012500041018     c     karbl         chain     fnarb01l
012600041018      *lbl
012700041018     c                   endif
012800041018      * reperisco dati relativi al contrassegno se esiste
012900041018     c                   exsr      lett_ar9
013000041019      * controllo se esiste una consegna parziale precedente se esiste
013100041019      * devo scalare i colli precedentemente consegnati dai colli affidati
013200041019      *
013300041019     c                   clear                   parziale          1
013400041020     c                   if        arbdcp <> ftdddc and arbdcp > 0
013500041020     c                   move      'S'           parziale
013600041020     c                   endif
013700041019      * ricerco peso totale della spedizione
013800041019     c                   exsr      Det_pes
013900041019      * ricerco volume totale della spedizione
014000041019     c                   exsr      Det_vol
014100041019      * imposto da arb i colli totali della spedizione
014200041019     c                   z-add     arbncl        wcolpr            5 0
014300041019      * non consegna scrive riga relativamente alla bolla su file dettaglio
014400041019      * con motivo di mancata consegna e senza segnacollo
014500041019     C                   if        ftdtmc <> *blank
014600041019     c                   z-add     ftdfgs        qdcfgs
014700041019     c                   z-add     ftdndc        qdcnfv
014800041019     c                   z-add     ftdddc        qdcddc
014900041019     c                   z-add     ftdpdr        qdcpdr
015000041019     c                   z-add     ftdaas        qdcaas
015100041019     c                   z-add     ftdlnp        qdclnp
015200041019     c                   z-add     ftdnrs        qdcnrs
015300041019     c                   z-add     ftdnsp        qdcnsp
015400041019     c                   z-add     ftdncl        qdcncl
015500041019     c                   z-add     0             qdcfls
015600041019     c                   z-add     0             qdcnsc
015700041019     c                   z-add     0             qdclna
015800041019     c                   move      ftdcmc        qdccev
015900041019     c                   move      *blank        qdcnspri
016000041019     c                   move      *blank        qdcflo
016100041112     c                   move      arbtsp        qdctsp
016200041019     c                   write     fiqdc000
016300041019     c                   endif
016400041019
016500041018      * reperisco dati relativi al dettaglio colli
016600041018     c                   exsr      lett_art
016700041018      *arb
0168000410152-   c                   endif
016900041019      * prepara i valori della singola bolla da sommare sul record di
017000041019      * riepilogo della distinta
017100041019     c                   exsr      tot_bol
017200041019      *ftd
0173000410152-   c                   enddo
017400041015     c                   endsr
017500041018     c*==================================================================*
017600041018     c     lett_ar6      begsr
017700041018     c*==================================================================*
017800041018      * i campi della chiave sono relativi ad arb in questo caso sono
017900041018      * i valori della bolla che sto consegnado
018000041018
018100041018     c     kbol          chain     fiar601l
018200041018     c                   if        %found(fiar601l)
018300041018      * verifico se non ci sono stati precedenti incassi parziali per
018400041018      * l'assegnato che sto calcolando
018500041020     c                   z-add     ar6ift        war6ift          13 3
018600041018     c                   endif
018700041018     c                   endsr
018800041018     c*==================================================================*
018900041018     c     lett_ar9      begsr
019000041018     c*==================================================================*
019100041018      * i campi della chiave sono relativi ad arb
019200041018      * potrei avere in linea in caso di legame l'arb relativo alla bolla
019300041018      * mamma altrimenti i valori della bolla che sto consegnando
019400041018
019500051114     c     kbol          chain     fiar901l
019600051114     c                   if        %found(fiar901l)
019700041020     c                   z-add     ar9cas        war9cas          13 3
019800041018     c                   endif
019900041018     c                   endsr
020000041018     c*==================================================================*
020100041018     c     lett_art      begsr
020200041018     c*==================================================================*
020300041018     c     kbol          setll     fnart01l
020400041018     c                   do        *hival
020500041018     c     kbol          reade     fnart01l
020600041018     c                   if        %eof(fnart01l)
020700041018     c                   leave
020800041018     c                   endif
020900041019      * in caso di consegna parziale precedente:
021000041019      * deve scalare il numero dei colli di una o più precedenti consegne
021100041019      * dal totale della bolla per avere il numero reale degli affidati
021200041019      *
021300041019     C                   if        parziale = 'S'
021400041019     c                   if        artdcm > 0 and artdcm < ftdddc
021500041019     c                   sub       1             wcolpr
021600041019     c                   sub       artvuc        wvolpr
021700041019     c                   sub       artpuc        wpespr
021800041019     c                   endif
021900041019     c                   endif
022000041019      * non consegna scrive file dettaglio per segnacollo
022100041019     C                   if        ftdtmc <> *blank
022200041019     c                   exsr      det_nocons
022300041019     c                   endif
022400041019
022500041018     c                   enddo
022600041018     c                   endsr
022700041019     c*==================================================================*
022800041019     c     det_nocons    begsr
022900041019     c*==================================================================*
023000041019      * i campi della chiave sono relativi ad art NON CONSEGNA
023100041019      * nel caso in cui non trovo la spunta 8 di rientro relativa al collo
023200041019      * scrivo un record di dettaglio per averlo in evidenza
023300041019     c                   z-add     ftdndc        wbrvnfv
023400041124     c                   z-add     ftdfgs        wbrvfgs
023500070123     c     kbrv          chain     fnbrv01l
023600070123     c                   if        not %found(fnbrv01l)
023700041019     c                   z-add     ftdfgs        qdcfgs
023800041019     c                   z-add     ftdndc        qdcnfv
023900041019     c                   z-add     ftdddc        qdcddc
024000041019     c                   z-add     ftdpdr        qdcpdr
024100041019     c                   z-add     ftdaas        qdcaas
024200041019     c                   z-add     ftdlnp        qdclnp
024300041019     c                   z-add     ftdnrs        qdcnrs
024400041019     c                   z-add     ftdnsp        qdcnsp
024500041019     c                   z-add     ftdncl        qdcncl
024600041019     c                   z-add     artfls        qdcfls
024700041019     c                   z-add     artnsc        qdcnsc
024800041019     c                   z-add     artlna        qdclna
024900041019     c                   move      *blank        qdccev
025000041019     c                   move      *blank        qdcnspri
025100041019     c                   move      *blank        qdcflo
025200041019     c                   write     fiqdc000
025300041019      *aggiorno contatore non spuntati al rientro se non fatti
025400041019     c                   add       1             wcolnspu          5 0
025500041019     c                   endif
025600041019     c                   endsr
025700041019     c*==================================================================*
025800041019     c     tot_bol       begsr
025900041019     c*==================================================================*
026000041019      * solo incasso considero solo i valori relativamente al C/A o
026100041019      * all'importo dell'assegnato
026200041019
026300041019      * solo incasso
026400041102     c                   if        ftdsic = 'S' or §3atb1 = 'A3'
026500041102     c                             or §3atb1 = 'F3'
026600041019     c                   z-add     0             wcolpr
026700041019     c                   z-add     0             wpespr
026800041019     c                   z-add     0             wvolpr
026900041019     c                   z-add     0             wcolnspu
027000041019     c                   endif
027100041019      * mancata consegna
027200041019     c                   if        ftdtmc <> *blank
027300041019     c                   add       1             QDTNBLNC
027400041019     c                   add       wcolpr        QDTNCLNC
027500041019     c                   add       wpespr        QDTPKFNC
027600041019     c                   add       wvolpr        QDTVLFNC
027700041019     c                   add       wcolnspu      QDTNCLNS
027800041019     c                   add       war6ift       QDTIFTNC
027900041021     c                   if        ar9vca = 'EUR'
028000041019     c                   add       war9cas       QDTCASNC
028100041019     c                   else
028200041019     c                   add       war9cas       QDTCAENC
028300041019     c                   endif
028400041019     c                   endif
028500041019      * affidati
028600041019     c                   add       1             QDTNBLAF
028700110301     c                   add       nban          wnban             2 0
028800041019     c                   add       wcolpr        QDTNCLAF
028900041019     c                   add       wpespr        QDTPKFAF
029000041019     c                   add       wvolpr        QDTVLFAF
029100041019     c                   add       war6ift       QDTIFTAF
029200041019      * divisa C/A
029300041021     c                   if        ar9vca = 'EUR'
029400041019     c                   add       war9cas       QDTCASAF
029500041019     c                   else
029600041019     c                   add       war9cas       QDTCAEAF
029700041019     c                   endif
029800041020      * in caso di consegna normale verifico se tutti i colli sono stati
029900041020      * consegnati i numeri devono essere uguali fra FTD e i colli affidati
030000041020     c                   if        ftdtmc = *blank
030100041102      * se la bolla è una bolla di recupero non faccio le differenze fra
030200041102      * la bolla e i conteggi perchè la bolla risulta a 0 come quantità e i
030300041102      * conteggi sono comunque valorizzati
030400041102     c                   if        §3atb1 = 'A3' or §3atb1 = 'F3'
030500041102     c                   add       wcolpr        QDTNCLNC
030600041102     c                   add       wpespr        QDTPKFNC
030700041102     c                   add       wvolpr        QDTVLFNC
030800041102     c                   else
030900041102     c     wcolpr        sub       ftdncl        wcolpr
031000041102     c                   add       wcolpr        QDTNCLNC
031100041102     c     wpespr        sub       ftdpkl        wpespr
031200041102     c                   add       wpespr        QDTPKFNC
031300041102     c     wvolpr        sub       ftdvlu        wvolpr
031400041102     c                   add       wvolpr        QDTVLFNC
031500041102     c                   endif
031600041020      * x l'assegnato elimino dal totale l'importo dei conteggi e lo sommo
031700041020      * nel totale non incassato
031800041020     c     war6ift       sub       ftdifp        war6ift
031900041020     c                   add       war6ift       QDTIFTNF
032000041019      * consegne effettuate incassi non fatti
032100041020      * x il contrassegno o incasso tutto o niente quindi sommo il totale
032200041020     c                   if        ftdcas = 0 and war9cas > 0
032300041021     c                   if        ar9vca = 'EUR'
032400041019     c                   add       war9cas       QDTCASNF
032500041019     c                   else
032600041019     c                   add       war9cas       QDTCAENF
032700041019     c                   endif
032800041019     c                   endif
032900041020      * ftdtmc = ' '
033000041020     c                   endif
033100041019      * azzera campi di work
033200041019     c                   clear                   war6ift
033300041019     c                   clear                   war9cas
033400041019     c                   clear                   wcolpr
033500041019     c                   clear                   wpespr
033600041019     c                   clear                   wvolpr
033700041019     c                   clear                   wcolnspu
033800041019
033900041019     c                   endsr
034000041019     c*==================================================================*
034100041019     C     det_pes       begsr
034200041019     c*==================================================================*
034300041019      * se è considerata un'unica spedizione
034400050112     C     arbncp        ifeq      arbncl
034500041021     C                   z-add     arbpkc        wpespr            7 1
034600041019     C*
034700041019     C                   else
034800041019     C*
034900041019     C     arbpkc        ifge      arbpkf
035000041019     C                   z-add     arbpkc        wpespr
035100041019     C                   else
035200041019     C                   z-add     arbpkf        wpespr
035300041019     C                   end
035400041019     C*
035500041019     C                   endif
035600041019     C*
035700041019     C                   endsr
035800041019     c*==================================================================*
035900041019     C     det_vol       begsr
036000041019     c*==================================================================*
036100041019     C* VOLUME : - SE ARBFVR   ("T"), VOLUME ARBVOL
036200041019     C*            ALTRIMENTI
036300041019     C*               - SE TOTALE   ("T"), VOLUME ARBVLS
036400041019     C*               - SE PARZIALE ("Z"), VOLUME ARBVLS (SE MAGGIORE)
036500041019     C*                               ALTRIMENTI SOMMO ARBVOL
036600041019     C     arbfvf        ifeq      'T'
036700041021     C                   z-add     arbvlf        wvolpr            5 3
036800041019     C                   else
036900050112     C     arbncr        ifeq      arbncl
037000041019     C                   z-add     arbvlc        wvolpr
037100041019     C*
037200041019     C                   else
037300041019     C     arbvlc        ifge      arbvlf
037400041019     C                   z-add     arbvlc        wvolpr
037500041019     C                   else
037600041019     C                   z-add     arbvlf        wvolpr
037700041019     C                   endif
037800041019     C                   endif
037900041019     C                   endif
038000041019     C*
038100041019     C                   endsr
038200110301      *----------------------------------------------------------------
038300110301     C     srnban        BEGSR
038400110301      *----------------------------------------------------------------
038500110301      * Bancali
038600110308     c                   If        %Subst(Arbgva:1:1) = 'B'
038700110308     c                   Clear                   dAr5Ban
038800110308     C                   move      'BAN'         ar5trd
038900110308     c     kFiar5        Chain     Fiar501l
039000110308     c                   If        %Found(Fiar501l)
039100110308     c                   Movel     Ar5Uni        dAr5Ban
039200110308     c                   EndIf
039300110308     c                   add       §Ar5Nba       nban
039400110308     c                   add       §ar5Nb2       nban
039500110308     c                   EndIf
039600110301      * Bancali Bartolini
039700110301     c                   If        %Subst(Arbgva:1:1) = 'O'
039800110308     c                   Clear                   dAr5BNB
039900110308     C                   move      'BNB'         ar5trd
040000110301     c     kFiar5        Chain     Fiar501l
040100110301     c                   If        %Found(Fiar501l)
040200110301     c                   Movel     Ar5Uni        dAr5BNB
040300110301     c                   EndIf
040400110301     c                   add       §Ar5bNba      nban
040500110308     c                   add       §ar5bNb2      nban
040600110308     c                   EndIf
040700110308     c                   endsr
040800030414     c*==================================================================*
040900030414     c     *inzsr        begsr
041000030414     c*==================================================================*
041100020626     c     *entry        plist
041200020626     c                   parm                    kpjba
041300030414      *
041400041020
041500041020     C                   time                    w0140            14 0
041600041020     C                   move      w0140         udate8            8 0
041700041020     C                   movel     W0140         hhmm              4 0          *ORA/MINUTI
041800041020     C*
041900041020     C                   z-add     udate8        g02dat                         *GG/MM/AAAA
042000041020     C                   move      *blank        g02err
042100041020     C                   call      'XSRDA8'
042200041020     C                   parm                    wlbdat
042300041020     C                   z-add     g02inv        dateu8            8 0          *AAAA/MM/GG
042400030414      *
042500030414     c     kftd          klist
042600041015     c                   kfld                    qdtpdr
042700041015     c                   kfld                    wtsr              1
042800041015     c                   kfld                    wndc              7 0
042900041015     c                   kfld                    qdtddc
043000041015     c                   move      'C'           wtsr
043100041015      *
043200041015     c     karb          klist
043300041015     c                   kfld                    ftdaas
043400041015     c                   kfld                    ftdlnp
043500041015     c                   kfld                    ftdnrs
043600041015     c                   kfld                    ftdnsp
043700030417      *
043800041018     c     karbl         klist
043900041018     c                   kfld                    lblaao
044000041018     c                   kfld                    lbllpo
044100041018     c                   kfld                    lblnro
044200041018     c                   kfld                    lblnso
044300041018      *
044400041015     c     kfvv          klist
044500041015     c                   kfld                    wnpg              1 0
044600041015     c                   kfld                    wnfv              5 0
044700041015     c                   kfld                    wfgs              3 0
044800041015     c                   z-add     4             wnpg
044900041018      *
045000041018     c     kbol          klist
045100041018     c                   kfld                    arbaas
045200041018     c                   kfld                    arblnp
045300041018     c                   kfld                    arbnrs
045400041018     c                   kfld                    arbnsp
045500041018      *
045600041019     c     kbrv          klist
045700041019     c                   kfld                    wnpgbrv
045800041019     c                   kfld                    wbrvnfv           6 0
045900041124     c                   kfld                    wbrvfgs           3 0
046000050315     c                   kfld                    artfls
046100041019     c                   kfld                    artlna
046200041019     c                   kfld                    artnrs
046300041019     c                   kfld                    artnsc
046400041019     c                   z-add     8             wnpgbrv           1 0
046500041020      *
046600041020     c     ktbe          klist
046700041020     c                   kfld                    tbecod
046800041020     c                   kfld                    tbeke1
046900041020     c                   kfld                    tbeke2
047000041020     c                   move      'CRA'         tbecod
047100041020     c                   movel     'DFT'         tbeke1
047200110301      *
047300110301     C     Kfiar5        KLIST
047400110301     C                   KFLD                    arbaas
047500110301     C                   KFLD                    arblnp
047600110301     C                   KFLD                    arbnrs
047700110301     C                   KFLD                    arbnsp
047800110301     C                   KFLD                    ar5trd
047900030321      *
048000030321     c     ktab          klist
048100030321     c                   kfld                    tblkut
048200030321     c                   kfld                    tblcod
048300030321     c                   z-add     1             tblkut
048400030321     c                   move      '3A'          tblcod
048500041102     c     ktab2         klist
048600041102     c                   kfld                    tblkut
048700041102     c                   kfld                    tblcod
048800041102     c                   kfld                    tblkey
048900041020      *leggo tabella per impostare limiti di lancio del periodo da elaborare
049000041020     c     ktbe          chain     tntbe01l                           93
049100041020     c                   if        not *in93 and tbeatb = *blank
049200041020     c                   movel     tbeuni        dcra
049300041020     c                   endif
049400041020     c                   move      dateu8        dataiso
049500041020     c     dataiso       subdur    d§craco:*d    dataiso
049600041020     c                   move      dataiso       datalimite
049700030321      *
049800041015      *carico in skiera i codici bolla relativamente agli assegnati
049900030321     c     ktab          setll     tabel00f
050000030321     c                   do        *hival
050100030321     c     ktab          reade     tabel00f                               93
050200030321     c   93              leave
050300030321     c                   if        tblflg <> *blank
050400030321     c                   iter
050500030321     c                   end
050600030321     c                   movel     tbluni        ds3a
050700041015     c                   if        %subst(§3atb1:1:1) <> 'A'
050800030321     c                   iter
050900030321     c                   end
051000030321     c                   z-add     1             i
051100030321     c     vuoto         lookup    sk3a(i)                                92
051200030321     c   92              movel     tblkey        sk3a(i)
051300030321     c                   enddo
051400020521     c                   endsr
