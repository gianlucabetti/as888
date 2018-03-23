000100160503      *PARMS DBGVIEW(*SOURCE)
000200100903     /*PRM dbgview(*source)
000300100903     /*END
000400070313      *---------------------------------------------------------------*
000500070313      *?STAMPA LDV DA DISTINTE AUTOTRASPORTATORI - BATCH             ?*
000600070313      *---------------------------------------------------------------*
000700070313
000800070313     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000900070313
001000070313      *---------------------------------------------------------------*
001100070313
001200070320     fFIAR401L  Uf   e           k disk
001300080903     fFIDSF01L  Uf A e           k disk
001400070627     fFIDST01L  Uf   e           k disk
001500070731     f                                     infds(InfDs_FIDST)
001600070313
001700070313      *---------------------------------------------------------------*
001800070313
001900070313      *
002000070313      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - -?*
002100070313      *
002200071015     d C_OrderBy_1     c                   const(' ORDER BY DSTnfv')
002300070313     d C_OrderBy_2     c                   const(' ORDER BY TR9pep, +
002400070313     d                                                      TR9bai, +
002500070313     d                                                      TR9pdc, +
002600071015     d                                                      TR9nfv')
002700071015     d C_OrderBy_3     c                   const(' ORDER BY DSTpdr, +
002800071015     d                                                      DSTnfv')
002900070313     d C_ForFetch      c                   const(' FOR FETCH ONLY')
003000070731     d C_Msg_01        c                   const('Uscire SUBITO dalla -
003100070827     d                                     distinta &NrDst, di cui si -
003200070829     d                                     stanno per stampare le LdV!-
003300070829     d                                     ')
003400070313      *
003500070313      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - -?*
003600070313      *
003700070313      * - Stringhe SQL da eseguire
003800160505     d $Sql1           s             45    dim(21) ctdata  perrcd(1)
003900081001     d $Sql2           s             40    dim( 9) ctdata  perrcd(1)
004000080714       // - Spedizioni non stampate in LdV nel 1° ciclo perchè A4
004100080808     d $SpA4           s                   like(ds_Sped)   dim(2000)
004200080808     d                                     inz
004300130307       // - Numeratori (chiavi elenchi) dei PDF da stampare alla fine
004400130307     d $Klist          s             15    dim(999)  inz
004500070313      *
004600070313      *?  D S   - - - - - - - - - - - - - - - - - - - - - - - - - - -?*
004700070313      *
004800070313      * - Parametri
004900070313     d KPJBA         e ds
005000070313     d FNLRS9ds      e ds                  inz
005100070313      *
005200070313      * - Parametri x Controllo profilo utenti
005300070313     d TIBS34ds      e ds                  inz
005400070313      * - Ds di riferimento al file esterno AZUTE00F
005500070313     d AZUTEds       e ds                       extname(AZUTE00F)
005600070313      * - Ds per dati organigramma
005700070313     d dDatiUte      e ds
005800070914      *
005900070914      * - Parametri x ricerca/controllo tabelle
006000070914     d TIBS02ds      e ds                  inz
006100070914      * - Tab. VPO = Variabili Programmi Operativi
006200070914     d dVPO          e ds                  inz
006300070313      *
006400070313      * - Parametri x Stampa LdV
006500070313     d FNLSB5ds      e ds                  inz
006600070313     d  DB0tbo       e                     inz('A')
006700101129     d  DB0fl4       e                     inz('D')
006800130307     d FNLSB6ds1     e ds                  inz
006900130307     d  DB6pdf       e                     inz('F')
007000070731      *
007100070731      * - Parametri x Invio msg per gestione allocazione record
007200070731     d TRUL82ds      e ds                  inz
007300070731     d  UL82§fil     e                     inz('FIDST00F  ')
007400070829     d  UL82§win     e                     inz('X')
007500070829     d  UL82§f5      e                     inz('N')
007600070731     d  UL82§f7      e                     inz('S')
007700070731     d  UL82§num     e                     inz(02)
007800070829     d  UL82§att     e                     inz(30)
007900100903       //
008000100903       // -?DS lettura campo DSTFLR del file FIDST00F?
008100100903     d dDSTflr       e ds                  inz
008200070320      *
008300070320      * - Estenzione per AR4NOT/BL4NOT tipo rec. "A"
008400070320     d dsBL4A        e ds                  inz
008500080808
008600080808       // - di comodo per la definizione dei campi di FNARB00F
008700080808     d FNARB_ds      e ds                  inz  extname(FNARB00F)
008800070313      *
008900070313     d Status         sds
009000070313     d  SDSpgm           *proc
009100070313     d  SDSprm           *parms
009200070731      *
009300070731     d InfDs_FIDST     ds
009400070731     d  ID_dstNRR            397    400i 0
009500070621      *
009600070621     d ds_Time         ds                  inz
009700070621     d  wTime                         6  0 inz
009800070621     d  wDate                         8  0 inz
009900080808
010000080714      // - Singola spedizione in $SpA4
010100080808     d ds_Sped         ds                  inz
010200080714     d   dsSp_AAS                          inz  like(ARBaas)
010300080714     d   dsSp_LNP                          inz  like(ARBlnp)
010400080714     d   dsSp_NRS                          inz  like(ARBnrs)
010500080714     d   dsSp_NSP                          inz  like(ARBnsp)
010600080808
010700080808      // - Ds di comodo per la definizione dei campi estratti via SQL
010800080808      //   del file FIDST00F
010900080808     d FIDSTds         ds                  inz
011000080808     d   sqlDSTnpg                         inz  like(DSTnpg)
011100080808     d   sqlDSTnfv                         inz  like(DSTnfv)
011200080808     d   sqlDSTfgs                         inz  like(DSTfgs)
011300080808
011400080808      // - Ds di comodo per la definizione dei campi estratti via SQL
011500080808      //   del file FNARB00F
011600080808     d FNARBds         ds                  inz
011700080808     d   sqlARBaas                         inz  like(ARBaas)
011800080808     d   sqlARBlnp                         inz  like(ARBlnp)
011900080808     d   sqlARBnrs                         inz  like(ARBnrs)
012000080808     d   sqlARBnsp                         inz  like(ARBnsp)
012100080916     d   sqlARBrsd                         inz  like(ARBrsd)
012200080808     d   sqlARBndc                         inz  like(ARBndc)
012300080808     d   sqlARBddc                         inz  like(ARBddc)
012400080916     d   sqlARBifp                         inz  like(ARBifp)
012500080916     d   sqlARBtmc                         inz  like(ARBtmc)
012600070313      *
012700070313      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
012800070313      *
012900070313      * - Flags booleani
013000080808     d $EoF1           s              1    inz(*off)
013100080808     d $EoF2           s              1    inz(*off)
013200070313     d $Err            s              1    inz(*off)
013300070827     d $FIDST          s              1    inz(*off)
013400080903     d $FIDSF          s               n   inz
013500080714       // - Indici di schiera
013600130307     d ww              s              3  0 inz
013700080714     d xx              s              5  0 inz
013800080714     d yy              s              5  0 inz
013900080808      * - Stringhe SQL da eseguire
014000080916     d wSQL_1          s            600    inz
014100080808     d wSQL_2          s            600    inz
014200070313      * - Campi di comodo
014300080820     d SAVndc          s                   inz         like(sqlARBndc)
014400080820     d SAVddc          s                   inz         like(sqlARBddc)
014500080916     d SAVifp          s                   inz(*loval) like(sqlARBifp)
014600080820     d WS9tpm          s                   inz         like(DS9tpm)
014700080714     d wCiclo          s              1  0 inz(1)
014800101207     d wCountA4        s              5  0 inz
014900101207     d wCountA5        s              5  0 inz
015000101207     d wCount4xA4      s              5  0 inz
015100070629     d wLdVxDis        s              5  0 inz
015200070621     d w0140           s             14  0 inz
015300080820     d wData_Iso       s               d   inz         datfmt(*iso)
015400070313      *
015500070313      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
015600070313      *
015700080903       // - File FIDSF01L
015800080903     d k04fidsf01    e ds                  extname(FIDSF01L:*key)
015900080903     d                                     prefix(k_)
016000080903     d                                     inz
016100071015      * - FIDST01L
016200071015     c     K03DST01      klist
016300080808     c                   kfld                    sqlDSTnpg
016400080808     c                   kfld                    sqlDSTnfv
016500080808     c                   kfld                    sqlDSTfgs
016600070320      *
016700070320      * - FIAR401L
016800070320     c     K05AR401      klist
016900080808     c                   kfld                    sqlARBaas
017000080808     c                   kfld                    sqlARBlnp
017100080808     c                   kfld                    sqlARBnrs
017200080808     c                   kfld                    sqlARBnsp
017300080808     c                   kfld                    AR4trc
017400070313
017500070313      *---------------------------------------------------------------*
017600070313      *?RIEPILOGO INDICATORI                                         ?*
017700070313      *---------------------------------------------------------------*
017800070313      * 10   - Comodo                                                 *
017900070313      *---------------------------------------------------------------*
018000070313
018100070313     c     *Entry        plist
018200070313     c                   parm                    KPJBA
018300070313     c                   movel     KPJBU         FNLRS9ds
018400070313      *
018500070313      * Elaborazione:
018600070313if  1c                   IF             DS9ndi <> *zeros
018700070313     c                             and  DS9ndi  = DS9ndf
018800070313      * - singola distinta
018900080808     c                   exsr      sr_Dist_Sing
019000070313x   1c                   ELSE
019100070313      * - più distinte
019200080808     c                   exsr      sr_Dist_Mult
019300070313e   1c                   ENDIF
019400070313      *
019500070313      * Fine
019600080903     c                   eval      *inLR = *on
019700070313      *
019800070313      *---------------------------------------------------------------*
019900070313      *?Operazioni iniziali                                          ?*
020000070313      *---------------------------------------------------------------*
020100070313     c     *InzSr        BEGSR
020200080808      /free
020300080808         // Impostazione opzioni SQL
020400080808         exec sql  set option  DynUsrPrf = *Owner,
020500080808                               CloSqlCsr = *EndMod;
020600080808      /end-free
020700070313      *
020800070313      * Reperimento dati job
020900070313     c     *dtaara       define    §azute        AZUTEds
021000070313     c     *dtaara       define    §datiute      dDATIUTE
021100070313      *
021200070313     c                   in(E)     *dtaara
021300070313     c                   IF        %ERROR or RSUT = *blanks
021400070313     c                   clear                   Tibs34Ds
021500070313     c                   call      'TIBS34R'
021600070313     c                   parm                    Tibs34Ds
021700070313     c                   in        *dtaara
021800070313     c                   ENDIF
021900070320      *
022000070320      * Impostazione dati fissi
022100070320     c                   eval      AR4trc   = 'A'
022200070621     c                   time                    w0140
022300070621     c                   movel     w0140         ds_Time
022400070621     c     *eur          move      wDate         wData_Iso
022500070621     c     *iso          move      wData_Iso     wDate
022600070313      *
022700070914      * Aggancio tabella "VPO" per reperimento data attivazione
022800070914      *   n° pagine limite in stampa LdV (per close/open PrtF)
022900070914     c                   clear                   dVPO
023000070914     c                   clear                   TIBS02ds
023100070914     c                   eval      T02tla  = 'L'
023200070914     c                   eval      T02mod  = 'C'
023300070914     c                   eval      T02sif  = KNSIF
023400070914     c                   eval      T02cod  = 'VPO'
023500070914     c                   movel(P)  'VPO'         T02ke1
023600070914     c                   call      'TIBS02R'
023700070914     c                   parm                    KPJBA
023800070914     c                   parm                    TIBS02ds
023900070914     c                   if        T02err  = *blanks
024000070914     c                   movel     T02uni        dVPO
024100070914     c                   else
024200070914     c                   eval      §VPOpagldv = 150
024300070914     c                   endif
024400070914      *
024500070313     c                   ENDSR
024600070313      *
024700070313      *---------------------------------------------------------------*
024800070313      *?Elaborazione singola distinta                                ?*
024900070313      *---------------------------------------------------------------*
025000080808     c     sr_Dist_Sing  BEGSR
025100070313      *
025200080808     c                   eval      sqlDSTnpg  = 4
025300080808     c                   eval      sqlDSTnfv  = DS9ndi
025400080808     c                   eval      sqlDSTfgs  = DS9fgs
025500071015     c     K03DST01      chain(n)  FIDST000
025600101021      /free
025700101021         if  %found(FIDST01L);
025800101021           dDSTflr = DSTflr;
025900101021         else;
026000101021           clear  dDSTflr;
026100101021         endif;
026200101021      /end-free
026300070621      *
026400070827      * SELEZIONE RECORD
026500070621sel 1c                   select
026600070621      * - record inesistente
026700071015w   1c                   when      NOT  %found(FIDST01L)
026800070621      * - record annullato
026900071015w   1c                   when      DSTatb  <> *blanks
027000070621      * - foglio già chiuso
027100071015     c                   when      DSTfcf   = 'S'
027200070621      * - data F.V./Distinta_Consegna diversa
027300071015     c                   when      DSTdfv  <> DS9ddc
027400071119      * - distinta già stampata
027500071119      *   (selezione valida solo per stampa di NON singola distinta)
027600071119     c***                when           DS9sms  = 'S'
027700071119     c***                          and  DSTfasd = 'STP'
027800070621      * - tipo zone consegna NON parcel
027900070621     c                   when           DS9tpm  = 'P'
028000071015     c                             and  DSTtpm <> *blanks
028100070621      * - tipo zone consegna NON messaggerie
028200070621     c                   when           DS9tpm  = 'M'
028300071015     c                             and  DSTtpm <> 'M'
028400070621      * - prestazione padroncino (Mattino/Pomeriggio) diversa
028500070621     c                   when           DS9fpp <> *blanks
028600071015     c                             and  DSTfpp <> DS9fpp
028700101008      * - filiale partita con PDA
028800101008      *   e non è stata richiesta la stampa distinte scaricate su PDA
028900101008      *   e la distinta (CONSEGNE o CONSEGNE-ORM) è su PDA
029000101008      *   e la distinta non è in TEST (né per le CONSEGNE né per CONSEGNE+ORM)
029100101008w   1c                   when      DS9pda    = 'N'    and
029200101008     c                             (DSTpda   = 'C'    or  DSTpda   = 'E')  and
029300101008     c                             (§DSTtstpda <> 'C' and §DSTtstpda <> 'E')
029400101008      * - filiale partita con PDA
029500101008      *   e è stata richiesta la stampa distinte scaricate su PDA
029600101008      *   e la distinta (CONSEGNE o CONSEGNE-ORM) NON è su PDA
029700101008      *     o la distinta è in TEST (per le CONSEGNE o per CONSEGNE+ORM)
029800101008w   1c                   when      DS9pda    = 'S'    and
029900101008     c                             ((DSTpda  <> 'C'   and DSTpda  <> 'E')  or
030000101008     c                              (§DSTtstpda = 'C' or  §DSTtstpda = 'E'))
030100070621      * - OK
030200070621x   1c                   other
030300070621     c                   exsr      sr_Elab
030400130307     c                   exsr      sr_PrtPDF
030500100903      *   - Richiamo del pgm. di stampa LdV per chiusura
030600080916     c                   clear                   FNLSB5ds
030700080916     c                   eval      DB0tla   = 'C'
030800080916     c                   call      DS9psl
030900080916     c                   parm                    FNLSB5ds
031000070621e   1c                   endsl
031100070313      *
031200070313     c                   ENDSR
031300070313      *
031400070313      *---------------------------------------------------------------*
031500070313      *?Elaborazione più distinte                                    ?*
031600070313      *---------------------------------------------------------------*
031700080808     c     sr_Dist_Mult  BEGSR
031800070313      *
031900080808      * Preparazione stringa SQL sul file FIDST00F
032000080808     c                   exsr      PrepSQL_1
032100070313      *
032200070313      * Ciclo di elaborazione
032300080808     c                   exsr      OpenCursor_1
032400070313      *
032500080808do  1c                   dou       $Eof1    = *on
032600080808     c                   exsr      ReadCursor_1
032700070313e   1c                   enddo
032800130307      *
032900130307     c                   exsr      sr_PrtPDF
033000070313      *
033100080808     c                   exsr      CloseCursor_1
033200070313      *
033300070313     c                   ENDSR
033400070313      *
033500070313      *---------------------------------------------------------------*
033600080808      *?Preparazione stringa SQL sul file FIDST00F                   ?*
033700070313      *---------------------------------------------------------------*
033800080808     c     PrepSQL_1     BEGSR
033900070621      *
034000070621      * Impostazione flag "Tipo Zone Consegna" da usare via SQL
034100070621if  1c                   if        DS9tpm  = 'P'
034200070621     c                   eval      WS9tpm  = *blanks
034300070621x   1c                   else
034400070621     c                   eval      WS9tpm  = DS9tpm
034500070621e   1c                   endif
034600070313      *
034700070313      * Impostazione parametri ricevuti come parzializzazioni
034800070313      *   nella stringa SQL
034900070313      * - filiale gestione
035000080808     c                   eval      $Sql1(10) = %trimr( $Sql1(10) ) + ' '
035100080808     c                                       + %char(DS9fgs)
035200070313      * - data distinta
035300080808     c                   eval      $Sql1(11) = %trimr( $Sql1(11) ) + ' '
035400080808     c                                       + %char(DS9ddc)
035500070313      * - numero distinta dal - al
035600080808     c                   eval      $Sql1(12) = %trimr( $Sql1(12) ) + ' '
035700080808     c                                       + %char(DS9ndi)  + ' and '
035800080808     c                                       + %char(DS9ndf)
035900070314      * - flag prestazione padroncino
036000080808if  1c                   if        DS9fpp   <> *blanks
036100080808     c                   eval      $Sql1(13) = %trimr( $Sql1(13) )
036200080808     c                                       + DS9fpp + ''''
036300070314e   1c                   endif
036400070621      * - flag tipo zone consegna
036500080808if  1c                   if        DS9tpm   <> *blanks
036600080808     c                   eval      $Sql1(14) = %trimr( $Sql1(14) )
036700080808     c                                       + WS9tpm + ''''
036800070621e   1c                   endif
036900070313      *
037000070313      *? E S E M P I O : ?
037100070313      *
037200070619      * SELECT *                                          1
037300071015      *  FROM  FIDST00F                                   2
037400070313      *  left outer join FLTR900F                         3
037500071015      *  ON    DSTfgs = TR9fgs                            4
037600071015      *   and  DSTnpg = TR9npg                            5
037700071015      *   and  DSTnfv = TR9nfv                            6
037800071015      *   and  DSTdfv = TR9dfv                            7
037900071015      *  WHERE DSTatb <> 'A'  and  DSTfcf <> 'S'          8
038000071015      *   and  DSTnpg = 4                                 9
038100071119      *   and  DSTfasd= '   '                            15
038200071015      *   and  DSTfgs = :DS9fgs                          10
038300071015      *   and  DSTdfv = :DS9ddc                          11
038400071015      *   and  DSTnfv between :DS9ndi   and :DS9ndf      12
038500071015      *   and  DSTfpp = :DS9fpp                          13
038600070313      * ORDER BY ...
038700070313      * FOR   FETCH ONLY
038800070313      *
038900070313      *? N.B. ?
039000070313      * Se richiesto l'ordinamento x baia picking: vengono ricercati
039100071015      *   i dati in FLTR900F, ma i record di FIDST che non hanno
039200070314      *   corrispondenza in FLTR9 vengono comunque estratti !
039300070314      *   ...Anche se?PROBABILMENTE?per
ULTIMI?!
039400071015      * Comunque, da FIDST vengono selezionati:
039500071015      * · F.V. non annullati         (DSTATB <> 'A')
039600071015      * · F.V. non chiusi            (DSTFCF <> 'S')
039700071015      * · F.V. di categoria consegne (DSTNPG = 4)
039800070313      *
039900080808     c                   clear                   wSQL_1
040000070313      *
040100080808     c                   eval      wSQL_1   = %trimr($Sql1( 1))
040200080808     c                                      + %trimr($Sql1( 2))
040300071015if  1c                   if        DS9ord   = '2'
040400080808     c                   eval      wSQL_1   = %trimr(wSQL_1)
040500080808     c                                      + %trimr($Sql1( 3))
040600080808     c                                      + %trimr($Sql1( 4))
040700080808     c                                      + %trimr($Sql1( 5))
040800080808     c                                      + %trimr($Sql1( 6))
040900080808     c                                      + %trimr($Sql1( 7))
041000071015e   1c                   endif
041100080808     c                   eval      wSQL_1   = %trimr(wSQL_1)
041200080808     c                                      + %trimr($Sql1( 8))
041300080808     c                                      + %trimr($Sql1( 9))
041400080808     c                                      + %trimr($Sql1(10))
041500080808     c                                      + %trimr($Sql1(11))
041600080808     c                                      + %trimr($Sql1(12))
041700070314if  1c                   if        DS9fpp  <> *blanks
041800080808     c                   eval      wSQL_1   = %trimr(wSQL_1)
041900080808     c                                      + %trimr($Sql1(13))
042000070314e   1c                   endif
042100070621if  1c                   if        DS9tpm  <> *blanks
042200080808     c                   eval      wSQL_1   = %trimr(wSQL_1)
042300080808     c                                      + %trimr($Sql1(14))
042400070621e   1c                   endif
042500071119if  1c                   if        DS9sms   = 'S'
042600080808     c                   eval      wSQL_1   = %trimr(wSQL_1)
042700080808     c                                      + %trimr($Sql1(15))
042800071119e   1c                   endif
042900101008      /free
043000160503         // -?Selezionato almeno un turno da stampare?
043100160503         //  ?(per Tipo Distinta Parcel)?
043200160503         if  DS9tpm <> 'M'  and  ( DS9turn1 <> *blank  or
043300160503                                   DS9turn2 <> *blank );
043400160505           select;
043500160505             when  DS9turn1 <> *blank  and  DS9turn2 <> *blank;
043600160505               wSQL_1 = %trimr(wSQL_1)
043700160505                      + %trimr($Sql1(21));
043800160505             when  DS9turn1 = '1'  or  DS9turn2 = '1';
043900160505               wSQL_1 = %trimr(wSQL_1)
044000160505                      + %trimr($Sql1(20));
044100160505             when  DS9turn1 = '2'  or  DS9turn2 = '2';
044200160505               wSQL_1 = %trimr(wSQL_1)
044300160505                      + %trimr(%replace('''2''':$Sql1(20):
044400160505                                  %scan(''' ''':$Sql1(20))));
044500160505           endsl;
044600160503         endif;
044700160503
044800101008         select;
044900101008           // -?Filiale senza procedure PDA consegne?
045000101008           when  DS9pda = *blank;
045100101008           // -?Stampa solo distinte NON scaricate su PDA?
045200101008           when  DS9pda = 'N';
045300101008             wSQL_1 = %trimr(wSQL_1)
045400101008                    + %trimr($Sql1(16)) + ' '
045500101008                    + %trim($Sql1(17));
045600101008           // -?Stampa solo distinte scaricate su PDA?
045700101008           when  DS9pda = 'S';
045800101008             wSQL_1 = %trimr(wSQL_1)
045900101008                    + %trimr($Sql1(18)) + ' '
046000101008                    + %trim($Sql1(19));
046100101008         endsl;
046200101008      /end-free
046300101008      *
046400071015sel 1c                   select
046500071015w   1c                   when      DS9ord   = '1'
046600080808     c                   eval      wSQL_1   = %trimr(wSQL_1)
046700071015     c                                      + C_OrderBy_1
046800071015w   1c                   when      DS9ord   = '2'
046900080808     c                   eval      wSQL_1   = %trimr(wSQL_1)
047000070313     c                                      + C_OrderBy_2
047100071015w   1c                   when      DS9ord   = '3'
047200080808     c                   eval      wSQL_1   = %trimr(wSQL_1)
047300071015     c                                      + C_OrderBy_3
047400071015e   1c                   endsl
047500080808     c                   eval      wSQL_1   = %trimr(wSQL_1)
047600070313     c                                      + C_ForFetch
047700070313      *
047800070313     c                   ENDSR
047900070313      *
048000070313      *---------------------------------------------------------------*
048100080808      *?Apertura cursore - file FIDST00F                             ?*
048200070313      *---------------------------------------------------------------*
048300080808     c     OpenCursor_1  BEGSR
048400070313      *
048500070313      * Preparazione stringa SQL da eseguire
048600070313      *
048700070313     c/exec sql
048800080808     c+     prepare S1 from :wSQL_1
048900070313     c/end-exec
049000070313      *
049100070313      * Dichiarazione cursore
049200070313      *
049300070313     c/exec sql
049400070313     c+     declare C1 cursor for S1
049500070313     c/end-exec
049600070313      *
049700070313      * Apertura del cursore
049800070313      *
049900070313     c/exec sql
050000070313     c+     open C1
050100070313     c/end-exec
050200080808      *
050300080808     c                   reset                   $EoF1
050400070313      *
050500070313     c                   ENDSR
050600070313      *
050700070313      *---------------------------------------------------------------*
050800080808      *?Chiusura cursore - file FIDST00F                             ?*
050900070313      *---------------------------------------------------------------*
051000080808     c     CloseCursor_1 BEGSR
051100070313      *
051200100903      * Richiamo del pgm. di stampa LdV per chiusura
051300070313     c                   clear                   FNLSB5ds
051400070313     c                   eval      DB0tla   = 'C'
051500070320     c                   call      DS9psl
051600070313     c                   parm                    FNLSB5ds
051700070313      *
051800070313     c/exec sql
051900070313     c+     close C1
052000070313     c/end-exec
052100070313      *
052200070313     c                   ENDSR
052300070313      *
052400070313      *---------------------------------------------------------------*
052500080808      *?Lettura cursore - file FIDST00F                              ?*
052600070313      *---------------------------------------------------------------*
052700080808     c     ReadCursor_1  BEGSR
052800070313      *
052900071015     c                   clear                   FIDSTds
053000070313      *
053100070313     c/exec sql
053200071015     c+     fetch next from C1 into :FIDSTds
053300070313     c/end-exec
053400070313      *
053500070313sel 1c                   select
053600070313w   1c                   when      SQLcod   = 100
053700080808     c                   eval      $EoF1    = *on
053800070313w   1c                   when      SQLcod   < *zeros
053900070313     c                   eval      $Err     = *on
054000080808     c                   eval      $Eof1    = *on
054100070313x   1c                   other
054200070313     c                   exsr      sr_Elab
054300070313e   1c                   endsl
054400070313      *
054500070313     c                   ENDSR
054600070313      *
054700070313      *---------------------------------------------------------------*
054800071015      *?Elaborazione singolo record da FIDST00F                      ?*
054900070313      *---------------------------------------------------------------*
055000070313     c     sr_Elab       BEGSR
055100070629      *
055200070629     c                   clear                   wLdVxDis
055300070827      *
055400071015      * Aggancio distinta consegna (di cui aggiornare la fase)
055500071015      * e verifica eventuale allocazione da parte di altri lavori
055600070827     c                   exsr      Rep_FIDST
055700070313      *
055800100903      /free
055900100903
056000100903         // -?Ciclo di reperimento spedizioni?
056100100903         //  ?(per stampa LdV ex-A5)?
056200080808
056300080808         wCiclo = 1;
056400080808         clear  xx;
056500080808         clear  $SpA4;
056600080808
056700100903         // -?Preparazione stringa SQL?
056800080808         exsr  PrepSQL_2;
056900080808
057000100903         // -?Ciclo di elaborazione?
057100080808         exsr  OpenCursor_2;
057200080808
057300130402         dou  $EoF2 = *on;
057400080808           exsr  ReadCursor_2;
057500080808         enddo;
057600080808
057700080808         exsr  CloseCursor_2;
057800080808
057900100903         // -?Elaborazione spedizioni "singole" in A4?
058000100903         //  ?(scartate dal ciclo precedente delle sole LdV ex-A5)?
058100080808
058200080808         IF  xx > *zero;
058300080808
058400080808           wCiclo = 2;
058500080808           For  yy = 1  by 1  to xx;
058600080808
058700080808             ds_Sped = $SpA4(yy);
058800080808             sqlARBaas = dsSp_AAS;
058900080808             sqlARBlnp = dsSp_LNP;
059000080808             sqlARBnrs = dsSp_NRS;
059100080808             sqlARBnsp = dsSp_NSP;
059200080808             sqlARBifp = DSTfgs;
059300080808             sqlARBndc = DSTnfv;
059400080808             sqlARBddc = DSTdfv;
059500080808             exsr  Stampa_LdV;
059600080808
059700080808           EndFor;
059800080808
059900080808         ENDIF;
060000080808
060100080714      /end-free
060200070627      *
060300070627      * Aggiornamento fase della distinta consegna
060400070629      *   (se stampata almeno una LdV della distinta)
060500070914if  1c                   if            wLdVxDis > *zeros
060600070828     c                             and  $FIDST  = *on
060700070627     c                   exsr      Upd_FIDST
060800070914x   1c                   else
060900070827     c                   UNLOCK    FIDST01L
061000070914e   1c                   endif
061100070313      *
061200070313     c                   ENDSR
061300080808      /free
061400080808
061500130307       //--------------------------------------------------------------
061600130307       //?Preparazione stringa SQL sul file FNARB00F                   ?
061700130307       //--------------------------------------------------------------
061800080808       BEGSR  PrepSQL_2;
061900080808
062000080808         clear  wSQL_2;
062100080808
062200080808         // Costruzione stringa SQL con parametri di selezione
062300080808         wSQL_2 = %trim($Sql2( 1)) + ' '
062400080808                + %trim($Sql2( 2)) + ' '
062500080808                + %trim($Sql2( 3)) + ' '
062600080916                + %trim($Sql2( 4)) + ' '
062700080916                + %trim($Sql2( 5)) + ' ' + %char(DSTfgs) + ' '
062800080916                + %trim($Sql2( 6)) + ' ' + %char(DSTnfv) + ' '
062900080916                + %trim($Sql2( 7)) + ' ' + %char(DSTdfv) + ' '
063000081001                + %trim($Sql2( 8)) + ' '
063100081001                + %trim($Sql2( 9));
063200080808
063300080808       ENDSR;
063400080808
063500080808       //--------------------------------------------------------------
063600130307       //?Apertura cursore - file FNARB00F                             ?
063700080808       //--------------------------------------------------------------
063800130307       BEGSR  OpenCursor_2;
063900080808
064000080808         // Preparazione stringa SQL da eseguire
064100080808         exec sql   prepare S2   from :wSQL_2;
064200080808
064300080808         // Dichiarazione cursore
064400080808         exec sql   declare C2   cursor for S2;
064500080808
064600080808         // Apertura del cursore
064700080808         exec sql   open C2;
064800080808
064900080808         reset  $EoF2;
065000080808
065100080808       ENDSR;
065200080808
065300080808       //--------------------------------------------------------------
065400130307       //?Chiusura cursore - file FNARB00F                             ?
065500080808       //--------------------------------------------------------------
065600080808       BEGSR  CloseCursor_2;
065700080808
065800080808         exec sql   close C2;
065900080808
066000080808       ENDSR;
066100080808
066200080808       //--------------------------------------------------------------
066300130307       //?Lettura cursore - file FNARB00F                              ?
066400080808       //--------------------------------------------------------------
066500080808       BEGSR  ReadCursor_2;
066600080808
066700080808         clear  FNARBds;
066800080808
066900080808         exec sql   fetch next   from C2   into :FNARBds;
067000080808
067100080808         select;
067200080808           when  SQLcod = 100;
067300080808             $EoF2 = *on;
067400080808           when  SQLcod < *zero;
067500080808             $Err  = *on;
067600080808             $EoF2 = *on;
067700080808           other;
067800080808             exsr  Stampa_LdV;
067900080808         endsl;
068000080808
068100080808       ENDSR;
068200080808
068300080808      /end-free
068400070313      *
068500070313      *---------------------------------------------------------------*
068600100903      *?Richiamo del pgm. di stampa LdV                              ?*
068700070313      *---------------------------------------------------------------*
068800080714     c     Stampa_LdV    BEGSR
068900070313      *
069000070320      * Reperimento rec. di FIAR401L per aggiornamento data stampa LdV
069100070320     c     K05AR401      chain     FIAR4000
069200070320if  1c                   if        %found(FIAR401L)
069300070320     c                   movel     AR4not        dsBL4A
069400070320x   1c                   else
069500070320     c                   clear                   dsBL4A
069600070320e   1c                   endif
069700070320      *
069800070320      * Stampa LdV
069900070313     c                   reset                   FNLSB5ds
070000080916      /free
070100100903         // -? PRIMA: ?
070200100903         //if wCiclo = 1;
070300100903         //  DB0fa4 = '2';
070400100903         //else;
070500100903         //  DB0fa4 = '1';
070600100903         //endif;
070700100903
070800100903         //? Casistiche & parametri:                                   ?
070900100903         //? 1° ciclo   &   Abilitazione al PDA (non in test)          ?
071000100903         // =>?FNLSB6R stampa 4 "ricevute" in A4?
071100100903         //    (stampando sempre e comunque;
071200100903         //     se si riceve la segnalazione di una pck-lst rimanente,
071300100903         //     la sped. verrà memorizzata per poter essere elaborata
071400100903         //     in un ciclo successivo)
071500100903         // Si passa  DB0FA4 = "4".
071600100927         // Si riceve DB0TMS = "Q" (se non c'è una pck-lst da stampare);
071700100903         //           DB0TMS = "D" (se rimane packing-list da stampare).
071800130422         //           DB0TMS = "i" (se rimane PDF da stampare).
071900130422         //           DB0TMS = "Y" (se rimangono pck-lst+PDF da stamp.).
072000100903         //? 2° ciclo   &   Abilitazione al PDA (non in test)          ?
072100100903         // =>?FNLSB6R stampa 2 packing-list in A4?
072200100903         //    (per le packing-list NON stampate durante il 1° ciclo)
072300100903         // Si passa  DB0FA4 = "P".
072400100903         // Si riceve DB0TMS = "P" (se stampata la packing-list);
072500100903         //           DB0TMS = " " (se non trovata la packing-list).
072600130422         //? 3° ciclo   &   Abilitazione al PDA (non in test)          ?
072700130422         // =>?FNLSB6R stampa 1 PDF in A4?
072800130422         //    (per tutti i .PDF - SE ce ne sono - alla fine di tutto)
072900100903         //? 1° ciclo   &   NON abilitazione al PDA                    ?
073000100903         // =>?FNLSB6R stampa 2 LdV in A4?
073100100903         //    (stampando solo ex-A5   e
073200100903         //     segnalando se la spedizione richiede A4:
073300100903         //     essa verrà memorizzata per poter essere elaborata in un
073400100903         //     ciclo successivo)
073500100903         // Si passa  DB0FA4 = "2".
073600100903         // Si riceve DB0TMS = "5" (se stampata LdV ex-A5);
073700130422         //           DB0TMS = "D" (se richiede A4).
073800130422         //           DB0TMS = "I" (se rimane PDF da stampare).
073900100903         //? 2° ciclo   &   NON abilitazione al PDA                    ?
074000100903         // =>?FNLSB6R stampa 1 LdV in A4?
074100100903         //    (saltando sempre all'inizio dell'A4 ssuccessivo)
074200100903         // Si passa  DB0FA4 = "1".
074300100903         // Si riceve DB0TMS = "5" (se stampata LdV ex-A5);
074400100903         //           DB0TMS = "4" (se stampata LdV A4).
074500130422         //           DB0TMS = "J" (se rimane PDF da stampare).
074600100903
074700100903         SELECT;
074800100903
074900100903           // -?Stampa di 4 LdV x A4?
075000100903           WHEN (DSTpda = 'C'      or  DSTpda = 'E')       and
075100100903                (§DSTtstPDA <> 'C' and §DSTtstPDA <> 'E')  and
075200100903                 wCiclo = 1;
075300100903             DB0fa4 = '4';
075400100903
075500100903           // -?Stampa di 2 packing-list x A4?
075600100903           WHEN (DSTpda = 'C'      or  DSTpda = 'E')       and
075700100903                (§DSTtstPDA <> 'C' and §DSTtstPDA <> 'E')  and
075800100903                 wCiclo = 2;
075900100903             DB0fa4 = 'P';
076000100903
076100100903           // -?Stampa di 2 LdV x A4?
076200100903           WHEN  wCiclo = 1;
076300100903             DB0fa4 = '2';
076400100903
076500100903           // -?Stampa di 1 LdV x A4  (con packing-list)?
076600100903           OTHER;
076700100903             DB0fa4 = '1';
076800100903
076900100903         ENDSL;
077000080916      /end-free
077100080916     c*** già così:      eval      DB0tbo   = 'A'
077200080808     c                   eval      DB0aas   = sqlARBaas
077300080808     c                   eval      DB0lnp   = sqlARBlnp
077400080808     c                   eval      DB0nrs   = sqlARBnrs
077500080808     c                   eval      DB0nsp   = sqlARBnsp
077600081010
077700080916      /free
077800100903                //? ASTERISCATA IN ATTESA DI CONFERMA ?
077900081010       // x le spedizioni packing list inserite in distinte con PDA consegne
078000081010       // dal 2° tentativo di consegna non bisogna stampare il packing list
078100100903         ////if  sqlARBtmc <> *blank and distinta PDA consegne;
078200100903         //if  sqlARBtmc  <> *blank                      and
078300100903         //   (DSTpda     =  'C'  or  DSTpda = 'E')      and
078400100903         //   (§DSTtstPDA <> 'C' and §DSTtstPDA <> 'E')
078500081010         //  DB0fl1 = 'D';
078600081010         //endif;
078700080916      /end-free
078800081010
078900080916      * Test "rottura" distinta consegna
079000080808if  1c                   IF             sqlARBifp <> SAVifp
079100080808     c                             or   sqlARBndc <> SAVndc
079200080808     c                             or   sqlARBddc <> SAVddc
079300080715      /free
079400080916           DB0fl6 = *on;
079500080916
079600080916           IF  DS9psl = 'FNLSB6R   ';
079700080916             if  (wCountA4 + (wCountA5 / 2) + (wCount4xA4 / 4))
079800080916                                          >= (§VPOpagLdV - 20);
079900080916               DB0fl7     = *on;
080000080916               wCountA4   = *zero;
080100080916               wCountA5   = *zero;
080200080916               wCount4xA4 = *zero;
080300080916             endif;
080400080916           ELSE;
080500080718      /end-free
080600070914if  2c                   if        wCountA4 >= §VPOpagldv
080700070313     c                   eval      DB0fl7   = *on
080800070316     c                   clear                   wCountA4
080900070313e   2c                   endif
081000070914if  2c                   if        wCountA5 >= §VPOpagldv
081100070313     c                   eval      DB0fl8   = *on
081200070316     c                   clear                   wCountA5
081300070313e   2c                   endif
081400080718      /free
081500080916           ENDIF;
081600080718      /end-free
081700070529e   1c                   ENDIF
081800070313      *
081900070320     c                   call      DS9psl
082000070313     c                   parm                    FNLSB5ds
082100130307     c                   parm                    FNLSB6ds1
082200070313      *
082300070313      * Conteggio delle pagine finora stampate
082400080714      * ...e delle LdV stampate per Distinta
082500100903sel 1c*//                select
082600100903w   1c*//                when      DB0tms   = '4'
082700100903     c*//                add       1             wCountA4
082800100903     c*//                add       1             wLdVxDis
082900100903w   1c*//                when      DB0tms   = '5'
083000100903     c*//                add       1             wCountA5
083100100903     c*//                add       1             wLdVxDis
083200100903x   1c*//                other
083300100903     c*//                exsr      sr_MemoSped
083400100903e   1c*//                endsl
083500080902      /free
083600100903
083700100903         // -?Conteggio delle LdV stampate per Distinta?
083800100903         SELECT;
083900100903
084000100903           // -?Stampato foglio A4 intero?
084100130307           WHEN  DB0tms = '4'  or  DB0tms = 'J';
084200100903             wCountA4   += 1;
084300100903             wLdVxDis   += 1;
084400100903
084500100903           // -?Stampato foglio ex-A5 (2 per A4)?
084600130307           WHEN  DB0tms = '5'  or  DB0tms = 'I';
084700100903             wCountA5   += 1;
084800100903             wLdVxDis   += 1;
084900100903
085000100927           // -?Stampata "tagliatella" (4 per A4)?
085100130307           WHEN  DB0tms = 'Q'  or  DB0tms = 'i';
085200100903             wCount4xA4 += 1;
085300100903             wLdVxDis   += 1;
085400130307           // -?Stampata "tagliatella" (4 per A4), ma rimane packing-list?
085500130307           WHEN  (DB0tms = 'D'  or  DB0tms = 'Y')  and  DB0fa4 = '4';
085600100903             wCount4xA4 += 1;
085700100903             //wLdVxDis += 1;   (NON conteggia; conteggerà nel ciclo "P")
085800100906             if  wCiclo = 1;
085900100906               exsr  sr_MemoSped;
086000100906             endif;
086100100903
086200130307           // -?Stampata packing-list (2 pag. per A4, ma salto pag. x?
086300130307           //  ?ogni packing-list - vedi *pgm FNLSB6R)?
086400100903           WHEN  DB0tms = 'P';
086500130307             //wCountA5   += 1;
086600130307             wCountA4   += 1;
086700100903             wLdVxDis   += 1;
086800100903
086900100903           // -?Memorizza il n° della spedizione NON stampata?
087000130307           //  ?- richiesto A5, ma in realtà è da A4 -?
087100100903           //  ?(IMPOSSIBILE succeda al 2° giro...)?
087200130307           //OTHER;
087300130307           WHEN  DB0tms = 'D'  and  DB0fa4 = '2';
087400100903             if  wCiclo = 1;
087500100903               exsr  sr_MemoSped;
087600100903             endif;
087700100903
087800100903         ENDSL;
087900130307
088000130307         // -?Memorizzazione gruppo di immagini PDF da stampare DOPO?
088100130307         If  (DB0tms = 'i'  or  DB0tms = 'I'  or
088200130402              DB0tms = 'J'  or  DB0tms = 'Y');
088300130307           exsr  sr_MemoKlist;
088400130307         EndIf;
088500100903
088600100903         // -?Memorizza il num. distinta x il controllo del salto pagina?
088700100903         //  ?(a rottura di distinta)   SE   stampata la LdV e/o la?
088800100903         //  ?packing-list?
088900100903         if  (DB0tms    =  '4'     or
089000100903              DB0tms    =  '5'     or
089100130422              DB0tms    =  'D'     or          //?tagliatella + pck-lst?
089200130422              DB0tms    =  'i'     or          //?tagliatella + PDF?
089300130422              DB0tms    =  'Y'     or          //?tagliatella + pck-lst + PDF?
089400130422              DB0tms    =  'I'     or          //?A4 + PDF?
089500130422              DB0tms    =  'J'     or          //?A5 + PDF?
089600130422              DB0tms    =  'Q'     or          //?tagliatella?
089700130422              DB0tms    =  'P')    AND         //?pck-lst?
089800080916             (sqlARBifp <> SAVifp  or
089900080916              sqlARBndc <> SAVndc  or
090000080916              sqlARBddc <> SAVddc);
090100080916           SAVifp = sqlARBifp;
090200080916           SAVndc = sqlARBndc;
090300080916           SAVddc = sqlARBddc;
090400080916         endif;
090500080902      /end-free
090600070320      *
090700070320      * Aggiornamento data stampa LdV
090800080714if  1c                   if        %found(FIAR401L)
090900080714if  2c                   if             DB0tms = '4'
091000080714     c                              or  DB0tms = '5'
091100100927     c***                           or  DB0tms = 'Q'
091200100903     c***                           or  DB0tms = 'P'
091300100903     c                              or  DB0fa4 = '4'
091400070320     c                   z-add     wDate         §B4asl
091500070420      * aggiorno flag stampa DDT se bolla mai stampata in arrivo
091600070420     c                   select
091700071015     c                   when      §B4abm = 'S'
091800071015     c                   eval      §B4abm = 'P'
091900071015     c                   when      §B4abm = 'C'
092000071015     c                   eval      §B4abm = 'Q'
092100071015     c                   when      §B4abm = 'K'
092200071015     c                   eval      §B4abm = 'W'
092300071015     c                   when      §B4abm = 'N' or §B4abm = 'Y'
092400071015     c                   eval      §B4abm = 'J'
092500070420     c                   endsl
092600070320     c                   movel     dsBL4A        AR4not
092700070320      *                  __________________
092800070320     c                   update    FIAR4000
092900070320      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
093000080714x   2c                   else
093100080714     c                   unlock    FIAR401L
093200080714e   2c                   endif
093300070320e   1c                   endif
093400070313      *
093500070313     c                   ENDSR
093600080714      /free
093700130307
093800080714       //--------------------------------------------------------------
093900130307       //?Memorizzazione spedizioni A4 non stampate                    ?
094000130307       //?  (da stampare in un ciclo successivo)                       ?
094100080714       //--------------------------------------------------------------
094200080714       BegSr  sr_MemoSped;
094300080714
094400080715         clear  ds_Sped;
094500080808         dsSp_AAS = sqlARBaas;
094600080808         dsSp_LNP = sqlARBlnp;
094700080808         dsSp_NRS = sqlARBnrs;
094800080808         dsSp_NSP = sqlARBnsp;
094900080715
095000080714         xx += 1;
095100080714         $SpA4(xx) = ds_Sped;
095200080714
095300080714       EndSr;
095400130307
095500130307       //--------------------------------------------------------------
095600130307       //?Memorizzazione numeratore (chiave elenco) per PDF            ?
095700130307       //?  (da stampare alla fine)                                    ?
095800130307       //--------------------------------------------------------------
095900130307       BegSr  sr_MemoKlist;
096000130307
096100130307         select;
096200130307
096300130307           // -?Nessun PDF allegato?
096400130307           when  DB6num = *blank  or  DB6num = *zero;
096500130307
096600130307           // -?1° PDF allegato?
096700130307           when  ww = *zero;
096800130307             ww  = 1;
096900130307             $Klist(ww) = DB6num;
097000130307
097100130307           // -?Nuova chiave elenco per ultimo PDF allegato?
097200130307           when  %lookup( DB6num : $Klist ) = *zero;
097300130307             ww += 1;
097400130307             $Klist(ww) = DB6num;
097500130307
097600130307         endsl;
097700130307
097800130307       EndSr;
097900130307
098000080714      /end-free
098100070827      *
098200070827      *---------------------------------------------------------------*
098300070827      *?Reperimento fase attuale della Distinta Consegna in FIDST00F ?*
098400070827      *---------------------------------------------------------------*
098500070827     c     Rep_FIDST     BEGSR
098600070827      *
098700070827     c                   eval      $FIDST   = *off
098800080903      /free
098900080903         $FIDSF = *off;
099000080903      /end-free
099100070827      *
099200071015      * Aggancio del record SE NON richiesta una sola distinta
099300071015      *   (che altrimenti è già stata agganciata)
099400071015if  1c                   if             DS9ndi  = *zeros
099500071015     c                             or   DS9ndi <> DS9ndf
099600070827     c     K03DST01      chain(n)  FIDST000
099700071015e   1c                   endif
099800100903      /free
099900100903         dDSTflr = DSTflr;
100000100903      /end-free
100100070827      *
100200070827sel 1c                   select
100300071015      * (IMPOSSIBILE che il record sia non reperito, o annullato,
100400071015      *    o già chiuso !!!)
100500070827w   1c                   when      NOT  %found(FIDST01L)
100600070827     c                   leavesr
100700071015w   1c***                when      DSTatb  <> *blanks
100800071015     c***                leavesr
100900070830      * SE le LdV risultano già stampate: NON si aggiorna il rec.
101000070827w   1c                   when      DSTfasd =  'STP'
101100070827     c                   leavesr
101200070827e   1c                   endsl
101300070827      *
101400071015      * "Allocazione" record di FIDST01L per aggiornamento fase in
101500070830      *   "Stampate LdV"
101600070827     c     K03DST01      chain(e)  FIDST000
101700070827      *
101800070827sel 1c                   SELECT
101900070830      * - Record allocato da altro lavoro
102000070830w   1c                   WHEN      %error
102100070830     c                   exsr      Ges_Lck
102200070830     c     K03DST01      chain     FIDST000
102300070827      * - Record aggiornabile
102400070829w   1c                   WHEN      %found(FIDST01L)
102500070827      * - Record non trovato (IMPOSSIBILE...!)
102600070827w   1c                   WHEN      NOT  %found(FIDST01L)
102700070827     c                   leavesr
102800070827e   1c                   ENDSL
102900070827      *
103000070827     c                   eval      $FIDST   = *on
103100080903      *
103200080903      /free
103300080903         // Reperimento eventuale record dal file FIDSF00F
103400080903         //   (in cui registrare la data/ora della 1ª stampa)
103500080903         k_DSFnpg = sqlDSTnpg;        // già "4"
103600080903         k_DSFnfv = sqlDSTnfv;
103700080903         k_DSFfgs = sqlDSTfgs;
103800080903         k_DSFtrd = 'STP';
103900080903         chain(n)  %kds(k04fidsf01)  FIDSF000;
104000080903
104100080903         select;
104200080903           // record non reperito => da aggiungere
104300080903           when  NOT  %found(FIDSF01L);
104400080903             $FIDSF = *on;
104500080903           // record annullato => da rispristinare ed aggiornare
104600080903           when  DSFatb <> *blank;
104700080903             $FIDSF = *off;
104800080903           // SE le LdV risultano già stampate: NON si aggiorna il rec.
104900080903           when  DSTfasd =  'STP';
105000080903             leavesr;
105100080903         endsl;
105200080903
105300080903         // "Allocazione" record di FIDSF01L per aggiornamento fase in
105400080903         //   "Stampate LdV"
105500080903         if  DSFatb <> *blank;
105600080903           chain  %kds(k04fidsf01)  FIDSF000;
105700080903         endif;
105800080903
105900080903         $FIDSF = *on;
106000080903
106100080903       ENDSR;
106200080903      /end-free
106300070627      *
106400070627      *---------------------------------------------------------------*
106500070627      *?Aggiornamento fase della Distinta Consegna in FIDST00F       ?*
106600070627      *---------------------------------------------------------------*
106700070627     c     Upd_FIDST     BEGSR
106800070731      *
106900070731      * Aggiornamento fase come "Stampate LdV"
107000070627     c                   eval      DSTfasd  = 'STP'
107100070627      *                  __________________
107200070627     c                   UPDATE    FIDST000
107300080903      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
107400080903      /free
107500080903         if  $FIDSF = *on;
107600080903           exsr  Upd_FIDSF;
107700080903         endif;
107800080903      /end-free
107900080903     c                   ENDSR
108000080903      *
108100080903      /free
108200130307
108300080903       //--------------------------------------------------------------
108400130307       //?Aggiornamento data/ora aggiornamento distinta uscita AUT     ?
108500080903       //--------------------------------------------------------------
108600080903       BEGSR  Upd_FIDSF;
108700080903
108800080903         //?Impostazione campi nel record?
108900080903
109000080903         if  NOT  %found(FIDSF01L);
109100080903           clear  FIDSF000;
109200080903           DSFfgs = sqlDSTfgs;
109300080903           DSFnpg = sqlDSTnpg;        // già "4"
109400080903           DSFnfv = sqlDSTnfv;
109500080903           DSFtrd = 'STP';
109600080903         endif;
109700080903
109800080903         DSFdtorin = %int( %subst( %char( %dec( %timestamp() ) )
109900080903                                 :1 :14 ) );
110000080903         DSFpdaas4 = 'A';
110100080903
110200080903         //?Scrittura / Aggiornamento?
110300080903
110400080903         if  %found(FIDSF01L);
110500080903         //________________
110600080903           UPDATE  FIDSF000;
110700080903         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
110800080903         else;
110900080903         //________________
111000080903           WRITE   FIDSF000;
111100080903         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
111200080903         endif;
111300080903
111400080903       ENDSR;
111500130307
111600130307       //--------------------------------------------------------------
111700130307       //?Stampa di TUTTI i documenti PDF allegati alle varie LdV      ?
111800130307       //--------------------------------------------------------------
111900130307       BEGSR  sr_PrtPDF;
112000130307
112100130307         IF  ww > *zero;
112200130307
112300130307           wCiclo = 3;
112400130307
112500130307           For  yy = 1  by 1  to ww;
112600130307
112700130307             reset  FNLSB5ds;
112800130307             DB0fa4 = 'I';
112900130307             DB0fl7 = *on;
113000130307
113100130307             clear  FNLSB6ds1;
113200130307             DB6pdf = 'S';
113300130307             DB6num = $Klist(yy);
113400130307
113500130307      /end-free
113600130307     c                   call      DS9psl
113700130307     c                   parm                    FNLSB5ds
113800130307     c                   parm                    FNLSB6ds1
113900130307      /free
114000130307
114100130307           EndFor;
114200130307
114300130307         ENDIF;
114400130307
114500130307       ENDSR;
114600130307
114700080903      /end-free
114800070731      *
114900070731      *---------------------------------------------------------------*
115000070731      *?Gestione record già allocato del file FIDST00F               ?*
115100070731      *---------------------------------------------------------------*
115200070731     c     Ges_Lck       BEGSR
115300070731      *
115400070731     c                   reset                   TRUL82ds
115500070731     c                   eval      UL82§rrn = ID_dstNRR
115600070731     c                   eval      UL82§mss = %replace(
115700070731     c                                          %editc(DSTnfv:'Z')
115800070731     c                                        : C_Msg_01
115900070731     c                                        : %scan( '&NrDst'
116000070731     c                                          : C_Msg_01 ))
116100070830     c                   eval      §UL82txt2= 'Questo lavoro sta blocc+
116200070830     c                                         cando la stampa delle L+
116300070830     c                                         dV dalla distinta n° '
116400070830     c                                      + %trim( %editc(
116500070830     c                                         DSTnfv : 'Z' ) )
116600070830     c                                      + ': TERMINARE !'
116700070731      * Effettuo la chiamata al *PGM d utilità
116800070731     c                   call(e)   'TRUL82R'
116900070731     c                   parm                    TRUL82ds
117000070830     c                   parm                    §UL82txt2       120
117100070731      *
117200080808     c                   ENDSR
117300080818
117400080818      *---------------------------------------------------------------*
117500080818
117600101008** - $Sql1 ---------------------------------*
117700080808SELECT DSTnpg, DSTnfv, DSTfgs                     1
117800071015 FROM  FIDST00F                                   2
117900070313 left outer join FLTR900F                         3
118000071015 ON    DSTfgs = TR9fgs                            4
118100071015  and  DSTnpg = TR9npg                            5
118200071015  and  DSTnfv = TR9nfv                            6
118300071015  and  DSTdfv = TR9dfv                            7
118400071015 WHERE DSTatb <> 'A' and DSTfcf <> 'S'            8
118500071015  and  DSTnpg = 4                                 9
118600071015  and  DSTfgs =                                  10
118700071015  and  DSTdfv =                                  11
118800071015  and  DSTnfv between                            12
118900071015  and  DSTfpp = '                                13
119000071015  and  DSTtpm = '                                14
119100071119  and  DSTfasd= '   '                            15
119200101008  and (DSTpda NOT in ('C', 'E') or               16
119300101008      substr(DSTflr, 1, 1) in ('C', 'E'))        17
119400101008  and (DSTpda in ('C', 'E') and                  18
119500101008      substr(DSTflr, 1, 1) NOT in ('C', 'E'))    19
119600160505  and substr(DSTflr, 2, 1) = ' '                 20
119700160505  and substr(DSTflr, 2, 1) in (' ', '2')         21
119800080808** - $Sql2 ----------------------------*
119900080808select ARBaas, ARBlnp, ARBnrs, ARBnsp,            1
120000080916       ARBrsd, ARBndc, ARBddc, ARBifp,            2
120100080916       ARBtmc                                     3
120200080916from   FNARB00F                                   4
120300080916where  ARBifp =                                   5
120400080916  and  ARBndc =                                   6
120500080916  and  ARBddc =                                   7
120600081001order  by  ARBifp,ARBddc,ARBndc, ARBrsd,          8
120700081001       ARBaas,ARBlnp,ARBnrs,ARBnsp                9
