000100070313      *PARMS CLOSQLCSR(*ENDMOD) DYNUSRPRF(*OWNER) DBGVIEW(*SOURCE)
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
003800101008     d $Sql1           s             45    dim(19) ctdata  perrcd(1)
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
043000101008         select;
043100101008           // -?Filiale senza procedure PDA consegne?
043200101008           when  DS9pda = *blank;
043300101008           // -?Stampa solo distinte NON scaricate su PDA?
043400101008           when  DS9pda = 'N';
043500101008             wSQL_1 = %trimr(wSQL_1)
043600101008                    + %trimr($Sql1(16)) + ' '
043700101008                    + %trim($Sql1(17));
043800101008           // -?Stampa solo distinte scaricate su PDA?
043900101008           when  DS9pda = 'S';
044000101008             wSQL_1 = %trimr(wSQL_1)
044100101008                    + %trimr($Sql1(18)) + ' '
044200101008                    + %trim($Sql1(19));
044300101008         endsl;
044400101008      /end-free
044500101008      *
044600071015sel 1c                   select
044700071015w   1c                   when      DS9ord   = '1'
044800080808     c                   eval      wSQL_1   = %trimr(wSQL_1)
044900071015     c                                      + C_OrderBy_1
045000071015w   1c                   when      DS9ord   = '2'
045100080808     c                   eval      wSQL_1   = %trimr(wSQL_1)
045200070313     c                                      + C_OrderBy_2
045300071015w   1c                   when      DS9ord   = '3'
045400080808     c                   eval      wSQL_1   = %trimr(wSQL_1)
045500071015     c                                      + C_OrderBy_3
045600071015e   1c                   endsl
045700080808     c                   eval      wSQL_1   = %trimr(wSQL_1)
045800070313     c                                      + C_ForFetch
045900070313      *
046000070313     c                   ENDSR
046100070313      *
046200070313      *---------------------------------------------------------------*
046300080808      *?Apertura cursore - file FIDST00F                             ?*
046400070313      *---------------------------------------------------------------*
046500080808     c     OpenCursor_1  BEGSR
046600070313      *
046700070313      * Preparazione stringa SQL da eseguire
046800070313      *
046900070313     c/exec sql
047000080808     c+     prepare S1 from :wSQL_1
047100070313     c/end-exec
047200070313      *
047300070313      * Dichiarazione cursore
047400070313      *
047500070313     c/exec sql
047600070313     c+     declare C1 cursor for S1
047700070313     c/end-exec
047800070313      *
047900070313      * Apertura del cursore
048000070313      *
048100070313     c/exec sql
048200070313     c+     open C1
048300070313     c/end-exec
048400080808      *
048500080808     c                   reset                   $EoF1
048600070313      *
048700070313     c                   ENDSR
048800070313      *
048900070313      *---------------------------------------------------------------*
049000080808      *?Chiusura cursore - file FIDST00F                             ?*
049100070313      *---------------------------------------------------------------*
049200080808     c     CloseCursor_1 BEGSR
049300070313      *
049400100903      * Richiamo del pgm. di stampa LdV per chiusura
049500070313     c                   clear                   FNLSB5ds
049600070313     c                   eval      DB0tla   = 'C'
049700070320     c                   call      DS9psl
049800070313     c                   parm                    FNLSB5ds
049900070313      *
050000070313     c/exec sql
050100070313     c+     close C1
050200070313     c/end-exec
050300070313      *
050400070313     c                   ENDSR
050500070313      *
050600070313      *---------------------------------------------------------------*
050700080808      *?Lettura cursore - file FIDST00F                              ?*
050800070313      *---------------------------------------------------------------*
050900080808     c     ReadCursor_1  BEGSR
051000070313      *
051100071015     c                   clear                   FIDSTds
051200070313      *
051300070313     c/exec sql
051400071015     c+     fetch next from C1 into :FIDSTds
051500070313     c/end-exec
051600070313      *
051700070313sel 1c                   select
051800070313w   1c                   when      SQLcod   = 100
051900080808     c                   eval      $EoF1    = *on
052000070313w   1c                   when      SQLcod   < *zeros
052100070313     c                   eval      $Err     = *on
052200080808     c                   eval      $Eof1    = *on
052300070313x   1c                   other
052400070313     c                   exsr      sr_Elab
052500070313e   1c                   endsl
052600070313      *
052700070313     c                   ENDSR
052800070313      *
052900070313      *---------------------------------------------------------------*
053000071015      *?Elaborazione singolo record da FIDST00F                      ?*
053100070313      *---------------------------------------------------------------*
053200070313     c     sr_Elab       BEGSR
053300070629      *
053400070629     c                   clear                   wLdVxDis
053500070827      *
053600071015      * Aggancio distinta consegna (di cui aggiornare la fase)
053700071015      * e verifica eventuale allocazione da parte di altri lavori
053800070827     c                   exsr      Rep_FIDST
053900070313      *
054000100903      /free
054100100903
054200100903         // -?Ciclo di reperimento spedizioni?
054300100903         //  ?(per stampa LdV ex-A5)?
054400080808
054500080808         wCiclo = 1;
054600080808         clear  xx;
054700080808         clear  $SpA4;
054800080808
054900100903         // -?Preparazione stringa SQL?
055000080808         exsr  PrepSQL_2;
055100080808
055200100903         // -?Ciclo di elaborazione?
055300080808         exsr  OpenCursor_2;
055400080808
055500130402         dou  $EoF2 = *on;
055600080808           exsr  ReadCursor_2;
055700080808         enddo;
055800080808
055900080808         exsr  CloseCursor_2;
056000080808
056100100903         // -?Elaborazione spedizioni "singole" in A4?
056200100903         //  ?(scartate dal ciclo precedente delle sole LdV ex-A5)?
056300080808
056400080808         IF  xx > *zero;
056500080808
056600080808           wCiclo = 2;
056700080808           For  yy = 1  by 1  to xx;
056800080808
056900080808             ds_Sped = $SpA4(yy);
057000080808             sqlARBaas = dsSp_AAS;
057100080808             sqlARBlnp = dsSp_LNP;
057200080808             sqlARBnrs = dsSp_NRS;
057300080808             sqlARBnsp = dsSp_NSP;
057400080808             sqlARBifp = DSTfgs;
057500080808             sqlARBndc = DSTnfv;
057600080808             sqlARBddc = DSTdfv;
057700080808             exsr  Stampa_LdV;
057800080808
057900080808           EndFor;
058000080808
058100080808         ENDIF;
058200080808
058300080714      /end-free
058400070627      *
058500070627      * Aggiornamento fase della distinta consegna
058600070629      *   (se stampata almeno una LdV della distinta)
058700070914if  1c                   if            wLdVxDis > *zeros
058800070828     c                             and  $FIDST  = *on
058900070627     c                   exsr      Upd_FIDST
059000070914x   1c                   else
059100070827     c                   UNLOCK    FIDST01L
059200070914e   1c                   endif
059300070313      *
059400070313     c                   ENDSR
059500080808      /free
059600080808
059700130307       //--------------------------------------------------------------
059800130307       //?Preparazione stringa SQL sul file FNARB00F                   ?
059900130307       //--------------------------------------------------------------
060000080808       BEGSR  PrepSQL_2;
060100080808
060200080808         clear  wSQL_2;
060300080808
060400080808         // Costruzione stringa SQL con parametri di selezione
060500080808         wSQL_2 = %trim($Sql2( 1)) + ' '
060600080808                + %trim($Sql2( 2)) + ' '
060700080808                + %trim($Sql2( 3)) + ' '
060800080916                + %trim($Sql2( 4)) + ' '
060900080916                + %trim($Sql2( 5)) + ' ' + %char(DSTfgs) + ' '
061000080916                + %trim($Sql2( 6)) + ' ' + %char(DSTnfv) + ' '
061100080916                + %trim($Sql2( 7)) + ' ' + %char(DSTdfv) + ' '
061200081001                + %trim($Sql2( 8)) + ' '
061300081001                + %trim($Sql2( 9));
061400080808
061500080808       ENDSR;
061600080808
061700080808       //--------------------------------------------------------------
061800130307       //?Apertura cursore - file FNARB00F                             ?
061900080808       //--------------------------------------------------------------
062000130307       BEGSR  OpenCursor_2;
062100080808
062200080808         // Preparazione stringa SQL da eseguire
062300080808         exec sql   prepare S2   from :wSQL_2;
062400080808
062500080808         // Dichiarazione cursore
062600080808         exec sql   declare C2   cursor for S2;
062700080808
062800080808         // Apertura del cursore
062900080808         exec sql   open C2;
063000080808
063100080808         reset  $EoF2;
063200080808
063300080808       ENDSR;
063400080808
063500080808       //--------------------------------------------------------------
063600130307       //?Chiusura cursore - file FNARB00F                             ?
063700080808       //--------------------------------------------------------------
063800080808       BEGSR  CloseCursor_2;
063900080808
064000080808         exec sql   close C2;
064100080808
064200080808       ENDSR;
064300080808
064400080808       //--------------------------------------------------------------
064500130307       //?Lettura cursore - file FNARB00F                              ?
064600080808       //--------------------------------------------------------------
064700080808       BEGSR  ReadCursor_2;
064800080808
064900080808         clear  FNARBds;
065000080808
065100080808         exec sql   fetch next   from C2   into :FNARBds;
065200080808
065300080808         select;
065400080808           when  SQLcod = 100;
065500080808             $EoF2 = *on;
065600080808           when  SQLcod < *zero;
065700080808             $Err  = *on;
065800080808             $EoF2 = *on;
065900080808           other;
066000080808             exsr  Stampa_LdV;
066100080808         endsl;
066200080808
066300080808       ENDSR;
066400080808
066500080808      /end-free
066600070313      *
066700070313      *---------------------------------------------------------------*
066800100903      *?Richiamo del pgm. di stampa LdV                              ?*
066900070313      *---------------------------------------------------------------*
067000080714     c     Stampa_LdV    BEGSR
067100070313      *
067200070320      * Reperimento rec. di FIAR401L per aggiornamento data stampa LdV
067300070320     c     K05AR401      chain     FIAR4000
067400070320if  1c                   if        %found(FIAR401L)
067500070320     c                   movel     AR4not        dsBL4A
067600070320x   1c                   else
067700070320     c                   clear                   dsBL4A
067800070320e   1c                   endif
067900070320      *
068000070320      * Stampa LdV
068100070313     c                   reset                   FNLSB5ds
068200080916      /free
068300100903         // -? PRIMA: ?
068400100903         //if wCiclo = 1;
068500100903         //  DB0fa4 = '2';
068600100903         //else;
068700100903         //  DB0fa4 = '1';
068800100903         //endif;
068900100903
069000100903         //? Casistiche & parametri:                                   ?
069100100903         //? 1° ciclo   &   Abilitazione al PDA (non in test)          ?
069200100903         // =>?FNLSB6R stampa 4 "ricevute" in A4?
069300100903         //    (stampando sempre e comunque;
069400100903         //     se si riceve la segnalazione di una pck-lst rimanente,
069500100903         //     la sped. verrà memorizzata per poter essere elaborata
069600100903         //     in un ciclo successivo)
069700100903         // Si passa  DB0FA4 = "4".
069800100927         // Si riceve DB0TMS = "Q" (se non c'è una pck-lst da stampare);
069900100903         //           DB0TMS = "D" (se rimane packing-list da stampare).
070000130422         //           DB0TMS = "i" (se rimane PDF da stampare).
070100130422         //           DB0TMS = "Y" (se rimangono pck-lst+PDF da stamp.).
070200100903         //? 2° ciclo   &   Abilitazione al PDA (non in test)          ?
070300100903         // =>?FNLSB6R stampa 2 packing-list in A4?
070400100903         //    (per le packing-list NON stampate durante il 1° ciclo)
070500100903         // Si passa  DB0FA4 = "P".
070600100903         // Si riceve DB0TMS = "P" (se stampata la packing-list);
070700100903         //           DB0TMS = " " (se non trovata la packing-list).
070800130422         //? 3° ciclo   &   Abilitazione al PDA (non in test)          ?
070900130422         // =>?FNLSB6R stampa 1 PDF in A4?
071000130422         //    (per tutti i .PDF - SE ce ne sono - alla fine di tutto)
071100100903         //? 1° ciclo   &   NON abilitazione al PDA                    ?
071200100903         // =>?FNLSB6R stampa 2 LdV in A4?
071300100903         //    (stampando solo ex-A5   e
071400100903         //     segnalando se la spedizione richiede A4:
071500100903         //     essa verrà memorizzata per poter essere elaborata in un
071600100903         //     ciclo successivo)
071700100903         // Si passa  DB0FA4 = "2".
071800100903         // Si riceve DB0TMS = "5" (se stampata LdV ex-A5);
071900130422         //           DB0TMS = "D" (se richiede A4).
072000130422         //           DB0TMS = "I" (se rimane PDF da stampare).
072100100903         //? 2° ciclo   &   NON abilitazione al PDA                    ?
072200100903         // =>?FNLSB6R stampa 1 LdV in A4?
072300100903         //    (saltando sempre all'inizio dell'A4 ssuccessivo)
072400100903         // Si passa  DB0FA4 = "1".
072500100903         // Si riceve DB0TMS = "5" (se stampata LdV ex-A5);
072600100903         //           DB0TMS = "4" (se stampata LdV A4).
072700130422         //           DB0TMS = "J" (se rimane PDF da stampare).
072800100903
072900100903         SELECT;
073000100903
073100100903           // -?Stampa di 4 LdV x A4?
073200100903           WHEN (DSTpda = 'C'      or  DSTpda = 'E')       and
073300100903                (§DSTtstPDA <> 'C' and §DSTtstPDA <> 'E')  and
073400100903                 wCiclo = 1;
073500100903             DB0fa4 = '4';
073600100903
073700100903           // -?Stampa di 2 packing-list x A4?
073800100903           WHEN (DSTpda = 'C'      or  DSTpda = 'E')       and
073900100903                (§DSTtstPDA <> 'C' and §DSTtstPDA <> 'E')  and
074000100903                 wCiclo = 2;
074100100903             DB0fa4 = 'P';
074200100903
074300100903           // -?Stampa di 2 LdV x A4?
074400100903           WHEN  wCiclo = 1;
074500100903             DB0fa4 = '2';
074600100903
074700100903           // -?Stampa di 1 LdV x A4  (con packing-list)?
074800100903           OTHER;
074900100903             DB0fa4 = '1';
075000100903
075100100903         ENDSL;
075200080916      /end-free
075300080916     c*** già così:      eval      DB0tbo   = 'A'
075400080808     c                   eval      DB0aas   = sqlARBaas
075500080808     c                   eval      DB0lnp   = sqlARBlnp
075600080808     c                   eval      DB0nrs   = sqlARBnrs
075700080808     c                   eval      DB0nsp   = sqlARBnsp
075800081010
075900080916      /free
076000100903                //? ASTERISCATA IN ATTESA DI CONFERMA ?
076100081010       // x le spedizioni packing list inserite in distinte con PDA consegne
076200081010       // dal 2° tentativo di consegna non bisogna stampare il packing list
076300100903         ////if  sqlARBtmc <> *blank and distinta PDA consegne;
076400100903         //if  sqlARBtmc  <> *blank                      and
076500100903         //   (DSTpda     =  'C'  or  DSTpda = 'E')      and
076600100903         //   (§DSTtstPDA <> 'C' and §DSTtstPDA <> 'E')
076700081010         //  DB0fl1 = 'D';
076800081010         //endif;
076900080916      /end-free
077000081010
077100080916      * Test "rottura" distinta consegna
077200080808if  1c                   IF             sqlARBifp <> SAVifp
077300080808     c                             or   sqlARBndc <> SAVndc
077400080808     c                             or   sqlARBddc <> SAVddc
077500080715      /free
077600080916           DB0fl6 = *on;
077700080916
077800080916           IF  DS9psl = 'FNLSB6R   ';
077900080916             if  (wCountA4 + (wCountA5 / 2) + (wCount4xA4 / 4))
078000080916                                          >= (§VPOpagLdV - 20);
078100080916               DB0fl7     = *on;
078200080916               wCountA4   = *zero;
078300080916               wCountA5   = *zero;
078400080916               wCount4xA4 = *zero;
078500080916             endif;
078600080916           ELSE;
078700080718      /end-free
078800070914if  2c                   if        wCountA4 >= §VPOpagldv
078900070313     c                   eval      DB0fl7   = *on
079000070316     c                   clear                   wCountA4
079100070313e   2c                   endif
079200070914if  2c                   if        wCountA5 >= §VPOpagldv
079300070313     c                   eval      DB0fl8   = *on
079400070316     c                   clear                   wCountA5
079500070313e   2c                   endif
079600080718      /free
079700080916           ENDIF;
079800080718      /end-free
079900070529e   1c                   ENDIF
080000070313      *
080100070320     c                   call      DS9psl
080200070313     c                   parm                    FNLSB5ds
080300130307     c                   parm                    FNLSB6ds1
080400070313      *
080500070313      * Conteggio delle pagine finora stampate
080600080714      * ...e delle LdV stampate per Distinta
080700100903sel 1c*//                select
080800100903w   1c*//                when      DB0tms   = '4'
080900100903     c*//                add       1             wCountA4
081000100903     c*//                add       1             wLdVxDis
081100100903w   1c*//                when      DB0tms   = '5'
081200100903     c*//                add       1             wCountA5
081300100903     c*//                add       1             wLdVxDis
081400100903x   1c*//                other
081500100903     c*//                exsr      sr_MemoSped
081600100903e   1c*//                endsl
081700080902      /free
081800100903
081900100903         // -?Conteggio delle LdV stampate per Distinta?
082000100903         SELECT;
082100100903
082200100903           // -?Stampato foglio A4 intero?
082300130307           WHEN  DB0tms = '4'  or  DB0tms = 'J';
082400100903             wCountA4   += 1;
082500100903             wLdVxDis   += 1;
082600100903
082700100903           // -?Stampato foglio ex-A5 (2 per A4)?
082800130307           WHEN  DB0tms = '5'  or  DB0tms = 'I';
082900100903             wCountA5   += 1;
083000100903             wLdVxDis   += 1;
083100100903
083200100927           // -?Stampata "tagliatella" (4 per A4)?
083300130307           WHEN  DB0tms = 'Q'  or  DB0tms = 'i';
083400100903             wCount4xA4 += 1;
083500100903             wLdVxDis   += 1;
083600130307           // -?Stampata "tagliatella" (4 per A4), ma rimane packing-list?
083700130307           WHEN  (DB0tms = 'D'  or  DB0tms = 'Y')  and  DB0fa4 = '4';
083800100903             wCount4xA4 += 1;
083900100903             //wLdVxDis += 1;   (NON conteggia; conteggerà nel ciclo "P")
084000100906             if  wCiclo = 1;
084100100906               exsr  sr_MemoSped;
084200100906             endif;
084300100903
084400130307           // -?Stampata packing-list (2 pag. per A4, ma salto pag. x?
084500130307           //  ?ogni packing-list - vedi *pgm FNLSB6R)?
084600100903           WHEN  DB0tms = 'P';
084700130307             //wCountA5   += 1;
084800130307             wCountA4   += 1;
084900100903             wLdVxDis   += 1;
085000100903
085100100903           // -?Memorizza il n° della spedizione NON stampata?
085200130307           //  ?- richiesto A5, ma in realtà è da A4 -?
085300100903           //  ?(IMPOSSIBILE succeda al 2° giro...)?
085400130307           //OTHER;
085500130307           WHEN  DB0tms = 'D'  and  DB0fa4 = '2';
085600100903             if  wCiclo = 1;
085700100903               exsr  sr_MemoSped;
085800100903             endif;
085900100903
086000100903         ENDSL;
086100130307
086200130307         // -?Memorizzazione gruppo di immagini PDF da stampare DOPO?
086300130307         If  (DB0tms = 'i'  or  DB0tms = 'I'  or
086400130402              DB0tms = 'J'  or  DB0tms = 'Y');
086500130307           exsr  sr_MemoKlist;
086600130307         EndIf;
086700100903
086800100903         // -?Memorizza il num. distinta x il controllo del salto pagina?
086900100903         //  ?(a rottura di distinta)   SE   stampata la LdV e/o la?
087000100903         //  ?packing-list?
087100100903         if  (DB0tms    =  '4'     or
087200100903              DB0tms    =  '5'     or
087300130422              DB0tms    =  'D'     or          //?tagliatella + pck-lst?
087400130422              DB0tms    =  'i'     or          //?tagliatella + PDF?
087500130422              DB0tms    =  'Y'     or          //?tagliatella + pck-lst + PDF?
087600130422              DB0tms    =  'I'     or          //?A4 + PDF?
087700130422              DB0tms    =  'J'     or          //?A5 + PDF?
087800130422              DB0tms    =  'Q'     or          //?tagliatella?
087900130422              DB0tms    =  'P')    AND         //?pck-lst?
088000080916             (sqlARBifp <> SAVifp  or
088100080916              sqlARBndc <> SAVndc  or
088200080916              sqlARBddc <> SAVddc);
088300080916           SAVifp = sqlARBifp;
088400080916           SAVndc = sqlARBndc;
088500080916           SAVddc = sqlARBddc;
088600080916         endif;
088700080902      /end-free
088800070320      *
088900070320      * Aggiornamento data stampa LdV
089000080714if  1c                   if        %found(FIAR401L)
089100080714if  2c                   if             DB0tms = '4'
089200080714     c                              or  DB0tms = '5'
089300100927     c***                           or  DB0tms = 'Q'
089400100903     c***                           or  DB0tms = 'P'
089500100903     c                              or  DB0fa4 = '4'
089600070320     c                   z-add     wDate         §B4asl
089700070420      * aggiorno flag stampa DDT se bolla mai stampata in arrivo
089800070420     c                   select
089900071015     c                   when      §B4abm = 'S'
090000071015     c                   eval      §B4abm = 'P'
090100071015     c                   when      §B4abm = 'C'
090200071015     c                   eval      §B4abm = 'Q'
090300071015     c                   when      §B4abm = 'K'
090400071015     c                   eval      §B4abm = 'W'
090500071015     c                   when      §B4abm = 'N' or §B4abm = 'Y'
090600071015     c                   eval      §B4abm = 'J'
090700070420     c                   endsl
090800070320     c                   movel     dsBL4A        AR4not
090900070320      *                  __________________
091000070320     c                   update    FIAR4000
091100070320      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
091200080714x   2c                   else
091300080714     c                   unlock    FIAR401L
091400080714e   2c                   endif
091500070320e   1c                   endif
091600070313      *
091700070313     c                   ENDSR
091800080714      /free
091900130307
092000080714       //--------------------------------------------------------------
092100130307       //?Memorizzazione spedizioni A4 non stampate                    ?
092200130307       //?  (da stampare in un ciclo successivo)                       ?
092300080714       //--------------------------------------------------------------
092400080714       BegSr  sr_MemoSped;
092500080714
092600080715         clear  ds_Sped;
092700080808         dsSp_AAS = sqlARBaas;
092800080808         dsSp_LNP = sqlARBlnp;
092900080808         dsSp_NRS = sqlARBnrs;
093000080808         dsSp_NSP = sqlARBnsp;
093100080715
093200080714         xx += 1;
093300080714         $SpA4(xx) = ds_Sped;
093400080714
093500080714       EndSr;
093600130307
093700130307       //--------------------------------------------------------------
093800130307       //?Memorizzazione numeratore (chiave elenco) per PDF            ?
093900130307       //?  (da stampare alla fine)                                    ?
094000130307       //--------------------------------------------------------------
094100130307       BegSr  sr_MemoKlist;
094200130307
094300130307         select;
094400130307
094500130307           // -?Nessun PDF allegato?
094600130307           when  DB6num = *blank  or  DB6num = *zero;
094700130307
094800130307           // -?1° PDF allegato?
094900130307           when  ww = *zero;
095000130307             ww  = 1;
095100130307             $Klist(ww) = DB6num;
095200130307
095300130307           // -?Nuova chiave elenco per ultimo PDF allegato?
095400130307           when  %lookup( DB6num : $Klist ) = *zero;
095500130307             ww += 1;
095600130307             $Klist(ww) = DB6num;
095700130307
095800130307         endsl;
095900130307
096000130307       EndSr;
096100130307
096200080714      /end-free
096300070827      *
096400070827      *---------------------------------------------------------------*
096500070827      *?Reperimento fase attuale della Distinta Consegna in FIDST00F ?*
096600070827      *---------------------------------------------------------------*
096700070827     c     Rep_FIDST     BEGSR
096800070827      *
096900070827     c                   eval      $FIDST   = *off
097000080903      /free
097100080903         $FIDSF = *off;
097200080903      /end-free
097300070827      *
097400071015      * Aggancio del record SE NON richiesta una sola distinta
097500071015      *   (che altrimenti è già stata agganciata)
097600071015if  1c                   if             DS9ndi  = *zeros
097700071015     c                             or   DS9ndi <> DS9ndf
097800070827     c     K03DST01      chain(n)  FIDST000
097900071015e   1c                   endif
098000100903      /free
098100100903         dDSTflr = DSTflr;
098200100903      /end-free
098300070827      *
098400070827sel 1c                   select
098500071015      * (IMPOSSIBILE che il record sia non reperito, o annullato,
098600071015      *    o già chiuso !!!)
098700070827w   1c                   when      NOT  %found(FIDST01L)
098800070827     c                   leavesr
098900071015w   1c***                when      DSTatb  <> *blanks
099000071015     c***                leavesr
099100070830      * SE le LdV risultano già stampate: NON si aggiorna il rec.
099200070827w   1c                   when      DSTfasd =  'STP'
099300070827     c                   leavesr
099400070827e   1c                   endsl
099500070827      *
099600071015      * "Allocazione" record di FIDST01L per aggiornamento fase in
099700070830      *   "Stampate LdV"
099800070827     c     K03DST01      chain(e)  FIDST000
099900070827      *
100000070827sel 1c                   SELECT
100100070830      * - Record allocato da altro lavoro
100200070830w   1c                   WHEN      %error
100300070830     c                   exsr      Ges_Lck
100400070830     c     K03DST01      chain     FIDST000
100500070827      * - Record aggiornabile
100600070829w   1c                   WHEN      %found(FIDST01L)
100700070827      * - Record non trovato (IMPOSSIBILE...!)
100800070827w   1c                   WHEN      NOT  %found(FIDST01L)
100900070827     c                   leavesr
101000070827e   1c                   ENDSL
101100070827      *
101200070827     c                   eval      $FIDST   = *on
101300080903      *
101400080903      /free
101500080903         // Reperimento eventuale record dal file FIDSF00F
101600080903         //   (in cui registrare la data/ora della 1ª stampa)
101700080903         k_DSFnpg = sqlDSTnpg;        // già "4"
101800080903         k_DSFnfv = sqlDSTnfv;
101900080903         k_DSFfgs = sqlDSTfgs;
102000080903         k_DSFtrd = 'STP';
102100080903         chain(n)  %kds(k04fidsf01)  FIDSF000;
102200080903
102300080903         select;
102400080903           // record non reperito => da aggiungere
102500080903           when  NOT  %found(FIDSF01L);
102600080903             $FIDSF = *on;
102700080903           // record annullato => da rispristinare ed aggiornare
102800080903           when  DSFatb <> *blank;
102900080903             $FIDSF = *off;
103000080903           // SE le LdV risultano già stampate: NON si aggiorna il rec.
103100080903           when  DSTfasd =  'STP';
103200080903             leavesr;
103300080903         endsl;
103400080903
103500080903         // "Allocazione" record di FIDSF01L per aggiornamento fase in
103600080903         //   "Stampate LdV"
103700080903         if  DSFatb <> *blank;
103800080903           chain  %kds(k04fidsf01)  FIDSF000;
103900080903         endif;
104000080903
104100080903         $FIDSF = *on;
104200080903
104300080903       ENDSR;
104400080903      /end-free
104500070627      *
104600070627      *---------------------------------------------------------------*
104700070627      *?Aggiornamento fase della Distinta Consegna in FIDST00F       ?*
104800070627      *---------------------------------------------------------------*
104900070627     c     Upd_FIDST     BEGSR
105000070731      *
105100070731      * Aggiornamento fase come "Stampate LdV"
105200070627     c                   eval      DSTfasd  = 'STP'
105300070627      *                  __________________
105400070627     c                   UPDATE    FIDST000
105500080903      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
105600080903      /free
105700080903         if  $FIDSF = *on;
105800080903           exsr  Upd_FIDSF;
105900080903         endif;
106000080903      /end-free
106100080903     c                   ENDSR
106200080903      *
106300080903      /free
106400130307
106500080903       //--------------------------------------------------------------
106600130307       //?Aggiornamento data/ora aggiornamento distinta uscita AUT     ?
106700080903       //--------------------------------------------------------------
106800080903       BEGSR  Upd_FIDSF;
106900080903
107000080903         //?Impostazione campi nel record?
107100080903
107200080903         if  NOT  %found(FIDSF01L);
107300080903           clear  FIDSF000;
107400080903           DSFfgs = sqlDSTfgs;
107500080903           DSFnpg = sqlDSTnpg;        // già "4"
107600080903           DSFnfv = sqlDSTnfv;
107700080903           DSFtrd = 'STP';
107800080903         endif;
107900080903
108000080903         DSFdtorin = %int( %subst( %char( %dec( %timestamp() ) )
108100080903                                 :1 :14 ) );
108200080903         DSFpdaas4 = 'A';
108300080903
108400080903         //?Scrittura / Aggiornamento?
108500080903
108600080903         if  %found(FIDSF01L);
108700080903         //________________
108800080903           UPDATE  FIDSF000;
108900080903         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
109000080903         else;
109100080903         //________________
109200080903           WRITE   FIDSF000;
109300080903         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
109400080903         endif;
109500080903
109600080903       ENDSR;
109700130307
109800130307       //--------------------------------------------------------------
109900130307       //?Stampa di TUTTI i documenti PDF allegati alle varie LdV      ?
110000130307       //--------------------------------------------------------------
110100130307       BEGSR  sr_PrtPDF;
110200130307
110300130307         IF  ww > *zero;
110400130307
110500130307           wCiclo = 3;
110600130307
110700130307           For  yy = 1  by 1  to ww;
110800130307
110900130307             reset  FNLSB5ds;
111000130307             DB0fa4 = 'I';
111100130307             DB0fl7 = *on;
111200130307
111300130307             clear  FNLSB6ds1;
111400130307             DB6pdf = 'S';
111500130307             DB6num = $Klist(yy);
111600130307
111700130307      /end-free
111800130307     c                   call      DS9psl
111900130307     c                   parm                    FNLSB5ds
112000130307     c                   parm                    FNLSB6ds1
112100130307      /free
112200130307
112300130307           EndFor;
112400130307
112500130307         ENDIF;
112600130307
112700130307       ENDSR;
112800130307
112900080903      /end-free
113000070731      *
113100070731      *---------------------------------------------------------------*
113200070731      *?Gestione record già allocato del file FIDST00F               ?*
113300070731      *---------------------------------------------------------------*
113400070731     c     Ges_Lck       BEGSR
113500070731      *
113600070731     c                   reset                   TRUL82ds
113700070731     c                   eval      UL82§rrn = ID_dstNRR
113800070731     c                   eval      UL82§mss = %replace(
113900070731     c                                          %editc(DSTnfv:'Z')
114000070731     c                                        : C_Msg_01
114100070731     c                                        : %scan( '&NrDst'
114200070731     c                                          : C_Msg_01 ))
114300070830     c                   eval      §UL82txt2= 'Questo lavoro sta blocc+
114400070830     c                                         cando la stampa delle L+
114500070830     c                                         dV dalla distinta n° '
114600070830     c                                      + %trim( %editc(
114700070830     c                                         DSTnfv : 'Z' ) )
114800070830     c                                      + ': TERMINARE !'
114900070731      * Effettuo la chiamata al *PGM d utilità
115000070731     c                   call(e)   'TRUL82R'
115100070731     c                   parm                    TRUL82ds
115200070830     c                   parm                    §UL82txt2       120
115300070731      *
115400080808     c                   ENDSR
115500080818
115600080818      *---------------------------------------------------------------*
115700080818
115800101008** - $Sql1 ---------------------------------*
115900080808SELECT DSTnpg, DSTnfv, DSTfgs                     1
116000071015 FROM  FIDST00F                                   2
116100070313 left outer join FLTR900F                         3
116200071015 ON    DSTfgs = TR9fgs                            4
116300071015  and  DSTnpg = TR9npg                            5
116400071015  and  DSTnfv = TR9nfv                            6
116500071015  and  DSTdfv = TR9dfv                            7
116600071015 WHERE DSTatb <> 'A' and DSTfcf <> 'S'            8
116700071015  and  DSTnpg = 4                                 9
116800071015  and  DSTfgs =                                  10
116900071015  and  DSTdfv =                                  11
117000071015  and  DSTnfv between                            12
117100071015  and  DSTfpp = '                                13
117200071015  and  DSTtpm = '                                14
117300071119  and  DSTfasd= '   '                            15
117400101008  and (DSTpda NOT in ('C', 'E') or               16
117500101008      substr(DSTflr, 1, 1) in ('C', 'E'))        17
117600101008  and (DSTpda in ('C', 'E') and                  18
117700101008      substr(DSTflr, 1, 1) NOT in ('C', 'E'))    19
117800080808** - $Sql2 ----------------------------*
117900080808select ARBaas, ARBlnp, ARBnrs, ARBnsp,            1
118000080916       ARBrsd, ARBndc, ARBddc, ARBifp,            2
118100080916       ARBtmc                                     3
118200080916from   FNARB00F                                   4
118300080916where  ARBifp =                                   5
118400080916  and  ARBndc =                                   6
118500080916  and  ARBddc =                                   7
118600081001order  by  ARBifp,ARBddc,ARBndc, ARBrsd,          8
118700081001       ARBaas,ARBlnp,ARBnrs,ARBnsp                9
