000100071106      *PARMS CLOSQLCSR(*ENDMOD) DBGVIEW(*SOURCE) DYNUSRPRF(*OWNER)
000200070313      *---------------------------------------------------------------*
000300070927      *?STAMPA ORM DA DISTINTE AUTOTRASPORTATORI - BATCH             ?*
000400070313      *---------------------------------------------------------------*
000500070313
000600070313     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700070313     h alwnull(*inputonly)
000800070313
000900070313      *---------------------------------------------------------------*
001000090202
001100080320     fAZORG01L  if   e           k disk
001200090202     fTABEL00F  if   e           k disk
001300070313
001400070927     fFIDST08L  if   e           k disk
001500071009      *
001600071009     fFNORM00F  Uf   e             disk    usropn
001700071105      *
001800071105     fWFORM00F  O    e             disk    usropn
001900071009
002000070313      *---------------------------------------------------------------*
002100070313
002200071009      *
002300071009      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - -?*
002400071009      *
002500071009     d C_Msg_01        C                   CONST('Si sta bloccando la -
002600071009     d                                     stampa degli ORM: USCIRE SU-
002700071009     d                                     BITO da questo lavoro!')
002800071105      *
002900071105      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - -?*
003000071105      *
003100071105      * - Comandi di sistema da eseguire
003200071105     d $Cmd            s             70    dim(03)  ctdata  perrcd(01)
003300071009      *
003400071009      *?  S T R U T T U R E   D A T I   - - - - - - - - - - - - - - -?*
003500071009      *
003600070313      * - Parametri
003700070313     d KPJBA         e ds
003800070927     d Fior78ds      e ds                  inz
003900070928     d Fior16ds      e ds                  inz
004000080327     d Fior90ds      e ds                  inz
004100070313      *
004200070313      * - Parametri x Controllo profilo utenti
004300070313     d TIBS34ds      e ds                  inz
004400070313      * - Ds di riferimento al file esterno AZUTE00F
004500070313     d AZUTEds       e ds                       extname(AZUTE00F)
004600070313      * - Ds per dati organigramma
004700070313     d dDatiUte      e ds
004800070928      *
004900071108     d FNORMds       e ds                  extname(FNORM00F)  inz
005000071108     d WFORMds       e ds                  extname(WFORM00F)  inz
005100071009      *
005200071009      * - Parametri x Invio msg per gestione allocazione record
005300071009     d TRUL82ds      e ds                  inz
005400071009     d  UL82§fil     e                     inz('FNORM00F  ')
005500071009     d  UL82§win     e                     inz('N')
005600071009     d  UL82§f5      e                     inz('N')
005700071009     d  UL82§f7      e                     inz('S')
005800071009     d  UL82§num     e                     inz(05)
005900071009     d  UL82§att     e                     inz(60)
006000071107      *
006100071107      * - Parametri x ricerca/controllo tabelle
006200071107     d TIBS02ds      e ds                  inz
006300071107      * - Tab. VPO = Variabili Programmi Operativi
006400071107     d dVPO          e ds                  inz
006500090202      *
006600090202      * - Tab. 5P = Associaz. Filiale utente / Stampante + Pgm. stampa
006700090202     d ds5P          e ds
006800090202      *
006900090202      * - azorg
007000080320     d OG148         e ds                  inz
007100090202      * - fidst
007200080326     d ddstflr       e ds                  inz
007300090202      * - fnorm
007400080326     d dorm01        e ds                  inz
007500080326      *
007600070313     d Status         sds
007700070313     d  SDSpgm           *proc
007800070621      *
007900070621     d ds_Time         ds                  inz
008000070621     d  wTime                         6  0 inz
008100070621     d  wDate                         8  0 inz
008200070313      *
008300070313      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
008400070313      *
008500071107      * - Campi per controllo "rotture di livello"
008600071107     d SAVnfv          s                   like(DSTnfv) inz
008700071107     d SAVke1          s                   like(WRMke1) inz
008800071107     d SAVke2          s                   like(WRMke2) inz
008900071105      * - Numero di records estratti nel WrkF
009000071105     d wRec            s              9  0 inz
009100071107      * - Numero di ORM già stampati
009200071107     d wCountORM       s              3  0 inz
009300071009      * - Numero relativo di record del file FNORM00F
009400071009     d wORMnrr         s              9  0 inz
009500071105      * - Parametri per pgm. "QCMDEXC"
009600071105     d Qcmd            s            256    inz
009700071105     d Qlen            s             15  5 inz(256)
009800070313      * - Campi di comodo
009900070621     d w0140           s             14  0 inz
010000070619     d wData_Iso       s               d   inz  datfmt(*iso)
010100070928     d Keynpg          s                   like(dstnpg) inz(4)
010200080326     d Wallega         s              1
010300080327     d Wfase_sto       s              1
010400070313      *
010500070313      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
010600070313      *
010700070928      * - FIDST08L
010800070928      *
010900070928     c     K04dst01      klist
011000070928     c                   kfld                    R78fgs
011100070928     c                   kfld                    KEYnpg
011200070928     c                   kfld                    R78ddc
011300070928     c                   kfld                    R78ndi
011400070928      *
011500070928     c     K03dst01      klist
011600070928     c                   kfld                    R78fgs
011700070928     c                   kfld                    KEYnpg
011800070928     c                   kfld                    R78ddc
011900070320      *
012000070313
012100070313      *---------------------------------------------------------------*
012200070313      *?RIEPILOGO INDICATORI                                         ?*
012300070313      *---------------------------------------------------------------*
012400071009      * 25   - Comodo                                                 *
012500070313      *---------------------------------------------------------------*
012600070313
012700070313     c     *Entry        plist
012800070313     c                   parm                    KPJBA
012900070928     c                   movel     KPJBU         Fior78ds
013000080320      *
013100080320      * Verifico se filiale partita con PDA
013200080320     c     R78fgs        chain     Azorg01l
013300080320     c                   if        %found(azorg01l)
013400080320     c                   movel     orgde8        og148
013500080320     c                   else
013600080320     c                   clear                   og148
013700080320     c                   endif
013800071009      *
013900071009      * Elaborazione
014000070928     c                   exsr      sr_Dist
014100080320
014200071108if  1c                   if        wRec  > *zeros
014300071105     c                   exsr      sr_ElabWF
014400071106e   1c                   endif
014500070313      *
014600070313      * Fine
014700071108     c                   eval      *inLR = *on
014800070313      *
014900070313      *---------------------------------------------------------------*
015000070313      *?Operazioni iniziali                                          ?*
015100070313      *---------------------------------------------------------------*
015200070313     c     *InzSr        BEGSR
015300070313      *
015400070313      * Reperimento dati job
015500070313     c     *dtaara       define    §azute        AZUTEds
015600070313     c     *dtaara       define    §datiute      dDATIUTE
015700070313      *
015800070313     c                   in(E)     *dtaara
015900070313     c                   IF        %ERROR or RSUT = *blanks
016000070313     c                   clear                   Tibs34Ds
016100070313     c                   call      'TIBS34R'
016200070313     c                   parm                    Tibs34Ds
016300070313     c                   in        *dtaara
016400070313     c                   ENDIF
016500070320      *
016600070320      * Impostazione dati fissi
016700070621     c                   time                    w0140
016800070621     c                   movel     w0140         ds_Time
016900070621     c     *eur          move      wDate         wData_Iso
017000070621     c     *iso          move      wData_Iso     wDate
017100090202
017200090202      /free
017300090202         // Reperimento dati per stampa O.R.M. da tab. 5P/O
017400090202         clear  ds5P;
017500090202         §5Ppgm = 'FIOR18R   ';
017600090202         if  %subst(knmus : 1 : 3) = 'EDP';
017700090202           TBLkey = 'O' + 'EDP';
017800090202         else;
017900090202           TBLkey = 'O' + %editc( DUTpou : 'X' );
018000090202         endif;
018100090202         chain  ( 1 : '5P' : TBLkey )  TABEL;
018200090202         if  %found( TABEL00F );
018300090202           ds5P = TBLuni;
018400090202         endif;
018500090202      /end-free
018600071107      *
018700071107      * Aggancio tabella "VPO" per reperimento n° pagine limite
018800071107      *   in stampa O.R.M. (per close/open PrtF)
018900071107     c                   clear                   dVPO
019000071107     c                   clear                   TIBS02ds
019100071107     c                   eval      T02tla  = 'L'
019200071107     c                   eval      T02mod  = 'C'
019300071107     c                   eval      T02sif  = KNSIF
019400071107     c                   eval      T02cod  = 'VPO'
019500071107     c                   movel(P)  'VPO'         T02ke1
019600071107     c                   call      'TIBS02R'
019700071107     c                   parm                    KPJBA
019800071107     c                   parm                    TIBS02ds
019900071107     c                   if        T02err  = *blanks
020000071107     c                   movel     T02uni        dVPO
020100071107     c                   else
020200071107     c                   eval      §VPOPagOrm = 100
020300071107     c                   endif
020400070914      *
020500070313     c                   ENDSR
020600070313      *
020700070313      *---------------------------------------------------------------*
020800070928      *?Elaborazione Distinte                                        ?*
020900070313      *---------------------------------------------------------------*
021000070928     c     sr_Dist       BEGSR
021100070313      *
021200071106      * - Cancellazione WrkF WFORM00F dalla libr. QTEMP (ci fosse...)
021300071106     c                   call      'QCMDEXC'                            90
021400071106     c                   parm      $Cmd(01)      Qcmd
021500071106     c                   parm                    Qlen
021600071106      * - Creazione WrkF FIDST00F nella libr. QTEMP (vuoto)
021700071106     c                   eval      Qcmd = $Cmd(2)
021800071106     c                                  + %trim($Cmd(3))
021900071106     c                   call      'QCMDEXC'
022000071106     c                   parm                    Qcmd
022100071106     c                   parm                    Qlen
022200071106     c                   open      WFORM00F
022300071106      *
022400071105     c                   clear                   wRec
022500071105      *
022600071009     c     k04dst01      setll     fidst000
022700070928     c                   do        *hival
022800071009     c     k03dst01      reade     fidst000
022900070928      * fine file
023000070928     c                   if        %eof(fidst08l)
023100070928     c                   leave
023200070928     c                   endif
023300070928      * distinta maggiore del limite selezionato a video
023400070928     c                   if        dstnfv > r78ndf
023500070928     c                   leave
023600070928     c                   endif
023700070928      *
023800070621sel 1c                   select
023900070621      * - record annullato
024000070928w   1c                   when      dstatb  <> *blanks
024100070621      * - foglio già chiuso
024200070928     c                   when      dstfcf   = 'S'
024300070928      * - no parcel
024400070928     c                   when           r78tpm  = 'P'
024500070928     c                             and dsttpm <> *blanks
024600070928      * - no messaggerie
024700070928     c                   when           r78tpm  = 'M'
024800071106     c                             and  dsttpm <> 'M'
024900070621      * - prestazione padroncino (Mattino/Pomeriggio) diversa
025000070928     c                   when           r78fpp <> *blanks
025100070928     c                             and  dstfpp <> r78fpp
025200070621      * - OK
025300070621x   1c                   other
025400080320
025500071105     c                   exsr      sr_Elab
025600080327      * se ho stampato degli orm per le distinta chiamo il pgm di scrittura fasi STO
025700080327     c                   if        wfase_sto = 'Y'
025800080327     c                   exsr      sr_Writefase
025900080327     c                   endif
026000080320
026100070621e   1c                   endsl
026200070313      *
026300070928e   1c                   enddo
026400071106      *
026500071106     c                   close     WFORM00F
026600070928      *
026700070313     c                   ENDSR
026800070313      *
026900070313      *---------------------------------------------------------------*
027000070313      *?Elaborazione singolo record da FNFVV00F                      ?*
027100070313      *---------------------------------------------------------------*
027200070313     c     sr_Elab       BEGSR
027300080326      *
027400080327     c                   eval      Wfase_STO = ' '
027500080326     c                   movel     dstflr        ddstflr
027600070629      *
027700070928     C/EXEC SQL
027800080326     C+ DECLARE B1 CURSOR FOR SELECT distinct fnorm01l.*,
027900080326     C+ rrn(FNORM01L),
028000080326     C+ fnorf01l.orffgs, fnorf01l.orfndc,
028100080326     C+ fnorf01l.orfddc
028200071106     C+ FROM (fnorm01l join fnorf01l on
028300071106     C+  ormpoe=orfpoe and ormnsr=orfnsr and ormnor=orfnor and
028400071106     C+  ormnrv=orfnrv and ormfao=orffar and ormdfo=orfdae and
028500071106     C+  ormofo=orfore)
028600071106     C+ WHERE orfndc=:dstnfv and orfddc=:dstdfv and
028700080326     C+       orffgs=:dstfgs and ORMfao in (400, 410, 420)
028800070928     C+ ORDER BY fnorf01l.orffgs, fnorf01l.orfndc, fnorf01l.orfddc
028900070928     C/END-EXEC
029000070928
029100070928     C/EXEC SQL
029200070928     C+ OPEN B1
029300070928     C/END-EXEC
029400070928
029500071105do  1c                   do        *hival
029600070928
029700070928     C/EXEC SQL
029800071009     C+ FETCH NEXT FROM B1 INTO :fnormds, :wORMnrr
029900070928     C/END-EXEC
030000070928
030100080326     c                   clear                   Wallega
030200071105sel 2c                   select
030300071105w   2c                   when      sqlcod = 100
030400070928     c                   leave
030500071105w   2c                   when      sqlcod < 0
030600071106     c***                leave
030700080326      * se filiale partita con PDA
030800080326      * e non è stata richiesta la stampa orm scaricati su PDA
030900080326      * e la distinta ORM o ORM + CONSEGNE è su PDA
031000080326      * e la distinta non è in TEST ne con gli ORM ne per ORM + CONSEGNE
031100080326     c                   when      §ogpdaorm = 'S'  and
031200080326     c                             r78pda =  'N' and
031300080326     c                             (dstpda = 'O' or dstpda = 'E')  and
031400080326     c                             (§dsttstpda <> 'O' and §dsttstpda <> 'E')
031500080326      * controllo se orm con allegati srcivo il file per stampare altrimenti no
031600080326     c                   movel     ormflo        dorm01
031700080326      * se con allegati scrive nel file di work
031800080326      * (non faccio controllo sul flag di ristampa al massimo viene stampato + volte )
031900080326     c                   if        §orsrm = 'S'
032000080326     c                   eval      Wallega = 'A'
032100080326     c                   exsr      sr_WriteWF
032200080326     c                   endif
032300071105      * Verifico se richiesta della stampa ORM dei già stampati
032400071105w   2c                   when      r78gia = ' ' and ormdst > 0
032500071009      * Non stampo gli ORM PrePagati (ma aggiorno data stampa)
032600071009      * ...In RI-stampa (ORMdst > *zero) vengono stammpati !
032700071105w   2c                   when      ORMtor = 'P' and ORMdst = *zeros
032800071009     c                   exsr      UpdatFNORM
032900071105      * Stampo tutto il resto...
033000071105x   2c                   other
033100071106     c                   exsr      sr_WriteWF
033200080327      * Se stampato almeno un ORM di una distinta sagnalo la richiesta del pgm fasi
033300080327     c                   eval      Wfase_STO = 'Y'
033400071105e   2c                   endsl
033500070928
033600071105e   1c                   enddo
033700070928
033800070928     C/EXEC SQL
033900070928     C+ CLOSE B1
034000070928     C/END-EXEC
034100070928
034200070313     c                   ENDSR
034300071105      *
034400071105      *---------------------------------------------------------------*
034500071105      *?Registrazione dati nel WrkF in QTEMP                         ?*
034600071105      *---------------------------------------------------------------*
034700071106     c     sr_WriteWF    BEGSR
034800071105      *
034900071105     c                   clear                   WFORM000
035000071105      *
035100071108sel 1c                   SELECT
035200071108w   1c                   WHEN      R78ord  = '1'
035300071108     c                   movel     DSTnfv        WRMke1
035400071108     c                   movel     ORMorr        WRMke2
035500071108     c                   movel     ORMlor        WRMke3
035600071108     c                   movel     ORMinr        WRMke4
035700071108e   1c                   WHEN      R78ord  = '2'
035800071105     c                   movel     DSTpdr        WRMke1
035900071105     c                   movel     DSTnfv        WRMke2
036000071105     c                   movel     ORMorr        WRMke3
036100071105     c                   movel     ORMlor        WRMke4
036200071105     c                   movel     ORMinr        WRMke5
036300071108e   1c                   ENDSL
036400071105      *
036500071105     c                   clear                   FIOR16ds
036600071105     c                   movel     R78mdo        R16mdo
036700071105     c                   movel     R78oqo        R16oqo
036800071105     c                   movel     ORMpoe        R16poe
036900071105     c                   movel     ORMnsr        R16nsr
037000071105     c                   movel     ORMnor        R16nor
037100071105     c                   movel     ORMnrv        R16nrv
037200071105     c                   if        ORMdst > 0
037300071105     c                   eval      R16ris = 'R'
037400071105     c                   else
037500071105     c                   eval      R16ris = 'S'
037600080326      * se si tratta di stampa orm con allegati per distinte su PDA imposto il flag R16RIS
037700080326      * uguale ad "A"
037800080326     c                   If        wallega = 'A'
037900080326     c                   eval      R16ris = 'A'
038000080326     c                   endif
038100080326
038200071105     c                   endif
038300071105     c                   movel     FIOR16ds      WRMuni
038400071105      *                  __________________
038500071105     c                   WRITE     WFORM000
038600071105      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
038700071105     c                   add       1             wRec
038800071105      *
038900071105     c                   ENDSR
039000071105      *
039100071105      *---------------------------------------------------------------*
039200071105      *?Elaborazione singolo record da WrkF in QTEMP                 ?*
039300071105      *---------------------------------------------------------------*
039400071105     c     sr_ElabWF     BEGSR
039500071105      *
039600071105     c/exec sql
039700071105     c+  declare A1 cursor for
039800071105     c+  select *  from QTEMP/WFORM00F
039900071105     c+  order  by WRMke1, WRMke2, WRMke3, WRMke4, WRMke5
040000071105     c/end-exec
040100071105      *
040200071105     c/exec sql
040300071105     c+  open A1
040400071105     c/end-exec
040500071105      *
040600071105do  1c                   DO        *hival
040700071105      *
040800071105     c/exec sql
040900071105     c+  fetch next from A1 into :WFORMds
041000071105     c/end-exec
041100071105      *
041200071105     c                   Select
041300071105     c                   When      SQLcod = 100
041400071105     c                   leave
041500071105     c                   When      SQLcod < *zeros
041600071106     c                   leave
041700071105     c                   Other
041800071105     c                   exsr      call_FIOR16
041900071105     c                   EndSl
042000071105      *
042100071105e   1c                   ENDDO
042200071105      *
042300071105     c/exec sql
042400071105     c+  close A1
042500071105     c/end-exec
042600071105      *
042700071105     c                   ENDSR
042800070313      *
042900070313      *---------------------------------------------------------------*
043000070928      *?Richiamo del pgm. di stampa ORM (FIOR16R)                    ?*
043100070313      *---------------------------------------------------------------*
043200070928     c     Call_FIOR16   BEGSR
043300070313      *
043400071107      * Chiusura PrtF al superamento del n° limite di pagine indicato
043500071107      *   in tab. VPO (per riapertura al richiamo successivo)
043600071107      * N.B. - Ogni pag. A5 contiene 2 ORM !!!
043700071107if  1c                   IF            (R78ord =  '1'
043800071108     c                             and  WRMke1 <> SAVke1)
043900071108     c                             OR  (R78ord =  '2'
044000071107     c                             and (WRMke1 <> SAVke1
044100071107     c                              or  WRMke2 <> SAVke2))
044200071108sel 2c                   select
044300071108w   2c                   when      R78ord = '1'
044400071108     c                   eval      SAVke1 = WRMke1
044500071108w   2c                   when      R78ord = '2'
044600071107     c                   eval      SAVke1 = WRMke1
044700071107     c                   eval      SAVke2 = WRMke2
044800071108e   2c                   endsl
044900071108if  2c                   if        wCountORM >= (§VPOpagorm * 2)
045000071107     c                   clear                   FIOR16ds
045100071107     c                   eval      R16tla = 'C'
045200071107     c                   movel(p)  FIOR16ds      KPJBU
045300090202     c                   call      §5Ppgm
045400071107     c                   parm                    KPJBA
045500071107     c                   movel     KPJBU         FIOR16ds
045600071107     c                   clear                   wCountORM
045700071107e   2c                   endif
045800071107e   1c                   ENDIF
045900071107      *
046000071107      * Stmpa O.R.M.
046100070928     c                   clear                   FIOR16ds
046200071106     c                   movel     WRMuni        FIOR16ds
046300071107     c                   movel(p)  FIOR16ds      KPJBU
046400090202     c                   call      §5Ppgm
046500071106     c                   parm                    KPJBA
046600071107     c                   movel     KPJBU         FIOR16ds
046700071107     c                   add       1             wCountORM
046800070313      *
046900070313     c                   ENDSR
047000070827      *
047100071009      *---------------------------------------------------------------*
047200071009      *?Aggiornamento data di stampa in ORM PrePagato                ?*
047300071009      *---------------------------------------------------------------*
047400071009     c     UpdatFNORM    BEGSR
047500071009      *
047600071009if  1c                   if        NOT %open(FNORM00F)
047700071009     c                   open      FNORM00F
047800071009e   1c                   endif
047900071009      *
048000071009      * Gestisco allocazione del record
048100071009      *   con invio del msg all'utente che blocca il rcd
048200071009     c     wORMnrr       chain(e)  FNORM000
048300071009sel 1c                   select
048400071009w   1c                   when      %error
048500071009     c                   reset                   TRUL82ds
048600071009     c                   eval      UL82§rrn = wORMnrr
048700071009     c                   eval      UL82§mss = C_Msg_01
048800071009      * Chiamata al *PGM d utilità
048900071009     c                   call(e)   'TRUL82R'
049000071009     c                   parm                    TRUL82ds
049100071009     c     wORMnrr       chain     FNORM000
049200071009w   1c                   when      NOT %found(FNORM00F)
049300071009     c                   leavesr
049400071009e   1c                   endsl
049500071009      *
049600071009      * Data di stampa
049700071009     c                   eval      ORMdst = wDate
049800071009      * Data ultima variazione
049900071009     c                   eval      ORMdtt = wDate
050000071009      *
050100071009     c                   UPDATE    FNORM000
050200071009      *
050300071009     c                   ENDSR
050400080327      *---------------------------------------------------------------*
050500080327      *?Registrazione fase stampa orm in distinta                    ?*
050600080327      *---------------------------------------------------------------*
050700080327     c     sr_WriteFase  BEGSR
050800080327      *
050900080327     c                   clear                   fior90ds
051000080327
051100080327     c                   eval      or90tla = 'W'
051200080327     c                   eval      or90fas = 'STO'
051300080327     c                   z-add     dstfgs        or90fgs
051400080327     c                   z-add     dstnfv        or90ndc
051500080327     c                   eval      or90flgins = 'A'
051600080327
051700080327     c                   call      'FIOR90R'
051800080327     c                   parm                    kpjba
051900080327     c                   parm                    fior90ds
052000080327
052100080327     c                   ENDSR
052200071105      *
052300071105** - $Cmd
052400071106DLTF FILE(QTEMP/WFORM00F)                                              1
052500071106CRTDUPOBJ OBJ(WFORM00F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)     2
052600071107          NEWOBJ(WFORM00F) DATA(*NO)                                   3
