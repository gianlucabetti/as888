000100071106      *PARMS CLOSQLCSR(*ENDMOD) DBGVIEW(*SOURCE) DYNUSRPRF(*OWNER)
000200070313      *---------------------------------------------------------------*
000300070927      *?STAMPA ORM DA DISTINTE AUTOTRASPORTATORI - BATCH             ?*
000400070313      *---------------------------------------------------------------*
000500070313
000600070313     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700070313     h alwnull(*inputonly)
000800070313
000900070313      *---------------------------------------------------------------*
000901080320     fAZORG01L  if   e           k disk
001000070313
001100070927     fFIDST08L  if   e           k disk
001200071009      *
001300071009     fFNORM00F  Uf   e             disk    usropn
001400071105      *
001500071105     fWFORM00F  O    e             disk    usropn
001600071009
001700070313      *---------------------------------------------------------------*
001800070313
001900071009      *
002000071009      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - -?*
002100071009      *
002200071009     d C_Msg_01        C                   CONST('Si sta bloccando la -
002300071009     d                                     stampa degli ORM: USCIRE SU-
002400071009     d                                     BITO da questo lavoro!')
002500071105      *
002600071105      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - -?*
002700071105      *
002800071105      * - Comandi di sistema da eseguire
002900071105     d $Cmd            s             70    dim(03)  ctdata  perrcd(01)
003000071009      *
003100071009      *?  S T R U T T U R E   D A T I   - - - - - - - - - - - - - - -?*
003200071009      *
003300070313      * - Parametri
003400070313     d KPJBA         e ds
003500070927     d Fior78ds      e ds                  inz
003600070928     d Fior16ds      e ds                  inz
003601080327     d Fior90ds      e ds                  inz
003700070313      *
003800070313      * - Parametri x Controllo profilo utenti
003900070313     d TIBS34ds      e ds                  inz
004000070313      * - Ds di riferimento al file esterno AZUTE00F
004100070313     d AZUTEds       e ds                       extname(AZUTE00F)
004200070313      * - Ds per dati organigramma
004300070313     d dDatiUte      e ds
004400070928      *
004500071108     d FNORMds       e ds                  extname(FNORM00F)  inz
004600071108     d WFORMds       e ds                  extname(WFORM00F)  inz
004700071009      *
004800071009      * - Parametri x Invio msg per gestione allocazione record
004900071009     d TRUL82ds      e ds                  inz
005000071009     d  UL82§fil     e                     inz('FNORM00F  ')
005100071009     d  UL82§win     e                     inz('N')
005200071009     d  UL82§f5      e                     inz('N')
005300071009     d  UL82§f7      e                     inz('S')
005400071009     d  UL82§num     e                     inz(05)
005500071009     d  UL82§att     e                     inz(60)
005600071107      *
005700071107      * - Parametri x ricerca/controllo tabelle
005800071107     d TIBS02ds      e ds                  inz
005900071107      * - Tab. VPO = Variabili Programmi Operativi
006000071107     d dVPO          e ds                  inz
006001080320      * azorg
006002080320     d OG148         e ds                  inz
006100080326      * fidst
006101080326     d ddstflr       e ds                  inz
006102080326      * fnorm
006103080326     d dorm01        e ds                  inz
006104080326      *
006200070313     d Status         sds
006300070313     d  SDSpgm           *proc
006400070621      *
006500070621     d ds_Time         ds                  inz
006600070621     d  wTime                         6  0 inz
006700070621     d  wDate                         8  0 inz
006800070313      *
006900070313      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
007000070313      *
007100071107      * - Campi per controllo "rotture di livello"
007200071107     d SAVnfv          s                   like(DSTnfv) inz
007300071107     d SAVke1          s                   like(WRMke1) inz
007400071107     d SAVke2          s                   like(WRMke2) inz
007500071105      * - Numero di records estratti nel WrkF
007600071105     d wRec            s              9  0 inz
007700071107      * - Numero di ORM già stampati
007800071107     d wCountORM       s              3  0 inz
007900071009      * - Numero relativo di record del file FNORM00F
008000071009     d wORMnrr         s              9  0 inz
008100071105      * - Parametri per pgm. "QCMDEXC"
008200071105     d Qcmd            s            256    inz
008300071105     d Qlen            s             15  5 inz(256)
008400070313      * - Campi di comodo
008500070621     d w0140           s             14  0 inz
008600070619     d wData_Iso       s               d   inz  datfmt(*iso)
008700070928     d Keynpg          s                   like(dstnpg) inz(4)
008701080326     d Wallega         s              1
008702080327     d Wfase_sto       s              1
008800070313      *
008900070313      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
009000070313      *
009100070928      * - FIDST08L
009200070928      *
009300070928     c     K04dst01      klist
009400070928     c                   kfld                    R78fgs
009500070928     c                   kfld                    KEYnpg
009600070928     c                   kfld                    R78ddc
009700070928     c                   kfld                    R78ndi
009800070928      *
009900070928     c     K03dst01      klist
010000070928     c                   kfld                    R78fgs
010100070928     c                   kfld                    KEYnpg
010200070928     c                   kfld                    R78ddc
010300070320      *
010400070313
010500070313      *---------------------------------------------------------------*
010600070313      *?RIEPILOGO INDICATORI                                         ?*
010700070313      *---------------------------------------------------------------*
010800071009      * 25   - Comodo                                                 *
010900070313      *---------------------------------------------------------------*
011000070313
011100070313     c     *Entry        plist
011200070313     c                   parm                    KPJBA
011300070928     c                   movel     KPJBU         Fior78ds
011301080320      *
011302080320      * Verifico se filiale partita con PDA
011303080320     c     R78fgs        chain     Azorg01l
011304080320     c                   if        %found(azorg01l)
011305080320     c                   movel     orgde8        og148
011306080320     c                   else
011307080320     c                   clear                   og148
011308080320     c                   endif
011400071009      *
011500071009      * Elaborazione
011600070928     c                   exsr      sr_Dist
011602080320
011700071108if  1c                   if        wRec  > *zeros
011800071105     c                   exsr      sr_ElabWF
011900071106e   1c                   endif
012000070313      *
012100070313      * Fine
012200071108     c                   eval      *inLR = *on
012300070313      *
012400070313      *---------------------------------------------------------------*
012500070313      *?Operazioni iniziali                                          ?*
012600070313      *---------------------------------------------------------------*
012700070313     c     *InzSr        BEGSR
012800070313      *
012900070313      * Reperimento dati job
013000070313     c     *dtaara       define    §azute        AZUTEds
013100070313     c     *dtaara       define    §datiute      dDATIUTE
013200070313      *
013300070313     c                   in(E)     *dtaara
013400070313     c                   IF        %ERROR or RSUT = *blanks
013500070313     c                   clear                   Tibs34Ds
013600070313     c                   call      'TIBS34R'
013700070313     c                   parm                    Tibs34Ds
013800070313     c                   in        *dtaara
013900070313     c                   ENDIF
014000070320      *
014100070320      * Impostazione dati fissi
014200070621     c                   time                    w0140
014300070621     c                   movel     w0140         ds_Time
014400070621     c     *eur          move      wDate         wData_Iso
014500070621     c     *iso          move      wData_Iso     wDate
014600071107      *
014700071107      * Aggancio tabella "VPO" per reperimento n° pagine limite
014800071107      *   in stampa O.R.M. (per close/open PrtF)
014900071107     c                   clear                   dVPO
015000071107     c                   clear                   TIBS02ds
015100071107     c                   eval      T02tla  = 'L'
015200071107     c                   eval      T02mod  = 'C'
015300071107     c                   eval      T02sif  = KNSIF
015400071107     c                   eval      T02cod  = 'VPO'
015500071107     c                   movel(P)  'VPO'         T02ke1
015600071107     c                   call      'TIBS02R'
015700071107     c                   parm                    KPJBA
015800071107     c                   parm                    TIBS02ds
015900071107     c                   if        T02err  = *blanks
016000071107     c                   movel     T02uni        dVPO
016100071107     c                   else
016200071107     c                   eval      §VPOPagOrm = 100
016300071107     c                   endif
016400070914      *
016500070313     c                   ENDSR
016600070313      *
016700070313      *---------------------------------------------------------------*
016800070928      *?Elaborazione Distinte                                        ?*
016900070313      *---------------------------------------------------------------*
017000070928     c     sr_Dist       BEGSR
017100070313      *
017200071106      * - Cancellazione WrkF WFORM00F dalla libr. QTEMP (ci fosse...)
017300071106     c                   call      'QCMDEXC'                            90
017400071106     c                   parm      $Cmd(01)      Qcmd
017500071106     c                   parm                    Qlen
017600071106      * - Creazione WrkF FIDST00F nella libr. QTEMP (vuoto)
017700071106     c                   eval      Qcmd = $Cmd(2)
017800071106     c                                  + %trim($Cmd(3))
017900071106     c                   call      'QCMDEXC'
018000071106     c                   parm                    Qcmd
018100071106     c                   parm                    Qlen
018200071106     c                   open      WFORM00F
018300071106      *
018400071105     c                   clear                   wRec
018500071105      *
018600071009     c     k04dst01      setll     fidst000
018700070928     c                   do        *hival
018800071009     c     k03dst01      reade     fidst000
018900070928      * fine file
019000070928     c                   if        %eof(fidst08l)
019100070928     c                   leave
019200070928     c                   endif
019300070928      * distinta maggiore del limite selezionato a video
019400070928     c                   if        dstnfv > r78ndf
019500070928     c                   leave
019600070928     c                   endif
019700070928      *
019800070621sel 1c                   select
019900070621      * - record annullato
020000070928w   1c                   when      dstatb  <> *blanks
020100070621      * - foglio già chiuso
020200070928     c                   when      dstfcf   = 'S'
020300070928      * - no parcel
020400070928     c                   when           r78tpm  = 'P'
020500070928     c                             and dsttpm <> *blanks
020600070928      * - no messaggerie
020700070928     c                   when           r78tpm  = 'M'
020800071106     c                             and  dsttpm <> 'M'
020900070621      * - prestazione padroncino (Mattino/Pomeriggio) diversa
021000070928     c                   when           r78fpp <> *blanks
021100070928     c                             and  dstfpp <> r78fpp
021200070621      * - OK
021300070621x   1c                   other
021301080320
021400071105     c                   exsr      sr_Elab
021401080327      * se ho stampato degli orm per le distinta chiamo il pgm di scrittura fasi STO
021402080327     c                   if        wfase_sto = 'Y'
021403080327     c                   exsr      sr_Writefase
021405080327     c                   endif
021406080320
021500070621e   1c                   endsl
021600070313      *
021700070928e   1c                   enddo
021800071106      *
021900071106     c                   close     WFORM00F
022000070928      *
022100070313     c                   ENDSR
022200070313      *
022300070313      *---------------------------------------------------------------*
022400070313      *?Elaborazione singolo record da FNFVV00F                      ?*
022500070313      *---------------------------------------------------------------*
022600070313     c     sr_Elab       BEGSR
022601080326      *
022602080327     c                   eval      Wfase_STO = ' '
022603080326     c                   movel     dstflr        ddstflr
022700070629      *
022800070928     C/EXEC SQL
022900080326     C+ DECLARE B1 CURSOR FOR SELECT distinct fnorm01l.*,
022901080326     C+ rrn(FNORM01L),
022902080326     C+ fnorf01l.orffgs, fnorf01l.orfndc,
022903080326     C+ fnorf01l.orfddc
023000071106     C+ FROM (fnorm01l join fnorf01l on
023100071106     C+  ormpoe=orfpoe and ormnsr=orfnsr and ormnor=orfnor and
023200071106     C+  ormnrv=orfnrv and ormfao=orffar and ormdfo=orfdae and
023300071106     C+  ormofo=orfore)
023400071106     C+ WHERE orfndc=:dstnfv and orfddc=:dstdfv and
023500080326     C+       orffgs=:dstfgs and ORMfao in (400, 410, 420)
023600070928     C+ ORDER BY fnorf01l.orffgs, fnorf01l.orfndc, fnorf01l.orfddc
023700070928     C/END-EXEC
023800070928
023900070928     C/EXEC SQL
024000070928     C+ OPEN B1
024100070928     C/END-EXEC
024200070928
024300071105do  1c                   do        *hival
024400070928
024500070928     C/EXEC SQL
024600071009     C+ FETCH NEXT FROM B1 INTO :fnormds, :wORMnrr
024700070928     C/END-EXEC
024800070928
024801080326     c                   clear                   Wallega
024900071105sel 2c                   select
025000071105w   2c                   when      sqlcod = 100
025100070928     c                   leave
025200071105w   2c                   when      sqlcod < 0
025300071106     c***                leave
025301080326      * se filiale partita con PDA
025302080326      * e non è stata richiesta la stampa orm scaricati su PDA
025303080326      * e la distinta ORM o ORM + CONSEGNE è su PDA
025304080326      * e la distinta non è in TEST ne con gli ORM ne per ORM + CONSEGNE
025306080326     c                   when      §ogpdaorm = 'S'  and
025307080326     c                             r78pda =  'N' and
025308080326     c                             (dstpda = 'O' or dstpda = 'E')  and
025309080326     c                             (§dsttstpda <> 'O' and §dsttstpda <> 'E')
025310080326      * controllo se orm con allegati srcivo il file per stampare altrimenti no
025311080326     c                   movel     ormflo        dorm01
025312080326      * se con allegati scrive nel file di work
025313080326      * (non faccio controllo sul flag di ristampa al massimo viene stampato + volte )
025314080326     c                   if        §orsrm = 'S'
025315080326     c                   eval      Wallega = 'A'
025316080326     c                   exsr      sr_WriteWF
025317080326     c                   endif
025400071105      * Verifico se richiesta della stampa ORM dei già stampati
025500071105w   2c                   when      r78gia = ' ' and ormdst > 0
025600071009      * Non stampo gli ORM PrePagati (ma aggiorno data stampa)
025700071009      * ...In RI-stampa (ORMdst > *zero) vengono stammpati !
025800071105w   2c                   when      ORMtor = 'P' and ORMdst = *zeros
025900071009     c                   exsr      UpdatFNORM
026000071105      * Stampo tutto il resto...
026100071105x   2c                   other
026200071106     c                   exsr      sr_WriteWF
026201080327      * Se stampato almeno un ORM di una distinta sagnalo la richiesta del pgm fasi
026202080327     c                   eval      Wfase_STO = 'Y'
026300071105e   2c                   endsl
026400070928
026500071105e   1c                   enddo
026600070928
026700070928     C/EXEC SQL
026800070928     C+ CLOSE B1
026900070928     C/END-EXEC
027000070928
027100070313     c                   ENDSR
027200071105      *
027300071105      *---------------------------------------------------------------*
027400071105      *?Registrazione dati nel WrkF in QTEMP                         ?*
027500071105      *---------------------------------------------------------------*
027600071106     c     sr_WriteWF    BEGSR
027700071105      *
027800071105     c                   clear                   WFORM000
027900071105      *
028000071108sel 1c                   SELECT
028100071108w   1c                   WHEN      R78ord  = '1'
028200071108     c                   movel     DSTnfv        WRMke1
028300071108     c                   movel     ORMorr        WRMke2
028400071108     c                   movel     ORMlor        WRMke3
028500071108     c                   movel     ORMinr        WRMke4
028600071108e   1c                   WHEN      R78ord  = '2'
028700071105     c                   movel     DSTpdr        WRMke1
028800071105     c                   movel     DSTnfv        WRMke2
028900071105     c                   movel     ORMorr        WRMke3
029000071105     c                   movel     ORMlor        WRMke4
029100071105     c                   movel     ORMinr        WRMke5
029200071108e   1c                   ENDSL
029300071105      *
029400071105     c                   clear                   FIOR16ds
029500071105     c                   movel     R78mdo        R16mdo
029600071105     c                   movel     R78oqo        R16oqo
029700071105     c                   movel     ORMpoe        R16poe
029800071105     c                   movel     ORMnsr        R16nsr
029900071105     c                   movel     ORMnor        R16nor
030000071105     c                   movel     ORMnrv        R16nrv
030100071105     c                   if        ORMdst > 0
030200071105     c                   eval      R16ris = 'R'
030300071105     c                   else
030400071105     c                   eval      R16ris = 'S'
030401080326      * se si tratta di stampa orm con allegati per distinte su PDA imposto il flag R16RIS
030402080326      * uguale ad "A"
030403080326     c                   If        wallega = 'A'
030404080326     c                   eval      R16ris = 'A'
030405080326     c                   endif
030406080326
030500071105     c                   endif
030600071105     c                   movel     FIOR16ds      WRMuni
030700071105      *                  __________________
030800071105     c                   WRITE     WFORM000
030900071105      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
031000071105     c                   add       1             wRec
031100071105      *
031200071105     c                   ENDSR
031300071105      *
031400071105      *---------------------------------------------------------------*
031500071105      *?Elaborazione singolo record da WrkF in QTEMP                 ?*
031600071105      *---------------------------------------------------------------*
031700071105     c     sr_ElabWF     BEGSR
031800071105      *
031900071105     c/exec sql
032000071105     c+  declare A1 cursor for
032100071105     c+  select *  from QTEMP/WFORM00F
032200071105     c+  order  by WRMke1, WRMke2, WRMke3, WRMke4, WRMke5
032300071105     c/end-exec
032400071105      *
032500071105     c/exec sql
032600071105     c+  open A1
032700071105     c/end-exec
032800071105      *
032900071105do  1c                   DO        *hival
033000071105      *
033100071105     c/exec sql
033200071105     c+  fetch next from A1 into :WFORMds
033300071105     c/end-exec
033400071105      *
033500071105     c                   Select
033600071105     c                   When      SQLcod = 100
033700071105     c                   leave
033800071105     c                   When      SQLcod < *zeros
033900071106     c                   leave
034000071105     c                   Other
034100071105     c                   exsr      call_FIOR16
034200071105     c                   EndSl
034300071105      *
034400071105e   1c                   ENDDO
034500071105      *
034600071105     c/exec sql
034700071105     c+  close A1
034800071105     c/end-exec
034900071105      *
035000071105     c                   ENDSR
035100070313      *
035200070313      *---------------------------------------------------------------*
035300070928      *?Richiamo del pgm. di stampa ORM (FIOR16R)                    ?*
035400070313      *---------------------------------------------------------------*
035500070928     c     Call_FIOR16   BEGSR
035600070313      *
035700071107      * Chiusura PrtF al superamento del n° limite di pagine indicato
035800071107      *   in tab. VPO (per riapertura al richiamo successivo)
035900071107      * N.B. - Ogni pag. A5 contiene 2 ORM !!!
036000071107if  1c                   IF            (R78ord =  '1'
036100071108     c                             and  WRMke1 <> SAVke1)
036200071108     c                             OR  (R78ord =  '2'
036300071107     c                             and (WRMke1 <> SAVke1
036400071107     c                              or  WRMke2 <> SAVke2))
036500071108sel 2c                   select
036600071108w   2c                   when      R78ord = '1'
036700071108     c                   eval      SAVke1 = WRMke1
036800071108w   2c                   when      R78ord = '2'
036900071107     c                   eval      SAVke1 = WRMke1
037000071107     c                   eval      SAVke2 = WRMke2
037100071108e   2c                   endsl
037200071108if  2c                   if        wCountORM >= (§VPOpagorm * 2)
037300071107     c                   clear                   FIOR16ds
037400071107     c                   eval      R16tla = 'C'
037500071107     c                   movel(p)  FIOR16ds      KPJBU
037600071107     c                   call      'FIOR16R'
037700071107     c                   parm                    KPJBA
037800071107     c                   movel     KPJBU         FIOR16ds
037900071107     c                   clear                   wCountORM
038000071107e   2c                   endif
038100071107e   1c                   ENDIF
038200071107      *
038300071107      * Stmpa O.R.M.
038400070928     c                   clear                   FIOR16ds
038500071106     c                   movel     WRMuni        FIOR16ds
038600071107     c                   movel(p)  FIOR16ds      KPJBU
038700071106     c                   call      'FIOR16R'
038800071106     c                   parm                    KPJBA
038900071107     c                   movel     KPJBU         FIOR16ds
039000071107     c                   add       1             wCountORM
039100070313      *
039200070313     c                   ENDSR
039300070827      *
039400071009      *---------------------------------------------------------------*
039500071009      *?Aggiornamento data di stampa in ORM PrePagato                ?*
039600071009      *---------------------------------------------------------------*
039700071009     c     UpdatFNORM    BEGSR
039800071009      *
039900071009if  1c                   if        NOT %open(FNORM00F)
040000071009     c                   open      FNORM00F
040100071009e   1c                   endif
040200071009      *
040300071009      * Gestisco allocazione del record
040400071009      *   con invio del msg all'utente che blocca il rcd
040500071009     c     wORMnrr       chain(e)  FNORM000
040600071009sel 1c                   select
040700071009w   1c                   when      %error
040800071009     c                   reset                   TRUL82ds
040900071009     c                   eval      UL82§rrn = wORMnrr
041000071009     c                   eval      UL82§mss = C_Msg_01
041100071009      * Chiamata al *PGM d utilità
041200071009     c                   call(e)   'TRUL82R'
041300071009     c                   parm                    TRUL82ds
041400071009     c     wORMnrr       chain     FNORM000
041500071009w   1c                   when      NOT %found(FNORM00F)
041600071009     c                   leavesr
041700071009e   1c                   endsl
041800071009      *
041900071009      * Data di stampa
042000071009     c                   eval      ORMdst = wDate
042100071009      * Data ultima variazione
042200071009     c                   eval      ORMdtt = wDate
042300071009      *
042400071009     c                   UPDATE    FNORM000
042500071009      *
042600071009     c                   ENDSR
042601080327      *---------------------------------------------------------------*
042602080327      *?Registrazione fase stampa orm in distinta                    ?*
042603080327      *---------------------------------------------------------------*
042604080327     c     sr_WriteFase  BEGSR
042605080327      *
042606080327     c                   clear                   fior90ds
042607080327
042608080327     c                   eval      or90tla = 'W'
042609080327     c                   eval      or90fas = 'STO'
042610080327     c                   z-add     dstfgs        or90fgs
042611080327     c                   z-add     dstnfv        or90ndc
042612080327     c                   eval      or90flgins = 'A'
042613080327
042614080327     c                   call      'FIOR90R'
042615080327     c                   parm                    kpjba
042616080327     c                   parm                    fior90ds
042617080327
042618080327     c                   ENDSR
042700071105      *
042800071105** - $Cmd
042900071106DLTF FILE(QTEMP/WFORM00F)                                              1
043000071106CRTDUPOBJ OBJ(WFORM00F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)     2
043100071107          NEWOBJ(WFORM00F) DATA(*NO)                                   3
