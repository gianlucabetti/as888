000100050722      *PARMS OPTION(*NOXREF) TGTRLS(*CURRENT)
000200130329     /*PRM dbgview(*source)
000300130329     /*END
000400050722      *===============================================================*
000500050722      *?FIMS06R    * Stampa riepilogo statistica letture VDL         ?*
000600050722      *===============================================================*
000700070918      *
000800050722     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000900070918      *
001000070918      *?A R C H I V I?------------------------------------------------*
001100070918      *
001200050722     fAZORG01L  if   e           k disk
001300050722      *
001400050722     fFIMS06D   cf   e             workstn
001500050722      *
001600070918      *?S C H I E R E?------------------------------------------------*
001700050722      *
001800050722      * Messaggi di errore
001900050830     d $Msg            s             78    dim(7) ctdata perrcd(1)
002000050722      *
002100070918      *?S T R U T T U R E   D A T I?----------------------------------*
002200050722      *
002300070918      * - Parametri
002400050722     d KPJBA         e ds
002500070918      * - Parametri x batch
002600130329     d FIMS07ds      e ds                  inz
002700050722      *
002800050722      * - Parametri x Controllo profilo utenti
002900050722     d TIBS34ds      e ds
003000050722      * - Ds di riferimento al file esterno AZUTE00F
003100050722     d AZUTEds       e ds                  extname(AZUTE00F)
003200050722      * - Ds per dati organigramma
003300070918     d dDatiUte      e ds
003400050722      *
003500050722     d Status         sds           333
003600050722     d   SDSpgm          *proc
003700050722      *
003800070918      * - Controllo/Inversione date
003900050722     d WLBdat          ds                  inz
004000050722     d  G02dat                 1      8  0 inz
004100050722     d  G02inv                 9     16  0 inz
004200050722     d  G02err                17     17    inz('3')
004300050722     d  G02tgi                18     22  0 inz
004400050722      *
004500070918      *?V A R I A B I L I?--------------------------------------------*
004600050722      *
004700070918      * - Flags
004800070918     d $Fine           s              1    inz(*off)
004900070918     d $InzD01         s              1    inz(*on)
005000070918      *
005100070918      * - Variabili riferite al data base o al display file
005200050722     d W1Cdti          s                   like(V1Cdti) inz
005300050722     d W1Cdtf          s                   like(V1Cdtf) inz
005400070918      *
005500070918      * - Parametri x QCMDEXC
005600070918     d Qcmd            s            100    inz
005700070918     d Qlen            s             15  5 inz(100)
005800050722
005900050722      *---------------------------------------------------------------*
006000070918      *?RIEPILOGO INDICATORI                                         ?*
006100050722      *---------------------------------------------------------------*
006200050722      * 10    - Comodo                                                *
006300050722      * 28    - Emette il messaggio di errore a video                 *
006400050722      * 41    - Data F.V. iniziale errata                             *
006500050722      * 42    - Data F.V. finale   errata                             *
006600050722      * 43    - Punto Operativo    errato                             *
006700050830      * 44    - Categoria F.V.     errata                             *
006800050722      * 90    - Generico di errore                                    *
006900050722      *---------------------------------------------------------------*
007000050722
007100050722     c     *Entry        plist
007200050722     c                   parm                    KPJBA
007300050722      *
007400050722      * Operazioni iniziali
007500070918     c                   exsr      RoutInz
007600050722      *
007700070918      * Gestione video - Filtro di lancio
007800070918do  1c                   dow       $Fine   =  *off
007900070918     c                   exsr      GesD01
008000070918e   1c                   enddo
008100050722      *
008200070918      * Fine
008300050722     c                   eval      *inLR   = *on
008400070918      *
008500070918      *?S U B R O U T I N E S?----------------------------------------*
008600070918      *
008700070918      *---------------------------------------------------------------*
008800070918      *?Operazioni Iniziali                                          ?*
008900070918      *---------------------------------------------------------------*
009000070918     c     RoutInz       BEGSR
009100050722      *
009200050722      * - Reperisco nome azienda per stampa PREPAGATI
009300050722     c     *dtaara       define    §azute        AZUTEds
009400050722     c     *dtaara       define    §datiute      dDATIUTE
009500050722     c                   in(E)     *dtaara
009600050722     c                   if        %ERROR or RSUT = *blanks
009700050722     c                   clear                   Tibs34Ds
009800050722     c                   call      'TIBS34R'
009900050722     c                   parm                    Tibs34Ds
010000050722     c                   in        *dtaara
010100050722     c                   endif
010200050722      *
010300050722     c                   movel     SDSpgm        V1Tpgm
010400050722      *
010500050722     c                   ENDSR
010600070918      *
010700070918      *---------------------------------------------------------------*
010800070918      *?Gestione tasto funzionale F09 da videata D01                 ?*
010900070918      *---------------------------------------------------------------*
011000070918     c     F09D01        BEGSR
011100070918      *
011200070918     c                   eval      Qcmd    =  'DSPDBF FILE'
011300070918     c                                     +  '(*LIBL/WFMSS00F) '
011400070918     c                                     +  'MBR(*FIRST) '
011500070918     c***                                  +  'RCDSLT(*YES) '
011600070918     c                                     +  'OUTPUT(*) '
011700070918     c                   call      'QCMDEXC'                            90
011800070918     c                   parm                    Qcmd
011900070918     c                   parm                    Qlen
012000070918      *
012100070918     c                   ENDSR
012200070918      *
012300070918      *---------------------------------------------------------------*
012400070918      *?Gestione videata D01                                         ?*
012500070918      *---------------------------------------------------------------*
012600070918     c     GesD01        BEGSR
012700070918      *
012800070918      * Inizializzazione videata
012900070918if  1c                   if        $InzD01 =  *on
013000070918     c                   exsr      InzD01
013100070918     c                   eval      $InzD01 =  *off
013200070918e   1c                   endif
013300070918      *
013400070918      * Emissione videata
013500070918     c                   exfmt     MS06D01
013600070918     c                   setoff                                       28  90
013700070918     c                   clear                   V1Dmsg
013800070918      *
013900070918sel 1c                   select
014000070918      *
014100070918      * F3=Fine
014200070918w   1c                   when      *inKC
014300070918     c                   exsr      F03D01
014400070918     c                   leavesr
014500070918      *
014600070918      * F9=Interrogazione file WFMSS00F
014700070918w   1c                   when      *inKI
014800070918     c                   exsr      F09D01
014900070918     c                   leavesr
015000070918      *
015100070918      * Invio / F6=Conferma
015200070918x   1c                   other
015300070918      * - Controllo dati immessi a video
015400070918     c                   exsr      CtrD01
015500070918if  2c                   if        NOT  *in90
015600070918     c                             and  *inKF
015700070918      * - Lancio batch
015800070918     c                   exsr      F06D01
015900070918     c                   leavesr
016000070918e   2c                   endif
016100070918      *
016200070918e   1c                   endsl
016300070918      *
016400070918     c                   ENDSR
016500070918      *
016600070918      *---------------------------------------------------------------*
016700070918      *?Inizializzazione videata D01                                 ?*
016800070918      *---------------------------------------------------------------*
016900070918     c     InzD01        BEGSR
017000070918      *
017100070918     c*** NO (completa)! clear                   MS06D01
017200070918      *
017300070918     c                   ENDSR
017400070918      *
017500070918      *---------------------------------------------------------------*
017600070918      *?Gestione tasto funzionale F03 da videata D01                 ?*
017700070918      *---------------------------------------------------------------*
017800070918     c     F03D01        BEGSR
017900070918      *
018000070918      * Chiudo il programma
018100070918     c                   eval      $Fine   =  *on
018200070918      *
018300070918     c                   ENDSR
018400070918      *
018500070918      *---------------------------------------------------------------*
018600070918      *?Gestione tasto funzionale F06 da videata D01                 ?*
018700070918      *---------------------------------------------------------------*
018800070918     c     F06D01        BEGSR
018900070918      *
019000130329     c                   clear                   FIMS07ds
019100130329     c                   eval      D07dti  = W1Cdti
019200130329     c                   eval      D07dtf  = W1Cdtf
019300130329     c                   eval      D07fil  = V1Cfil
019400130329     c***                eval      D07npg  = V1Cnpg
019500070918     c                   select
019600070918     c                   when      V1Cnpg  = *blanks
019700130329     c                   eval      D07npg  = 9
019800070918     c                   when      V1Cnpg  = 'P'
019900130329     c                   eval      D07npg  = 5
020000070918     c                   when      V1Cnpg  = 'A'
020100130329     c                   eval      D07npg  = 2
020200070918     c                   endsl
020300070918     c                   if        V1Cwf   = 'S'
020400130329     c                   eval      D07wf   = *on
020500070918     c                   else
020600130329     c                   eval      D07wf   = *off
020700070918     c                   endif
020800070918      *
020900130329     c                   movel(p)  FIMS07ds      KPJBU
021000130329     c                   eval      KCOAZ   = 'MS07'
021100070918      *
021200070918     c                   if        KNMUS   = *all'1'
021300130329     c                   call      'FIMS07C'
021400070918     c                   parm                    KPJBA
021500070918     c                   else
021600070918     c                   call      'BCH10'
021700070918     c                   parm                    KPJBA
021800070918     c                   endif
021900070918      *
022000070918     c                   ENDSR
022100050722
022200050722      *---------------------------------------------------------------*
022300070918      *?Controllo dati immessi in videata D01                        ?*
022400050722      *---------------------------------------------------------------*
022500070918     c     CtrD01        BEGSR
022600050722      *
022700050722     c                   movea     *zeros        *in(40)
022800050722      *
022900050722      * - Data iniziale
023000050722     c                   clear                   W1Cdti
023100050722if  1c                   if        V1Cdti =  *zeros
023200051010     c                   seton                                        284190
023300050722     c                   eval      V1Dmsg =  $Msg(01)
023400050722     c                   leavesr
023500050722x   1c                   else
023600050722     c                   clear                   WLBdat
023700050722     c                   z-add     V1Cdti        G02dat
023800050722     c                   call      'XSRDA8'
023900050722     c                   parm                    WLBdat
024000050722if  2c                   if        G02err =  *on
024100051010     c                   seton                                        284190
024200050722     c                   eval      V1Dmsg =  $Msg(02)
024300050722     c                   leavesr
024400050722x   2c                   else
024500050722     c                   z-add     G02dat        V1Cdti
024600050722     c                   z-add     G02inv        W1Cdti
024700050722e   2c                   endif
024800050722e   1c                   endif
024900050722      *
025000050722      * - Data finale
025100050722     c                   clear                   W1Cdtf
025200050722if  1c                   if        V1Cdtf =  *zeros
025300051010     c***                seton                                        284290
025400050722     c***                eval      V1Dmsg =  $Msg(01)
025500050722     c***                leavesr
025600050722     c                   z-add     W1Cdti        W1Cdtf
025700050722x   1c                   else
025800050722     c                   clear                   WLBdat
025900050722     c                   z-add     V1Cdtf        G02dat
026000050722     c                   call      'XSRDA8'
026100050722     c                   parm                    WLBdat
026200050722if  2c                   if        G02err =  *on
026300051010     c                   seton                                        284290
026400050722     c                   eval      V1Dmsg =  $Msg(02)
026500050722     c                   leavesr
026600050722x   2c                   else
026700050722     c                   z-add     G02dat        V1Cdtf
026800050722     c                   z-add     G02inv        W1Cdtf
026900050722e   2c                   endif
027000050722e   1c                   endif
027100050722      *
027200050722      * - Range
027300050722if  1c                   if        W1Cdti > W1Cdtf
027400051010     c                   seton                                        284190
027500050722     c                   eval      V1Dmsg =  $Msg(03)
027600050722     c                   leavesr
027700050722e   1c                   endif
027800050722      *
027900070918      * - Filiale obbligatoria
028000070918if  1c                   if        V1Cfil =  *zeros
028100051010     c                   seton                                        284390
028200070918     c                   eval      V1Dmsg =  $Msg(04)
028300050722     c                   leavesr
028400050722e   1c                   endif
028500070918      * - Filiale errata
028600070918     c                   clear                   V1Dfil
028700070918if  1c                   if        V1Cfil <  999
028800070918     c                   exsr      CtrFIL
028900051010if  2c                   if        *in90
029000051010     c                   seton                                        284390
029100070918     c                   eval      V1Dmsg =  $Msg(05)
029200050722     c                   leavesr
029300050722e   2c                   endif
029400070918     c                   movel     ORGdes        V1Dfil
029500050722e   1c                   endif
029600050830      * - Categoria F.V. errata
029700050830if  1c                   if            V1Cnpg <> *blanks
029800050830     c                             and V1Cnpg <> 'P'
029900050830     c                             and V1Cnpg <> 'A'
030000051010     c                   seton                                        284490
030100070918     c                   eval      V1Dmsg =  $Msg(06)
030200050830     c                   leavesr
030300050830e   1c                   endif
030400050722      *
030500050722     c                   ENDSR
030600050722
030700050722      *---------------------------------------------------------------*
030800070918      *?Controllo Filiale                                            ?*
030900050722      *---------------------------------------------------------------*
031000070918     c     CtrFIL        BEGSR
031100050722      *
031200050722     c                   clear                   ORGdes
031300070918     c                   move      V1Cfil        ORGfil
031400050722     c     ORGfil        chain     AZORG01L
031500050722if  1c                   if        not %found(AZORG01L)
031600050722     c                             or  ORGfva =  '*'
031700051010     c                   seton                                            90
031800050722e   1c                   endif
031900050722      *
032000050722     c                   ENDSR
032100050722
032200070918** - $MSG                                                                    *
032300050722Immettere una data                                                             01
032400050722Data errata                                                                    02
032500050722Data iniziale > Data finale                                                    03
032600050722Immettere il Punto Operativo                                                   04
032700050722Punto Operativo inesistente o annullato                                        05
032800050830Categoria F.V. errata                                                          06
032900050830Funzione eseguibile solo in ambiente di SEDE                                   07
