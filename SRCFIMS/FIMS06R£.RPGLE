000100050722      *PARMS OPTION(*NOXREF) TGTRLS(*CURRENT)
000200050722      *===============================================================*
000300050722      *?FIMS06R    * Stampa riepilogo statistica letture VDL         ?*
000400050722      *===============================================================*
000500070918      *
000600050722     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700070918      *
000800070918      *?A R C H I V I?------------------------------------------------*
000900070918      *
001000050722     fAZORG01L  if   e           k disk
001100050722      *
001200050722     fFIMS06D   cf   e             workstn
001300050722      *
001400070918      *?S C H I E R E?------------------------------------------------*
001500050722      *
001600050722      * Messaggi di errore
001700050830     d $Msg            s             78    dim(7) ctdata perrcd(1)
001800050722      *
001900070918      *?S T R U T T U R E   D A T I?----------------------------------*
002000050722      *
002100070918      * - Parametri
002200050722     d KPJBA         e ds
002300070918      * - Parametri x batch
002400061003     d FIMS09ds      e ds                  inz
002500050722      *
002600050722      * - Parametri x Controllo profilo utenti
002700050722     d TIBS34ds      e ds
002800050722      * - Ds di riferimento al file esterno AZUTE00F
002900050722     d AZUTEds       e ds                  extname(AZUTE00F)
003000050722      * - Ds per dati organigramma
003100070918     d dDatiUte      e ds
003200050722      *
003300050722     d Status         sds           333
003400050722     d   SDSpgm          *proc
003500050722      *
003600070918      * - Controllo/Inversione date
003700050722     d WLBdat          ds                  inz
003800050722     d  G02dat                 1      8  0 inz
003900050722     d  G02inv                 9     16  0 inz
004000050722     d  G02err                17     17    inz('3')
004100050722     d  G02tgi                18     22  0 inz
004200050722      *
004300070918      *?V A R I A B I L I?--------------------------------------------*
004400050722      *
004500070918      * - Flags
004600070918     d $Fine           s              1    inz(*off)
004700070918     d $InzD01         s              1    inz(*on)
004800070918      *
004900070918      * - Variabili riferite al data base o al display file
005000050722     d W1Cdti          s                   like(V1Cdti) inz
005100050722     d W1Cdtf          s                   like(V1Cdtf) inz
005200070918      *
005300070918      * - Parametri x QCMDEXC
005400070918     d Qcmd            s            100    inz
005500070918     d Qlen            s             15  5 inz(100)
005600050722
005700050722      *---------------------------------------------------------------*
005800070918      *?RIEPILOGO INDICATORI                                         ?*
005900050722      *---------------------------------------------------------------*
006000050722      * 10    - Comodo                                                *
006100050722      * 28    - Emette il messaggio di errore a video                 *
006200050722      * 41    - Data F.V. iniziale errata                             *
006300050722      * 42    - Data F.V. finale   errata                             *
006400050722      * 43    - Punto Operativo    errato                             *
006500050830      * 44    - Categoria F.V.     errata                             *
006600050722      * 90    - Generico di errore                                    *
006700050722      *---------------------------------------------------------------*
006800050722
006900050722     c     *Entry        plist
007000050722     c                   parm                    KPJBA
007100050722      *
007200050722      * Operazioni iniziali
007300070918     c                   exsr      RoutInz
007400050722      *
007500070918      * Gestione video - Filtro di lancio
007600070918do  1c                   dow       $Fine   =  *off
007700070918     c                   exsr      GesD01
007800070918e   1c                   enddo
007900050722      *
008000070918      * Fine
008100050722     c                   eval      *inLR   = *on
008200070918      *
008300070918      *?S U B R O U T I N E S?----------------------------------------*
008400070918      *
008500070918      *---------------------------------------------------------------*
008600070918      *?Operazioni Iniziali                                          ?*
008700070918      *---------------------------------------------------------------*
008800070918     c     RoutInz       BEGSR
008900050722      *
009000050722      * - Reperisco nome azienda per stampa PREPAGATI
009100050722     c     *dtaara       define    §azute        AZUTEds
009200050722     c     *dtaara       define    §datiute      dDATIUTE
009300050722     c                   in(E)     *dtaara
009400050722     c                   if        %ERROR or RSUT = *blanks
009500050722     c                   clear                   Tibs34Ds
009600050722     c                   call      'TIBS34R'
009700050722     c                   parm                    Tibs34Ds
009800050722     c                   in        *dtaara
009900050722     c                   endif
010000050722      *
010100050722     c                   movel     SDSpgm        V1Tpgm
010200050722      *
010300050722     c                   ENDSR
010400070918      *
010500070918      *---------------------------------------------------------------*
010600070918      *?Gestione tasto funzionale F09 da videata D01                 ?*
010700070918      *---------------------------------------------------------------*
010800070918     c     F09D01        BEGSR
010900070918      *
011000070918     c                   eval      Qcmd    =  'DSPDBF FILE'
011100070918     c                                     +  '(*LIBL/WFMSS00F) '
011200070918     c                                     +  'MBR(*FIRST) '
011300070918     c***                                  +  'RCDSLT(*YES) '
011400070918     c                                     +  'OUTPUT(*) '
011500070918     c                   call      'QCMDEXC'                            90
011600070918     c                   parm                    Qcmd
011700070918     c                   parm                    Qlen
011800070918      *
011900070918     c                   ENDSR
012000070918      *
012100070918      *---------------------------------------------------------------*
012200070918      *?Gestione videata D01                                         ?*
012300070918      *---------------------------------------------------------------*
012400070918     c     GesD01        BEGSR
012500070918      *
012600070918      * Inizializzazione videata
012700070918if  1c                   if        $InzD01 =  *on
012800070918     c                   exsr      InzD01
012900070918     c                   eval      $InzD01 =  *off
013000070918e   1c                   endif
013100070918      *
013200070918      * Emissione videata
013300070918     c                   exfmt     MS06D01
013400070918     c                   setoff                                       28  90
013500070918     c                   clear                   V1Dmsg
013600070918      *
013700070918sel 1c                   select
013800070918      *
013900070918      * F3=Fine
014000070918w   1c                   when      *inKC
014100070918     c                   exsr      F03D01
014200070918     c                   leavesr
014300070918      *
014400070918      * F9=Interrogazione file WFMSS00F
014500070918w   1c                   when      *inKI
014600070918     c                   exsr      F09D01
014700070918     c                   leavesr
014800070918      *
014900070918      * Invio / F6=Conferma
015000070918x   1c                   other
015100070918      * - Controllo dati immessi a video
015200070918     c                   exsr      CtrD01
015300070918if  2c                   if        NOT  *in90
015400070918     c                             and  *inKF
015500070918      * - Lancio batch
015600070918     c                   exsr      F06D01
015700070918     c                   leavesr
015800070918e   2c                   endif
015900070918      *
016000070918e   1c                   endsl
016100070918      *
016200070918     c                   ENDSR
016300070918      *
016400070918      *---------------------------------------------------------------*
016500070918      *?Inizializzazione videata D01                                 ?*
016600070918      *---------------------------------------------------------------*
016700070918     c     InzD01        BEGSR
016800070918      *
016900070918     c*** NO (completa)! clear                   MS06D01
017000070918      *
017100070918     c                   ENDSR
017200070918      *
017300070918      *---------------------------------------------------------------*
017400070918      *?Gestione tasto funzionale F03 da videata D01                 ?*
017500070918      *---------------------------------------------------------------*
017600070918     c     F03D01        BEGSR
017700070918      *
017800070918      * Chiudo il programma
017900070918     c                   eval      $Fine   =  *on
018000070918      *
018100070918     c                   ENDSR
018200070918      *
018300070918      *---------------------------------------------------------------*
018400070918      *?Gestione tasto funzionale F06 da videata D01                 ?*
018500070918      *---------------------------------------------------------------*
018600070918     c     F06D01        BEGSR
018700070918      *
018800070918     c                   clear                   FIMS09ds
018900070918     c                   eval      D09dti  = W1Cdti
019000070918     c                   eval      D09dtf  = W1Cdtf
019100070918     c                   eval      D09fil  = V1Cfil
019200070918     c***                eval      D09npg  = V1Cnpg
019300070918     c                   select
019400070918     c                   when      V1Cnpg  = *blanks
019500070918     c                   eval      D09npg  = 9
019600070918     c                   when      V1Cnpg  = 'P'
019700070918     c                   eval      D09npg  = 5
019800070918     c                   when      V1Cnpg  = 'A'
019900070918     c                   eval      D09npg  = 2
020000070918     c                   endsl
020100070918     c                   if        V1Cwf   = 'S'
020200070918     c                   eval      D09wf   = *on
020300070918     c                   else
020400070918     c                   eval      D09wf   = *off
020500070918     c                   endif
020600070918      *
020700070918     c                   movel(p)  FIMS09ds      KPJBU
020800070918     c                   eval      KCOAZ   = 'MS09'
020900070918      *
021000070918     c                   if        KNMUS   = *all'1'
021100070918     c                   call      'FIMS09C'
021200070918     c                   parm                    KPJBA
021300070918     c                   else
021400070918     c                   call      'BCH10'
021500070918     c                   parm                    KPJBA
021600070918     c                   endif
021700070918      *
021800070918     c                   ENDSR
021900050722
022000050722      *---------------------------------------------------------------*
022100070918      *?Controllo dati immessi in videata D01                        ?*
022200050722      *---------------------------------------------------------------*
022300070918     c     CtrD01        BEGSR
022400050722      *
022500050722     c                   movea     *zeros        *in(40)
022600050722      *
022700050722      * - Data iniziale
022800050722     c                   clear                   W1Cdti
022900050722if  1c                   if        V1Cdti =  *zeros
023000051010     c                   seton                                        284190
023100050722     c                   eval      V1Dmsg =  $Msg(01)
023200050722     c                   leavesr
023300050722x   1c                   else
023400050722     c                   clear                   WLBdat
023500050722     c                   z-add     V1Cdti        G02dat
023600050722     c                   call      'XSRDA8'
023700050722     c                   parm                    WLBdat
023800050722if  2c                   if        G02err =  *on
023900051010     c                   seton                                        284190
024000050722     c                   eval      V1Dmsg =  $Msg(02)
024100050722     c                   leavesr
024200050722x   2c                   else
024300050722     c                   z-add     G02dat        V1Cdti
024400050722     c                   z-add     G02inv        W1Cdti
024500050722e   2c                   endif
024600050722e   1c                   endif
024700050722      *
024800050722      * - Data finale
024900050722     c                   clear                   W1Cdtf
025000050722if  1c                   if        V1Cdtf =  *zeros
025100051010     c***                seton                                        284290
025200050722     c***                eval      V1Dmsg =  $Msg(01)
025300050722     c***                leavesr
025400050722     c                   z-add     W1Cdti        W1Cdtf
025500050722x   1c                   else
025600050722     c                   clear                   WLBdat
025700050722     c                   z-add     V1Cdtf        G02dat
025800050722     c                   call      'XSRDA8'
025900050722     c                   parm                    WLBdat
026000050722if  2c                   if        G02err =  *on
026100051010     c                   seton                                        284290
026200050722     c                   eval      V1Dmsg =  $Msg(02)
026300050722     c                   leavesr
026400050722x   2c                   else
026500050722     c                   z-add     G02dat        V1Cdtf
026600050722     c                   z-add     G02inv        W1Cdtf
026700050722e   2c                   endif
026800050722e   1c                   endif
026900050722      *
027000050722      * - Range
027100050722if  1c                   if        W1Cdti > W1Cdtf
027200051010     c                   seton                                        284190
027300050722     c                   eval      V1Dmsg =  $Msg(03)
027400050722     c                   leavesr
027500050722e   1c                   endif
027600050722      *
027700070918      * - Filiale obbligatoria
027800070918if  1c                   if        V1Cfil =  *zeros
027900051010     c                   seton                                        284390
028000070918     c                   eval      V1Dmsg =  $Msg(04)
028100050722     c                   leavesr
028200050722e   1c                   endif
028300070918      * - Filiale errata
028400070918     c                   clear                   V1Dfil
028500070918if  1c                   if        V1Cfil <  999
028600070918     c                   exsr      CtrFIL
028700051010if  2c                   if        *in90
028800051010     c                   seton                                        284390
028900070918     c                   eval      V1Dmsg =  $Msg(05)
029000050722     c                   leavesr
029100050722e   2c                   endif
029200070918     c                   movel     ORGdes        V1Dfil
029300050722e   1c                   endif
029400050830      * - Categoria F.V. errata
029500050830if  1c                   if            V1Cnpg <> *blanks
029600050830     c                             and V1Cnpg <> 'P'
029700050830     c                             and V1Cnpg <> 'A'
029800051010     c                   seton                                        284490
029900070918     c                   eval      V1Dmsg =  $Msg(06)
030000050830     c                   leavesr
030100050830e   1c                   endif
030200050722      *
030300050722     c                   ENDSR
030400050722
030500050722      *---------------------------------------------------------------*
030600070918      *?Controllo Filiale                                            ?*
030700050722      *---------------------------------------------------------------*
030800070918     c     CtrFIL        BEGSR
030900050722      *
031000050722     c                   clear                   ORGdes
031100070918     c                   move      V1Cfil        ORGfil
031200050722     c     ORGfil        chain     AZORG01L
031300050722if  1c                   if        not %found(AZORG01L)
031400050722     c                             or  ORGfva =  '*'
031500051010     c                   seton                                            90
031600050722e   1c                   endif
031700050722      *
031800050722     c                   ENDSR
031900050722
032000070918** - $MSG                                                                    *
032100050722Immettere una data                                                             01
032200050722Data errata                                                                    02
032300050722Data iniziale > Data finale                                                    03
032400050722Immettere il Punto Operativo                                                   04
032500050722Punto Operativo inesistente o annullato                                        05
032600050830Categoria F.V. errata                                                          06
032700050830Funzione eseguibile solo in ambiente di SEDE                                   07
