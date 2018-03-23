000100130402      *PARMS OPTION(*NOXREF) TGTRLS(*CURRENT)
000200130402     /*PRM dbgview(*source)
000300130402     /*END
000400130402      *===============================================================*
000500130402      *?FIMS09R * Riepilogo statistica letture Picking con Rulliera  ?*
000600130402      *===============================================================*
000700130402      *
000800130402     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000900130402      *
001000130402      *?A R C H I V I?------------------------------------------------*
001100130402      *
001200130402     fAZORG01L  if   e           k disk
001300130402      *
001400130402     fFIMS09D   cf   e             workstn
001500130402      *
001600130402      *?S C H I E R E?------------------------------------------------*
001700130402      *
001800130402      * -?Messaggi di errore?
001900130402     d $Msg            s             78    dim(7) ctdata perrcd(1)
002000130402      *
002100130402      *?S T R U T T U R E   D A T I?----------------------------------*
002200130402      *
002300130402      * -?Parametri?
002400130402     d KPJBA         e ds
002500130402      * -?Parametri x batch?
002600130402     d FIMS10ds      e ds                  inz
002700130402      *
002800130402      * -?Parametri x Controllo profilo utenti?
002900130402     d TIBS34ds      e ds
003000130402      * -?Ds di riferimento al file esterno AZUTE00F?
003100130402     d AZUTEds       e ds                  extname(AZUTE00F)
003200130402      * -?Ds per dati organigramma?
003300130402     d dDatiUte      e ds
003400130402      *
003500130402     d Status         sds           333
003600130402     d   SDSpgm          *proc
003700130402      *
003800130402      * -?Controllo/Inversione date?
003900130402     d WLBdat          ds                  inz
004000130402     d  G02dat                 1      8  0 inz
004100130402     d  G02inv                 9     16  0 inz
004200130402     d  G02err                17     17    inz('3')
004300130402     d  G02tgi                18     22  0 inz
004400130402      *
004500130402      *?V A R I A B I L I?--------------------------------------------*
004600130402      *
004700130402      * -?Flags booleani?
004800130402     d $Fine           s               n   inz(*off)
004900130402     d $InzD01         s               n   inz(*on)
005000130402      *
005100130402      * -?Variabili riferite al data base o al display file?
005200130402     d W1Cdti          s                   like(V1Cdti) inz
005300130402     d W1Cdtf          s                   like(V1Cdtf) inz
005400130402      *
005500130402      * -?Parametri x QCMDEXC?
005600130402     d Qcmd            s            100    inz
005700130402     d Qlen            s             15  5 inz(100)
005800130402
005900130402      *---------------------------------------------------------------*
006000130402      *?RIEPILOGO INDICATORI                                         ?*
006100130402      *---------------------------------------------------------------*
006200130402      * 10    - Comodo                                                *
006300130402      * 28    - Emette il messaggio di errore a video                 *
006400130402      * 41    - Data F.V. iniziale errata                             *
006500130402      * 42    - Data F.V. finale   errata                             *
006600130402      * 43    - Punto Operativo    errato                             *
006700130402      * 44    - Categoria F.V.     errata                             *
006800130402      * 90    - Generico di errore                                    *
006900130402      *---------------------------------------------------------------*
007000130402
007100130402     c     *Entry        plist
007200130402     c                   parm                    KPJBA
007300130402      *
007400130402      * -?Operazioni iniziali?
007500130402     c                   exsr      RoutInz
007600130402      *
007700130402      * -?Gestione video - Filtro di lancio?
007800130402do  1c                   dow       $Fine   =  *off
007900130402     c                   exsr      GesD01
008000130402e   1c                   enddo
008100130402      *
008200130402      * -?Fine?
008300130402     c                   eval      *inLR   = *on
008400130402      *
008500130402      *?S U B R O U T I N E S?----------------------------------------*
008600130402      *
008700130402      *---------------------------------------------------------------*
008800130402      *?Operazioni Iniziali                                          ?*
008900130402      *---------------------------------------------------------------*
009000130402     c     RoutInz       BEGSR
009100130402      *
009200130402      * -?Reperimento dati aziendali?
009300130402     c     *dtaara       define    §azute        AZUTEds
009400130402     c     *dtaara       define    §datiute      dDATIUTE
009500130402     c                   in(E)     *dtaara
009600130402     c                   if        %ERROR or RSUT = *blanks
009700130402     c                   clear                   Tibs34Ds
009800130402     c                   call      'TIBS34R'
009900130402     c                   parm                    Tibs34Ds
010000130402     c                   in        *dtaara
010100130402     c                   endif
010200130402      *
010300130402     c                   movel     SDSpgm        V1Tpgm
010400130402      *
010500130402     c                   ENDSR
010600130402      *
010700130402      *---------------------------------------------------------------*
010800130402      *?Gestione tasto funzionale F09 da videata D01                 ?*
010900130402      *---------------------------------------------------------------*
011000130402     c     F09D01        BEGSR
011100130402      *
011200130402     c                   eval      Qcmd    =  'DSPDBF FILE'
011300130402     c                                     +  '(*LIBL/WFMSS10F) '
011400130402     c                                     +  'MBR(*FIRST) '
011500130402     c***                                  +  'RCDSLT(*YES) '
011600130402     c                                     +  'OUTPUT(*) '
011700130402     c                   call      'QCMDEXC'                            90
011800130402     c                   parm                    Qcmd
011900130402     c                   parm                    Qlen
012000130402      *
012100130402     c                   ENDSR
012200130402      *
012300130402      *---------------------------------------------------------------*
012400130402      *?Gestione videata D01                                         ?*
012500130402      *---------------------------------------------------------------*
012600130402     c     GesD01        BEGSR
012700130402      *
012800130402      * -?Inizializzazione videata?
012900130402if  1c                   if        $InzD01 =  *on
013000130402     c                   exsr      InzD01
013100130402     c                   eval      $InzD01 =  *off
013200130402e   1c                   endif
013300130402      *
013400130402      * -?Emissione videata?
013500130402     c                   exfmt     MS09D01
013600130402     c                   setoff                                       28  90
013700130402     c                   clear                   V1Dmsg
013800130402      *
013900130402sel 1c                   select
014000130402      *
014100130402      * -?F3=Fine?
014200130402w   1c                   when      *inKC
014300130402     c                   exsr      F03D01
014400130402     c                   leavesr
014500130402      *
014600130402      * -?F9=Interrogazione file WFMSS10F?
014700130402w   1c                   when      *inKI
014800130402     c                   exsr      F09D01
014900130402     c                   leavesr
015000130402      *
015100130402      * -?Invio / F6=Conferma?
015200130402x   1c                   other
015300130402      * - -?Controllo dati immessi a video?
015400130402     c                   exsr      CtrD01
015500130402if  2c                   if        NOT  *in90
015600130402     c                             and  *inKF
015700130402      * - -?Lancio batch?
015800130402     c                   exsr      F06D01
015900130402     c                   leavesr
016000130402e   2c                   endif
016100130402      *
016200130402e   1c                   endsl
016300130402      *
016400130402     c                   ENDSR
016500130402      *
016600130402      *---------------------------------------------------------------*
016700130402      *?Inizializzazione videata D01                                 ?*
016800130402      *---------------------------------------------------------------*
016900130402     c     InzD01        BEGSR
017000130402      *
017100130402     c*** NO (completa)! clear                   MS09D01
017200130402      *
017300130402     c                   ENDSR
017400130402      *
017500130402      *---------------------------------------------------------------*
017600130402      *?Gestione tasto funzionale F03 da videata D01                 ?*
017700130402      *---------------------------------------------------------------*
017800130402     c     F03D01        BEGSR
017900130402      *
018000130402      * -?Chiusura del programma?
018100130402     c                   eval      $Fine   =  *on
018200130402      *
018300130402     c                   ENDSR
018400130402      *
018500130402      *---------------------------------------------------------------*
018600130402      *?Gestione tasto funzionale F06 da videata D01                 ?*
018700130402      *---------------------------------------------------------------*
018800130402     c     F06D01        BEGSR
018900130402      *
019000130402     c                   clear                   FIMS10ds
019100130402     c                   eval      D10dti  = W1Cdti
019200130402     c                   eval      D10dtf  = W1Cdtf
019300130402     c                   eval      D10fil  = V1Cfil
019400130402     c***                eval      D10npg  = V1Cnpg
019500130403     c*//                select
019600130403     c*//                when      V1Cnpg  = *blanks
019700130403     c*//                eval      D10npg  = 9
019800130403     c*//                when      V1Cnpg  = 'P'
019900130403     c*//                eval      D10npg  = 5
020000130403     c*//                when      V1Cnpg  = 'A'
020100130403     c*//                eval      D10npg  = 2
020200130403     c*//                endsl
020300130402     c                   if        V1Cwf   = 'S'
020400130402     c                   eval      D10wf   = *on
020500130402     c                   else
020600130402     c                   eval      D10wf   = *off
020700130402     c                   endif
020800130402      *
020900130402     c                   movel(p)  FIMS10ds      KPJBU
021000130402     c                   eval      KCOAZ   = 'MS10'
021100130402      *
021200130402     c                   if        KNMUS   = *all'1'
021300130402     c                   call      'FIMS10R'
021400130402     c                   parm                    KPJBA
021500130402     c                   else
021600130402     c                   call      'BCH10'
021700130402     c                   parm                    KPJBA
021800130402     c                   endif
021900130404      *
022000130404     c                   eval      $Fine   =  *on
022100130402      *
022200130402     c                   ENDSR
022300130402
022400130402      *---------------------------------------------------------------*
022500130402      *?Controllo dati immessi in videata D01                        ?*
022600130402      *---------------------------------------------------------------*
022700130402     c     CtrD01        BEGSR
022800130402      *
022900130402     c                   movea     *zeros        *in(40)
023000130402      *
023100130402      * -?Data iniziale?
023200130402     c                   clear                   W1Cdti
023300130402if  1c                   if        V1Cdti =  *zeros
023400130402     c                   seton                                        284190
023500130402     c                   eval      V1Dmsg =  $Msg(01)
023600130402     c                   leavesr
023700130402x   1c                   else
023800130402     c                   clear                   WLBdat
023900130402     c                   z-add     V1Cdti        G02dat
024000130402     c                   call      'XSRDA8'
024100130402     c                   parm                    WLBdat
024200130402if  2c                   if        G02err =  *on
024300130402     c                   seton                                        284190
024400130402     c                   eval      V1Dmsg =  $Msg(02)
024500130402     c                   leavesr
024600130402x   2c                   else
024700130402     c                   z-add     G02dat        V1Cdti
024800130402     c                   z-add     G02inv        W1Cdti
024900130402e   2c                   endif
025000130402e   1c                   endif
025100130402      *
025200130402      * -?Data finale?
025300130402     c                   clear                   W1Cdtf
025400130402if  1c                   if        V1Cdtf =  *zeros
025500130402     c***                seton                                        284290
025600130402     c***                eval      V1Dmsg =  $Msg(01)
025700130402     c***                leavesr
025800130402     c                   z-add     W1Cdti        W1Cdtf
025900130402x   1c                   else
026000130402     c                   clear                   WLBdat
026100130402     c                   z-add     V1Cdtf        G02dat
026200130402     c                   call      'XSRDA8'
026300130402     c                   parm                    WLBdat
026400130402if  2c                   if        G02err =  *on
026500130402     c                   seton                                        284290
026600130402     c                   eval      V1Dmsg =  $Msg(02)
026700130402     c                   leavesr
026800130402x   2c                   else
026900130402     c                   z-add     G02dat        V1Cdtf
027000130402     c                   z-add     G02inv        W1Cdtf
027100130402e   2c                   endif
027200130402e   1c                   endif
027300130402      *
027400130402      * -?Range?
027500130402if  1c                   if        W1Cdti > W1Cdtf
027600130402     c                   seton                                        284190
027700130402     c                   eval      V1Dmsg =  $Msg(03)
027800130402     c                   leavesr
027900130402e   1c                   endif
028000130402      *
028100130402      * -?Filiale obbligatoria?
028200130402if  1c                   if        V1Cfil =  *zeros
028300130402     c                   seton                                        284390
028400130402     c                   eval      V1Dmsg =  $Msg(04)
028500130402     c                   leavesr
028600130402e   1c                   endif
028700130402      * -?Filiale errata?
028800130402     c                   clear                   V1Dfil
028900130402if  1c                   if        V1Cfil <  999
029000130402     c                   exsr      CtrFIL
029100130402if  2c                   if        *in90
029200130402     c                   seton                                        284390
029300130402     c                   eval      V1Dmsg =  $Msg(05)
029400130402     c                   leavesr
029500130402e   2c                   endif
029600130402     c                   movel     ORGdes        V1Dfil
029700130402e   1c                   endif
029800130402      * -?Categoria F.V. errata?
029900130403if  1c*//                if            V1Cnpg <> *blanks
030000130403     c*//                          and V1Cnpg <> 'P'
030100130403     c*//                          and V1Cnpg <> 'A'
030200130403     c*//                seton                                        284490
030300130403     c*//                eval      V1Dmsg =  $Msg(06)
030400130403     c*//                leavesr
030500130403e   1c*//                endif
030600130402      *
030700130402     c                   ENDSR
030800130402
030900130402      *---------------------------------------------------------------*
031000130402      *?Controllo Filiale                                            ?*
031100130402      *---------------------------------------------------------------*
031200130402     c     CtrFIL        BEGSR
031300130402      *
031400130402     c                   clear                   ORGdes
031500130402     c                   move      V1Cfil        ORGfil
031600130402     c     ORGfil        chain     AZORG01L
031700130402if  1c                   if        not %found(AZORG01L)
031800130402     c                             or  ORGfva =  '*'
031900130402     c                   seton                                            90
032000130402e   1c                   endif
032100130402      *
032200130402     c                   ENDSR
032300130402      *
032400130402      *?S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E?*
032500130402      *
032600130402** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
032700130402Immettere una data                                                             01
032800130402Data errata                                                                    02
032900130402Data iniziale > Data finale                                                    03
033000130402Immettere la Filiale                                                           04
033100130402Filiale inesistente o annullata                                                05
033200130403Categoria F.V. errata -------------------------------------------- L I B E R O 06
033300130402Funzione eseguibile solo in ambiente di SEDE                                   07
