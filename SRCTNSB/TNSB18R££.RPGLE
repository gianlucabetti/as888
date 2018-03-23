000100050809      *===============================================================*
000200050809      *?TNSB18R * Ricerca nominativi in bolle sede per G.d.F.        ?*
000300050809      *===============================================================*
000400050902      *
000500050809     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600050902      *
000700050902      *?A R C H I V I?------------------------------------------------*
000800050902      *
000900070702     fTNSB18D   cf   e             workstn infds(DSfmt)
001000050809      *
001100050902      *?C O S T A N T I?----------------------------------------------*
001200050809      *
001300070702      * - Tasti di funzione
001400070702     d Enter           c                   const(x'F1')
001500070702     d RollDwn         c                   const(x'F4')
001600070702     d RollUp          c                   const(x'F5')
001700050809      *
001800050902      *?S C H I E R E?------------------------------------------------*
001900050809      *
002000050809      * - Messaggi di errore
002100100128     d $Msg            s             78    dim(12) ctdata  perrcd(1)
002200050809      *
002300050902      *?S T R U T T U R E   D A T I?----------------------------------*
002400050809      *
002500050809      * - Parametri
002600050809     d KPJBA         e ds
002700050809     d TNSB19ds      e ds                  inz
002800070629      *
002900070629      * - Parametri per gestione parametri batch
003000070629     d TRUL01ds      e ds                  inz
003100070629     d   UL01opz     e                     inz(*on)
003200070629     d   UL01err     e                     inz(*off)
003300050809      *
003400050809      * - Parametri x Controllo profilo utenti
003500070704     d TIBS34ds      e ds                  inz
003600050809      * - Ds di riferimento al file esterno AZUTE00F
003700050809     d AZUTEds       e ds                  extname(AZUTE00F)
003800050809      * - Ds per dati organigramma
003900070704     d dDatiUte      e ds
004000051024      *
004100051024      * - Decodifica clienti
004200051024     d TIBS69ds      e ds                  inz
004300051024     d   I69tla      e                     inz('C')
004400051024     d ds_CNACO      e ds                  extname(CNACO00F) inz
004500051024     d ds_CNIND      e ds                  extname(CNIND00F) inz
004600051024     d ds_CNCLP      e ds                  extname(CNCLP00F) inz
004700051024     d ds_FNCLS      e ds                  extname(FNCLS00F) inz
004800050809      *
004900050809     d Status         sds           333
005000050809     d   SDSpgm          *proc
005100070702      *
005200070702     d dsFMT           ds           512
005300070702     d  £Tasto               369    369
005400050809      *
005500050809      * - Controllo/Inversione date
005600050809     d WLBdat          ds                  inz
005700050809     d   G08dat                       8  0 inz
005800050809     d   G08inv                       8  0 inz
005900050809     d   G08err                       1    inz('3')
006000050809     d   G08tgi                       5  0 inz
006100070629      *
006200070629     d ds_RS           ds                  inz
006300070629     d   V1Crs1                            inz
006400070629     d   V1Crs2                            inz
006500070629     d   V1Crs3                            inz
006600070629     d   V1Crs4                            inz
006700070629     d   V1Crs5                            inz
006800070629     d   $V1Crs                1    150    inz     dim (5)
006900051024      *
007000051024     d ds_KSC          ds                  inz
007100051024     d   V1Ccc1                            inz
007200051024     d   V1Ccc2                            inz
007300051024     d   V1Ccc3                            inz
007400051024     d   V1Ccc4                            inz
007500070629     d   V1Ccc5                            inz
007600070629     d   $CC                   1     35  0 inz     dim (5)
007700051024     d ds_KSCD         ds                  inz
007800051024     d   V1Dcc1                            inz
007900051024     d   V1Dcc2                            inz
008000051024     d   V1Dcc3                            inz
008100051024     d   V1Dcc4                            inz
008200070629     d   V1Dcc5                            inz
008300070808     d   $CCD                  1    115    inz     dim (5)
008400050809      *
008500050902      *?V A R I A B I L I?--------------------------------------------*
008600050809      *
008700050809      * - Flags
008800100128     d $Fine           s               n   inz(*off)
008900100128     d $InzD01         s               n   inz(*on)
009000100128     d $ErrSisInf      s               n   inz
009100050809      *
009200050809      * - Indici di schiera
009300050809      *
009400050809      * - Variabili riferite al data base o al display file
009500050831     d W1Cdal          s                   inz     like(SB19dal)
009600050831     d W1Cal           s                   inz     like(SB19al)
009700050809      *
009800050809      * - Variabili definite a programma
009900050902      *
010000050902      * Parametri x QCMDEXC
010100050902     d Qcmd            s            100    inz
010200050902     d Qlen            s             15  5 inz(100)
010300050809      *
010400050902      *?K E Y - L I S T?----------------------------------------------*
010500050809      *
010600050902      *
010700050902      *?I N D I C A T O R I?------------------------------------------*
010800050902      *
010900051024     d XX              s              3  0 inz
011000050902      *
011100050902      *===============================================================*
011200050902      *
011300050809     c     *Entry        plist
011400050809     c                   parm                    KPJBA
011500050809      *
011600050809      * Operazioni Iniziali
011700050809     c                   exsr      RoutInz
011800050809      *
011900050809      * Gestione video - Filtro di lancio
012000050809do  1c                   dow       $Fine   =  *off
012100070808     c                   exsr      GesD01
012200050809e   1c                   enddo
012300050809      *
012400050809      * Fine
012500051024     c                   reset                   TIBS69ds
012600051024     c                   clear                   ds_CNACO
012700051024     c                   clear                   ds_CNIND
012800051024     c                   clear                   ds_CNCLP
012900051024     c                   clear                   ds_FNCLS
013000051024     c                   call      'TIBS69R'
013100051024     c                   parm                    TIBS69ds
013200051024     c                   parm                    ds_CNACO
013300051024     c                   parm                    ds_CNIND
013400051024     c                   parm                    ds_CNCLP
013500051024     c                   parm                    ds_FNCLS
013600050809     c                   movel     *on           *inLR
013700050902      *
013800050902      *?S U B R O U T I N E S?----------------------------------------*
013900050902      *
014000050809      *---------------------------------------------------------------*
014100050902      *?Operazioni Iniziali                                          ?*
014200050809      *---------------------------------------------------------------*
014300050809     c     RoutInz       BEGSR
014400050809      *
014500050809      * Reperisco dati job
014600050809     c                   exsr      DatiJob
014700050809      *
014800050809     c                   movel     SDSpgm        V1Tpgm
014900050809      *
015000050809     c                   ENDSR
015100050809      *
015200050809      *---------------------------------------------------------------*
015300050902      *?Reperimento Dati del job (Utente/Operativi)                  ?*
015400050809      *---------------------------------------------------------------*
015500050809     c     DatiJob       BEGSR
015600050809      *
015700050809     c     *dtaara       define    §azute        azuteds
015800050809     c     *dtaara       define    §datiute      ddatiute
015900050809      *
016000050809     c                   in(E)     *dtaara
016100050809     c                   IF        %ERROR or RSUT = *blanks
016200050809     c                   clear                   Tibs34Ds
016300050809     c                   call      'TIBS34R'
016400050809     c                   parm                    Tibs34Ds
016500050809     c                   in        *dtaara
016600050809     c                   ENDIF
016700050809      *
016800050809     c                   ENDSR
016900050809      *
017000050809      *---------------------------------------------------------------*
017100050902      *?Gestione videata D01                                         ?*
017200050809      *---------------------------------------------------------------*
017300050809     c     GesD01        BEGSR
017400050809      *
017500050809      * Inizializzo la videata
017600050809if  1c                   if        $InzD01 =  *on
017700050809     c                   exsr      InzD01
017800050809     c                   eval      $InzD01 =  *off
017900050809e   1c                   endif
018000050809      *
018100050809      * Scrivo la testata e la riga tasti funzionali abilitati
018200100128if  1c*//                if        NOT *in90
018300050809     c                   write     SB18T01
018400050809     c                   write     SB18Z01
018500100128e   1c*//                endif
018600050809      * Emetto la videata
018700050809     c                   exfmt     SB18D01
018800050809     c                   setoff                                       28  90
018900050809     c                   clear                   V1Dmsg
019000061228      *
019100061228sel 1c                   select
019200100128      *
019300100128      * Sistema Informativo Errato
019400100128w   1c                   when      $ErrSisInf
019500100128     c                   exsr      F03D01
019600100128     c                   leavesr
019700050809      *
019800050809      * F3=Fine
019900061228w   1c                   when      *inKC
020000050809     c                   exsr      F03D01
020100050809     c                   leavesr
020200050902      *
020300050902      * F9=Interrogazione file WFNIB00F
020400061228w   1c                   when      *inKI
020500050902     c                   exsr      F09D01
020600050902     c                   leavesr
020700050809      *
020800070808      * Invio / F6=Conferma
020900061228x   1c                   other
021000070808      * - Controllo dati immessi a video
021100050809     c                   exsr      CtrD01
021200061228if  2c                   if        NOT  *in90
021300061228     c                             and  *inKF
021400070808      * - Lancio batch
021500050809     c                   exsr      F06D01
021600050809     c                   leavesr
021700061228e   2c                   endif
021800061228      *
021900061228e   1c                   endsl
022000050809      *
022100050809     c                   ENDSR
022200050809      *
022300050809      *---------------------------------------------------------------*
022400050902      *?Inizializzazione videata D01                                 ?*
022500050809      *---------------------------------------------------------------*
022600050809     c     InzD01        BEGSR
022700050809      *
022800050809     c                   clear                   SB18D01
022900050809      *
023000050902     c                   eval      V1Cric  =  'E'
023100100128      *
023200100128     c                   if        KNSIF  <> 'GAITRA201 '   and
023300100128     c                             %subst(KNSIF:1:7) <> 'GAITRAP'
023400100128     c                   eval      $ErrSisInf = *on
023500100128     c                   eval      $Fine   = *on
023600100128     c                   seton                                        28  90
023700100128     c                   eval      V1Dmsg  = $Msg(12)
023800100128     c                   leavesr
023900100128     c                   endif
024000050809      *
024100050809     c                   ENDSR
024200050809      *
024300050809      *---------------------------------------------------------------*
024400050902      *?Gestione tasto funzionale F03 da videata D01                 ?*
024500050809      *---------------------------------------------------------------*
024600050809     c     F03D01        BEGSR
024700050809      *
024800050809      * Chiudo il programma
024900050809     c                   eval      $Fine   =  *on
025000050809      *
025100050809     c                   ENDSR
025200050809      *
025300050809      *---------------------------------------------------------------*
025400050902      *?Gestione tasto funzionale F06 da videata D01                 ?*
025500050809      *---------------------------------------------------------------*
025600050809     c     F06D01        BEGSR
025700050809      *
025800050809      * Preparazione ds parametri
025900050809     c                   clear                   TNSB19ds
026000070704     c                   eval      SB19cwf =  V1Cclrwf
026100070704     c                   eval      SB19dal =  W1Cdal
026200070704     c                   eval      SB19al  =  W1Cal
026300051024if  1c                   IF             V1Ccc1 =  *zeros
026400051024     c                             and  V1Ccc2 =  *zeros
026500051024     c                             and  V1Ccc3 =  *zeros
026600051024     c                             and  V1Ccc4 =  *zeros
026700070629     c                             and  V1Ccc5 =  *zeros
026800070704     c                   eval      SB19ric =  V1Cric
026900050809     c                   eval      SB19rs1 =  V1Crs1
027000050809     c                   eval      SB19rs2 =  V1Crs2
027100050809     c                   eval      SB19rs3 =  V1Crs3
027200050809     c                   eval      SB19rs4 =  V1Crs4
027300070629     c                   eval      SB19rs5 =  V1Crs5
027400050809     c                   eval      SB19in1 =  V1Cin1
027500050809     c                   eval      SB19in2 =  V1Cin2
027600050809     c                   eval      SB19in3 =  V1Cin3
027700050809     c                   eval      SB19in4 =  V1Cin4
027800070629     c                   eval      SB19in5 =  V1Cin5
027900070629     c                   eval      SB19lo1 =  V1Clo1
028000070629     c                   eval      SB19lo2 =  V1Clo2
028100070629     c                   eval      SB19lo3 =  V1Clo3
028200070629     c                   eval      SB19lo4 =  V1Clo4
028300070629     c                   eval      SB19lo5 =  V1Clo5
028400050809     c                   eval      SB19pr1 =  V1Cpr1
028500050809     c                   eval      SB19pr2 =  V1Cpr2
028600050809     c                   eval      SB19pr3 =  V1Cpr3
028700050809     c                   eval      SB19pr4 =  V1Cpr4
028800070629     c                   eval      SB19pr5 =  V1Cpr5
028900051024x   1c                   ELSE
029000070704     c                   eval      SB19ric =  'K'
029100051024     c                   if        V1Ccc1  >  *zeros
029200051024     c                   movel(p)  V1Ccc1        SB19rs1
029300051024     c                   endif
029400051024     c                   if        V1Ccc2  >  *zeros
029500051024     c                   movel(p)  V1Ccc2        SB19rs2
029600051024     c                   endif
029700051024     c                   if        V1Ccc3  >  *zeros
029800051024     c                   movel(p)  V1Ccc3        SB19rs3
029900051024     c                   endif
030000051024     c                   if        V1Ccc4  >  *zeros
030100051024     c                   movel(p)  V1Ccc4        SB19rs4
030200051024     c                   endif
030300070629     c                   if        V1Ccc5  >  *zeros
030400070629     c                   movel(p)  V1Ccc5        SB19rs5
030500070629     c                   endif
030600051024e   1c                   ENDIF
030700070629     c***                movel(p)  TNSB19ds      KPJBU
030800061228      *
030900061228     c                   eval      KCOAZ   =  'SB19'
031000070629      *
031100070629     c                   reset                   TRUL01ds
031200070629     c*** già così:      eval      UL01opz = *on
031300070629     c                   eval      UL01azn = KCOAZ
031400070629     c                   movel(p)  TNSB19ds      UL01prm
031500070629     c*** già così:      eval      UL01err = *off
031600070629     c                   call      'TRUL01R'
031700070629     c                   parm                    TRUL01ds
031800070629if  1c                   if        UL01err = *on
031900070629     c                   seton                                        28  90
032000070629     c                   eval      V1Dmsg  = $Msg(11)
032100070629     c                   leavesr
032200070629e   1c                   endif
032300070629     c                   movel(p)  UL01prg       KPJBU
032400071126     c                   eval      KPJBU   = %trimr(KPJBU)
032500071126     c                                     + '-'
032600071126     c                                     + %subst( UL01prm : 1 :
032700071126     c                                       %len(KPJBU) - 1 -
032800071126     c                                         %len(%trimr(KPJBU)) )
032900050809      *
033000050809      * Sottomissione lavoro batch
033100070628     c***                call      'BCH09'
033200070628     c***                parm                    KPJBA
033300050809      *
033400061220     c                   if        KNMUS   =  *all'1'
033500061220     c                   call      'TNSB19R'
033600061220     c                   parm                    KPJBA
033700061220     c                   else
033800061220     c                   call      'BCH10'
033900050809     c                   parm                    KPJBA
034000061220     c                   endif
034100050809      *
034200050809      * Chiudo il programma
034300050809     c                   eval      $Fine   =  *on
034400050809      *
034500050809     c                   ENDSR
034600050902      *
034700050902      *---------------------------------------------------------------*
034800050902      *?Gestione tasto funzionale F09 da videata D01                 ?*
034900050902      *---------------------------------------------------------------*
035000050902     c     F09D01        BEGSR
035100050902      *
035200050902     c                   eval      Qcmd    =  'DSPDBF FILE'
035300050902     c                                     +  '(*LIBL/WFNIB00F) '
035400050902     c                                     +  'MBR(*FIRST) '
035500050902     c***                                  +  'RCDSLT(*YES) '
035600050902     c                                     +  'OUTPUT(*) '
035700050902     c                   call      'QCMDEXC'                            90
035800050902     c                   parm                    Qcmd
035900050902     c                   parm                    Qlen
036000050902      *
036100050902     c                   ENDSR
036200050809      *
036300050809      *---------------------------------------------------------------*
036400050902      *?Controllo dati immessi in videata D01                        ?*
036500050809      *---------------------------------------------------------------*
036600050809     c     CtrD01        BEGSR
036700050809      *
036800050809     c                   movea     *zeros        *in(50)
036900050809      *
037000050809      * ragione sociale: obbligatoria almeno una selezione
037100051024      *                  SE non inseriti codici
037200070629if  1c                   if             V1Crs1 =  *blanks
037300050809     c                             and  V1Crs2 =  *blanks
037400050809     c                             and  V1Crs3 =  *blanks
037500050809     c                             and  V1Crs4 =  *blanks
037600070629     c                             and  V1Crs5 =  *blanks
037700061201     c                             and  V1Cin1 =  *blanks
037800061201     c                             and  V1Cin2 =  *blanks
037900061201     c                             and  V1Cin3 =  *blanks
038000061201     c                             and  V1Cin4 =  *blanks
038100070629     c                             and  V1Cin5 =  *blanks
038200070629     c                             and  V1Clo1 =  *blanks
038300070629     c                             and  V1Clo2 =  *blanks
038400070629     c                             and  V1Clo3 =  *blanks
038500070629     c                             and  V1Clo4 =  *blanks
038600070629     c                             and  V1Clo5 =  *blanks
038700061201     c                             and  V1Cpr1 =  *blanks
038800061201     c                             and  V1Cpr2 =  *blanks
038900061201     c                             and  V1Cpr3 =  *blanks
039000061201     c                             and  V1Cpr4 =  *blanks
039100070629     c                             and  V1Cpr5 =  *blanks
039200070629     c                             and  V1Ccc1 =  *zeros
039300070629     c                             and  V1Ccc2 =  *zeros
039400070629     c                             and  V1Ccc3 =  *zeros
039500070629     c                             and  V1Ccc4 =  *zeros
039600070629     c                             and  V1Ccc5 =  *zeros
039700050809     c                   seton                                        512890
039800070629     c                   eval      V1Dmsg  = $Msg(1)
039900050809     c                   leavesr
040000070629e   1c                   endif
040100070629      *
040200070629      * Controllo dei caratteri
040300070629     c                   clear                   xx
040400070629do  1c                   DOW       xx      < %elem($V1Crs)
040500070629     c                   add       1             xx
040600070629      * "apostrofi" non ammessi né in ragione sociale né in indirizzo
040700070629if  2c                   if             $V1Crs(xx) <> *blanks
040800070629     c                             and  %scan('''':%trimr($V1Crs(xx)))
040900070629     c                                             >  *zeros
041000061228     c                   seton                                        512890
041100070629     c                   eval      *in(50+xx)      =  *on
041200070629     c                   eval      V1Dmsg          =  $Msg(9)
041300061228     c                   leavesr
041400070629e   2c                   endif
041500070629      * "percentuali" non ammessi né in ragione sociale né in indirizzo
041600070629if  2c                   if             $V1Crs(xx) <> *blanks
041700070629     c                             and  %scan('%':%trimr($V1Crs(xx)))
041800070629     c                                             >  *zeros
041900070629     c                   seton                                        512890
042000070629     c                   eval      V1Dmsg          =  $Msg(10)
042100070629     c                   leavesr
042200070629e   2c                   endif
042300070629e   1c                   ENDDO
042400070629      *
042500061219      * provincia .....: obbligatoria almeno una selezione
042600061219      *                  SE inserito almeno un solo indirizzo
042700070629if  1c                   IF             V1Crs1 =  *blanks
042800061205     c                             and  V1Crs2 =  *blanks
042900061205     c                             and  V1Crs3 =  *blanks
043000061205     c                             and  V1Crs4 =  *blanks
043100070629     c                             and  V1Crs5 =  *blanks
043200061205     c                             and (V1Cin1 <> *blanks
043300061205     c                              or  V1Cin2 <> *blanks
043400061205     c                              or  V1Cin3 <> *blanks
043500070629     c                              or  V1Cin4 <> *blanks
043600070629     c                              or  V1Cin5 <> *blanks)
043700070629     c                             and  V1Clo1 =  *blanks
043800070629     c                             and  V1Clo2 =  *blanks
043900070629     c                             and  V1Clo3 =  *blanks
044000070629     c                             and  V1Clo4 =  *blanks
044100070629     c                             and  V1Clo5 =  *blanks
044200061205     c                             and  V1Cpr1 =  *blanks
044300061205     c                             and  V1Cpr2 =  *blanks
044400061205     c                             and  V1Cpr3 =  *blanks
044500061205     c                             and  V1Cpr4 =  *blanks
044600070629     c                             and  V1Cpr5 =  *blanks
044700070629     c                             and  V1Ccc1 =  *zeros
044800070629     c                             and  V1Ccc2 =  *zeros
044900070629     c                             and  V1Ccc3 =  *zeros
045000070629     c                             and  V1Ccc4 =  *zeros
045100070629     c                             and  V1Ccc5 =  *zeros
045200070629     c                   seton                                        662890
045300061205     c                   eval      V1Dmsg = $Msg(8)
045400061205     c                   leavesr
045500070629e   1c                   ENDIF
045600051024      *
045700051024      * codice cliente: inibisce l'immissione di altri dati
045800070629if  1c                   IF            (V1Ccc1 <> *zeros
045900051024     c                              or  V1Ccc2 <> *zeros
046000051024     c                              or  V1Ccc3 <> *zeros
046100051024     c                              or  V1Ccc4 <> *zeros)
046200051024     c                             and (V1Crs1 <> *blanks
046300051024     c                              or  V1Crs2 <> *blanks
046400051024     c                              or  V1Crs3 <> *blanks
046500051024     c                              or  V1Crs4 <> *blanks
046600070629     c                              or  V1Crs5 <> *blanks
046700051024     c                              or  V1Cin1 <> *blanks
046800051024     c                              or  V1Cin2 <> *blanks
046900051024     c                              or  V1Cin3 <> *blanks
047000051024     c                              or  V1Cin4 <> *blanks
047100070629     c                              or  V1Cin5 <> *blanks
047200070629     c                              or  V1Clo1 <> *blanks
047300070629     c                              or  V1Clo2 <> *blanks
047400070629     c                              or  V1Clo3 <> *blanks
047500070629     c                              or  V1Clo4 <> *blanks
047600070629     c                              or  V1Clo5 <> *blanks
047700051024     c                              or  V1Cpr1 <> *blanks
047800051024     c                              or  V1Cpr2 <> *blanks
047900051024     c                              or  V1Cpr3 <> *blanks
048000070629     c                              or  V1Cpr4 <> *blanks
048100070629     c                              or  V1Cpr5 <> *blanks)
048200051024sel 2c                   select
048300051024w   2c                   when      V1Crs1 <> *blanks
048400051024     c                   eval      *in51  =  *on
048500051024w   2c                   when      V1Crs2 <> *blanks
048600070629     c                   eval      *in52  =  *on
048700051024w   2c                   when      V1Crs3 <> *blanks
048800070629     c                   eval      *in53  =  *on
048900051024w   2c                   when      V1Crs4 <> *blanks
049000070629     c                   eval      *in54  =  *on
049100070629w   2c                   when      V1Crs5 <> *blanks
049200070629     c                   eval      *in55  =  *on
049300051024w   2c                   when      V1Cin1 <> *blanks
049400070629     c                   eval      *in56  =  *on
049500051024w   2c                   when      V1Cin2 <> *blanks
049600070629     c                   eval      *in57  =  *on
049700051024w   2c                   when      V1Cin3 <> *blanks
049800051024     c                   eval      *in58  =  *on
049900051024w   2c                   when      V1Cin4 <> *blanks
050000070629     c                   eval      *in59  =  *on
050100070629w   2c                   when      V1Cin5 <> *blanks
050200070629     c                   eval      *in60  =  *on
050300070629w   2c                   when      V1Clo1 <> *blanks
050400070629     c                   eval      *in61  =  *on
050500070629w   2c                   when      V1Clo2 <> *blanks
050600070629     c                   eval      *in62  =  *on
050700070629w   2c                   when      V1Clo3 <> *blanks
050800070629     c                   eval      *in63  =  *on
050900070629w   2c                   when      V1Clo4 <> *blanks
051000070629     c                   eval      *in64  =  *on
051100070629w   2c                   when      V1Clo5 <> *blanks
051200070629     c                   eval      *in65  =  *on
051300051024w   2c                   when      V1Cpr1 <> *blanks
051400070629     c                   eval      *in66  =  *on
051500051024w   2c                   when      V1Cpr2 <> *blanks
051600070629     c                   eval      *in67  =  *on
051700051024w   2c                   when      V1Cpr3 <> *blanks
051800070629     c                   eval      *in68  =  *on
051900051024w   2c                   when      V1Cpr4 <> *blanks
052000070629     c                   eval      *in69  =  *on
052100070629w   2c                   when      V1Cpr5 <> *blanks
052200070629     c                   eval      *in70  =  *on
052300051024e   2c                   endsl
052400051024     c                   seton                                        28  90
052500051024     c                   eval      V1Dmsg = $Msg(6)
052600051024     c                   leavesr
052700070629e   1c                   ENDIF
052800070629      *
052900050902      * tipo ricerca: M=Mittente, D=Destinatario, E=Entrambi
053000070629      *               SE non inseriti codici
053100070629if  1c                   IF             V1Cric <> 'M'
053200050902     c                             and  V1Cric <> 'D'
053300050902     c                             and  V1Cric <> 'E'
053400051024     c                             and (V1Crs1 <> *blanks
053500051024     c                              or  V1Crs2 <> *blanks
053600051024     c                              or  V1Crs3 <> *blanks
053700070629     c                              or  V1Crs4 <> *blanks
053800070629     c                              or  V1Crs5 <> *blanks)
053900070629     c                   seton                                        712890
054000050809     c                   eval      V1Dmsg  =  $Msg(2)
054100050809     c                   leavesr
054200070629e   1c                   ENDIF
054300051024      * Codici Cliente
054400051024     c                   clear                   xx
054500051024     c                   clear                   $CCD
054600070629do  1c                   DOW       xx      <  %elem($CC)
054700051024     c                   add       1             xx
054800051024if  2c                   if        $CC(xx) =  *zeros
054900051024     c                   iter
055000051024e   2c                   endif
055100051024     c                   clear                   TIBS69ds
055200051024     c                   clear                   ds_CNACO
055300051024     c                   clear                   ds_CNIND
055400051024     c                   clear                   ds_CNCLP
055500051024     c                   clear                   ds_FNCLS
055600051024     c                   eval      I69kac  =  $CC(xx)
055700051024     c                   call      'TIBS69R'
055800051024     c                   parm                    TIBS69ds
055900051024     c                   parm                    ds_CNACO
056000051024     c                   parm                    ds_CNIND
056100051024     c                   parm                    ds_CNCLP
056200051024     c                   parm                    ds_FNCLS
056300051024if  2c                   if        O69err  =  *blanks
056400051024     c                   movel     ACOrag        $CCD(xx)
056500051024x   2c                   else
056600051024     c                   movel     *all'? '      $CCD(xx)
056700070629     c                   eval      *in(50+xx) =  *on
056800051024     c                   seton                                        28  90
056900051024     c                   eval      V1Dmsg  =  $Msg(7)
057000051024     c                   leavesr
057100051024e   2c                   endif
057200070629e   1c                   ENDDO
057300070629      *
057400050831      * Data limite iniziale ("dal") per ricerca
057500050831     c                   reset                   W1Cdal
057600070629if  1c                   IF        V1Cdal  <> *zeros
057700050809     c                   clear                   WLBdat
057800050809     c                   eval      G08dat  =  V1Cdal
057900050809     c                   call      'XSRDA8'
058000050809     c                   parm                    WLBdat
058100050809if  2c                   if        G08err  =  *on
058200070629     c                   seton                                        812890
058300050809     c                   eval      V1Dmsg  =  $Msg(3)
058400050809     c                   leavesr
058500050809e   2c                   endif
058600050809     c                   eval      V1Cdal  =  G08dat
058700050809     c                   eval      W1Cdal  =  G08inv
058800070629e   1c                   ENDIF
058900070629      *
059000050831      * Data limite finale ("al") per ricerca
059100050831     c                   reset                   W1Cal
059200070629if  1c                   IF        V1Cal   <> *zeros
059300050831     c                   clear                   WLBdat
059400050831     c                   eval      G08dat  =  V1Cal
059500050831     c                   call      'XSRDA8'
059600050831     c                   parm                    WLBdat
059700050831if  2c                   if        G08err  =  *on
059800070629     c                   seton                                        822890
059900050831     c                   eval      V1Dmsg  =  $Msg(3)
060000050831     c                   leavesr
060100050831e   2c                   endif
060200050831     c                   eval      V1Cal   =  G08dat
060300050831     c                   eval      W1Cal   =  G08inv
060400070629x   1c                   ELSE
060500050901     c                   eval      W1Cal   =  *hival
060600070629e   1c                   ENDIF
060700050831      * Range di date
060800070629if  1c                   IF        W1Cdal  >  W1Cal
060900070629     c                   seton                                        812890
061000050831     c                   eval      V1Dmsg  =  $Msg(4)
061100050831     c                   leavesr
061200070629e   1c                   ENDIF
061300050902      *
061400050902      * Pulizia anticipata del file: 'S' o 'N'
061500070629if  1c                   IF            V1Cclrwf <> 'S'
061600050902     c                             and V1Cclrwf <> 'N'
061700050902     c                   eval      V1Dmsg  =  $Msg(5)
061800070629     c                   seton                                        832890
061900050902     c                   leavesr
062000070629e   1c                   ENDIF
062100050809      *
062200050809     c                   ENDSR
062300050902      *
062400050902      *?S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E?-------*
062500050902      *
062600050809**   $MSG -------------------------------------------------------------------*
062700051024Immettere almeno una ragione sociale (anche parziale) o un codice cliente      1
062800050902Tipo ricerca immesso errato: ammessi i valori "M", "D" e "E"                   2
062900050809Data formalmente errata                                                        3
063000050831Data iniziale > Data finale                                                    4
063100050902Immettere "S" (sì) o "N" (no)                                                  5
063200051024Selezionare per   Ragione Sociale   OPPURE   per   Codice Cliente              6
063300051024Codice cliente errato                                                          7
063400061205Se inserito il solo indirizzo inserire anche la provincia                      8
063500061228Carattere "'" non gestibile in selezione                                       9
063600061228Carattere "%" non gestibile in selezione                                       10
063700070629Rilevati problemi nell'impostazione parametri per l'azione batch               11
063800100128Lavoro NON eseguibile in questo Sistema Informativo. Premere Invio per uscire. 12
