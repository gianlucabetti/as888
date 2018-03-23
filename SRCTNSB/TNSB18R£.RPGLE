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
006900100623     d ds_IND          ds                  inz
007000100623     d   V1Cin1                            inz
007100100623     d   V1Cin2                            inz
007200100623     d   V1Cin3                            inz
007300100623     d   V1Cin4                            inz
007400100623     d   V1Cin5                            inz
007500100623     d   $V1Cin                1    150    inz     dim (5)
007600100623     d ds_LOC          ds                  inz
007700100623     d   V1Clo1                            inz
007800100623     d   V1Clo2                            inz
007900100623     d   V1Clo3                            inz
008000100623     d   V1Clo4                            inz
008100100623     d   V1Clo5                            inz
008200100623     d   $V1Clo                1    150    inz     dim (5)
008300051024      *
008400051024     d ds_KSC          ds                  inz
008500051024     d   V1Ccc1                            inz
008600051024     d   V1Ccc2                            inz
008700051024     d   V1Ccc3                            inz
008800051024     d   V1Ccc4                            inz
008900070629     d   V1Ccc5                            inz
009000070629     d   $CC                   1     35  0 inz     dim (5)
009100051024     d ds_KSCD         ds                  inz
009200051024     d   V1Dcc1                            inz
009300051024     d   V1Dcc2                            inz
009400051024     d   V1Dcc3                            inz
009500051024     d   V1Dcc4                            inz
009600070629     d   V1Dcc5                            inz
009700070808     d   $CCD                  1    115    inz     dim (5)
009800050809      *
009900050902      *?V A R I A B I L I?--------------------------------------------*
010000050809      *
010100050809      * - Flags
010200100128     d $Fine           s               n   inz(*off)
010300100128     d $InzD01         s               n   inz(*on)
010400100128     d $ErrSisInf      s               n   inz
010500050809      *
010600050809      * - Indici di schiera
010700050809      *
010800050809      * - Variabili riferite al data base o al display file
010900050831     d W1Cdal          s                   inz     like(SB19dal)
011000050831     d W1Cal           s                   inz     like(SB19al)
011100050809      *
011200050809      * - Variabili definite a programma
011300050902      *
011400050902      * Parametri x QCMDEXC
011500050902     d Qcmd            s            100    inz
011600050902     d Qlen            s             15  5 inz(100)
011700050809      *
011800050902      *?K E Y - L I S T?----------------------------------------------*
011900050809      *
012000050902      *
012100050902      *?I N D I C A T O R I?------------------------------------------*
012200050902      *
012300051024     d XX              s              3  0 inz
012400050902      *
012500050902      *===============================================================*
012600050902      *
012700050809     c     *Entry        plist
012800050809     c                   parm                    KPJBA
012900050809      *
013000050809      * Operazioni Iniziali
013100050809     c                   exsr      RoutInz
013200050809      *
013300050809      * Gestione video - Filtro di lancio
013400050809do  1c                   dow       $Fine   =  *off
013500070808     c                   exsr      GesD01
013600050809e   1c                   enddo
013700050809      *
013800050809      * Fine
013900051024     c                   reset                   TIBS69ds
014000051024     c                   clear                   ds_CNACO
014100051024     c                   clear                   ds_CNIND
014200051024     c                   clear                   ds_CNCLP
014300051024     c                   clear                   ds_FNCLS
014400051024     c                   call      'TIBS69R'
014500051024     c                   parm                    TIBS69ds
014600051024     c                   parm                    ds_CNACO
014700051024     c                   parm                    ds_CNIND
014800051024     c                   parm                    ds_CNCLP
014900051024     c                   parm                    ds_FNCLS
015000050809     c                   movel     *on           *inLR
015100050902      *
015200050902      *?S U B R O U T I N E S?----------------------------------------*
015300050902      *
015400050809      *---------------------------------------------------------------*
015500050902      *?Operazioni Iniziali                                          ?*
015600050809      *---------------------------------------------------------------*
015700050809     c     RoutInz       BEGSR
015800050809      *
015900050809      * Reperisco dati job
016000050809     c                   exsr      DatiJob
016100050809      *
016200050809     c                   movel     SDSpgm        V1Tpgm
016300050809      *
016400050809     c                   ENDSR
016500050809      *
016600050809      *---------------------------------------------------------------*
016700050902      *?Reperimento Dati del job (Utente/Operativi)                  ?*
016800050809      *---------------------------------------------------------------*
016900050809     c     DatiJob       BEGSR
017000050809      *
017100050809     c     *dtaara       define    §azute        azuteds
017200050809     c     *dtaara       define    §datiute      ddatiute
017300050809      *
017400050809     c                   in(E)     *dtaara
017500050809     c                   IF        %ERROR or RSUT = *blanks
017600050809     c                   clear                   Tibs34Ds
017700050809     c                   call      'TIBS34R'
017800050809     c                   parm                    Tibs34Ds
017900050809     c                   in        *dtaara
018000050809     c                   ENDIF
018100050809      *
018200050809     c                   ENDSR
018300050809      *
018400050809      *---------------------------------------------------------------*
018500050902      *?Gestione videata D01                                         ?*
018600050809      *---------------------------------------------------------------*
018700050809     c     GesD01        BEGSR
018800050809      *
018900050809      * Inizializzo la videata
019000050809if  1c                   if        $InzD01 =  *on
019100050809     c                   exsr      InzD01
019200050809     c                   eval      $InzD01 =  *off
019300050809e   1c                   endif
019400050809      *
019500050809      * Scrivo la testata e la riga tasti funzionali abilitati
019600100128if  1c*//                if        NOT *in90
019700050809     c                   write     SB18T01
019800050809     c                   write     SB18Z01
019900100128e   1c*//                endif
020000050809      * Emetto la videata
020100050809     c                   exfmt     SB18D01
020200050809     c                   setoff                                       28  90
020300050809     c                   clear                   V1Dmsg
020400061228      *
020500061228sel 1c                   select
020600100128      *
020700100128      * Sistema Informativo Errato
020800100128w   1c                   when      $ErrSisInf
020900100128     c                   exsr      F03D01
021000100128     c                   leavesr
021100050809      *
021200050809      * F3=Fine
021300061228w   1c                   when      *inKC
021400050809     c                   exsr      F03D01
021500050809     c                   leavesr
021600050902      *
021700050902      * F9=Interrogazione file WFNIB00F
021800061228w   1c                   when      *inKI
021900050902     c                   exsr      F09D01
022000050902     c                   leavesr
022100050809      *
022200070808      * Invio / F6=Conferma
022300061228x   1c                   other
022400070808      * - Controllo dati immessi a video
022500050809     c                   exsr      CtrD01
022600061228if  2c                   if        NOT  *in90
022700061228     c                             and  *inKF
022800070808      * - Lancio batch
022900050809     c                   exsr      F06D01
023000050809     c                   leavesr
023100061228e   2c                   endif
023200061228      *
023300061228e   1c                   endsl
023400050809      *
023500050809     c                   ENDSR
023600050809      *
023700050809      *---------------------------------------------------------------*
023800050902      *?Inizializzazione videata D01                                 ?*
023900050809      *---------------------------------------------------------------*
024000050809     c     InzD01        BEGSR
024100050809      *
024200050809     c                   clear                   SB18D01
024300050809      *
024400050902     c                   eval      V1Cric  =  'E'
024500100128      *
024600100128     c                   if        KNSIF  <> 'GAITRA201 '   and
024700100128     c                             %subst(KNSIF:1:7) <> 'GAITRAP'
024800100128     c                   eval      $ErrSisInf = *on
024900100128     c                   eval      $Fine   = *on
025000100128     c                   seton                                        28  90
025100100128     c                   eval      V1Dmsg  = $Msg(12)
025200100128     c                   leavesr
025300100128     c                   endif
025400050809      *
025500050809     c                   ENDSR
025600050809      *
025700050809      *---------------------------------------------------------------*
025800050902      *?Gestione tasto funzionale F03 da videata D01                 ?*
025900050809      *---------------------------------------------------------------*
026000050809     c     F03D01        BEGSR
026100050809      *
026200050809      * Chiudo il programma
026300050809     c                   eval      $Fine   =  *on
026400050809      *
026500050809     c                   ENDSR
026600050809      *
026700050809      *---------------------------------------------------------------*
026800050902      *?Gestione tasto funzionale F06 da videata D01                 ?*
026900050809      *---------------------------------------------------------------*
027000050809     c     F06D01        BEGSR
027100050809      *
027200050809      * Preparazione ds parametri
027300050809     c                   clear                   TNSB19ds
027400070704     c                   eval      SB19cwf =  V1Cclrwf
027500070704     c                   eval      SB19dal =  W1Cdal
027600070704     c                   eval      SB19al  =  W1Cal
027700051024if  1c                   IF             V1Ccc1 =  *zeros
027800051024     c                             and  V1Ccc2 =  *zeros
027900051024     c                             and  V1Ccc3 =  *zeros
028000051024     c                             and  V1Ccc4 =  *zeros
028100070629     c                             and  V1Ccc5 =  *zeros
028200070704     c                   eval      SB19ric =  V1Cric
028300050809     c                   eval      SB19rs1 =  V1Crs1
028400050809     c                   eval      SB19rs2 =  V1Crs2
028500050809     c                   eval      SB19rs3 =  V1Crs3
028600050809     c                   eval      SB19rs4 =  V1Crs4
028700070629     c                   eval      SB19rs5 =  V1Crs5
028800050809     c                   eval      SB19in1 =  V1Cin1
028900050809     c                   eval      SB19in2 =  V1Cin2
029000050809     c                   eval      SB19in3 =  V1Cin3
029100050809     c                   eval      SB19in4 =  V1Cin4
029200070629     c                   eval      SB19in5 =  V1Cin5
029300070629     c                   eval      SB19lo1 =  V1Clo1
029400070629     c                   eval      SB19lo2 =  V1Clo2
029500070629     c                   eval      SB19lo3 =  V1Clo3
029600070629     c                   eval      SB19lo4 =  V1Clo4
029700070629     c                   eval      SB19lo5 =  V1Clo5
029800050809     c                   eval      SB19pr1 =  V1Cpr1
029900050809     c                   eval      SB19pr2 =  V1Cpr2
030000050809     c                   eval      SB19pr3 =  V1Cpr3
030100050809     c                   eval      SB19pr4 =  V1Cpr4
030200070629     c                   eval      SB19pr5 =  V1Cpr5
030300051024x   1c                   ELSE
030400070704     c                   eval      SB19ric =  'K'
030500051024     c                   if        V1Ccc1  >  *zeros
030600051024     c                   movel(p)  V1Ccc1        SB19rs1
030700051024     c                   endif
030800051024     c                   if        V1Ccc2  >  *zeros
030900051024     c                   movel(p)  V1Ccc2        SB19rs2
031000051024     c                   endif
031100051024     c                   if        V1Ccc3  >  *zeros
031200051024     c                   movel(p)  V1Ccc3        SB19rs3
031300051024     c                   endif
031400051024     c                   if        V1Ccc4  >  *zeros
031500051024     c                   movel(p)  V1Ccc4        SB19rs4
031600051024     c                   endif
031700070629     c                   if        V1Ccc5  >  *zeros
031800070629     c                   movel(p)  V1Ccc5        SB19rs5
031900070629     c                   endif
032000051024e   1c                   ENDIF
032100070629     c***                movel(p)  TNSB19ds      KPJBU
032200061228      *
032300061228     c                   eval      KCOAZ   =  'SB19'
032400070629      *
032500070629     c                   reset                   TRUL01ds
032600070629     c*** già così:      eval      UL01opz = *on
032700070629     c                   eval      UL01azn = KCOAZ
032800070629     c                   movel(p)  TNSB19ds      UL01prm
032900070629     c*** già così:      eval      UL01err = *off
033000070629     c                   call      'TRUL01R'
033100070629     c                   parm                    TRUL01ds
033200070629if  1c                   if        UL01err = *on
033300070629     c                   seton                                        28  90
033400070629     c                   eval      V1Dmsg  = $Msg(11)
033500070629     c                   leavesr
033600070629e   1c                   endif
033700070629     c                   movel(p)  UL01prg       KPJBU
033800071126     c                   eval      KPJBU   = %trimr(KPJBU)
033900071126     c                                     + '-'
034000071126     c                                     + %subst( UL01prm : 1 :
034100071126     c                                       %len(KPJBU) - 1 -
034200071126     c                                         %len(%trimr(KPJBU)) )
034300050809      *
034400050809      * Sottomissione lavoro batch
034500070628     c***                call      'BCH09'
034600070628     c***                parm                    KPJBA
034700050809      *
034800061220     c                   if        KNMUS   =  *all'1'
034900061220     c                   call      'TNSB19R'
035000061220     c                   parm                    KPJBA
035100061220     c                   else
035200061220     c                   call      'BCH10'
035300050809     c                   parm                    KPJBA
035400061220     c                   endif
035500050809      *
035600050809      * Chiudo il programma
035700050809     c                   eval      $Fine   =  *on
035800050809      *
035900050809     c                   ENDSR
036000050902      *
036100050902      *---------------------------------------------------------------*
036200050902      *?Gestione tasto funzionale F09 da videata D01                 ?*
036300050902      *---------------------------------------------------------------*
036400050902     c     F09D01        BEGSR
036500050902      *
036600050902     c                   eval      Qcmd    =  'DSPDBF FILE'
036700050902     c                                     +  '(*LIBL/WFNIB00F) '
036800050902     c                                     +  'MBR(*FIRST) '
036900050902     c***                                  +  'RCDSLT(*YES) '
037000050902     c                                     +  'OUTPUT(*) '
037100050902     c                   call      'QCMDEXC'                            90
037200050902     c                   parm                    Qcmd
037300050902     c                   parm                    Qlen
037400050902      *
037500050902     c                   ENDSR
037600050809      *
037700050809      *---------------------------------------------------------------*
037800050902      *?Controllo dati immessi in videata D01                        ?*
037900050809      *---------------------------------------------------------------*
038000050809     c     CtrD01        BEGSR
038100050809      *
038200050809     c                   movea     *zeros        *in(50)
038300050809      *
038400050809      * ragione sociale: obbligatoria almeno una selezione
038500051024      *                  SE non inseriti codici
038600070629if  1c                   if             V1Crs1 =  *blanks
038700050809     c                             and  V1Crs2 =  *blanks
038800050809     c                             and  V1Crs3 =  *blanks
038900050809     c                             and  V1Crs4 =  *blanks
039000070629     c                             and  V1Crs5 =  *blanks
039100061201     c                             and  V1Cin1 =  *blanks
039200061201     c                             and  V1Cin2 =  *blanks
039300061201     c                             and  V1Cin3 =  *blanks
039400061201     c                             and  V1Cin4 =  *blanks
039500070629     c                             and  V1Cin5 =  *blanks
039600070629     c                             and  V1Clo1 =  *blanks
039700070629     c                             and  V1Clo2 =  *blanks
039800070629     c                             and  V1Clo3 =  *blanks
039900070629     c                             and  V1Clo4 =  *blanks
040000070629     c                             and  V1Clo5 =  *blanks
040100061201     c                             and  V1Cpr1 =  *blanks
040200061201     c                             and  V1Cpr2 =  *blanks
040300061201     c                             and  V1Cpr3 =  *blanks
040400061201     c                             and  V1Cpr4 =  *blanks
040500070629     c                             and  V1Cpr5 =  *blanks
040600070629     c                             and  V1Ccc1 =  *zeros
040700070629     c                             and  V1Ccc2 =  *zeros
040800070629     c                             and  V1Ccc3 =  *zeros
040900070629     c                             and  V1Ccc4 =  *zeros
041000070629     c                             and  V1Ccc5 =  *zeros
041100050809     c                   seton                                        512890
041200070629     c                   eval      V1Dmsg  = $Msg(1)
041300050809     c                   leavesr
041400070629e   1c                   endif
041500070629      *
041600070629      * Controllo dei caratteri
041700070629     c                   clear                   xx
041800070629do  1c                   DOW       xx      < %elem($V1Crs)
041900070629     c                   add       1             xx
042000070629      * "apostrofi" non ammessi né in ragione sociale né in indirizzo
042100070629if  2c                   if             $V1Crs(xx) <> *blanks
042200070629     c                             and  %scan('''':%trimr($V1Crs(xx)))
042300070629     c                                             >  *zeros
042400100623     c                   seton                                          2890
042500070629     c                   eval      *in(50+xx)      =  *on
042600070629     c                   eval      V1Dmsg          =  $Msg(9)
042700061228     c                   leavesr
042800070629e   2c                   endif
042900100623if  2c                   if             $V1Cin(xx) <> *blanks
043000100623     c                             and  %scan('''':%trimr($V1Cin(xx)))
043100100623     c                                             >  *zeros
043200100623     c                   seton                                          2890
043300100623     c                   eval      *in(55+xx)      =  *on
043400100623     c                   eval      V1Dmsg          =  $Msg(9)
043500100623     c                   leavesr
043600100623e   2c                   endif
043700100623if  2c                   if             $V1Clo(xx) <> *blanks
043800100623     c                             and  %scan('''':%trimr($V1Clo(xx)))
043900100623     c                                             >  *zeros
044000100623     c                   seton                                          2890
044100100623     c                   eval      *in(60+xx)      =  *on
044200100623     c                   eval      V1Dmsg          =  $Msg(9)
044300100623     c                   leavesr
044400100623e   2c                   endif
044500070629      * "percentuali" non ammessi né in ragione sociale né in indirizzo
044600070629if  2c                   if             $V1Crs(xx) <> *blanks
044700070629     c                             and  %scan('%':%trimr($V1Crs(xx)))
044800070629     c                                             >  *zeros
044900100623     c                   seton                                          2890
045000100623     c                   eval      *in(50+xx)      =  *on
045100070629     c                   eval      V1Dmsg          =  $Msg(10)
045200070629     c                   leavesr
045300070629e   2c                   endif
045400100623if  2c                   if             $V1Cin(xx) <> *blanks
045500100623     c                             and  %scan('%':%trimr($V1Cin(xx)))
045600100623     c                                             >  *zeros
045700100623     c                   seton                                          2890
045800100623     c                   eval      *in(55+xx)      =  *on
045900100623     c                   eval      V1Dmsg          =  $Msg(10)
046000100623     c                   leavesr
046100100623e   2c                   endif
046200100623if  2c                   if             $V1Clo(xx) <> *blanks
046300100623     c                             and  %scan('%':%trimr($V1Clo(xx)))
046400100623     c                                             >  *zeros
046500100623     c                   seton                                          2890
046600100623     c                   eval      *in(60+xx)      =  *on
046700100623     c                   eval      V1Dmsg          =  $Msg(10)
046800100623     c                   leavesr
046900100623e   2c                   endif
047000070629e   1c                   ENDDO
047100070629      *
047200061219      * provincia .....: obbligatoria almeno una selezione
047300061219      *                  SE inserito almeno un solo indirizzo
047400070629if  1c                   IF             V1Crs1 =  *blanks
047500061205     c                             and  V1Crs2 =  *blanks
047600061205     c                             and  V1Crs3 =  *blanks
047700061205     c                             and  V1Crs4 =  *blanks
047800070629     c                             and  V1Crs5 =  *blanks
047900061205     c                             and (V1Cin1 <> *blanks
048000061205     c                              or  V1Cin2 <> *blanks
048100061205     c                              or  V1Cin3 <> *blanks
048200070629     c                              or  V1Cin4 <> *blanks
048300070629     c                              or  V1Cin5 <> *blanks)
048400070629     c                             and  V1Clo1 =  *blanks
048500070629     c                             and  V1Clo2 =  *blanks
048600070629     c                             and  V1Clo3 =  *blanks
048700070629     c                             and  V1Clo4 =  *blanks
048800070629     c                             and  V1Clo5 =  *blanks
048900061205     c                             and  V1Cpr1 =  *blanks
049000061205     c                             and  V1Cpr2 =  *blanks
049100061205     c                             and  V1Cpr3 =  *blanks
049200061205     c                             and  V1Cpr4 =  *blanks
049300070629     c                             and  V1Cpr5 =  *blanks
049400070629     c                             and  V1Ccc1 =  *zeros
049500070629     c                             and  V1Ccc2 =  *zeros
049600070629     c                             and  V1Ccc3 =  *zeros
049700070629     c                             and  V1Ccc4 =  *zeros
049800070629     c                             and  V1Ccc5 =  *zeros
049900070629     c                   seton                                        662890
050000061205     c                   eval      V1Dmsg = $Msg(8)
050100061205     c                   leavesr
050200070629e   1c                   ENDIF
050300051024      *
050400051024      * codice cliente: inibisce l'immissione di altri dati
050500070629if  1c                   IF            (V1Ccc1 <> *zeros
050600051024     c                              or  V1Ccc2 <> *zeros
050700051024     c                              or  V1Ccc3 <> *zeros
050800051024     c                              or  V1Ccc4 <> *zeros)
050900051024     c                             and (V1Crs1 <> *blanks
051000051024     c                              or  V1Crs2 <> *blanks
051100051024     c                              or  V1Crs3 <> *blanks
051200051024     c                              or  V1Crs4 <> *blanks
051300070629     c                              or  V1Crs5 <> *blanks
051400051024     c                              or  V1Cin1 <> *blanks
051500051024     c                              or  V1Cin2 <> *blanks
051600051024     c                              or  V1Cin3 <> *blanks
051700051024     c                              or  V1Cin4 <> *blanks
051800070629     c                              or  V1Cin5 <> *blanks
051900070629     c                              or  V1Clo1 <> *blanks
052000070629     c                              or  V1Clo2 <> *blanks
052100070629     c                              or  V1Clo3 <> *blanks
052200070629     c                              or  V1Clo4 <> *blanks
052300070629     c                              or  V1Clo5 <> *blanks
052400051024     c                              or  V1Cpr1 <> *blanks
052500051024     c                              or  V1Cpr2 <> *blanks
052600051024     c                              or  V1Cpr3 <> *blanks
052700070629     c                              or  V1Cpr4 <> *blanks
052800070629     c                              or  V1Cpr5 <> *blanks)
052900051024sel 2c                   select
053000051024w   2c                   when      V1Crs1 <> *blanks
053100051024     c                   eval      *in51  =  *on
053200051024w   2c                   when      V1Crs2 <> *blanks
053300070629     c                   eval      *in52  =  *on
053400051024w   2c                   when      V1Crs3 <> *blanks
053500070629     c                   eval      *in53  =  *on
053600051024w   2c                   when      V1Crs4 <> *blanks
053700070629     c                   eval      *in54  =  *on
053800070629w   2c                   when      V1Crs5 <> *blanks
053900070629     c                   eval      *in55  =  *on
054000051024w   2c                   when      V1Cin1 <> *blanks
054100070629     c                   eval      *in56  =  *on
054200051024w   2c                   when      V1Cin2 <> *blanks
054300070629     c                   eval      *in57  =  *on
054400051024w   2c                   when      V1Cin3 <> *blanks
054500051024     c                   eval      *in58  =  *on
054600051024w   2c                   when      V1Cin4 <> *blanks
054700070629     c                   eval      *in59  =  *on
054800070629w   2c                   when      V1Cin5 <> *blanks
054900070629     c                   eval      *in60  =  *on
055000070629w   2c                   when      V1Clo1 <> *blanks
055100070629     c                   eval      *in61  =  *on
055200070629w   2c                   when      V1Clo2 <> *blanks
055300070629     c                   eval      *in62  =  *on
055400070629w   2c                   when      V1Clo3 <> *blanks
055500070629     c                   eval      *in63  =  *on
055600070629w   2c                   when      V1Clo4 <> *blanks
055700070629     c                   eval      *in64  =  *on
055800070629w   2c                   when      V1Clo5 <> *blanks
055900070629     c                   eval      *in65  =  *on
056000051024w   2c                   when      V1Cpr1 <> *blanks
056100070629     c                   eval      *in66  =  *on
056200051024w   2c                   when      V1Cpr2 <> *blanks
056300070629     c                   eval      *in67  =  *on
056400051024w   2c                   when      V1Cpr3 <> *blanks
056500070629     c                   eval      *in68  =  *on
056600051024w   2c                   when      V1Cpr4 <> *blanks
056700070629     c                   eval      *in69  =  *on
056800070629w   2c                   when      V1Cpr5 <> *blanks
056900070629     c                   eval      *in70  =  *on
057000051024e   2c                   endsl
057100051024     c                   seton                                        28  90
057200051024     c                   eval      V1Dmsg = $Msg(6)
057300051024     c                   leavesr
057400070629e   1c                   ENDIF
057500070629      *
057600050902      * tipo ricerca: M=Mittente, D=Destinatario, E=Entrambi
057700070629      *               SE non inseriti codici
057800070629if  1c                   IF             V1Cric <> 'M'
057900050902     c                             and  V1Cric <> 'D'
058000050902     c                             and  V1Cric <> 'E'
058100051024     c                             and (V1Crs1 <> *blanks
058200051024     c                              or  V1Crs2 <> *blanks
058300051024     c                              or  V1Crs3 <> *blanks
058400070629     c                              or  V1Crs4 <> *blanks
058500070629     c                              or  V1Crs5 <> *blanks)
058600070629     c                   seton                                        712890
058700050809     c                   eval      V1Dmsg  =  $Msg(2)
058800050809     c                   leavesr
058900070629e   1c                   ENDIF
059000051024      * Codici Cliente
059100051024     c                   clear                   xx
059200051024     c                   clear                   $CCD
059300070629do  1c                   DOW       xx      <  %elem($CC)
059400051024     c                   add       1             xx
059500051024if  2c                   if        $CC(xx) =  *zeros
059600051024     c                   iter
059700051024e   2c                   endif
059800051024     c                   clear                   TIBS69ds
059900051024     c                   clear                   ds_CNACO
060000051024     c                   clear                   ds_CNIND
060100051024     c                   clear                   ds_CNCLP
060200051024     c                   clear                   ds_FNCLS
060300051024     c                   eval      I69kac  =  $CC(xx)
060400051024     c                   call      'TIBS69R'
060500051024     c                   parm                    TIBS69ds
060600051024     c                   parm                    ds_CNACO
060700051024     c                   parm                    ds_CNIND
060800051024     c                   parm                    ds_CNCLP
060900051024     c                   parm                    ds_FNCLS
061000051024if  2c                   if        O69err  =  *blanks
061100051024     c                   movel     ACOrag        $CCD(xx)
061200051024x   2c                   else
061300051024     c                   movel     *all'? '      $CCD(xx)
061400070629     c                   eval      *in(50+xx) =  *on
061500051024     c                   seton                                        28  90
061600051024     c                   eval      V1Dmsg  =  $Msg(7)
061700051024     c                   leavesr
061800051024e   2c                   endif
061900070629e   1c                   ENDDO
062000070629      *
062100050831      * Data limite iniziale ("dal") per ricerca
062200050831     c                   reset                   W1Cdal
062300070629if  1c                   IF        V1Cdal  <> *zeros
062400050809     c                   clear                   WLBdat
062500050809     c                   eval      G08dat  =  V1Cdal
062600050809     c                   call      'XSRDA8'
062700050809     c                   parm                    WLBdat
062800050809if  2c                   if        G08err  =  *on
062900070629     c                   seton                                        812890
063000050809     c                   eval      V1Dmsg  =  $Msg(3)
063100050809     c                   leavesr
063200050809e   2c                   endif
063300050809     c                   eval      V1Cdal  =  G08dat
063400050809     c                   eval      W1Cdal  =  G08inv
063500070629e   1c                   ENDIF
063600070629      *
063700050831      * Data limite finale ("al") per ricerca
063800050831     c                   reset                   W1Cal
063900070629if  1c                   IF        V1Cal   <> *zeros
064000050831     c                   clear                   WLBdat
064100050831     c                   eval      G08dat  =  V1Cal
064200050831     c                   call      'XSRDA8'
064300050831     c                   parm                    WLBdat
064400050831if  2c                   if        G08err  =  *on
064500070629     c                   seton                                        822890
064600050831     c                   eval      V1Dmsg  =  $Msg(3)
064700050831     c                   leavesr
064800050831e   2c                   endif
064900050831     c                   eval      V1Cal   =  G08dat
065000050831     c                   eval      W1Cal   =  G08inv
065100070629x   1c                   ELSE
065200050901     c                   eval      W1Cal   =  *hival
065300070629e   1c                   ENDIF
065400050831      * Range di date
065500070629if  1c                   IF        W1Cdal  >  W1Cal
065600070629     c                   seton                                        812890
065700050831     c                   eval      V1Dmsg  =  $Msg(4)
065800050831     c                   leavesr
065900070629e   1c                   ENDIF
066000050902      *
066100050902      * Pulizia anticipata del file: 'S' o 'N'
066200070629if  1c                   IF            V1Cclrwf <> 'S'
066300050902     c                             and V1Cclrwf <> 'N'
066400050902     c                   eval      V1Dmsg  =  $Msg(5)
066500070629     c                   seton                                        832890
066600050902     c                   leavesr
066700070629e   1c                   ENDIF
066800050809      *
066900050809     c                   ENDSR
067000050902      *
067100050902      *?S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E?-------*
067200050902      *
067300050809**   $MSG -------------------------------------------------------------------*
067400051024Immettere almeno una ragione sociale (anche parziale) o un codice cliente      1
067500050902Tipo ricerca immesso errato: ammessi i valori "M", "D" e "E"                   2
067600050809Data formalmente errata                                                        3
067700050831Data iniziale > Data finale                                                    4
067800050902Immettere "S" (sì) o "N" (no)                                                  5
067900051024Selezionare per   Ragione Sociale   OPPURE   per   Codice Cliente              6
068000051024Codice cliente errato                                                          7
068100061205Se inserito il solo indirizzo inserire anche la provincia                      8
068200061228Carattere "'" non gestibile in selezione                                       9
068300061228Carattere "%" non gestibile in selezione                                       10
068400070629Rilevati problemi nell'impostazione parametri per l'azione batch               11
068500100128Lavoro NON eseguibile in questo Sistema Informativo. Premere Invio per uscire. 12
