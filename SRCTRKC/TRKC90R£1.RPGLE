000100150129     /*PRM  dbgview(*source)
000200150129     /*CMD  ovrdbf file(WADELAUM) tofile(*libl/WADELAUM) +
000300150129     /*CMD         ovrscope(*calllvl)
000400150129     /*END  dltovr file(WADELAUM) lvl(*)
000500150129     /*END
000600150129      *========================================================================*
000700150129      *?Prep. file TICMC, TICMI e TICMF per Campagna aumento                  ?*
000800150129      *========================================================================*
000900150129
001000150130     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001100150129
001200150129      *------------------------------------------------------------------------*
001300150129      *?A R C H I V I                                                         ?*
001400150129      *------------------------------------------------------------------------*
001500150129
001600150129     fWADELAUM  if   e             disk
001700150130      *
001800150129     fAZCMM01L  if   e           k disk
001900150129     fAZORG01L  if   e           k disk
002000150129      *
002100160202     fTICMP01L  if   e           k disk
002200150203     fTICMC02L  if A e           k disk
002300150129      *
002400150129     fTICMI00F  O    e             disk
002500150129     fTICMF00F  O    e             disk
002600150129      *
002700150129     fQSYSPRT   O    f  132        printer usropn
002800150129     f                                     oflind(*inOF)
002900031103
003000031103      *------------------------------------------------------------------------*
003100150129      *?C O S T A N T I                                                       ?*
003200150129      *------------------------------------------------------------------------*
003300150203
003400150203       // -?Tipo Aumento che si sta elaborando?
003500150203     d c_TipoAumento_Campagna...
003600150203     d                 c                   const('C')
003700150203     d c_TipoAumento_Lettera...
003800150203     d                 c                   const('L')
003900071026
004000150129      * -?Numero Campagna che si sta generando?
004100161115     d c_Cli_TDA       c                   const(5)
004200161115     d c_Cli_BC        c                   const(6)
004300150130
004400150204      * -?Fasi da impostare?
004500150130     d c_Fase_ObiettivoIniziale...
004600150130     d                 c                   const(' 10')
004700150204     d c_Fase_ObiettivoFinale...
004800150204     d                 c                   const(' 30')
004900150129
005000160202      *------------------------------------------------------------------------*
005100160202      *?S C H I E R E                                                         ?*
005200160202      *------------------------------------------------------------------------*
005300160202
005400160202      // -?Campagne attive ad oggi?
005500160202     d sk_CampagneAttive...
005600160202     d                 s                   like(CMPncm)  dim(9)  inz
005700160202
005800150129      *------------------------------------------------------------------------*
005900150129      *?S T R U T T U R E - D A T I                                           ?*
006000150129      *------------------------------------------------------------------------*
006100150129
006200150129      * -?Status?
006300150129     d Status         sds
006400150129     d   SDSpgm          *proc
006500150129     d   SDSprm          *parms
006600150129     d   SDSdta              191    198
006700150129     d   SDSjob              244    253
006800150129     d   SDSusr              254    263
006900150129      *
007000150129      * -?Data/Ora attuali?
007100150129     d wTime_ds        ds                  inz
007200150129     d   wDate                        8s 0 inz
007300150129     d   wTime                        6s 0 inz
007400031103
007500150129      * -?Parametri per pgm. TIBS69R (decodifica cliente)?
007600150129     d TIBS69ds      e ds                  inz
007700150130     d   I69tla      e                     inz('C')
007800150130     d ds_CNACO      e ds                  extname(CNACO00F) inz
007900150130     d ds_CNIND      e ds                  extname(CNIND00F) inz
008000150130     d ds_CNCLP      e ds                  extname(CNCLP00F) inz
008100150130     d ds_FNCLS      e ds                  extname(FNCLS00F) inz
008200150129
008300150129      *------------------------------------------------------------------------*
008400150129      *?V A R I A B I L I                                                     ?*
008500150129      *------------------------------------------------------------------------*
008600150129
008700150130      * -?Flags booleani?
008800150130     d $RecOK          s               n   inz
008900160202      *
009000160202      * -?Indici di schiera?
009100160202     d xx              s              3  0 inz
009200150130      *
009300150129      * -?Codice e Descrizione errore rilevato?
009400150129     d wERR            s              1  0 inz
009500150129     d prt_Msg         s            132    inz
009600150129      *
009700150129      * -?Data *ISO?
009800150129     d wDate_Iso       s               d   datfmt(*ISO)    inz(*loval)
009900150129      * -?Data *EUR?
010000150129     d wDate_Eur       s               d   datfmt(*EUR)    inz(*loval)
010100150130
010200150130      *------------------------------------------------------------------------*
010300150130      *?D E F I N I Z I O N E   K E Y - L I S T                               ?*
010400150130      *------------------------------------------------------------------------*
010500150130
010600150130     d k_CMCncm        s                   like(CMCncm)    inz
010700150130     d k_CMCksu        s                   like(CMCksu)    inz
010800150130      *
010900150203     c     K01CMC02      klist
011000150130     c                   kfld                    k_CMCksu
011100150129
011200031103      *------------------------------------------------------------------------*
011300150129      *?M A I N - L I N E                                                     ?*
011400150129      *------------------------------------------------------------------------*
011500031103
011600150130      * -?Operazioni Iniziali?
011700150130     c                   exsr      sr_RoutInz
011800150129      *
011900150129      * -?Ciclo di lettura file WADELAUM?
012000150129do  1c                   DO        *hival
012100031103      *
012200150129     c                   read      WADELAUM0
012300031103      *
012400150129     c                   if        %eof(WADELAUM)
012500031103     c                   leave
012600031103     c                   endif
012700141027      *
012800150130      * - -?Selezione record da elaborare?
012900150130     c                   exsr      sr_SelezRec
013000150130     c                   if        Not $RecOk
013100150130     c                   iter
013200150130     c                   endif
013300150130      *
013400150130      * - -?Scrittura record in TICMC / TICMI / TICMF?
013500150130     c                   exsr      sr_WriteREC
013600150130      *
013700150129e   1c                   ENDDO
013800150129      *
013900150130      * -?Operazioni Finali?
014000150130     c                   exsr      sr_RoutEnd
014100150130
014200150130      *------------------------------------------------------------------------*
014300150130      *?Operazioni Iniziali.                                                  ?*
014400150130      *------------------------------------------------------------------------*
014500150130
014600150130     c     sr_RoutInz    BEGSR
014700150130      *
014800150130     c                   eval      *inLR = *on
014900150130      *
015000150130      * -?Reperimento data e ora attuali?
015100150130     c                   eval      wTime_ds  = %editc( %dec( %timestamp() )
015200150130     c                                                 : 'X' )
015300150130     c                   eval      wDate_Iso = %date( wDate : *Iso )
015400150130     c                   eval      wDate_Eur = wDate_Iso
015500160202     c                   eval      wDate     = %dec( wDate_Eur )
015600160202      /free
015700160202         // -?Memorizzazione Campagne Attive oggi?
015800160202         setll  ( *loval )  TICMP000;
015900160202         read   TICMP000;
016000160202         DoW  NOT %eof( TICMP01L );
016100160202           if  CMPdfc >= %dec( wDate_Iso );
016200160202             xx += 1;
016300160202             sk_CampagneAttive(xx) = CMPncm;
016400160202           endif;
016500160202           read  TICMP000;
016600160202         EndDo;
016700160202      /end-free
016800150130      *
016900150130     c                   ENDSR
017000150130
017100150130      *------------------------------------------------------------------------*
017200150130      *?Selezione record in lettura.                                          ?*
017300150130      *------------------------------------------------------------------------*
017400150130
017500150130     c     sr_SelezRec   BEGSR
017600150130      *
017700150130     c                   eval      $RecOK = *off
017800150130      *
017900150203      * -?Selezione dei rec. con Tipo AUmento "C" o "L"?
018000160202     c*//                if        WAPtau <> c_TipoAumento_Campagna  and
018100160202     c*//                          WAPtau <> c_TipoAumento_Lettera
018200160202     c*//                leavesr
018300160202     c*//                endif
018400150130      *
018500150130      * -?Esclusione dei clienti esteri?
018600150130     c*//                if        WAPioe = 'E'
018700150130     c*//                leavesr
018800150130     c*//                endif
018900150130      *
019000150130      * -?Esclusione dei clienti già inseriti?
019100150130     c                   eval      k_CMCksu = WAPcli
019200150203     c     K01CMC02      setll     TICMC000
019300160202if  1c                   if        %equal(TICMC02L)
019400160202     c     K01CMC02      reade     TICMC000
019500160202do  2c                   DoW       NOT %eof(TICMC02L)
019600160202if  3c                   if        %lookup( CMCncm : sk_CampagneAttive )
019700160202     c                                  > *zero
019800150130     c                   eval      wERR = 1
019900150130     c                   exsr      sr_StampaERR
020000150130     c                   leavesr
020100160202e   3c                   endif
020200160202     c     K01CMC02      reade     TICMC000
020300160202e   2c                   EndDo
020400160202e   1c                   endif
020500150130      *
020600150130     c                   eval      $RecOK = *on
020700150130      *
020800150130     c                   ENDSR
020900150130
021000150130      *------------------------------------------------------------------------*
021100150130      *?Scrittura record in TICMC / TICMI / TICMF.                            ?*
021200150130      *------------------------------------------------------------------------*
021300150130
021400150130     c     sr_WriteREC   BEGSR
021500150130      *
021600150130     c                   clear                   TICMC000
021700150130     c                   clear                   TICMI000
021800150130     c                   clear                   TICMF000
021900150130      *
022000150130     c                   exsr      sr_WrtRecCMC
022100150130     c                   exsr      sr_WrtRecCMI
022200150130     c                   exsr      sr_WrtRecCMF
022300150130      *
022400150130     c                   ENDSR
022500150130
022600150130      *------------------------------------------------------------------------*
022700150130      *?Scrittura record in TICMC00F.                                         ?*
022800150130      *------------------------------------------------------------------------*
022900150130
023000150130     c     sr_WrtRecCMC  BEGSR
023100150130      *
023200161115     c                   if        WAPobi_I = 1
023300161115     c                   eval      CMCncm = c_Cli_TDA
023400150205     c                   else
023500161115     c                   eval      CMCncm = c_Cli_BC
023600150205     c                   endif
023700150130     c                   eval      CMCksu = WAPcli
023800150203     c                   eval      CMCpru = 'BATCH '
023900150130     c                   eval      CMCdin = %dec( wDate_Iso )
024000161116     c                   eval      CMCufe = c_Fase_ObiettivoFinale
024100150130      *                  __________________
024200150130     c                   write     TICMC000
024300150130      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
024400150130     c                   ENDSR
024500150130
024600150130      *------------------------------------------------------------------------*
024700150130      *?Scrittura record in TICMI00F.                                         ?*
024800150130      *------------------------------------------------------------------------*
024900150130
025000150130     c     sr_WrtRecCMI  BEGSR
025100150130      *
025200150130      * -?Num. Campagna?
025300150205     c                   eval      CMIncm = CMCncm
025400150130      * -?Cod. Cliente Unificante?
025500150130     c                   eval      CMIksu = WAPcli
025600150130      * -?DTABC?
025700150130     c                   eval      CMIclv = WAPclv
025800150130      * -?Comm.le Unificante + relativi Distretto, Area e Filiale?
025900150130      * - -?reperimento comm.le unificante dal cliente?
026000150130     c                   clear                   TIBS69ds
026100150130     c                   clear                   ds_CNACO
026200150130     c                   clear                   ds_CNIND
026300150130     c                   clear                   ds_CNCLP
026400150130     c                   clear                   ds_FNCLS
026500150130     c                   eval      I69kac = WAPcli
026600150130     c                   eval      I69kcp = WAPcli
026700150130     c                   call      'TIBS69R'
026800150130     c                   parm                    TIBS69ds
026900150130     c                   parm                    ds_CNACO
027000150130     c                   parm                    ds_CNIND
027100150130     c                   parm                    ds_CNCLP
027200150130     c                   parm                    ds_FNCLS
027300150202if  2c                   if        O69err = *blanks
027400150130     c     CLPage        chain     AZCMM000
027500150202if  3c                   if        %found(AZCMM01L)
027600150130     c     CMMuni        chain     AZCMM000
027700150202if  4c                   if        %found(AZCMM01L)
027800150130     c                   eval      CMIcmm = CMMuni
027900150202e   4c                   endif
028000150202e   3c                   endif
028100150202e   2c                   endif
028200150202if  2c                   if        CMIcmm = *zeros
028300150130     c                   eval      wERR = 2
028400150130     c                   exsr      sr_StampaERR
028500150202e   2c                   endif
028600150130      * - -?filiale del comm.le unificante?
028700150130     c                   eval      CMIfcm = CMIcmm / 10000
028800150130      * - -?distretto e area del comm.le unificante?
028900150130     c     CMIfcm        chain     AZORG
029000150202if  2c                   if        %found(AZORG01L)
029100150130     c                   movel     ORGfl3        CMIdcm
029200150130     c                   z-add     ORGcar        CMIacm
029300150202e   2c                   endif
029400150130      * -?Delta (anno corrente)?
029500150130     c                   eval      CMIpde = WAPdac
029600150130      * -?Numero Spedizioni?
029700150130     c                   eval      CMInsp = WAPsac
029800150130      * -?Ricavi?
029900150130     c                   eval      CMIric = WAPfac
030000150202      * -?Peso Medio?
030100150203     c                   if        WAPkgc > *zero
030200150203     c                   eval      CMIpme = WAPkgc
030300150202     c                   endif
030400150130      * -?Istat?
030500150202     c                   eval      CMIist = WAPist
030600150204      * -?Cliente Nuovo/Acquisito?
030700150130     c                   eval      CMIcln = WAPcln
030800150204      * -?Mese Inizio per cliente nuovo?
030900150130     c                   eval      CMImes = WAPnew_mes
031000150130      * -?Fascia Delta?
031100150130     c                   eval      CMIfad = WAPfdl
031200150130      * -?Flag Operativi?
031300150130     c*//                eval      CMIflo = ......
031400150130      *                  __________________
031500150130     c                   write     TICMI000
031600150130      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
031700150130     c                   ENDSR
031800150130
031900150130      *------------------------------------------------------------------------*
032000150130      *?Scrittura record in TICMF00F.                                         ?*
032100150130      *------------------------------------------------------------------------*
032200150130
032300150130     c     sr_WrtRecCMF  BEGSR
032400150130      *
032500150205     c                   eval      CMFncm = CMCncm
032600150130     c                   eval      CMFksu = WAPcli
032700150130     c                   eval      CMFacm = c_Fase_ObiettivoIniziale
032800150130     c                   eval      CMFdfa = %dec( wDate_Iso )
032900150130     c                   eval      CMFhfc = wTime
033000150130     c                   eval      CMFfef = 046
033100150203     c                   eval      CMFpru = 'BATCH '
033200150203      * -?% Aumento (% Obiettivo Iniziale)?
033300150130     c                   eval      CMFpea = WAPobi_I
033400150202      * -?Fase Automatica?
033500150130     c                   eval      CMFaut = 'A'
033600161116      *//* -?Anno/Mese Confronto Fatturazione?
033700161116     c*//                eval      CMFaacf = *year
033800161116     c*//                eval      CMFmmcf = 01
033900150130      *                  __________________
034000150130     c                   write     TICMF000
034100150130      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
034200161116      *
034300161116     c                   eval      CMFacm = c_Fase_ObiettivoFinale
034400161116      *                  __________________
034500161116     c                   write     TICMF000
034600161116      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
034700150130     c                   ENDSR
034800150130
034900150130      *------------------------------------------------------------------------*
035000150130      *?Operazioni Finali.                                                    ?*
035100150130      *------------------------------------------------------------------------*
035200150130
035300150130     c     sr_RoutEnd    BEGSR
035400150130      *
035500150130      * -?SE stampato almeno un Errore: stampa "fine"?
035600150130     c                   if        %open(QSYSPRT)
035700150130     c                   except    PRTend
035800150130     c                   close     QSYSPRT
035900150130     c                   endif
036000150130      *
036100150130      * -?Chiusura applicazioni precedentemente aperte?
036200150130     c                   reset                   TIBS69ds
036300150130     c                   clear                   ds_CNACO
036400150130     c                   clear                   ds_CNIND
036500150130     c                   clear                   ds_CNCLP
036600150130     c                   clear                   ds_FNCLS
036700150130     c                   call      'TIBS69R'
036800150130     c                   parm                    TIBS69ds
036900150130     c                   parm                    ds_CNACO
037000150130     c                   parm                    ds_CNIND
037100150130     c                   parm                    ds_CNCLP
037200150130     c                   parm                    ds_FNCLS
037300150130      *
037400150130     c                   return
037500150130      *
037600150130     c                   ENDSR
037700150129
037800150129      *------------------------------------------------------------------------*
037900150130      *?Stampa singolo Errore.                                                ?*
038000150130      *------------------------------------------------------------------------*
038100150129
038200150129     c     sr_StampaERR  BEGSR
038300150129      *
038400150129     c                   if        Not  %open(QSYSPRT)
038500150129     c                   open      QSYSPRT
038600150129     c                   eval      *inOF = *on
038700150129     c                   endif
038800150129      *
038900150129     c                   clear                   prt_Msg
039000150129     c                   select
039100150129     c                   when      wERR = 1
039200150129     c                   eval      prt_Msg = 'CLIENTE ' +
039300150129     c                                       %editc( WAPcli : 'X' ) +
039400150129     c                                       ' GIÀ INSERITO'
039500150130     c                   when      wERR = 2
039600150130     c                   eval      prt_Msg = 'CLIENTE ' +
039700150130     c                                       %editc( WAPcli : 'X' ) +
039800150130     c                                       ': FALLITO IL REPERIMENTO +
039900150130     c                                        DEL COMM.LE UNIFICANTE DI ' +
040000150130     c                                       %editc( CLPage : 'X' )
040100150129     c                   other
040200150129     c                   eval      prt_Msg = 'CLIENTE ' +
040300150130     c                                       %editc( WAPcli : 'X' ) +
040400150130     c                                       ' ?????'
040500150129     c                   endsl
040600150129      *
040700150129     c                   if        *inOF
040800150129     c                   except    PRTtxt
040900150129     c                   eval      *inOF = *off
041000150129     c                   endif
041100150129      *
041200150129     c                   except    PRTdet
041300150129      *
041400150129     c                   ENDSR
041500150129
041600150129      *------------------------------------------------------------------------*
041700150129      *?O U T P U T                                                           ?*
041800150129      *------------------------------------------------------------------------*
041900150129
042000150129     oQSYSPRT   e            PRTtxt            1
042100150129     o                                              'LISTA ERRORI RILE-
042200150129     o                                              VATI IN FASE DI SC-
042300150129     o                                              RITTURA CAMPAGNA P-
042400150129     o                                              ER CLIENTE'
042500150129     o                       SDSpgm           +   5
042600150129     o                       SDSusr           +   5
042700150129     o                       wDate         y  +   5
042800150129     o                       wTime            +   3 '  :  :  '
042900150129     o                                        +   5 'Pag.'
043000150129     o                       Page          z  +   0
043100150129     o          e            PRTtxt      0
043200150129     o                                              'LISTA ERRORI RILE-
043300150129     o                                              VATI IN FASE DI SC-
043400150129     o                                              RITTURA CAMPAGNA P-
043500150129     o                                              ER CLIENTE'
043600150129     o          e            PRTtxt      1
043700150129     o                                              '------------------
043800150129     o                                              -------------------
043900150129     o                                              -------------------
044000150129     o                                              ----------'
044100150129     o          e            PRTtxt      0  1
044200150129     o                                              '------------------
044300150129     o                                              -------------------
044400150129     o                                              -------------------
044500150129     o                                              ----------'
044600150129      *
044700150129     o          e            PRTdet      1
044800150129     o                       prt_Msg
044900150129      *
045000150129     o          e            PRTend      2
045100150129     o                                              '***  Fine Lista  ***'
045200150129     o          e            PRTend      0
045300150129     o                                              '***  Fine Lista  ***'
