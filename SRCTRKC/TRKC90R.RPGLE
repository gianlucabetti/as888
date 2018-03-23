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
003900170116
004000170116       // -?Costante per controllo "caratteri solo numerici"?
004100170116     d c_Digits        c                   const('0123456789')
004200071026
004300150129      * -?Numero Campagna che si sta generando?
004400171018     d c_Cli_TDA       c                   const(9)
004500171018     d c_Cli_BC        c                   const(10)
004600170207     d c_Cli_Incomp    c                   const(7)
004700170207     d c_Cli_Delta40   c                   const(8)
004800150130
004900150204      * -?Fasi da impostare?
005000150130     d c_Fase_ObiettivoIniziale...
005100150130     d                 c                   const(' 10')
005200150204     d c_Fase_ObiettivoFinale...
005300150204     d                 c                   const(' 30')
005400150129
005500160202      *------------------------------------------------------------------------*
005600160202      *?S C H I E R E                                                         ?*
005700160202      *------------------------------------------------------------------------*
005800160202
005900160202      // -?Campagne attive ad oggi?
006000160202     d sk_CampagneAttive...
006100160202     d                 s                   like(CMPncm)  dim(9)  inz
006200160202
006300150129      *------------------------------------------------------------------------*
006400150129      *?S T R U T T U R E - D A T I                                           ?*
006500150129      *------------------------------------------------------------------------*
006600150129
006700150129      * -?Status?
006800150129     d Status         sds
006900150129     d   SDSpgm          *proc
007000150129     d   SDSprm          *parms
007100150129     d   SDSdta              191    198
007200150129     d   SDSjob              244    253
007300150129     d   SDSusr              254    263
007400150129      *
007500150129      * -?Data/Ora attuali?
007600150129     d wTime_ds        ds                  inz
007700150129     d   wDate                        8s 0 inz
007800150129     d   wTime                        6s 0 inz
007900031103
008000150129      * -?Parametri per pgm. TIBS69R (decodifica cliente)?
008100150129     d TIBS69ds      e ds                  inz
008200150130     d   I69tla      e                     inz('C')
008300150130     d ds_CNACO      e ds                  extname(CNACO00F) inz
008400150130     d ds_CNIND      e ds                  extname(CNIND00F) inz
008500150130     d ds_CNCLP      e ds                  extname(CNCLP00F) inz
008600150130     d ds_FNCLS      e ds                  extname(FNCLS00F) inz
008700150129
008800150129      *------------------------------------------------------------------------*
008900150129      *?V A R I A B I L I                                                     ?*
009000150129      *------------------------------------------------------------------------*
009100150129
009200150130      * -?Flags booleani?
009300150130     d $RecOK          s               n   inz
009400160202      *
009500160202      * -?Indici di schiera?
009600160202     d xx              s              3  0 inz
009700170116      *
009800170116      * -?Eventuale n° Campagna ricevuto come parametro?
009900170116     d prm_Campagna    s              7
010000170116     d wp_Campagna     s              7  0 inz
010100150130      *
010200150129      * -?Codice e Descrizione errore rilevato?
010300150129     d wERR            s              1  0 inz
010400150129     d prt_Msg         s            132    inz
010500150129      *
010600150129      * -?Data *ISO?
010700150129     d wDate_Iso       s               d   datfmt(*ISO)    inz(*loval)
010800150129      * -?Data *EUR?
010900150129     d wDate_Eur       s               d   datfmt(*EUR)    inz(*loval)
011000170206      * -?Orarrio?
011100170206     d wTime_HMS       s               t   timfmt(*HMS)    inz(*loval)
011200150130
011300150130      *------------------------------------------------------------------------*
011400150130      *?D E F I N I Z I O N E   K E Y - L I S T                               ?*
011500150130      *------------------------------------------------------------------------*
011600150130
011700150130     d k_CMCncm        s                   like(CMCncm)    inz
011800150130     d k_CMCksu        s                   like(CMCksu)    inz
011900150130      *
012000150203     c     K01CMC02      klist
012100150130     c                   kfld                    k_CMCksu
012200150129
012300031103      *------------------------------------------------------------------------*
012400150129      *?M A I N - L I N E                                                     ?*
012500150129      *------------------------------------------------------------------------*
012600170116
012700170116     c     *Entry        plist
012800170116     c                   parm                    prm_Campagna
012900031103
013000150130      * -?Operazioni Iniziali?
013100150130     c                   exsr      sr_RoutInz
013200150129      *
013300150129      * -?Ciclo di lettura file WADELAUM?
013400150129do  1c                   DO        *hival
013500031103      *
013600150129     c                   read      WADELAUM0
013700031103      *
013800150129     c                   if        %eof(WADELAUM)
013900031103     c                   leave
014000031103     c                   endif
014100141027      *
014200150130      * - -?Selezione record da elaborare?
014300150130     c                   exsr      sr_SelezRec
014400150130     c                   if        Not $RecOk
014500150130     c                   iter
014600150130     c                   endif
014700150130      *
014800150130      * - -?Scrittura record in TICMC / TICMI / TICMF?
014900150130     c                   exsr      sr_WriteREC
015000150130      *
015100150129e   1c                   ENDDO
015200150129      *
015300150130      * -?Operazioni Finali?
015400150130     c                   exsr      sr_RoutEnd
015500150130
015600150130      *------------------------------------------------------------------------*
015700150130      *?Operazioni Iniziali.                                                  ?*
015800150130      *------------------------------------------------------------------------*
015900150130
016000150130     c     sr_RoutInz    BEGSR
016100150130      *
016200150130     c                   eval      *inLR = *on
016300150130      *
016400150130      * -?Reperimento data e ora attuali?
016500150130     c                   eval      wTime_ds  = %editc( %dec( %timestamp() )
016600150130     c                                                 : 'X' )
016700150130     c                   eval      wDate_Iso = %date( wDate : *Iso )
016800150130     c                   eval      wDate_Eur = wDate_Iso
016900160202     c                   eval      wDate     = %dec( wDate_Eur )
017000170206     c                   eval      wTime_HMS  = %time( wTime : *HMS )
017100170206     c                   eval      wTime_HMS += %minutes(1)
017200160202      /free
017300160202         // -?Memorizzazione Campagne Attive oggi?
017400160202         setll  ( *loval )  TICMP000;
017500160202         read   TICMP000;
017600160202         DoW  NOT %eof( TICMP01L );
017700160202           if  CMPdfc >= %dec( wDate_Iso );
017800160202             xx += 1;
017900160202             sk_CampagneAttive(xx) = CMPncm;
018000160202           endif;
018100160202           read  TICMP000;
018200160202         EndDo;
018300170116
018400170116         // -?Verifica SE ricevute parzializzazioni?
018500170116         clear  wp_Campagna;
018600170116         if  %parms()               > *zero   and
018700170116             %trim( prm_Campagna ) <> *blank  and
018800170116             %check( c_Digits : %trim( prm_Campagna ) ) = *zero;
018900170116           wp_Campagna = %int( %trim( prm_Campagna ) );
019000170116         endif;
019100170116
019200160202      /end-free
019300150130      *
019400150130     c                   ENDSR
019500150130
019600150130      *------------------------------------------------------------------------*
019700150130      *?Selezione record in lettura.                                          ?*
019800150130      *------------------------------------------------------------------------*
019900150130
020000150130     c     sr_SelezRec   BEGSR
020100150130      *
020200150130     c                   eval      $RecOK = *off
020300150130      *
020400150203      * -?Selezione dei rec. con Tipo AUmento "C" o "L"?
020500160202     c*//                if        WAPtau <> c_TipoAumento_Campagna  and
020600160202     c*//                          WAPtau <> c_TipoAumento_Lettera
020700160202     c*//                leavesr
020800160202     c*//                endif
020900150130      *
021000150130      * -?Esclusione dei clienti esteri?
021100150130     c*//                if        WAPioe = 'E'
021200150130     c*//                leavesr
021300150130     c*//                endif
021400150130      *
021500150130      * -?Esclusione dei clienti già inseriti?
021600150130     c                   eval      k_CMCksu = WAPcli
021700150203     c     K01CMC02      setll     TICMC000
021800160202if  1c                   if        %equal(TICMC02L)
021900160202     c     K01CMC02      reade     TICMC000
022000160202do  2c                   DoW       NOT %eof(TICMC02L)
022100160202if  3c                   if        %lookup( CMCncm : sk_CampagneAttive )
022200160202     c                                  > *zero
022300150130     c                   eval      wERR = 1
022400150130     c                   exsr      sr_StampaERR
022500150130     c                   leavesr
022600160202e   3c                   endif
022700160202     c     K01CMC02      reade     TICMC000
022800160202e   2c                   EndDo
022900160202e   1c                   endif
023000150130      *
023100150130     c                   eval      $RecOK = *on
023200150130      *
023300150130     c                   ENDSR
023400150130
023500150130      *------------------------------------------------------------------------*
023600150130      *?Scrittura record in TICMC / TICMI / TICMF.                            ?*
023700150130      *------------------------------------------------------------------------*
023800150130
023900150130     c     sr_WriteREC   BEGSR
024000150130      *
024100150130     c                   clear                   TICMC000
024200150130     c                   clear                   TICMI000
024300150130     c                   clear                   TICMF000
024400150130      *
024500150130     c                   exsr      sr_WrtRecCMC
024600150130     c                   exsr      sr_WrtRecCMI
024700150130     c                   exsr      sr_WrtRecCMF
024800150130      *
024900150130     c                   ENDSR
025000150130
025100150130      *------------------------------------------------------------------------*
025200150130      *?Scrittura record in TICMC00F.                                         ?*
025300150130      *------------------------------------------------------------------------*
025400150130
025500150130     c     sr_WrtRecCMC  BEGSR
025600150130      *
025700171018     c                   if        WAPobi_I = 1
025800171018     c                   eval      CMCncm = c_Cli_TDA
025900171018     c                   else
026000171018     c                   eval      CMCncm = c_Cli_BC
026100171018     c                   endif
026200170116      *
026300171018     c*//                select
026400170116      * -?Impostazione Campagna reperita da WADELAUM.WAPAAD?
026500171018     c*//                when      WAPaad > *zero
026600171018     c*//                eval      CMCncm = WAPaad
026700170116      * -?Forzatura campagna ricevuta come parametro?
026800171018     c*//                when      wp_Campagna > *zero
026900171018     c*//                eval      CMCncm      = wp_Campagna
027000170116      * -?Forzatura Campagna 7 o 8?
027100171018     c*//                other
027200171018     c*//                eval      CMCncm = c_Cli_Incomp
027400171018     c*//                endsl
027500170116      *
027600150130     c                   eval      CMCksu = WAPcli
027700150203     c                   eval      CMCpru = 'BATCH '
027800150130     c                   eval      CMCdin = %dec( wDate_Iso )
027900161116     c                   eval      CMCufe = c_Fase_ObiettivoFinale
028000150130      *                  __________________
028100150130     c                   write     TICMC000
028200150130      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
028300150130     c                   ENDSR
028400150130
028500150130      *------------------------------------------------------------------------*
028600150130      *?Scrittura record in TICMI00F.                                         ?*
028700150130      *------------------------------------------------------------------------*
028800150130
028900150130     c     sr_WrtRecCMI  BEGSR
029000150130      *
029100150130      * -?Num. Campagna?
029200150205     c                   eval      CMIncm = CMCncm
029300150130      * -?Cod. Cliente Unificante?
029400150130     c                   eval      CMIksu = WAPcli
029500150130      * -?DTABC?
029600150130     c                   eval      CMIclv = WAPclv
029700150130      * -?Comm.le Unificante + relativi Distretto, Area e Filiale?
029800150130      * - -?reperimento comm.le unificante dal cliente?
029900150130     c                   clear                   TIBS69ds
030000150130     c                   clear                   ds_CNACO
030100150130     c                   clear                   ds_CNIND
030200150130     c                   clear                   ds_CNCLP
030300150130     c                   clear                   ds_FNCLS
030400150130     c                   eval      I69kac = WAPcli
030500150130     c                   eval      I69kcp = WAPcli
030600150130     c                   call      'TIBS69R'
030700150130     c                   parm                    TIBS69ds
030800150130     c                   parm                    ds_CNACO
030900150130     c                   parm                    ds_CNIND
031000150130     c                   parm                    ds_CNCLP
031100150130     c                   parm                    ds_FNCLS
031200150202if  2c                   if        O69err = *blanks
031300150130     c     CLPage        chain     AZCMM000
031400150202if  3c                   if        %found(AZCMM01L)
031500150130     c     CMMuni        chain     AZCMM000
031600150202if  4c                   if        %found(AZCMM01L)
031700150130     c                   eval      CMIcmm = CMMuni
031800150202e   4c                   endif
031900150202e   3c                   endif
032000150202e   2c                   endif
032100150202if  2c                   if        CMIcmm = *zeros
032200150130     c                   eval      wERR = 2
032300150130     c                   exsr      sr_StampaERR
032400150202e   2c                   endif
032500150130      * - -?filiale del comm.le unificante?
032600150130     c                   eval      CMIfcm = CMIcmm / 10000
032700150130      * - -?distretto e area del comm.le unificante?
032800150130     c     CMIfcm        chain     AZORG
032900150202if  2c                   if        %found(AZORG01L)
033000150130     c                   movel     ORGfl3        CMIdcm
033100150130     c                   z-add     ORGcar        CMIacm
033200150202e   2c                   endif
033300150130      * -?Delta (anno corrente)?
033400150130     c                   eval      CMIpde = WAPdac
033500150130      * -?Numero Spedizioni?
033600150130     c                   eval      CMInsp = WAPsac
033700150130      * -?Ricavi?
033800150130     c                   eval      CMIric = WAPfac
033900150202      * -?Peso Medio?
034000150203     c                   if        WAPkgc > *zero
034100150203     c                   eval      CMIpme = WAPkgc
034200150202     c                   endif
034300150130      * -?Istat?
034400150202     c                   eval      CMIist = WAPist
034500150204      * -?Cliente Nuovo/Acquisito?
034600150130     c                   eval      CMIcln = WAPcln
034700150204      * -?Mese Inizio per cliente nuovo?
034800150130     c                   eval      CMImes = WAPnew_mes
034900150130      * -?Fascia Delta?
035000150130     c                   eval      CMIfad = WAPfdl
035100150130      * -?Flag Operativi?
035200150130     c*//                eval      CMIflo = ......
035300150130      *                  __________________
035400150130     c                   write     TICMI000
035500150130      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
035600150130     c                   ENDSR
035700150130
035800150130      *------------------------------------------------------------------------*
035900150130      *?Scrittura record in TICMF00F.                                         ?*
036000150130      *------------------------------------------------------------------------*
036100150130
036200150130     c     sr_WrtRecCMF  BEGSR
036300150130      *
036400150205     c                   eval      CMFncm = CMCncm
036500150130     c                   eval      CMFksu = WAPcli
036600150130     c                   eval      CMFacm = c_Fase_ObiettivoIniziale
036700150130     c                   eval      CMFdfa = %dec( wDate_Iso )
036800150130     c                   eval      CMFhfc = wTime
036900150130     c                   eval      CMFfef = 046
037000150203     c                   eval      CMFpru = 'BATCH '
037100150203      * -?% Aumento (% Obiettivo Iniziale)?
037200150130     c                   eval      CMFpea = WAPobi_I
037300150202      * -?Fase Automatica?
037400150130     c                   eval      CMFaut = 'A'
037500161116      *//* -?Anno/Mese Confronto Fatturazione?
037600161116     c*//                eval      CMFaacf = *year
037700161116     c*//                eval      CMFmmcf = 01
037800150130      *                  __________________
037900150130     c                   write     TICMF000
038000150130      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
038100161116      *
038200161116     c                   eval      CMFacm = c_Fase_ObiettivoFinale
038300170207     c                   eval      CMFhfc = %dec( wTime_HMS )
038400161116      *                  __________________
038500161116     c                   write     TICMF000
038600161116      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
038700150130     c                   ENDSR
038800150130
038900150130      *------------------------------------------------------------------------*
039000150130      *?Operazioni Finali.                                                    ?*
039100150130      *------------------------------------------------------------------------*
039200150130
039300150130     c     sr_RoutEnd    BEGSR
039400150130      *
039500150130      * -?SE stampato almeno un Errore: stampa "fine"?
039600150130     c                   if        %open(QSYSPRT)
039700150130     c                   except    PRTend
039800150130     c                   close     QSYSPRT
039900150130     c                   endif
040000150130      *
040100150130      * -?Chiusura applicazioni precedentemente aperte?
040200150130     c                   reset                   TIBS69ds
040300150130     c                   clear                   ds_CNACO
040400150130     c                   clear                   ds_CNIND
040500150130     c                   clear                   ds_CNCLP
040600150130     c                   clear                   ds_FNCLS
040700150130     c                   call      'TIBS69R'
040800150130     c                   parm                    TIBS69ds
040900150130     c                   parm                    ds_CNACO
041000150130     c                   parm                    ds_CNIND
041100150130     c                   parm                    ds_CNCLP
041200150130     c                   parm                    ds_FNCLS
041300150130      *
041400150130     c                   return
041500150130      *
041600150130     c                   ENDSR
041700150129
041800150129      *------------------------------------------------------------------------*
041900150130      *?Stampa singolo Errore.                                                ?*
042000150130      *------------------------------------------------------------------------*
042100150129
042200150129     c     sr_StampaERR  BEGSR
042300150129      *
042400150129     c                   if        Not  %open(QSYSPRT)
042500150129     c                   open      QSYSPRT
042600150129     c                   eval      *inOF = *on
042700150129     c                   endif
042800150129      *
042900150129     c                   clear                   prt_Msg
043000150129     c                   select
043100150129     c                   when      wERR = 1
043200150129     c                   eval      prt_Msg = 'CLIENTE ' +
043300150129     c                                       %editc( WAPcli : 'X' ) +
043400150129     c                                       ' GIÀ INSERITO'
043500150130     c                   when      wERR = 2
043600150130     c                   eval      prt_Msg = 'CLIENTE ' +
043700150130     c                                       %editc( WAPcli : 'X' ) +
043800150130     c                                       ': FALLITO IL REPERIMENTO +
043900150130     c                                        DEL COMM.LE UNIFICANTE DI ' +
044000150130     c                                       %editc( CLPage : 'X' )
044100150129     c                   other
044200150129     c                   eval      prt_Msg = 'CLIENTE ' +
044300150130     c                                       %editc( WAPcli : 'X' ) +
044400150130     c                                       ' ?????'
044500150129     c                   endsl
044600150129      *
044700150129     c                   if        *inOF
044800150129     c                   except    PRTtxt
044900150129     c                   eval      *inOF = *off
045000150129     c                   endif
045100150129      *
045200150129     c                   except    PRTdet
045300150129      *
045400150129     c                   ENDSR
045500150129
045600150129      *------------------------------------------------------------------------*
045700150129      *?O U T P U T                                                           ?*
045800150129      *------------------------------------------------------------------------*
045900150129
046000150129     oQSYSPRT   e            PRTtxt            1
046100150129     o                                              'LISTA ERRORI RILE-
046200150129     o                                              VATI IN FASE DI SC-
046300150129     o                                              RITTURA CAMPAGNA P-
046400150129     o                                              ER CLIENTE'
046500150129     o                       SDSpgm           +   5
046600150129     o                       SDSusr           +   5
046700150129     o                       wDate         y  +   5
046800150129     o                       wTime            +   3 '  :  :  '
046900150129     o                                        +   5 'Pag.'
047000150129     o                       Page          z  +   0
047100150129     o          e            PRTtxt      0
047200150129     o                                              'LISTA ERRORI RILE-
047300150129     o                                              VATI IN FASE DI SC-
047400150129     o                                              RITTURA CAMPAGNA P-
047500150129     o                                              ER CLIENTE'
047600150129     o          e            PRTtxt      1
047700150129     o                                              '------------------
047800150129     o                                              -------------------
047900150129     o                                              -------------------
048000150129     o                                              ----------'
048100150129     o          e            PRTtxt      0  1
048200150129     o                                              '------------------
048300150129     o                                              -------------------
048400150129     o                                              -------------------
048500150129     o                                              ----------'
048600150129      *
048700150129     o          e            PRTdet      1
048800150129     o                       prt_Msg
048900150129      *
049000150129     o          e            PRTend      2
049100150129     o                                              '***  Fine Lista  ***'
049200150129     o          e            PRTend      0
049300150129     o                                              '***  Fine Lista  ***'
