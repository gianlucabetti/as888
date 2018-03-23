000100071024      * FIOR37R2 *----------------------------------------------------*
000200071022      *?ATTRIBUZIONE GIRO AD ANAGRAFICA CLIENTI RITIRO               ?*
000300071022      *---------------------------------------------------------------*
000400071022
000500071022     h decedit('0,') datedit(*YMD.) option(*nodebugio)
000600071022
000700071022      *---------------------------------------------------------------*
000800071022
000900071022     fFNACR01L  if   e           k disk
001000071030     fFNACR13L  Uf A e           k disk
001100071022      *
001200071024     fFIOR37D2  cf   e             workstn
001300071022
001400071022      *
001500071022      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - -?*
001600071022      *
001700071022      * - Titoli in testata
001800071025     d C_Txt_1         c                   const('*   al SINGOLO Clien-
001900071022     d                                     te Ritiro   *')
002000071025     d C_Txt_2         c                   const('*     a PIU'' Client-
002100071022     d                                     i Ritiro     *')
002200071105      * - Num. Max di codici giro ritiro inseribili per cliente ritiro
002300071105     d C_MaxCGI        c                   const(03)
002400071022      *
002500071022      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - -?*
002600071022      *
002700071022      * - Messaggi di errore
002800071022     D $Msg            S             78    dim(11)  ctdata  perrcd(1)
002900071108      * - Filiali in £1
003000071108     d $L1             s              3  0 dim(30)  inz                         P.O. Gestiti £1
003100071022      *
003200071022      *?  S T R U T T U R E   D A T I   - - - - - - - - - - - - - - -?*
003300071022      *
003400071022      * - Parametri
003500071022     d KPJBA         e ds
003600071024     d FIOR37ds      e ds
003700071022      *
003800071022      * - Reperimento dati utente
003900071022     d TIBS34ds      e ds
004000071022     d AZUTEds       e ds                  extname(AZUTE00F)
004100071022     d dDatiUte      e ds
004200071108      *
004300071108      * - Caricamento schiera filiali
004400071108     d TRUL06ds      e ds                  inz
004500071108     d  D06lin                 1     90  0 inz  dim(30)
004600071022      *
004700071022      * - Lettura anagrafiche cliente
004800071022     d TIBS69ds      e ds                  inz
004900071026     d  I69tla       e                     inz('L')
005000071022     d ds_CNACO      e ds                  inz  extname(CNACO00F)
005100071022     d ds_CNIND      e ds                  inz  extname(CNIND00F)
005200071022     d ds_CNCLP      e ds                  inz  extname(CNCLP00F)
005300071022     d ds_FNCLS      e ds                  inz  extname(FNCLS00F)
005400071022      *
005500071022      * - Gestione anagrafica giri
005600071022     d FIDG09ds      e ds                  inz
005700071022      *
005800071022     d Status         sds           333
005900071025     d  SDSpgm           *proc
006000071022      *
006100071022      * - Codice cliente ritiro
006200071022     d ds_CRO          ds            10    inz
006300071022     d  dsCRO1                        3  0 inz
006400071022     d  dsCRO2                        4  0 inz
006500071022     d  dsCRO3                        3  0 inz
006600071022      *
006700071022      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
006800071022      *
006900071022      * - Flags
007000071022     d $InzD01         s              1    inz(*on)
007100071022     d $Fine           s              1    inz(*off)
007200071031      * - Indici di schiera / Contatori
007300071031     d wNrCGI          s              3  0 inz
007400071022      * - Comodo
007500071030     d SAVpocgi        s                   inz  like(V1Cpocgi1)
007600071030     d SAVcgi          s                   inz  like(V1Ccgi1)
007700071030     d Wpocgi          s                   inz  like(V1Cpocgi1)
007800071030     d Wcgi            s                   inz  like(V1Ccgi1)
007900071030     d WcgiD           s                   inz  like(V1Dcgi1)
008000071022      *
008100071022      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
008200071022      *
008300071030      * - FNACR13L
008400071030     c     k02acr13      klist
008500071024     c                   kfld                    I37cro
008600071024     c                   kfld                    I37pocgi
008700071030     c     k03acr13      klist
008800071030     c                   kfld                    I37cro
008900071030     c                   kfld                    I37pocgi
009000071030     c                   kfld                    ACR1ain
009100071022
009200071022      *---------------------------------------------------------------*
009300071022      *?RIEPILOGO INDICATORI                                         ?*
009400071022      *---------------------------------------------------------------*
009500071022      *  25    - Comodo                                               *
009600071022      *  28    - Emissione del messaggio di errore a video            *
009700071022      *  41    - Attribuzione giro su più Clienti Ritiro              *
009800071031      *  51    - Filiale Giro Ritiro (=) errata                       *
009900071031      *  52    - Codice  Giro Ritiro (=) errato                       *
010000071031      *  53    - Filiale Giro Ritiro (>) errata                       *
010100071031      *  54    - Codice  Giro Ritiro (>) errato                       *
010200071031      *  55    - Filiale Giro Ritiro (<) errata                       *
010300071031      *  56    - Codice  Giro Ritiro (<) errato                       *
010400071022      *  90    - Generico di errore                                   *
010500071022      *---------------------------------------------------------------*
010600071022
010700071022     c     *Entry        plist
010800071022     c                   parm                    KPJBA
010900071022      *
011000071022      * Operazioni iniziali
011100071022     c                   exsr      RoutInz
011200071022      *
011300071022      * Gestione video
011400071022do  1c                   dow       $Fine   = *off
011500071022     c                   exsr      GesD01
011600071022e   1c                   enddo
011700071022      *
011800071022      * Operazioni finali
011900071024     c                   movel(p)  FIOR37ds      KPJBU
012000071022     c                   eval      *inLR    = *on
012100071022
012200071022      *---------------------------------------------------------------*
012300071022      *?Operazioni Iniziali                                          ?*
012400071022      *---------------------------------------------------------------*
012500071022     c     RoutInz       BEGSR
012600071022      *
012700071022      * Reperimento dati job
012800071022     c                   exsr      DatiJob
012900071022      *
013000071022      * Impostazione nome programma a video
013100071022     c                   movel     SDSpgm        V1Tpgm
013200071022      *
013300071022      * Impostazione parametri ricevuti
013400071022     c                   if        KPJBU    <> *blanks
013500071024     c                   movel     KPJBU         FIOR37ds
013600071022     c                   else
013700071022     c                   eval      $Fine    =  *on
013800071108     c                   leavesr
013900071022     c                   endif
014000071108      *
014100071108      * Reperimento filiali gestite (£1)
014200071108     c                   clear                   TRUL06ds
014300071108     c                   eval      D06cod   = '£1'
014400071108     c                   movel(p)  I37fgs        D06key
014500071108     c                   movel(p)  TRUL06ds      KPJBU
014600071108     c                   call      'TRUL06R'
014700071108     c                   parm                    KPJBA
014800071108     c                   movel     KPJBU         TRUL06ds
014900071108     c                   movea     D06lin        $L1
015000071022      *
015100071022     c                   ENDSR
015200071022
015300071022      *---------------------------------------------------------------*
015400071022      *?Reperimento Dati del job (Utente/Operativi)                  ?*
015500071022      *---------------------------------------------------------------*
015600071022     c     DatiJob       BEGSR
015700071022      *
015800071022     c     *dtaara       define    §azute        azuteds
015900071022     c     *dtaara       define    §datiute      ddatiute
016000071022      *
016100071022     c                   in(E)     *dtaara
016200071022     c                   IF        %ERROR or RSUT = *blanks
016300071022     c                   clear                   Tibs34Ds
016400071022     c                   call      'TIBS34R'
016500071022     c                   parm                    Tibs34Ds
016600071022     c                   in        *dtaara
016700071022     c                   ENDIF
016800071022      *
016900071022     c                   ENDSR
017000071022
017100071022      *---------------------------------------------------------------*
017200071022      *?Controllo e decodifica giro ritiro                           ?*
017300071022      *---------------------------------------------------------------*
017400071022     c     Ctr_Giro      BEGSR
017500071030      *
017600071030      * - immessa solo la filiale (senza codice giro)
017700071030if  1c                   if             Wpocgi  <> *zeros
017800071030     c                             and  Wcgi     = *blanks
017900071030     c***                seton                                        285290
018000071030     c***                eval      V1Dmsg   =  $Msg(6)
018100071030     c***                leavesr
018200071031     c                   clear                   Wpocgi
018300071031     c                   clear                   WcgiD
018400071030e   1c                   endif
018500071030      *
018600071030      * - immesso solo il codice giro (senza filiale)
018700071030if  1c                   if             Wpocgi   = *zeros
018800071030     c                             and  Wcgi    <> *blanks
018900071030     c***                seton                                        285190
019000071030     c***                eval      V1Dmsg   =  $Msg(7)
019100071030     c***                leavesr
019200071031     c                   eval      Wpocgi   =  I37pocgi
019300071030e   1c                   endif
019400071030      *
019500071030      * - controllo codici filiale giri ritiro
019600071030      *   (filiale giro diversa dalla filiale utente)
019700071030if  1c                   if             Wpocgi  <> DUTpou
019800071030     c                             and  Wcgi    <> *blank
019900071108if  2c                   if        %lookup(Wpocgi:$L1) = *zeros
020000071031     c***                seton                                        285190
020100071031     c                   seton                                        285290
020200071030     c                   eval      V1Dmsg   =  $Msg(8)
020300071030     c                   leavesr
020400071108e   2c                   endif
020500071030e   1c                   endif
020600071022      *
020700071023      * - controllo  CON  data validità ("bypassabile")
020800071031if  1c                   IF             Wpocgi  <> D09ifgs
020900071031     c                             or   Wcgi    <> D09icgi
021000071031     c                   clear                   FIDG09ds
021100071022     c                   eval      D09iop0  =  '001'
021200071031     c                   eval      D09ifgs  =  Wpocgi
021300071031     c                   eval      D09icgi  =  Wcgi
021400071022     c                   eval      D09idat  =  *date
021500071022     c                   eval      D09itug  =  'R'
021600071022     c                   movel(p)  FIDG09ds      KPJBU
021700071022     c                   call      'FIDG09R'
021800071022     c                   parm                    KPJBA
021900071022     c                   movel     KPJBU         FIDG09ds
022000071105e   1c                   ENDIF
022100071105      *
022200071105sel 1c                   select
022300071105w   1c                   when      D09oerr  = *on
022400071105     c                   movel     D09odes       WcgiD
022500071031     c                   seton                                        285290
022600071031     c                   eval      V1Dmsg   =  $Msg(10)
022700071112     c                   leavesr
022800071105w   1c                   when      D09ocgi  <> *blanks
022900071031     c                   movel     D09odes       WcgiD
023000071105e   1c                   endsl
023100071022      *
023200071022     c                   ENDSR
023300071022
023400071022      *---------------------------------------------------------------*
023500071022      *?Interrogazione giri ritiro                                   ?*
023600071022      *---------------------------------------------------------------*
023700071022     c     Search_Giro   BEGSR
023800071030      *
023900071031     c                   clear                   Wpocgi
024000071031     c                   clear                   Wcgi
024100071030     c                   clear                   WcgiD
024200071022      *
024300071022     c                   clear                   FIDG09ds
024400071022     c                   eval      D09iop0  =  'P01'
024500071030     c                   eval      D09ifgs  =  I37pocgi
024600071030     c                   eval      D09idat  =  *date
024700071022     c                   movel     VPosInt       D09ides
024800071022     c                   eval      D09itug  =  'R'
024900071022      *
025000071022     c                   movel(p)  FIDG09ds      KPJBU
025100071022     c                   call      'FIDG09R'
025200071022     c                   parm                    KPJBA
025300071022     c                   movel     KPJBU         FIDG09ds
025400071022      *
025500071022if  1c                   if             D09Of03 <> *on
025600071022     c                             and  D09Of12 <> *on
025700071022     c                             and  D09Oerr <> *on
025800071030     c                   eval      Wpocgi   =  D09ofgs
025900071030     c                   eval      Wcgi     =  D09ocgi
026000071030     c                   movel     D09odes       WcgiD
026100071022e   1c                   endif
026200071022      *
026300071022     c                   ENDSR
026400071022
026500071022      *---------------------------------------------------------------*
026600071022      *?Gestione videata D01                                         ?*
026700071022      *---------------------------------------------------------------*
026800071022     c     GesD01        BEGSR
026900071022      *
027000071022      * Inizializzazione videata
027100071022if  1c                   if        $InzD01  =  *on
027200071022     c                   exsr      InzD01
027300071022     c                   eval      $InzD01  =  *off
027400071022e   1c                   endif
027500071023      *
027600071023      * Emissione testata
027700071024     c                   write     OR37T01
027800071022      *
027900071022      * Emissione videata
028000071024     c                   exfmt     OR37D01
028100071022     c                   setoff                                       28  90
028200071022     c                   clear                   V1Dmsg
028300071022      *
028400071022sel 1c                   SELECT
028500071022      * F3=Fine
028600071022w   1c                   WHEN      *inKC
028700071022     c                   exsr      F03D01
028800071022      * F12=Ritorno
028900071022w   1c                   WHEN      *inKL
029000071022     c                   exsr      F12D01
029100071022      * Enter / F6=Conferma
029200071022x   1c                   OTHER
029300071022     c                   exsr      CtrD01
029400071022if  2c                   if        NOT  *in90
029500071022     c                             and  *inKF
029600071030     c                   exsr      F06D01
029700071022if  3c                   if        NOT  *in90
029800071022     c                   eval      $Fine    =  *on
029900071022e   3c                   endif
030000071022e   2c                   endif
030100071022e   1c                   ENDSL
030200071022      *
030300071022     c                   ENDSR
030400071022
030500071022      *---------------------------------------------------------------*
030600071022      *?Inizializzazione videata D01                                 ?*
030700071022      *---------------------------------------------------------------*
030800071022     c     InzD01        BEGSR
030900071022      *
031000071024     c                   clear                   OR37D01
031100071026      *
031200071026      * Impostazione indicatori utilizzati nella videata
031300071026     c                   eval      *in41    =  (I37opz = '9')
031400071022      *
031500071022      * Impostazione testata
031600071025if  1c                   if        I37opz   =  '9'
031700071025     c                   eval      V1Ttxt   =  C_Txt_2
031800071025x   1c                   else
031900071025     c                   eval      V1Ttxt   =  C_Txt_1
032000071025e   1c                   endif
032100071022      *
032200071022      * Impostazione campi a video
032300071022      *
032400071024     c     I37cro        chain     FNACR000
032500071022      *
032600071022if  1c                   IF        %found(FNACR01L)
032700071022      *
032800071022     c                   eval      H1Ccro   =  ACRcro
032900071022     c                   eval      H1Ccco   =  ACRcco
033000071022      *
033100071022     c                   movel     ACRcro        ds_CRO
033200071022     c                   eval      V1Ccd1   =  dsCRO1
033300071022     c                   eval      V1Ccd2   =  dsCRO2
033400071022     c                   eval      V1Ccd3   =  dsCRO3
033500071022     c                   eval      V1Crsr   =  ACRrsr
033600071022     c                   eval      V1Cinr   =  ACRinr
033700071022     c                   eval      V1Ccar   =  ACRcar
033800071022     c                   eval      V1Ccar   =  ACRcar
033900071022     c                   eval      V1Clor   =  ACRlor
034000071022     c                   eval      V1Cprr   =  ACRprr
034100071022     c                   eval      V1Cnar   =  ACRnar
034200071022if  2c                   if        ACRcco   <> *zeros
034300071022     c                   movel     ACRcco        ds_CRO
034400071022     c                   move      dsCRO1        V1Cco1
034500071022     c                   eval      V1Cco2   =  dsCRO2
034600071022     c                   move      dsCRO3        V1Cco3
034700071022e   2c                   endif
034800071022     c                   eval      V1Cksc   =  ACRksc
034900071022if  2c                   if        ACRccc   <> 999
035000071022     c                   z-add     ACRccc        V1Cctr
035100071022e   2c                   endif
035200071022sel 2c                   select
035300071022w   2c                   when      ACRtcr   =  'F'
035400071022     c                   eval      V1Dtcr   =  'Fisso               '
035500071022w   2c                   when      ACRtcr   =  'R'
035600071022     c                   eval      V1Dtcr   =  'Ricorrente          '
035700071022w   2c                   when      ACRtcr   =  'O'
035800081015     c                   eval      V1Dtcr   =  'No Internet         '
035900071022w   2c                   when      ACRtcr   =  'M'
036000071022     c                   eval      V1Dtcr   =  'Mai                 '
036100071022x   2c                   other
036200071022     c                   eval      V1Dtcr   =  '> ' + ACRtcr + ' <'
036300071022e   2c                   endsl
036400071022if  2c                   if        ACRfcc  <>  'N'
036500071022     c                   eval      V1Cfcc   =  'SI'
036600071022x   2c                   else
036700071022     c                   eval      V1Cfcc   =  'NO'
036800071022e   2c                   endif
036900071022     c                   eval      V1Corr   =  ACRorr
037000071022     c                   eval      V1Cper   =  ACRper
037100071022     c                   eval      V1Ctel   =  ACRtel
037200071022     c*** NO mail:       eval      V1Cmai   =  ACRmai
037300071022     c                   eval      V1Cfcl   =  ACRfcl
037400071022     c                   eval      V1Cfbn   =  ACRfbn
037500071022     c                   eval      V1Cfpk   =  ACRfpk
037600071022     c                   eval      V1Cfmc   =  ACRfmc
037700071022     c                   eval      V1Cfbl   =  ACRfbl
037800071022     c                   eval      V1Cfmt   =  ACRfmt
037900071022     c                   eval      V1Cfat   =  ACRfat
038000071022if  2c                   if        ACRspi   =  'S'
038100071022     c                   eval      V1Cspi   =  'SI'
038200071022x   2c                   else
038300071022     c                   eval      V1Cspi   =  'NO'
038400071022e   2c                   endif
038500071022     c                   eval      V1Cntm   =  ACRntm
038600071022     c                   eval      V1Cno1   =  ACRno1
038700071022     c                   eval      V1Cno2   =  ACRno2
038800071022      *
038900071022e   1c                   ENDIF
039000071022      *
039100071030     c     k02acr13      setll     FNACR100
039200071030     c     k02acr13      reade     FNACR100
039300071030do  1c                   DOW       NOT  %eof(FNACR13L)
039400071022     c                             and  ACR1atb = *blanks
039500071030sel 2c                   select
039600071030w   2c                   when      ACR1atb  <> ACRatb
039700071030w   2c                   when      ACR1ain   = '='
039800071030     c                   eval      V1Cpocgi1 = ACR1pocgi
039900071030     c                   eval      V1Ccgi1   = ACR1cgi
040000071030w   2c                   when      ACR1ain   = '>'
040100071030     c                   eval      V1Cpocgi2 = ACR1pocgi
040200071030     c                   eval      V1Ccgi2   = ACR1cgi
040300071030w   2c                   when      ACR1ain   = '<'
040400071030     c                   eval      V1Cpocgi3 = ACR1pocgi
040500071030     c                   eval      V1Ccgi3   = ACR1cgi
040600071030e   2c                   endsl
040700071030     c     k02acr13      reade     FNACR100
040800071030e   1c                   ENDDO
040900071022      *
041000071022      * Decodifica codice ordinante
041100071022if  1c                   IF        ACRcco   <> *zeros
041200071022     c     ACRcco        chain     FNACR000
041300071022if  2c                   if             %found(FNACR01L)
041400071022     c                             and  ACRatb  = *blanks
041500071022     c                   movel     ACRrsr        V1Ddeo
041600071022x   2c                   else
041700071022     c                   eval      V1Ddeo   =  *all'? '
041800071022e   2c                   endif
041900071022e   1c                   ENDIF
042000071022      *
042100071022      * Decodifica codice PdC
042200071026if  1c                   IF        V1Cksc   <> *zeros
042300071026     c                   reset                   TIBS69ds
042400071026     c*** già così:      eval      I69tla   =  'L'
042500071022     c                   eval      I69kac   =  V1Cksc
042600071022     c                   exsr      Call_TIBS69
042700071022     c                   movel     ACOrag        V1Dksc
042800071022e   1c                   ENDIF
042900071022      *
043000071030      * Decodifica codici giro
043100071030if  1c                   IF        V1Ccgi1  <> *blank
043200071105     c                   eval      wNrCGI   =  1
043300071031     c                   eval      Wpocgi   =  V1Cpocgi1
043400071031     c                   eval      Wcgi     =  V1Ccgi1
043500071022     c                   exsr      Ctr_Giro
043600071031     c                   eval      V1Dcgi1  =  WcgiD
043700071022if  2c                   if        *in90    =  *on
043800071022     c                   eval      *in28    =  *off
043900071022     c                   movea     *zeros        *in(50)
044000071022e   2c                   endif
044100071022e   1c                   ENDIF
044200071030if  1c                   IF        V1Ccgi2  <> *blank
044300071105     c                   eval      wNrCGI   =  2
044400071031     c                   eval      Wpocgi   =  V1Cpocgi2
044500071031     c                   eval      Wcgi     =  V1Ccgi2
044600071030     c                   exsr      Ctr_Giro
044700071031     c                   eval      V1Dcgi2  =  WcgiD
044800071030if  2c                   if        *in90    =  *on
044900071030     c                   eval      *in28    =  *off
045000071030     c                   movea     *zeros        *in(50)
045100071030e   2c                   endif
045200071030e   1c                   ENDIF
045300071030if  1c                   IF        V1Ccgi3  <> *blank
045400071105     c                   eval      wNrCGI   =  3
045500071031     c                   eval      Wpocgi   =  V1Cpocgi3
045600071031     c                   eval      Wcgi     =  V1Ccgi3
045700071030     c                   exsr      Ctr_Giro
045800071031     c                   eval      V1Dcgi3  =  WcgiD
045900071030if  2c                   if        *in90    =  *on
046000071030     c                   eval      *in28    =  *off
046100071030     c                   movea     *zeros        *in(50)
046200071030e   2c                   endif
046300071030e   1c                   ENDIF
046400071022      *
046500071022     c                   ENDSR
046600071022
046700071022      *---------------------------------------------------------------*
046800071022      *?Gestione tasto funzionale F3 da videata D01                  ?*
046900071022      *?F3=Fine                                                      ?*
047000071022      *---------------------------------------------------------------*
047100071022     c     F03D01        BEGSR
047200071022      *
047300071024     c                   eval      O37ret   =  '1'
047400071022      * Chiusura del programma
047500071022     c                   eval      $Fine    =  *on
047600071022      *
047700071022     c                   ENDSR
047800071022
047900071022      *---------------------------------------------------------------*
048000071022      *?Gestione tasto funzionale F12 da videata D01                 ?*
048100071022      *?F12=Ritorno                                                  ?*
048200071022      *---------------------------------------------------------------*
048300071022     c     F12D01        BEGSR
048400071022      *
048500071024     c                   eval      O37ret   =  '2'
048600071022      * Chiusura del programma
048700071022     c                   eval      $Fine    =  *on
048800071022      *
048900071022     c                   ENDSR
049000071022
049100071022      *---------------------------------------------------------------*
049200071022      *?Controllo dati videata D01                                   ?*
049300071022      *---------------------------------------------------------------*
049400071022     c     CtrD01        BEGSR
049500071105      *
049600071105     c                   movea     *zeros        *in(50)
049700071022      *
049800071030      * Giri di ritiro
049900071105do  1c                   DO        C_MaxCGI      wNrCGI
050000071030      *
050100071031     c                   clear                   WcgiD
050200071030sel 2c                   select
050300071030w   2c                   when      wNrCGI  = 1
050400071030     c                   eval      Wpocgi  = V1Cpocgi1
050500071030     c                   eval      Wcgi    = V1Ccgi1
050600071030w   2c                   when      wNrCGI  = 2
050700071030     c                   eval      Wpocgi  = V1Cpocgi2
050800071030     c                   eval      Wcgi    = V1Ccgi2
050900071030w   2c                   when      wNrCGI  = 3
051000071030     c                   eval      Wpocgi  = V1Cpocgi3
051100071030     c                   eval      Wcgi    = V1Ccgi3
051200071030e   2c                   endsl
051300071030      *
051400071030      * - controllo codice giro ritiro
051500071031sel 2c                   Select
051600071031w   2c                   When      Wcgi    = *blanks
051700071031     c                   clear                   Wpocgi
051800071031      *   - interrogazione giri di ritiro
051900071031w   2c                   When      %scan('?' : %trimr(Wcgi)) > *zeros
052000071030     c                   exsr      Search_Giro
052100071030     c                   eval      *in90   = *on
052200071030      *   - controllo codice giro ritiro
052300071031x   2c                   Other
052400071030     c                   exsr      Ctr_Giro
052500071031e   2c                   EndSl
052600071031      *
052700071031sel 2c                   Select
052800071031w   2c                   When      wNrCGI  = 1
052900071030     c                   eval      V1Cpocgi1 = Wpocgi
053000071031     c                   eval      V1Ccgi1   = Wcgi
053100071030     c                   eval      V1Dcgi1   = WcgiD
053200071031w   2c                   When      wNrCGI  = 2
053300071030     c                   eval      V1Cpocgi2 = Wpocgi
053400071031     c                   eval      V1Ccgi2   = Wcgi
053500071030     c                   eval      V1Dcgi2   = WcgiD
053600071031     c                   eval      *in53   = (*in51 = *on)
053700071031     c                   eval      *in54   = (*in52 = *on)
053800071031     c                   eval      *in51   = *off
053900071031     c                   eval      *in52   = *off
054000071031w   2c                   When      wNrCGI  = 3
054100071030     c                   eval      V1Cpocgi3 = Wpocgi
054200071031     c                   eval      V1Ccgi3   = Wcgi
054300071030     c                   eval      V1Dcgi3   = WcgiD
054400071031     c                   eval      *in55   = (*in51 = *on)
054500071031     c                   eval      *in56   = (*in52 = *on)
054600071031     c                   eval      *in51   = *off
054700071031     c                   eval      *in52   = *off
054800071031e   2c                   EndSl
054900071031if  2c                   if        *in90
055000071030     c                   leavesr
055100071031e   2c                   endif
055200071030      *
055300071030e   1c                   ENDDO
055400071022      *
055500071022     c                   ENDSR
055600071022
055700071022      *---------------------------------------------------------------*
055800071022      *?Reperimento dati anagrafici                                  ?*
055900071022      *---------------------------------------------------------------*
056000071022     c     Call_TIBS69   BEGSR
056100071022      *
056200071022     c                   clear                   ds_CNACO
056300071022     c                   clear                   ds_CNIND
056400071022     c                   clear                   ds_CNCLP
056500071022     c                   clear                   ds_FNCLS
056600071022      *
056700071022     c                   call      'TIBS69R'
056800071022     c                   parm                    TIBS69ds
056900071022     c                   parm                    ds_CNACO
057000071022     c                   parm                    ds_CNIND
057100071022     c                   parm                    ds_CNCLP
057200071022     c                   parm                    ds_FNCLS
057300071022      *
057400071022if  1c                   if        O69err   =  *on
057500071022     c                   seton                                        28  90
057600071022     c                   eval      V1Dmsg   =  O69msg
057700071022e   1c                   endif
057800071022      *
057900071022     c                   ENDSR
058000071030
058100071030      *---------------------------------------------------------------*
058200071030      *?Gestione tasto funzionale F6 da videata D01                  ?*
058300071030      *?F6=Conferma                                                  ?*
058400071030      *---------------------------------------------------------------*
058500071030     c     F06D01        BEGSR
058600071030      *
058700071030      * Aggiornamento ambito "standard" di intradamento codice giro
058800071030     c                   eval      ACR1ain = '='
058900071030     c                   eval      Wpocgi  = V1Cpocgi1
059000071030     c                   eval      Wcgi    = V1Ccgi1
059100071030     c                   exsr      Upd_FNACR1
059200071030      *
059300071030      * Aggiornamento ambito "oltre" di intradamento codice giro
059400071030     c                   eval      ACR1ain = '>'
059500071030     c                   eval      Wpocgi  = V1Cpocgi2
059600071030     c                   eval      Wcgi    = V1Ccgi2
059700071030     c                   exsr      Upd_FNACR1
059800071030      *
059900071030      * Aggiornamento ambito "sotto" di intradamento codice giro
060000071030     c                   eval      ACR1ain = '<'
060100071030     c                   eval      Wpocgi  = V1Cpocgi3
060200071030     c                   eval      Wcgi    = V1Ccgi3
060300071030     c                   exsr      Upd_FNACR1
060400071030      *
060500071030     c                   ENDSR
060600071022
060700071022      *---------------------------------------------------------------*
060800071022      *?Aggiornamento dati file FNACR10F                             ?*
060900071022      *---------------------------------------------------------------*
061000071022     c     Upd_FNACR1    BEGSR
061100071030      *
061200071031     c     k03acr13      chain     FNACR100
061300071105     c                   clear                   SAVpocgi
061400071105     c                   clear                   SAVcgi
061500071105if  2c                   if        %found(FNACR13L)
061600071105     c                   eval      SAVpocgi     = ACR1pocgi
061700071105     c                   eval      SAVcgi       = ACR1cgi
061800071105e   2c                   endif
061900071022      *
062000071022      * Impostazione campi nel record
062100071030if  1c                   if        Wcgi     <> *blanks
062200071030     c                   exsr      RieRec1
062300071030e   1c                   endif
062400071022      *
062500071022      * Inserimento / Modifica
062600071030     c                   eval      *in92     = *off
062700071022sel 1c                   select
062800071030w   1c                   when      NOT  %found(FNACR13L)
062900071030     c                             and  Wcgi   <> *blanks
063000071022     c                   WRITE     FNACR100                             92
063100071022      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
063200071030w   1c                   when           %found(FNACR13L)
063300071030     c                             and  Wcgi   <> *blanks
063400071105     c                             and  Wcgi   <> SAVcgi
063500071022     c                   UPDATE    FNACR100
063600071022      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
063700071030w   1c                   when           %found(FNACR13L)
063800071030     c                             and  Wcgi   =  *blanks
063900071026     c                   DELETE    FNACR100
064000071026      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
064100071030w   1c                   when           %found(FNACR13L)
064200071105     c                             and  Wcgi   =  SAVcgi
064300071030     c                   UNLOCK    FNACR13L
064400071030      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
064500071022e   1c                   endsl
064600071022      *
064700071022      * Errore in fase di aggiornamento record
064800071022if  1c                   if        *in92
064900071022     c                   seton                                        28  90
065000071022     c                   eval      V1Dmsg   = %trimr($Msg(9))
065100071022     c                                      + ' FNACR10F'
065200071022      * uscita dal programma
065300071022x   1c                   else
065400071031     c***                eval      O37pocgi = Wpocgi
065500071031     c***                eval      O37cgi   = Wcgi
065600071024     c                   eval      O37ret   = *off
065700071022     c                   eval      $Fine    = *on
065800071022e   1c                   endif
065900071022      *
066000071022     c                   ENDSR
066100071030
066200071030      *---------------------------------------------------------------*
066300071030      *?Impostazione valori nei campi del record FNACR10F            ?*
066400071030      *---------------------------------------------------------------*
066500071030     c     RieRec1       BEGSR
066600071030      *
066700071030     c*** NON fare:      clear                   FNACR100
066800071030      *
066900071030     c***                clear                   ACR1atb
067000071030     c                   eval      ACR1cro   = H1Ccro
067100071030     c                   eval      ACR1pocgi = Wpocgi
067200071030     c                   eval      ACR1cgi   = Wcgi
067300071030     c*** già impostato: eval      ACR1ain   = Wain
067400071030if  1c                   if        NOT  %found(FNACR13L)
067500071030     c                             or  (SAVpocgi <> Wpocgi
067600071030     c                             or   SAVcgi   <> Wcgi)
067700071030     c                   eval      ACR1dtv   = *Date
067800071030     c                   eval      ACR1ute   = KNMUS
067900071030e   1c                   endif
068000071030      *
068100071030     c                   ENDSR
068200071022
068300071022** - $MSG -------------------------------------------------------------------*
068400071022Filiale errata                                                                  1
068500071022La Filiale non è in gestione all'utente                                         2
068600071022Parzializzazione per località errata                                            3
068700071022Codice giro di ritiro inesistente o non valido                                  4
068800071022Opzione errata                                                                  5
068900071022Immettere anche il giro di consegna  o  togliere la filiale del giro            6
069000071022Immettere anche la filiale del giro  o  togliere il giro di consegna            7
069100071022Filiale errata per codice giro di ritiro                                        8
069200071022Errore in fase di scrittura record nel file                                     9
069300071112Codice giro di ritiro errato o NON in vigore                                   10
069400071022Procedura ORM non attivata                                                     11
