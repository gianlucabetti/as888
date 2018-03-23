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
025500081211sel 1c                   select
025600081211w   1c                   when           D09Of03 <> *on
025700071022     c                             and  D09Of12 <> *on
025800071022     c                             and  D09Oerr <> *on
025900071030     c                   eval      Wpocgi   =  D09ofgs
026000071030     c                   eval      Wcgi     =  D09ocgi
026100071030     c                   movel     D09odes       WcgiD
026200081211w   1c                   when      D09Of03   = *on
026300081211     c                   eval      *in52     = *on
026400081211     c                   eval      *in90     = *on
026500081211w   1c                   when      D09Of12   = *on
026600081211e   1c                   endsl
026700071022      *
026800071022     c                   ENDSR
026900071022
027000071022      *---------------------------------------------------------------*
027100071022      *?Gestione videata D01                                         ?*
027200071022      *---------------------------------------------------------------*
027300071022     c     GesD01        BEGSR
027400071022      *
027500071022      * Inizializzazione videata
027600071022if  1c                   if        $InzD01  =  *on
027700071022     c                   exsr      InzD01
027800071022     c                   eval      $InzD01  =  *off
027900071022e   1c                   endif
028000071023      *
028100071023      * Emissione testata
028200081211     c                   if        NOT *in28
028300071024     c                   write     OR37T01
028400081211     c                   endif
028500071022      *
028600071022      * Emissione videata
028700071024     c                   exfmt     OR37D01
028800071022     c                   setoff                                       28  90
028900071022     c                   clear                   V1Dmsg
029000071022      *
029100071022sel 1c                   SELECT
029200071022      * F3=Fine
029300071022w   1c                   WHEN      *inKC
029400071022     c                   exsr      F03D01
029500071022      * F12=Ritorno
029600071022w   1c                   WHEN      *inKL
029700071022     c                   exsr      F12D01
029800071022      * Enter / F6=Conferma
029900071022x   1c                   OTHER
030000071022     c                   exsr      CtrD01
030100071022if  2c                   if        NOT  *in90
030200071022     c                             and  *inKF
030300071030     c                   exsr      F06D01
030400071022if  3c                   if        NOT  *in90
030500071022     c                   eval      $Fine    =  *on
030600071022e   3c                   endif
030700071022e   2c                   endif
030800071022e   1c                   ENDSL
030900071022      *
031000071022     c                   ENDSR
031100071022
031200071022      *---------------------------------------------------------------*
031300071022      *?Inizializzazione videata D01                                 ?*
031400071022      *---------------------------------------------------------------*
031500071022     c     InzD01        BEGSR
031600071022      *
031700071024     c                   clear                   OR37D01
031800071026      *
031900071026      * Impostazione indicatori utilizzati nella videata
032000071026     c                   eval      *in41    =  (I37opz = '9')
032100071022      *
032200071022      * Impostazione testata
032300071025if  1c                   if        I37opz   =  '9'
032400071025     c                   eval      V1Ttxt   =  C_Txt_2
032500071025x   1c                   else
032600071025     c                   eval      V1Ttxt   =  C_Txt_1
032700071025e   1c                   endif
032800071022      *
032900071022      * Impostazione campi a video
033000071022      *
033100071024     c     I37cro        chain     FNACR000
033200071022      *
033300071022if  1c                   IF        %found(FNACR01L)
033400071022      *
033500071022     c                   eval      H1Ccro   =  ACRcro
033600071022     c                   eval      H1Ccco   =  ACRcco
033700071022      *
033800071022     c                   movel     ACRcro        ds_CRO
033900071022     c                   eval      V1Ccd1   =  dsCRO1
034000071022     c                   eval      V1Ccd2   =  dsCRO2
034100071022     c                   eval      V1Ccd3   =  dsCRO3
034200071022     c                   eval      V1Crsr   =  ACRrsr
034300071022     c                   eval      V1Cinr   =  ACRinr
034400071022     c                   eval      V1Ccar   =  ACRcar
034500071022     c                   eval      V1Ccar   =  ACRcar
034600071022     c                   eval      V1Clor   =  ACRlor
034700071022     c                   eval      V1Cprr   =  ACRprr
034800071022     c                   eval      V1Cnar   =  ACRnar
034900071022if  2c                   if        ACRcco   <> *zeros
035000071022     c                   movel     ACRcco        ds_CRO
035100071022     c                   move      dsCRO1        V1Cco1
035200071022     c                   eval      V1Cco2   =  dsCRO2
035300071022     c                   move      dsCRO3        V1Cco3
035400071022e   2c                   endif
035500071022     c                   eval      V1Cksc   =  ACRksc
035600071022if  2c                   if        ACRccc   <> 999
035700071022     c                   z-add     ACRccc        V1Cctr
035800071022e   2c                   endif
035900071022sel 2c                   select
036000071022w   2c                   when      ACRtcr   =  'F'
036100071022     c                   eval      V1Dtcr   =  'Fisso               '
036200071022w   2c                   when      ACRtcr   =  'R'
036300071022     c                   eval      V1Dtcr   =  'Ricorrente          '
036400071022w   2c                   when      ACRtcr   =  'O'
036500081015     c                   eval      V1Dtcr   =  'No Internet         '
036600071022w   2c                   when      ACRtcr   =  'M'
036700071022     c                   eval      V1Dtcr   =  'Mai                 '
036800071022x   2c                   other
036900071022     c                   eval      V1Dtcr   =  '> ' + ACRtcr + ' <'
037000071022e   2c                   endsl
037100071022if  2c                   if        ACRfcc  <>  'N'
037200071022     c                   eval      V1Cfcc   =  'SI'
037300071022x   2c                   else
037400071022     c                   eval      V1Cfcc   =  'NO'
037500071022e   2c                   endif
037600071022     c                   eval      V1Corr   =  ACRorr
037700071022     c                   eval      V1Cper   =  ACRper
037800071022     c                   eval      V1Ctel   =  ACRtel
037900071022     c*** NO mail:       eval      V1Cmai   =  ACRmai
038000071022     c                   eval      V1Cfcl   =  ACRfcl
038100071022     c                   eval      V1Cfbn   =  ACRfbn
038200071022     c                   eval      V1Cfpk   =  ACRfpk
038300071022     c                   eval      V1Cfmc   =  ACRfmc
038400071022     c                   eval      V1Cfbl   =  ACRfbl
038500071022     c                   eval      V1Cfmt   =  ACRfmt
038600071022     c                   eval      V1Cfat   =  ACRfat
038700071022if  2c                   if        ACRspi   =  'S'
038800071022     c                   eval      V1Cspi   =  'SI'
038900071022x   2c                   else
039000071022     c                   eval      V1Cspi   =  'NO'
039100071022e   2c                   endif
039200071022     c                   eval      V1Cntm   =  ACRntm
039300071022     c                   eval      V1Cno1   =  ACRno1
039400071022     c                   eval      V1Cno2   =  ACRno2
039500071022      *
039600071022e   1c                   ENDIF
039700071022      *
039800071030     c     k02acr13      setll     FNACR100
039900071030     c     k02acr13      reade     FNACR100
040000071030do  1c                   DOW       NOT  %eof(FNACR13L)
040100071022     c                             and  ACR1atb = *blanks
040200071030sel 2c                   select
040300071030w   2c                   when      ACR1atb  <> ACRatb
040400071030w   2c                   when      ACR1ain   = '='
040500071030     c                   eval      V1Cpocgi1 = ACR1pocgi
040600071030     c                   eval      V1Ccgi1   = ACR1cgi
040700071030w   2c                   when      ACR1ain   = '>'
040800071030     c                   eval      V1Cpocgi2 = ACR1pocgi
040900071030     c                   eval      V1Ccgi2   = ACR1cgi
041000071030w   2c                   when      ACR1ain   = '<'
041100071030     c                   eval      V1Cpocgi3 = ACR1pocgi
041200071030     c                   eval      V1Ccgi3   = ACR1cgi
041300071030e   2c                   endsl
041400071030     c     k02acr13      reade     FNACR100
041500071030e   1c                   ENDDO
041600071022      *
041700071022      * Decodifica codice ordinante
041800071022if  1c                   IF        ACRcco   <> *zeros
041900071022     c     ACRcco        chain     FNACR000
042000071022if  2c                   if             %found(FNACR01L)
042100071022     c                             and  ACRatb  = *blanks
042200071022     c                   movel     ACRrsr        V1Ddeo
042300071022x   2c                   else
042400071022     c                   eval      V1Ddeo   =  *all'? '
042500071022e   2c                   endif
042600071022e   1c                   ENDIF
042700071022      *
042800071022      * Decodifica codice PdC
042900071026if  1c                   IF        V1Cksc   <> *zeros
043000071026     c                   reset                   TIBS69ds
043100071026     c*** già così:      eval      I69tla   =  'L'
043200071022     c                   eval      I69kac   =  V1Cksc
043300071022     c                   exsr      Call_TIBS69
043400071022     c                   movel     ACOrag        V1Dksc
043500071022e   1c                   ENDIF
043600071022      *
043700071030      * Decodifica codici giro
043800071030if  1c                   IF        V1Ccgi1  <> *blank
043900071105     c                   eval      wNrCGI   =  1
044000071031     c                   eval      Wpocgi   =  V1Cpocgi1
044100071031     c                   eval      Wcgi     =  V1Ccgi1
044200071022     c                   exsr      Ctr_Giro
044300071031     c                   eval      V1Dcgi1  =  WcgiD
044400071022if  2c                   if        *in90    =  *on
044500071022     c                   eval      *in28    =  *off
044600071022     c                   movea     *zeros        *in(50)
044700071022e   2c                   endif
044800071022e   1c                   ENDIF
044900071030if  1c                   IF        V1Ccgi2  <> *blank
045000071105     c                   eval      wNrCGI   =  2
045100071031     c                   eval      Wpocgi   =  V1Cpocgi2
045200071031     c                   eval      Wcgi     =  V1Ccgi2
045300071030     c                   exsr      Ctr_Giro
045400071031     c                   eval      V1Dcgi2  =  WcgiD
045500071030if  2c                   if        *in90    =  *on
045600071030     c                   eval      *in28    =  *off
045700071030     c                   movea     *zeros        *in(50)
045800071030e   2c                   endif
045900071030e   1c                   ENDIF
046000071030if  1c                   IF        V1Ccgi3  <> *blank
046100071105     c                   eval      wNrCGI   =  3
046200071031     c                   eval      Wpocgi   =  V1Cpocgi3
046300071031     c                   eval      Wcgi     =  V1Ccgi3
046400071030     c                   exsr      Ctr_Giro
046500071031     c                   eval      V1Dcgi3  =  WcgiD
046600071030if  2c                   if        *in90    =  *on
046700071030     c                   eval      *in28    =  *off
046800071030     c                   movea     *zeros        *in(50)
046900071030e   2c                   endif
047000071030e   1c                   ENDIF
047100071022      *
047200071022     c                   ENDSR
047300071022
047400071022      *---------------------------------------------------------------*
047500071022      *?Gestione tasto funzionale F3 da videata D01                  ?*
047600071022      *?F3=Fine                                                      ?*
047700071022      *---------------------------------------------------------------*
047800071022     c     F03D01        BEGSR
047900071022      *
048000071024     c                   eval      O37ret   =  '1'
048100071022      * Chiusura del programma
048200071022     c                   eval      $Fine    =  *on
048300071022      *
048400071022     c                   ENDSR
048500071022
048600071022      *---------------------------------------------------------------*
048700071022      *?Gestione tasto funzionale F12 da videata D01                 ?*
048800071022      *?F12=Ritorno                                                  ?*
048900071022      *---------------------------------------------------------------*
049000071022     c     F12D01        BEGSR
049100071022      *
049200071024     c                   eval      O37ret   =  '2'
049300071022      * Chiusura del programma
049400071022     c                   eval      $Fine    =  *on
049500071022      *
049600071022     c                   ENDSR
049700071022
049800071022      *---------------------------------------------------------------*
049900071022      *?Controllo dati videata D01                                   ?*
050000071022      *---------------------------------------------------------------*
050100071022     c     CtrD01        BEGSR
050200071105      *
050300071105     c                   movea     *zeros        *in(50)
050400071022      *
050500071030      * Giri di ritiro
050600071105do  1c                   DO        C_MaxCGI      wNrCGI
050700071030      *
050800071031     c                   clear                   WcgiD
050900071030sel 2c                   select
051000071030w   2c                   when      wNrCGI  = 1
051100071030     c                   eval      Wpocgi  = V1Cpocgi1
051200071030     c                   eval      Wcgi    = V1Ccgi1
051300071030w   2c                   when      wNrCGI  = 2
051400071030     c                   eval      Wpocgi  = V1Cpocgi2
051500071030     c                   eval      Wcgi    = V1Ccgi2
051600071030w   2c                   when      wNrCGI  = 3
051700071030     c                   eval      Wpocgi  = V1Cpocgi3
051800071030     c                   eval      Wcgi    = V1Ccgi3
051900071030e   2c                   endsl
052000071030      *
052100071030      * - controllo codice giro ritiro
052200071031sel 2c                   Select
052300071031w   2c                   When      Wcgi    = *blanks
052400071031     c                   clear                   Wpocgi
052500071031      *   - interrogazione giri di ritiro
052600071031w   2c                   When      %scan('?' : %trimr(Wcgi)) > *zeros
052700071030     c                   exsr      Search_Giro
052800081211     c*** già impostato: eval      *in90   = *on
052900071030      *   - controllo codice giro ritiro
053000071031x   2c                   Other
053100071030     c                   exsr      Ctr_Giro
053200071031e   2c                   EndSl
053300071031      *
053400071031sel 2c                   Select
053500071031w   2c                   When      wNrCGI  = 1
053600071030     c                   eval      V1Cpocgi1 = Wpocgi
053700071031     c                   eval      V1Ccgi1   = Wcgi
053800071030     c                   eval      V1Dcgi1   = WcgiD
053900071031w   2c                   When      wNrCGI  = 2
054000071030     c                   eval      V1Cpocgi2 = Wpocgi
054100071031     c                   eval      V1Ccgi2   = Wcgi
054200071030     c                   eval      V1Dcgi2   = WcgiD
054300071031     c                   eval      *in53   = (*in51 = *on)
054400071031     c                   eval      *in54   = (*in52 = *on)
054500071031     c                   eval      *in51   = *off
054600071031     c                   eval      *in52   = *off
054700071031w   2c                   When      wNrCGI  = 3
054800071030     c                   eval      V1Cpocgi3 = Wpocgi
054900071031     c                   eval      V1Ccgi3   = Wcgi
055000071030     c                   eval      V1Dcgi3   = WcgiD
055100071031     c                   eval      *in55   = (*in51 = *on)
055200071031     c                   eval      *in56   = (*in52 = *on)
055300071031     c                   eval      *in51   = *off
055400071031     c                   eval      *in52   = *off
055500071031e   2c                   EndSl
055600071031if  2c                   if        *in90
055700071030     c                   leavesr
055800071031e   2c                   endif
055900071030      *
056000071030e   1c                   ENDDO
056100071022      *
056200071022     c                   ENDSR
056300071022
056400071022      *---------------------------------------------------------------*
056500071022      *?Reperimento dati anagrafici                                  ?*
056600071022      *---------------------------------------------------------------*
056700071022     c     Call_TIBS69   BEGSR
056800071022      *
056900071022     c                   clear                   ds_CNACO
057000071022     c                   clear                   ds_CNIND
057100071022     c                   clear                   ds_CNCLP
057200071022     c                   clear                   ds_FNCLS
057300071022      *
057400071022     c                   call      'TIBS69R'
057500071022     c                   parm                    TIBS69ds
057600071022     c                   parm                    ds_CNACO
057700071022     c                   parm                    ds_CNIND
057800071022     c                   parm                    ds_CNCLP
057900071022     c                   parm                    ds_FNCLS
058000071022      *
058100071022if  1c                   if        O69err   =  *on
058200071022     c                   seton                                        28  90
058300071022     c                   eval      V1Dmsg   =  O69msg
058400071022e   1c                   endif
058500071022      *
058600071022     c                   ENDSR
058700071030
058800071030      *---------------------------------------------------------------*
058900071030      *?Gestione tasto funzionale F6 da videata D01                  ?*
059000071030      *?F6=Conferma                                                  ?*
059100071030      *---------------------------------------------------------------*
059200071030     c     F06D01        BEGSR
059300071030      *
059400071030      * Aggiornamento ambito "standard" di intradamento codice giro
059500071030     c                   eval      ACR1ain = '='
059600071030     c                   eval      Wpocgi  = V1Cpocgi1
059700071030     c                   eval      Wcgi    = V1Ccgi1
059800071030     c                   exsr      Upd_FNACR1
059900071030      *
060000071030      * Aggiornamento ambito "oltre" di intradamento codice giro
060100071030     c                   eval      ACR1ain = '>'
060200071030     c                   eval      Wpocgi  = V1Cpocgi2
060300071030     c                   eval      Wcgi    = V1Ccgi2
060400071030     c                   exsr      Upd_FNACR1
060500071030      *
060600071030      * Aggiornamento ambito "sotto" di intradamento codice giro
060700071030     c                   eval      ACR1ain = '<'
060800071030     c                   eval      Wpocgi  = V1Cpocgi3
060900071030     c                   eval      Wcgi    = V1Ccgi3
061000071030     c                   exsr      Upd_FNACR1
061100071030      *
061200071030     c                   ENDSR
061300071022
061400071022      *---------------------------------------------------------------*
061500071022      *?Aggiornamento dati file FNACR10F                             ?*
061600071022      *---------------------------------------------------------------*
061700071022     c     Upd_FNACR1    BEGSR
061800071030      *
061900071031     c     k03acr13      chain     FNACR100
062000071105     c                   clear                   SAVpocgi
062100071105     c                   clear                   SAVcgi
062200071105if  2c                   if        %found(FNACR13L)
062300071105     c                   eval      SAVpocgi     = ACR1pocgi
062400071105     c                   eval      SAVcgi       = ACR1cgi
062500071105e   2c                   endif
062600071022      *
062700071022      * Impostazione campi nel record
062800071030if  1c                   if        Wcgi     <> *blanks
062900071030     c                   exsr      RieRec1
063000071030e   1c                   endif
063100071022      *
063200071022      * Inserimento / Modifica
063300071030     c                   eval      *in92     = *off
063400071022sel 1c                   select
063500071030w   1c                   when      NOT  %found(FNACR13L)
063600071030     c                             and  Wcgi   <> *blanks
063700071022     c                   WRITE     FNACR100                             92
063800071022      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
063900071030w   1c                   when           %found(FNACR13L)
064000071030     c                             and  Wcgi   <> *blanks
064100071105     c                             and  Wcgi   <> SAVcgi
064200071022     c                   UPDATE    FNACR100
064300071022      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
064400071030w   1c                   when           %found(FNACR13L)
064500071030     c                             and  Wcgi   =  *blanks
064600071026     c                   DELETE    FNACR100
064700071026      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
064800071030w   1c                   when           %found(FNACR13L)
064900071105     c                             and  Wcgi   =  SAVcgi
065000071030     c                   UNLOCK    FNACR13L
065100071030      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
065200071022e   1c                   endsl
065300071022      *
065400071022      * Errore in fase di aggiornamento record
065500071022if  1c                   if        *in92
065600071022     c                   seton                                        28  90
065700071022     c                   eval      V1Dmsg   = %trimr($Msg(9))
065800071022     c                                      + ' FNACR10F'
065900071022      * uscita dal programma
066000071022x   1c                   else
066100071031     c***                eval      O37pocgi = Wpocgi
066200071031     c***                eval      O37cgi   = Wcgi
066300071024     c                   eval      O37ret   = *off
066400071022     c                   eval      $Fine    = *on
066500071022e   1c                   endif
066600071022      *
066700071022     c                   ENDSR
066800071030
066900071030      *---------------------------------------------------------------*
067000071030      *?Impostazione valori nei campi del record FNACR10F            ?*
067100071030      *---------------------------------------------------------------*
067200071030     c     RieRec1       BEGSR
067300071030      *
067400071030     c*** NON fare:      clear                   FNACR100
067500071030      *
067600071030     c***                clear                   ACR1atb
067700071030     c                   eval      ACR1cro   = H1Ccro
067800071030     c                   eval      ACR1pocgi = Wpocgi
067900071030     c                   eval      ACR1cgi   = Wcgi
068000071030     c*** già impostato: eval      ACR1ain   = Wain
068100071030if  1c                   if        NOT  %found(FNACR13L)
068200071030     c                             or  (SAVpocgi <> Wpocgi
068300071030     c                             or   SAVcgi   <> Wcgi)
068400071030     c                   eval      ACR1dtv   = *Date
068500071030     c                   eval      ACR1ute   = KNMUS
068600071030e   1c                   endif
068700071030      *
068800071030     c                   ENDSR
068900071022
069000071022** - $MSG -------------------------------------------------------------------*
069100071022Filiale errata                                                                  1
069200071022La Filiale non è in gestione all'utente                                         2
069300071022Parzializzazione per località errata                                            3
069400071022Codice giro di ritiro inesistente o non valido                                  4
069500071022Opzione errata                                                                  5
069600071022Immettere anche il giro di consegna  o  togliere la filiale del giro            6
069700071022Immettere anche la filiale del giro  o  togliere il giro di consegna            7
069800071022Filiale errata per codice giro di ritiro                                        8
069900071022Errore in fase di scrittura record nel file                                     9
070000071112Codice giro di ritiro errato o NON in vigore                                   10
070100071022Procedura ORM non attivata                                                     11
