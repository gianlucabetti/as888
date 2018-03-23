000100071024      * FIOR37R2 *----------------------------------------------------*
000200071022      *?ATTRIBUZIONE GIRO AD ANAGRAFICA CLIENTI RITIRO               ?*
000300071022      *---------------------------------------------------------------*
000400071022
000500071022     h decedit('0,') datedit(*YMD.) option(*nodebugio)
000600071022
000700071022      *---------------------------------------------------------------*
000800150909
000900150909     fAZORG01L  if   e           k disk
001000071022
001100071022     fFNACR01L  if   e           k disk
001200071030     fFNACR13L  Uf A e           k disk
001300071022      *
001400071024     fFIOR37D2  cf   e             workstn
001500071022
001600071022      *
001700071022      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - -?*
001800071022      *
001900071022      * - Titoli in testata
002000071025     d C_Txt_1         c                   const('*   al SINGOLO Clien-
002100071022     d                                     te Ritiro   *')
002200071025     d C_Txt_2         c                   const('*     a PIU'' Client-
002300071022     d                                     i Ritiro     *')
002400071105      * - Num. Max di codici giro ritiro inseribili per cliente ritiro
002500071105     d C_MaxCGI        c                   const(03)
002600071022      *
002700071022      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - -?*
002800071022      *
002900071022      * - Messaggi di errore
003000071022     D $Msg            S             78    dim(11)  ctdata  perrcd(1)
003100071108      * - Filiali in £1
003200071108     d $L1             s              3  0 dim(30)  inz                         P.O. Gestiti £1
003300071022      *
003400071022      *?  S T R U T T U R E   D A T I   - - - - - - - - - - - - - - -?*
003500071022      *
003600071022      * - Parametri
003700071022     d KPJBA         e ds
003800071024     d FIOR37ds      e ds
003900071022      *
004000071022      * - Reperimento dati utente
004100071022     d TIBS34ds      e ds
004200071022     d AZUTEds       e ds                  extname(AZUTE00F)
004300071022     d dDatiUte      e ds
004400071108      *
004500071108      * - Caricamento schiera filiali
004600071108     d TRUL06ds      e ds                  inz
004700071108     d  D06lin                 1     90  0 inz  dim(30)
004800071022      *
004900071022      * - Lettura anagrafiche cliente
005000071022     d TIBS69ds      e ds                  inz
005100071026     d  I69tla       e                     inz('L')
005200071022     d ds_CNACO      e ds                  inz  extname(CNACO00F)
005300071022     d ds_CNIND      e ds                  inz  extname(CNIND00F)
005400071022     d ds_CNCLP      e ds                  inz  extname(CNCLP00F)
005500071022     d ds_FNCLS      e ds                  inz  extname(FNCLS00F)
005600071022      *
005700071022      * - Gestione anagrafica giri
005800071022     d FIDG09ds      e ds                  inz
005900071022      *
006000071022     d Status         sds           333
006100071025     d  SDSpgm           *proc
006200071022      *
006300071022      * - Codice cliente ritiro
006400071022     d ds_CRO          ds            10    inz
006500071022     d  dsCRO1                        3  0 inz
006600071022     d  dsCRO2                        4  0 inz
006700071022     d  dsCRO3                        3  0 inz
006800071022      *
006900071022      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
007000071022      *
007100071022      * - Flags
007200071022     d $InzD01         s              1    inz(*on)
007300071022     d $Fine           s              1    inz(*off)
007400071031      * - Indici di schiera / Contatori
007500071031     d wNrCGI          s              3  0 inz
007600071022      * - Comodo
007700071030     d SAVpocgi        s                   inz  like(V1Cpocgi1)
007800071030     d SAVcgi          s                   inz  like(V1Ccgi1)
007900071030     d Wpocgi          s                   inz  like(V1Cpocgi1)
008000071030     d Wcgi            s                   inz  like(V1Ccgi1)
008100071030     d WcgiD           s                   inz  like(V1Dcgi1)
008200071022      *
008300071022      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
008400071022      *
008500071030      * - FNACR13L
008600071030     c     k02acr13      klist
008700071024     c                   kfld                    I37cro
008800071024     c                   kfld                    I37pocgi
008900071030     c     k03acr13      klist
009000071030     c                   kfld                    I37cro
009100071030     c                   kfld                    I37pocgi
009200071030     c                   kfld                    ACR1ain
009300071022
009400071022      *---------------------------------------------------------------*
009500071022      *?RIEPILOGO INDICATORI                                         ?*
009600071022      *---------------------------------------------------------------*
009700071022      *  25    - Comodo                                               *
009800071022      *  28    - Emissione del messaggio di errore a video            *
009900071022      *  41    - Attribuzione giro su più Clienti Ritiro              *
010000071031      *  51    - Filiale Giro Ritiro (=) errata                       *
010100071031      *  52    - Codice  Giro Ritiro (=) errato                       *
010200071031      *  53    - Filiale Giro Ritiro (>) errata                       *
010300071031      *  54    - Codice  Giro Ritiro (>) errato                       *
010400071031      *  55    - Filiale Giro Ritiro (<) errata                       *
010500071031      *  56    - Codice  Giro Ritiro (<) errato                       *
010600150910      *  57    - Visualizza filiale ritiro forzata                    *
010700071022      *  90    - Generico di errore                                   *
010800071022      *---------------------------------------------------------------*
010900071022
011000071022     c     *Entry        plist
011100071022     c                   parm                    KPJBA
011200071022      *
011300071022      * Operazioni iniziali
011400071022     c                   exsr      RoutInz
011500071022      *
011600071022      * Gestione video
011700071022do  1c                   dow       $Fine   = *off
011800071022     c                   exsr      GesD01
011900071022e   1c                   enddo
012000071022      *
012100071022      * Operazioni finali
012200071024     c                   movel(p)  FIOR37ds      KPJBU
012300071022     c                   eval      *inLR    = *on
012400071022
012500071022      *---------------------------------------------------------------*
012600071022      *?Operazioni Iniziali                                          ?*
012700071022      *---------------------------------------------------------------*
012800071022     c     RoutInz       BEGSR
012900071022      *
013000071022      * Reperimento dati job
013100071022     c                   exsr      DatiJob
013200071022      *
013300071022      * Impostazione nome programma a video
013400071022     c                   movel     SDSpgm        V1Tpgm
013500071022      *
013600071022      * Impostazione parametri ricevuti
013700071022     c                   if        KPJBU    <> *blanks
013800071024     c                   movel     KPJBU         FIOR37ds
013900071022     c                   else
014000071022     c                   eval      $Fine    =  *on
014100071108     c                   leavesr
014200071022     c                   endif
014300071108      *
014400071108      * Reperimento filiali gestite (£1)
014500071108     c                   clear                   TRUL06ds
014600071108     c                   eval      D06cod   = '£1'
014700071108     c                   movel(p)  I37fgs        D06key
014800071108     c                   movel(p)  TRUL06ds      KPJBU
014900071108     c                   call      'TRUL06R'
015000071108     c                   parm                    KPJBA
015100071108     c                   movel     KPJBU         TRUL06ds
015200071108     c                   movea     D06lin        $L1
015300071022      *
015400071022     c                   ENDSR
015500071022
015600071022      *---------------------------------------------------------------*
015700071022      *?Reperimento Dati del job (Utente/Operativi)                  ?*
015800071022      *---------------------------------------------------------------*
015900071022     c     DatiJob       BEGSR
016000071022      *
016100071022     c     *dtaara       define    §azute        azuteds
016200071022     c     *dtaara       define    §datiute      ddatiute
016300071022      *
016400071022     c                   in(E)     *dtaara
016500071022     c                   IF        %ERROR or RSUT = *blanks
016600071022     c                   clear                   Tibs34Ds
016700071022     c                   call      'TIBS34R'
016800071022     c                   parm                    Tibs34Ds
016900071022     c                   in        *dtaara
017000071022     c                   ENDIF
017100071022      *
017200071022     c                   ENDSR
017300071022
017400071022      *---------------------------------------------------------------*
017500071022      *?Controllo e decodifica giro ritiro                           ?*
017600071022      *---------------------------------------------------------------*
017700071022     c     Ctr_Giro      BEGSR
017800071030      *
017900071030      * - immessa solo la filiale (senza codice giro)
018000071030if  1c                   if             Wpocgi  <> *zeros
018100071030     c                             and  Wcgi     = *blanks
018200071031     c                   clear                   Wpocgi
018300071031     c                   clear                   WcgiD
018400071030e   1c                   endif
018500071030      *
018600071030      * - immesso solo il codice giro (senza filiale)
018700071030if  1c                   if             Wpocgi   = *zeros
018800071030     c                             and  Wcgi    <> *blanks
018900071031     c                   eval      Wpocgi   =  I37pocgi
019000071030e   1c                   endif
019100071030      *
019200071030      * - controllo codici filiale giri ritiro
019300071030      *   (filiale giro diversa dalla filiale utente)
019400071030if  1c                   if             Wpocgi  <> DUTpou
019500071030     c                             and  Wcgi    <> *blank
019600071108if  2c                   if        %lookup(Wpocgi:$L1) = *zeros
019700071031     c                   seton                                        285290
019800071030     c                   eval      V1Dmsg   =  $Msg(8)
019900071030     c                   leavesr
020000071108e   2c                   endif
020100071030e   1c                   endif
020200071022      *
020300071023      * - controllo  CON  data validità ("bypassabile")
020400071031if  1c                   IF             Wpocgi  <> D09ifgs
020500071031     c                             or   Wcgi    <> D09icgi
020600071031     c                   clear                   FIDG09ds
020700071022     c                   eval      D09iop0  =  '001'
020800071031     c                   eval      D09ifgs  =  Wpocgi
020900071031     c                   eval      D09icgi  =  Wcgi
021000071022     c                   eval      D09idat  =  *date
021100071022     c                   eval      D09itug  =  'R'
021200071022     c                   movel(p)  FIDG09ds      KPJBU
021300071022     c                   call      'FIDG09R'
021400071022     c                   parm                    KPJBA
021500071022     c                   movel     KPJBU         FIDG09ds
021600071105e   1c                   ENDIF
021700071105      *
021800071105sel 1c                   select
021900071105w   1c                   when      D09oerr  = *on
022000071105     c                   movel     D09odes       WcgiD
022100071031     c                   seton                                        285290
022200071031     c                   eval      V1Dmsg   =  $Msg(10)
022300071112     c                   leavesr
022400071105w   1c                   when      D09ocgi  <> *blanks
022500071031     c                   movel     D09odes       WcgiD
022600071105e   1c                   endsl
022700071022      *
022800071022     c                   ENDSR
022900071022
023000071022      *---------------------------------------------------------------*
023100071022      *?Interrogazione giri ritiro                                   ?*
023200071022      *---------------------------------------------------------------*
023300071022     c     Search_Giro   BEGSR
023400071030      *
023500071031     c                   clear                   Wpocgi
023600071031     c                   clear                   Wcgi
023700071030     c                   clear                   WcgiD
023800071022      *
023900071022     c                   clear                   FIDG09ds
024000071022     c                   eval      D09iop0  =  'P01'
024100071030     c                   eval      D09ifgs  =  I37pocgi
024200071030     c                   eval      D09idat  =  *date
024300071022     c                   movel     VPosInt       D09ides
024400071022     c                   eval      D09itug  =  'R'
024500071022      *
024600071022     c                   movel(p)  FIDG09ds      KPJBU
024700071022     c                   call      'FIDG09R'
024800071022     c                   parm                    KPJBA
024900071022     c                   movel     KPJBU         FIDG09ds
025000071022      *
025100081211sel 1c                   select
025200081211w   1c                   when           D09Of03 <> *on
025300071022     c                             and  D09Of12 <> *on
025400071022     c                             and  D09Oerr <> *on
025500071030     c                   eval      Wpocgi   =  D09ofgs
025600071030     c                   eval      Wcgi     =  D09ocgi
025700071030     c                   movel     D09odes       WcgiD
025800081211w   1c                   when      D09Of03   = *on
025900081211     c                   eval      *in52     = *on
026000081211     c                   eval      *in90     = *on
026100081211w   1c                   when      D09Of12   = *on
026200081211e   1c                   endsl
026300071022      *
026400071022     c                   ENDSR
026500071022
026600071022      *---------------------------------------------------------------*
026700071022      *?Gestione videata D01                                         ?*
026800071022      *---------------------------------------------------------------*
026900071022     c     GesD01        BEGSR
027000071022      *
027100071022      * Inizializzazione videata
027200071022if  1c                   if        $InzD01  =  *on
027300071022     c                   exsr      InzD01
027400071022     c                   eval      $InzD01  =  *off
027500071022e   1c                   endif
027600071023      *
027700071023      * Emissione testata
027800081211     c                   if        NOT *in28
027900071024     c                   write     OR37T01
028000081211     c                   endif
028100071022      *
028200071022      * Emissione videata
028300071024     c                   exfmt     OR37D01
028400071022     c                   setoff                                       28  90
028500071022     c                   clear                   V1Dmsg
028600071022      *
028700071022sel 1c                   SELECT
028800071022      * F3=Fine
028900071022w   1c                   WHEN      *inKC
029000071022     c                   exsr      F03D01
029100071022      * F12=Ritorno
029200071022w   1c                   WHEN      *inKL
029300071022     c                   exsr      F12D01
029400071022      * Enter / F6=Conferma
029500071022x   1c                   OTHER
029600071022     c                   exsr      CtrD01
029700071022if  2c                   if        NOT  *in90
029800071022     c                             and  *inKF
029900071030     c                   exsr      F06D01
030000071022if  3c                   if        NOT  *in90
030100071022     c                   eval      $Fine    =  *on
030200071022e   3c                   endif
030300071022e   2c                   endif
030400071022e   1c                   ENDSL
030500071022      *
030600071022     c                   ENDSR
030700071022
030800071022      *---------------------------------------------------------------*
030900071022      *?Inizializzazione videata D01                                 ?*
031000071022      *---------------------------------------------------------------*
031100071022     c     InzD01        BEGSR
031200071022      *
031300071024     c                   clear                   OR37D01
031400071026      *
031500071026      * Impostazione indicatori utilizzati nella videata
031600071026     c                   eval      *in41    =  (I37opz = '9')
031700071022      *
031800071022      * Impostazione testata
031900071025if  1c                   if        I37opz   =  '9'
032000071025     c                   eval      V1Ttxt   =  C_Txt_2
032100071025x   1c                   else
032200071025     c                   eval      V1Ttxt   =  C_Txt_1
032300071025e   1c                   endif
032400071022      *
032500071022      * Impostazione campi a video
032600071022      *
032700071024     c     I37cro        chain     FNACR000
032800071022      *
032900071022if  1c                   IF        %found(FNACR01L)
033000071022      *
033100071022     c                   eval      H1Ccro   =  ACRcro
033200071022     c                   eval      H1Ccco   =  ACRcco
033300071022      *
033400071022     c                   movel     ACRcro        ds_CRO
033500071022     c                   eval      V1Ccd1   =  dsCRO1
033600071022     c                   eval      V1Ccd2   =  dsCRO2
033700071022     c                   eval      V1Ccd3   =  dsCRO3
033800071022     c                   eval      V1Crsr   =  ACRrsr
033900071022     c                   eval      V1Cinr   =  ACRinr
034000071022     c                   eval      V1Ccar   =  ACRcar
034100071022     c                   eval      V1Ccar   =  ACRcar
034200071022     c                   eval      V1Clor   =  ACRlor
034300071022     c                   eval      V1Cprr   =  ACRprr
034400071022     c                   eval      V1Cnar   =  ACRnar
034500071022if  2c                   if        ACRcco   <> *zeros
034600071022     c                   movel     ACRcco        ds_CRO
034700071022     c                   move      dsCRO1        V1Cco1
034800071022     c                   eval      V1Cco2   =  dsCRO2
034900071022     c                   move      dsCRO3        V1Cco3
035000071022e   2c                   endif
035100071022     c                   eval      V1Cksc   =  ACRksc
035200071022if  2c                   if        ACRccc   <> 999
035300071022     c                   z-add     ACRccc        V1Cctr
035400071022e   2c                   endif
035500071022sel 2c                   select
035600071022w   2c                   when      ACRtcr   =  'F'
035700071022     c                   eval      V1Dtcr   =  'Fisso               '
035800071022w   2c                   when      ACRtcr   =  'R'
035900071022     c                   eval      V1Dtcr   =  'Ricorrente          '
036000071022w   2c                   when      ACRtcr   =  'O'
036100081015     c                   eval      V1Dtcr   =  'No Internet         '
036200071022w   2c                   when      ACRtcr   =  'M'
036300071022     c                   eval      V1Dtcr   =  'Mai                 '
036400071022x   2c                   other
036500071022     c                   eval      V1Dtcr   =  '> ' + ACRtcr + ' <'
036600071022e   2c                   endsl
036700071022if  2c                   if        ACRfcc  <>  'N'
036800071022     c                   eval      V1Cfcc   =  'SI'
036900071022x   2c                   else
037000071022     c                   eval      V1Cfcc   =  'NO'
037100071022e   2c                   endif
037200071022     c                   eval      V1Corr   =  ACRorr
037300071022     c                   eval      V1Cper   =  ACRper
037400071022     c                   eval      V1Ctel   =  ACRtel
037500071022     c                   eval      V1Cfcl   =  ACRfcl
037600071022     c                   eval      V1Cfbn   =  ACRfbn
037700071022     c                   eval      V1Cfpk   =  ACRfpk
037800071022     c                   eval      V1Cfmc   =  ACRfmc
037900071022     c                   eval      V1Cfbl   =  ACRfbl
038000071022     c                   eval      V1Cfmt   =  ACRfmt
038100071022     c                   eval      V1Cfat   =  ACRfat
038200071022if  2c                   if        ACRspi   =  'S'
038300071022     c                   eval      V1Cspi   =  'SI'
038400071022x   2c                   else
038500071022     c                   eval      V1Cspi   =  'NO'
038600071022e   2c                   endif
038700071022     c                   eval      V1Cntm   =  ACRntm
038800071022     c                   eval      V1Cno1   =  ACRno1
038900071022     c                   eval      V1Cno2   =  ACRno2
039000150909     c                   clear                   V1Dpoa
039100150910     c                   eval      *in57 = *off
039200150910     c                   IF        ACRpoa <>
039300150910     c                             %dec(%subst(%editc(ACRcro:'X'):1:3):3:0)
039400150909     c     ACRpoa        chain     AZORG01L
039500150909     c                   IF        %found(AZORG01L)
039600150909     c                   eval      V1Dpoa = ORGdes
039700150909     c                   ENDIF
039800150910     c                   eval      *in57 = *on
039900150910     c                   ENDIF
040000071022      *
040100071022e   1c                   ENDIF
040200071022      *
040300071030     c     k02acr13      setll     FNACR100
040400071030     c     k02acr13      reade     FNACR100
040500071030do  1c                   DOW       NOT  %eof(FNACR13L)
040600071022     c                             and  ACR1atb = *blanks
040700071030sel 2c                   select
040800071030w   2c                   when      ACR1atb  <> ACRatb
040900071030w   2c                   when      ACR1ain   = '='
041000071030     c                   eval      V1Cpocgi1 = ACR1pocgi
041100071030     c                   eval      V1Ccgi1   = ACR1cgi
041200071030w   2c                   when      ACR1ain   = '>'
041300071030     c                   eval      V1Cpocgi2 = ACR1pocgi
041400071030     c                   eval      V1Ccgi2   = ACR1cgi
041500071030w   2c                   when      ACR1ain   = '<'
041600071030     c                   eval      V1Cpocgi3 = ACR1pocgi
041700071030     c                   eval      V1Ccgi3   = ACR1cgi
041800071030e   2c                   endsl
041900071030     c     k02acr13      reade     FNACR100
042000071030e   1c                   ENDDO
042100071022      *
042200071022      * Decodifica codice ordinante
042300071022if  1c                   IF        ACRcco   <> *zeros
042400071022     c     ACRcco        chain     FNACR000
042500071022if  2c                   if             %found(FNACR01L)
042600071022     c                             and  ACRatb  = *blanks
042700071022     c                   movel     ACRrsr        V1Ddeo
042800071022x   2c                   else
042900071022     c                   eval      V1Ddeo   =  *all'? '
043000071022e   2c                   endif
043100071022e   1c                   ENDIF
043200071022      *
043300071022      * Decodifica codice PdC
043400071026if  1c                   IF        V1Cksc   <> *zeros
043500071026     c                   reset                   TIBS69ds
043600071022     c                   eval      I69kac   =  V1Cksc
043700071022     c                   exsr      Call_TIBS69
043800071022     c                   movel     ACOrag        V1Dksc
043900071022e   1c                   ENDIF
044000071022      *
044100071030      * Decodifica codici giro
044200071030if  1c                   IF        V1Ccgi1  <> *blank
044300071105     c                   eval      wNrCGI   =  1
044400071031     c                   eval      Wpocgi   =  V1Cpocgi1
044500071031     c                   eval      Wcgi     =  V1Ccgi1
044600071022     c                   exsr      Ctr_Giro
044700071031     c                   eval      V1Dcgi1  =  WcgiD
044800071022if  2c                   if        *in90    =  *on
044900071022     c                   eval      *in28    =  *off
045000071022     c                   movea     *zeros        *in(50)
045100071022e   2c                   endif
045200071022e   1c                   ENDIF
045300071030if  1c                   IF        V1Ccgi2  <> *blank
045400071105     c                   eval      wNrCGI   =  2
045500071031     c                   eval      Wpocgi   =  V1Cpocgi2
045600071031     c                   eval      Wcgi     =  V1Ccgi2
045700071030     c                   exsr      Ctr_Giro
045800071031     c                   eval      V1Dcgi2  =  WcgiD
045900071030if  2c                   if        *in90    =  *on
046000071030     c                   eval      *in28    =  *off
046100071030     c                   movea     *zeros        *in(50)
046200071030e   2c                   endif
046300071030e   1c                   ENDIF
046400071030if  1c                   IF        V1Ccgi3  <> *blank
046500071105     c                   eval      wNrCGI   =  3
046600071031     c                   eval      Wpocgi   =  V1Cpocgi3
046700071031     c                   eval      Wcgi     =  V1Ccgi3
046800071030     c                   exsr      Ctr_Giro
046900071031     c                   eval      V1Dcgi3  =  WcgiD
047000071030if  2c                   if        *in90    =  *on
047100071030     c                   eval      *in28    =  *off
047200071030     c                   movea     *zeros        *in(50)
047300071030e   2c                   endif
047400071030e   1c                   ENDIF
047500071022      *
047600071022     c                   ENDSR
047700071022
047800071022      *---------------------------------------------------------------*
047900071022      *?Gestione tasto funzionale F3 da videata D01                  ?*
048000071022      *?F3=Fine                                                      ?*
048100071022      *---------------------------------------------------------------*
048200071022     c     F03D01        BEGSR
048300071022      *
048400071024     c                   eval      O37ret   =  '1'
048500071022      * Chiusura del programma
048600071022     c                   eval      $Fine    =  *on
048700071022      *
048800071022     c                   ENDSR
048900071022
049000071022      *---------------------------------------------------------------*
049100071022      *?Gestione tasto funzionale F12 da videata D01                 ?*
049200071022      *?F12=Ritorno                                                  ?*
049300071022      *---------------------------------------------------------------*
049400071022     c     F12D01        BEGSR
049500071022      *
049600071024     c                   eval      O37ret   =  '2'
049700071022      * Chiusura del programma
049800071022     c                   eval      $Fine    =  *on
049900071022      *
050000071022     c                   ENDSR
050100071022
050200071022      *---------------------------------------------------------------*
050300071022      *?Controllo dati videata D01                                   ?*
050400071022      *---------------------------------------------------------------*
050500071022     c     CtrD01        BEGSR
050600071105      *
050700071105     c                   movea     *zeros        *in(50)
050800071022      *
050900071030      * Giri di ritiro
051000071105do  1c                   DO        C_MaxCGI      wNrCGI
051100071030      *
051200071031     c                   clear                   WcgiD
051300071030sel 2c                   select
051400071030w   2c                   when      wNrCGI  = 1
051500071030     c                   eval      Wpocgi  = V1Cpocgi1
051600071030     c                   eval      Wcgi    = V1Ccgi1
051700071030w   2c                   when      wNrCGI  = 2
051800071030     c                   eval      Wpocgi  = V1Cpocgi2
051900071030     c                   eval      Wcgi    = V1Ccgi2
052000071030w   2c                   when      wNrCGI  = 3
052100071030     c                   eval      Wpocgi  = V1Cpocgi3
052200071030     c                   eval      Wcgi    = V1Ccgi3
052300071030e   2c                   endsl
052400071030      *
052500071030      * - controllo codice giro ritiro
052600071031sel 2c                   Select
052700071031w   2c                   When      Wcgi    = *blanks
052800071031     c                   clear                   Wpocgi
052900071031      *   - interrogazione giri di ritiro
053000071031w   2c                   When      %scan('?' : %trimr(Wcgi)) > *zeros
053100071030     c                   exsr      Search_Giro
053200081211     c*** già impostato: eval      *in90   = *on
053300071030      *   - controllo codice giro ritiro
053400071031x   2c                   Other
053500071030     c                   exsr      Ctr_Giro
053600071031e   2c                   EndSl
053700071031      *
053800071031sel 2c                   Select
053900071031w   2c                   When      wNrCGI  = 1
054000071030     c                   eval      V1Cpocgi1 = Wpocgi
054100071031     c                   eval      V1Ccgi1   = Wcgi
054200071030     c                   eval      V1Dcgi1   = WcgiD
054300071031w   2c                   When      wNrCGI  = 2
054400071030     c                   eval      V1Cpocgi2 = Wpocgi
054500071031     c                   eval      V1Ccgi2   = Wcgi
054600071030     c                   eval      V1Dcgi2   = WcgiD
054700071031     c                   eval      *in53   = (*in51 = *on)
054800071031     c                   eval      *in54   = (*in52 = *on)
054900071031     c                   eval      *in51   = *off
055000071031     c                   eval      *in52   = *off
055100071031w   2c                   When      wNrCGI  = 3
055200071030     c                   eval      V1Cpocgi3 = Wpocgi
055300071031     c                   eval      V1Ccgi3   = Wcgi
055400071030     c                   eval      V1Dcgi3   = WcgiD
055500071031     c                   eval      *in55   = (*in51 = *on)
055600071031     c                   eval      *in56   = (*in52 = *on)
055700071031     c                   eval      *in51   = *off
055800071031     c                   eval      *in52   = *off
055900071031e   2c                   EndSl
056000071031if  2c                   if        *in90
056100071030     c                   leavesr
056200071031e   2c                   endif
056300071030      *
056400071030e   1c                   ENDDO
056500071022      *
056600071022     c                   ENDSR
056700071022
056800071022      *---------------------------------------------------------------*
056900071022      *?Reperimento dati anagrafici                                  ?*
057000071022      *---------------------------------------------------------------*
057100071022     c     Call_TIBS69   BEGSR
057200071022      *
057300071022     c                   clear                   ds_CNACO
057400071022     c                   clear                   ds_CNIND
057500071022     c                   clear                   ds_CNCLP
057600071022     c                   clear                   ds_FNCLS
057700071022      *
057800071022     c                   call      'TIBS69R'
057900071022     c                   parm                    TIBS69ds
058000071022     c                   parm                    ds_CNACO
058100071022     c                   parm                    ds_CNIND
058200071022     c                   parm                    ds_CNCLP
058300071022     c                   parm                    ds_FNCLS
058400071022      *
058500071022if  1c                   if        O69err   =  *on
058600071022     c                   seton                                        28  90
058700071022     c                   eval      V1Dmsg   =  O69msg
058800071022e   1c                   endif
058900071022      *
059000071022     c                   ENDSR
059100071030
059200071030      *---------------------------------------------------------------*
059300071030      *?Gestione tasto funzionale F6 da videata D01                  ?*
059400071030      *?F6=Conferma                                                  ?*
059500071030      *---------------------------------------------------------------*
059600071030     c     F06D01        BEGSR
059700071030      *
059800071030      * Aggiornamento ambito "standard" di intradamento codice giro
059900071030     c                   eval      ACR1ain = '='
060000071030     c                   eval      Wpocgi  = V1Cpocgi1
060100071030     c                   eval      Wcgi    = V1Ccgi1
060200071030     c                   exsr      Upd_FNACR1
060300071030      *
060400071030      * Aggiornamento ambito "oltre" di intradamento codice giro
060500071030     c                   eval      ACR1ain = '>'
060600071030     c                   eval      Wpocgi  = V1Cpocgi2
060700071030     c                   eval      Wcgi    = V1Ccgi2
060800071030     c                   exsr      Upd_FNACR1
060900071030      *
061000071030      * Aggiornamento ambito "sotto" di intradamento codice giro
061100071030     c                   eval      ACR1ain = '<'
061200071030     c                   eval      Wpocgi  = V1Cpocgi3
061300071030     c                   eval      Wcgi    = V1Ccgi3
061400071030     c                   exsr      Upd_FNACR1
061500071030      *
061600071030     c                   ENDSR
061700071022
061800071022      *---------------------------------------------------------------*
061900071022      *?Aggiornamento dati file FNACR10F                             ?*
062000071022      *---------------------------------------------------------------*
062100071022     c     Upd_FNACR1    BEGSR
062200071030      *
062300071031     c     k03acr13      chain     FNACR100
062400071105     c                   clear                   SAVpocgi
062500071105     c                   clear                   SAVcgi
062600071105if  2c                   if        %found(FNACR13L)
062700071105     c                   eval      SAVpocgi     = ACR1pocgi
062800071105     c                   eval      SAVcgi       = ACR1cgi
062900071105e   2c                   endif
063000071022      *
063100071022      * Impostazione campi nel record
063200071030if  1c                   if        Wcgi     <> *blanks
063300071030     c                   exsr      RieRec1
063400071030e   1c                   endif
063500071022      *
063600071022      * Inserimento / Modifica
063700071030     c                   eval      *in92     = *off
063800071022sel 1c                   select
063900071030w   1c                   when      NOT  %found(FNACR13L)
064000071030     c                             and  Wcgi   <> *blanks
064100071022     c                   WRITE     FNACR100                             92
064200071022      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
064300071030w   1c                   when           %found(FNACR13L)
064400071030     c                             and  Wcgi   <> *blanks
064500071105     c                             and  Wcgi   <> SAVcgi
064600071022     c                   UPDATE    FNACR100
064700071022      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
064800071030w   1c                   when           %found(FNACR13L)
064900071030     c                             and  Wcgi   =  *blanks
065000071026     c                   DELETE    FNACR100
065100071026      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
065200071030w   1c                   when           %found(FNACR13L)
065300071105     c                             and  Wcgi   =  SAVcgi
065400071030     c                   UNLOCK    FNACR13L
065500071030      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
065600071022e   1c                   endsl
065700071022      *
065800071022      * Errore in fase di aggiornamento record
065900071022if  1c                   if        *in92
066000071022     c                   seton                                        28  90
066100071022     c                   eval      V1Dmsg   = %trimr($Msg(9))
066200071022     c                                      + ' FNACR10F'
066300071022      * uscita dal programma
066400071022x   1c                   else
066500071024     c                   eval      O37ret   = *off
066600071022     c                   eval      $Fine    = *on
066700071022e   1c                   endif
066800170222      *
066900170222      * -?"Conferma" dell'aggiornamento appena eseguito?
067000170222     c                   if             Not  *in90
067100170222     c                             and  Wcgi <> SAVcgi
067200170222     c                   feod      FNACR13L
067300170222     c                   endif
067400071022      *
067500071022     c                   ENDSR
067600071030
067700071030      *---------------------------------------------------------------*
067800071030      *?Impostazione valori nei campi del record FNACR10F            ?*
067900071030      *---------------------------------------------------------------*
068000071030     c     RieRec1       BEGSR
068100071030      *
068200071030      *
068300071030     c                   eval      ACR1cro   = H1Ccro
068400071030     c                   eval      ACR1pocgi = Wpocgi
068500071030     c                   eval      ACR1cgi   = Wcgi
068600071030if  1c                   if        NOT  %found(FNACR13L)
068700071030     c                             or  (SAVpocgi <> Wpocgi
068800071030     c                             or   SAVcgi   <> Wcgi)
068900071030     c                   eval      ACR1dtv   = *Date
069000071030     c                   eval      ACR1ute   = KNMUS
069100071030e   1c                   endif
069200071030      *
069300071030     c                   ENDSR
069400071022
069500071022** - $MSG -------------------------------------------------------------------*
069600071022Filiale errata                                                                  1
069700071022La Filiale non è in gestione all'utente                                         2
069800071022Parzializzazione per località errata                                            3
069900071022Codice giro di ritiro inesistente o non valido                                  4
070000071022Opzione errata                                                                  5
070100071022Immettere anche il giro di consegna  o  togliere la filiale del giro            6
070200071022Immettere anche la filiale del giro  o  togliere il giro di consegna            7
070300071022Filiale errata per codice giro di ritiro                                        8
070400071022Errore in fase di scrittura record nel file                                     9
070500071112Codice giro di ritiro errato o NON in vigore                                   10
070600071022Procedura ORM non attivata                                                     11
