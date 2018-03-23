000100101115      //--------------------------------------------------------------------
000200101115      // Recupero tipo invio fattura e relative caratteriste di fatturazione
000300101115      //--------------------------------------------------------------------
000400040107
000500040108     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600040107
000700101115      //--------------------------------------------------------------*
000800040107
001000101115
001100101108     ftrul57d_t cf   e             workstn
001101101115     f                                     indds(IndDspF)
001102101115     f                                     infds(InfDspF)
001103101115
001300040107
001401101115       //--------------------------------------------------------------
001402101115       //?Definizione costanti.
001403101115       //--------------------------------------------------------------
001404101115
001405101115       // - Tasti funzionali a video
001406101115     d c_F01           c                   const(x'31')
001407101115     d c_F02           c                   const(x'32')
001408101115     d c_F03           c                   const(x'33')
001409101115     d c_F04           c                   const(x'34')
001410101115     d c_F05           c                   const(x'35')
001411101115     d c_F06           c                   const(x'36')
001412101115     d c_F07           c                   const(x'37')
001413101115     d c_F08           c                   const(x'38')
001414101115     d c_F09           c                   const(x'39')
001415101115     d c_F10           c                   const(x'3A')
001416101115     d c_F11           c                   const(x'3B')
001417101115     d c_F12           c                   const(x'3C')
001418101115     d c_F13           c                   const(x'B1')
001419101115     d c_F14           c                   const(x'B2')
001420101115     d c_F15           c                   const(x'B3')
001421101115     d c_F16           c                   const(x'B4')
001422101115     d c_F17           c                   const(x'B5')
001423101115     d c_F18           c                   const(x'B6')
001424101115     d c_F19           c                   const(x'B7')
001425101115     d c_F20           c                   const(x'B8')
001426101115     d c_F21           c                   const(x'B9')
001427101115     d c_F22           c                   const(x'BA')
001428101115     d c_F23           c                   const(x'BB')
001429101115     d c_F24           c                   const(x'BC')
001430101115     d c_Enter         c                   const(x'F1')
001431101115     d c_RollDown      c                   const(x'F4')
001432101115     d c_RollUp        c                   const(x'F5')
001433101115
001434101115
001435101115       //--------------------------------------------------------------
001436101115       //?Definizione schiere.
001437101115       //--------------------------------------------------------------
001438101115
001439101115       // - Messaggi di errore
001440101115     d $Msg            s             78    dim( 2) ctdata perrcd(1)
001441101115
001443101115       //--------------------------------------------------------------
001444101115       //?Definizione aree dati.
001445101115       //--------------------------------------------------------------
001446101115
001447101115       // - Dati utente
001448101115     d §AzUte        e ds                  extname(AZUTE00F)
001449101115     d                                     dtaara
001450101115     d §DatiUte      e ds                  extname(dDatiUte)
001451101115     d                                     dtaara
001452101115
001453101115       //--------------------------------------------------------------
001454101115       //?Definizione strutture dati.
001455101115       //--------------------------------------------------------------
001456101115
001458101115       // - InfDS
001459101115     d InfDspF         ds
001460101115     d  dsp_aid              369    369a
001461101115
001462101115       // - Indicatori su DspF
001463101115     d IndDspF         ds
001467101115         // - Indicatori di errore
001468101115     d  ErrMessage                    1n   overlay(IndDspF : 28)
001469101115     d  PosCurkscb                    1n   overlay(IndDspF : 50)
001471101115     d  ErrGenerico                   1n   overlay(IndDspF : 99)
001472101115
001500040107
001600040107      *
003400040107     d KPJBA         e ds
003401101115
003402101115       // - Parametri x Controllo profilo utenti
003403101115     d TIBS34ds      e ds
003404101115
003500040108      * - Parametri
003600101108     d trul57ds      e ds
003900040108      *
004000040108      * - Parametri x Controllo profilo utenti
004505090414     d Status         sds
004506090414     d  SDSpgm           *proc
009300040107      *
009301101115       // - Reperimento dati anagrafici
009302101115     d TIBS69ds      e ds
009303101115     d DS_cnaco      e ds                  inz extname(CNACO00F)
009304101115     d DS_cnind      e ds                  inz extname(CNIND00F)
009305101115     d DS_cnclp      e ds                  inz extname(CNCLP00F)
009306101115     d DS_fncls      e ds                  inz extname(FNCLS00F)
009307101115
009308101115
009309101115       //--------------------------------------------------------------
009310101115       //?Definizione variabili globali.
009311101115       //--------------------------------------------------------------
009312101115
009313101115       // - Flags booleani
009314101115     d $Fine           s               n   inz(*off)
009315101115     d $InzD01         s               n   inz(*on)
009316101115     d $InzD02         s               n   inz(*on)
009317101115
009318101115       // - Campi per la gestione del video
009319101115     d $Video          s              2a   inz('D1')
009320101115
012316081023
012318101115
012319101115       //--------------------------------------------------------------
012320101115       //?Definizione procedure usate.
012321101115       //--------------------------------------------------------------
012322101115
012323101115       // - Reperimento dati utente
012324101115     d tibs34r         pr                  extpgm('TIBS34R')
012325101115     d   tibs34ds                          likeds(TIBS34ds)
012326101115
012327101115       // - Reperimento dati fatturazione
012328101115     d trul57rPr       pr                  extpgm('TRUL57R')
012329101115     d   Kpjba                             likeds(KPJBA)
012330101115     d   trul57Pds...
012331101115     d                                     likeds(trul57ds)
012332101118       // - Reperimento codici collegati
012333101118     d tnta83rPr       pr                  extpgm('TNTA83R')
012334101118     d   Kpjba                             likeds(KPJBA)
012335101118
012336101115      /copy gaitrasrc/srcprotopr,tibs69r
014600040107
016600040107
016700040107     c     *Entry        plist
016800040107     c                   parm                    KPJBA
017503090403
017504101115      /free
017505101115
017506101115       // - Start
017507101115       *inLR = *on;
017511101115
017512101115       // - Elab
017513101115       dow  $Fine = *off;
017514101115         select;
017515101115           when  $Video = 'D1';
017516101115             exsr sr_GesD01;
017517101115           when  $Video = 'D2';
017518101115             exsr sr_GesD02;
017519101115           other;
017520101115             $Fine = *on;
017521101115         endsl;
017522101115       enddo;
017523101115
017524101115       // - End
017525101115       return;
017526101115
017527101115       //--------------------------------------------------------------
017528101115       //?Operazioni iniziali
017529101115       //--------------------------------------------------------------
017530101115       BEGSR *InzSR;
017531101115
017532101115         // - Reperimento dati utente
017533101115         exsr sr_DatiJob;
017534101115
017538101115       ENDSR;
017539101115
017540101115       //--------------------------------------------------------------
017541101115       //?Reperimento Dati del job (Utente/Operativi).
017542101115       //--------------------------------------------------------------
017543101115       BEGSR sr_DatiJob;
017544101115
017545101115         in(E) §AzUte;
017546101115         if NOT %error;
017547101115           in(E) §DatiUte;
017548101115         endif;
017549101115         if %error or RSut = *blanks;
017550101115           clear TIBS34ds;
017551101115           tibs34r(tibs34ds);
017552101115           in §AzUte;
017553101115           in §DatiUte;
017554101115         endif;
017555101115
017556101123         VRsut = RSut ;
017557101123         VKnsif = Knsif ;
017558101123
017559101115       ENDSR;
017560101115
017561101115       //--------------------------------------------------------------
017562101115       //?Gestione videata D01
017563101115       //--------------------------------------------------------------
017564101115       BEGSR  sr_GesD01;
017565101115
017566101115         // - Inizializzazione videata
017567101115         if  $InzD01 = *on;
017568101115           clear Vkscb ;
017569101115           $InzD01  = *off;
017570101115         endif;
017571101115
017572101115         // - Emissione videata
017576101115
017577101123         VRsut = RSut ;
017578101123         VKnsif = Knsif ;
017579101115         exfmt tr57t01 ;
017580101115
017581101115         reset ErrMessage;
017582101115         reset ErrGenerico;
017583101115         clear V1Dmsg;
017584101115
017585101115         SELECT;
017586101115
017587101115           // - F3=Fine
017588101115           WHEN  dsp_aid = c_F03;
017589101115             $Fine = *on;
017590101115
017591101115           // - Invio
017592101115           OTHER;
017593101115             exsr sr_CtrD01;
017594101115             select;
017595101115               // - rilevato errore
017596101115               when  ErrGenerico = *on;
017598101115               other;
017599101115                 $Video = 'D2';
017600101115                 reset  $InzD02;
017601101115             endsl;
017602101115
017603101115         ENDSL;
017604101115
017605101115       ENDSR;
017606101115
017607101115       //--------------------------------------------------------------
017608101115       //?Controllo dati in videata D01
017609101115       //--------------------------------------------------------------
017610101115       BEGSR sr_CtrD01;
017611101115
017612101115         %subst(IndDspF : 28) = *off;
017613101115
017614101115         SELECT;
017615101115
017616101115           // - Codice bollettazione uguale a zero o errato
017617101115           WHEN  vkscb  = *zeros ;
017618101115             ErrMessage  = *on;
017619101115             ErrGenerico = *on;
017620101115             PosCurKscb  = *on;
017621101115             V1Dmsg = $Msg(01);
017622101115             leavesr;
017623101115
017624101115           // - Codice di bollettazione inesistente
017625101115           WHEN  vkscb  <> *zeros ;
017626101115
017627101115                 clear  tibs69ds;
017628101115                 I69kac = Vkscb   ;
017630101115                 TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
017631101115                 If  o69err <>  *blanks ;
017632101115                     ErrMessage  = *on;
017633101115                     ErrGenerico = *on;
017634101115                     PosCurKscb  = *on;
017635101115                     V1Dmsg = $Msg(02);
017636101115                     leavesr;
017640101115                 Endif ;
017641101115
017642101115         ENDSL;
017643101115
017644101115       ENDSR;
017645101115
017646101115       //--------------------------------------------------------------
017647101115       //?Gestione videata D02                                         ?
017648101115       //--------------------------------------------------------------
017649101115       BEGSR  sr_GesD02;
017650101115
017651101115         // Inizializzazione videata
017652101115         if  $InzD02 = *on;
017656101115            clear tr57t11 ;
017657101123            VRsut = RSut ;
017658101123            VKnsif = Knsif ;
017659101115            v11kscb = vkscb ;
017660101115            vkscbd = acorag ;
017661101115            clear  tibs69ds;
017662101115            I69kcp = v11kscb ;
017663101115            I69kcs = v11kscb ;
017664101115            TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
017665101115            If  o69err = *blanks ;
017666101115                vfunib = %subst(clsflo: 2: 1);
017667101115                vtftb  = %editc(clptft: 'X') ;
017668101115                vfftb  = %editc(clpfft: 'X') ;
017669101115                vtdfb  = %subst(clpfun: 1: 1);
017670101115            Endif ;
017671101115            If  clpscf = clpksc  ;
017672101115                vkscf  = clpscf;
017673101115                vkscfd = acorag ;
017677101115                vfunif = %subst(clsflo: 2: 1);
017678101115                vtftf  = %editc(clptft: 'X') ;
017679101115                vfftf  = %editc(clpfft: 'X') ;
017680101115                vtdff  = %subst(clpfun: 1: 1);
017681101115            Endif ;
017682101115            // se sottoconto intestazione fattura diverso da codice di bollettazione recupero i dati
017683101115            // dall'intestazione fattura
017684101115            If clpscf <> clpksc ;
017685101115               clear  tibs69ds;
017686101115               I69kac = clpscf  ;
017687101115               I69kcp = clpscf  ;
017688101115               I69kcs = clpscf  ;
017689101115               TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
017690101115               If  o69err = *blanks ;
017691101214                   vkscf  = clpksc;
017692101115                   vkscfd = acorag ;
017693101115                   vfunif = %subst(clsflo: 2: 1);
017694101115                   vtftf  = %editc(clptft: 'X') ;
017695101115                   vfftf  = %editc(clpfft: 'X') ;
017696101115                   vtdff  = %subst(clpfun: 1: 1);
017697101115               Endif ;
017698101115               // verifico se fattura unificata e codice intestazione fattura del codice di
017699101115               // raggruppamento diverso da se stesso sostituisco i dati di bollettazione con quelli
017700101115               // di raggruppamento nel passaggio dei dati al pgm di visualizzazione
017701101115               If clpscf <> clpksc and vfunib = 'S' and vfunif = 'S' ;
017711101115                  clear  tibs69ds;
017712101115                  I69kac = clpscf  ;
017713101115                  I69kcp = clpscf  ;
017714101115                  I69kcs = clpscf  ;
017715101115                  TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
017716101115                  If  o69err = *blanks ;
017717101214                      vkscfi = clpksc;
017718101115                      vkscfid= acorag ;
017719101115                      vfunifi= %subst(clsflo: 2: 1);
017720101115                      vtftfi = %editc(clptft: 'X') ;
017721101115                      vfftfi = %editc(clpfft: 'X') ;
017722101115                      vtdffi = %subst(clpfun: 1: 1);
017723101115                  endif ;
017724101115               Endif ;
017727101115            Endif ;
017743101115           $InzD02  = *off;
017744101115         endif;
017745101115
017753101115
017754101115         exfmt tr57T11 ;
017755101115
017756101115         reset ErrMessage;
017757101115         reset ErrGenerico;
017758101115         clear V1Dmsg;
017759101115
017760101115         SELECT;
017761101115
017762101115         // - F3=Fine
017763101115           WHEN  dsp_aid = c_F03;
017764101115             $Fine = *on;
017765101115
017766101115         // - F12=Ritorno
017767101115           WHEN  dsp_aid = c_F12;
017768101115             $Video = 'D1';
017769101115             //reset $InzD01;
017770101115             reset $InzD02;
017771101115
017772101115         // - F6=Ricerca
017773101115           WHEN  dsp_aid = c_F06;
017774101115             exsr call_trul57 ;
017778101115
017779101118         // - F15=Codici collegati
017780101118           WHEN  dsp_aid = c_F15;
017781101118             kpjbu = %editc(vkscf:'X') ;
017782101118             tnta83rpr (Kpjba);
017783101118
017784101115         // - Invio
017785101115           OTHER;
017786101115
017787101115         ENDSL;
017788101115
017789101115       ENDSR;
017790101115
017791101115       //--------------------------------------------------------------
017792101115       //?Richiamo programma che visualizza i dati di fatturazione
017793101115       //--------------------------------------------------------------
017794101115       BEGSR Call_trul57  ;
017795101115
017796101115         clear trul57ds ;
017797101115         D57ipgm ='TRUL57R'  ;
017798101115         D57IRIT ='V' ;
017799101115         // verifico se raggruppamento fattura ed è valorizzato anche l'intestazione fattura
017800101115         // del codice di raggrupamento passo il codice di raggruppamento al posto
017801101115         // di quello di bollettazione
017802101117         If vkscfi <> *zeros and d57iunib = 'S' and d57iunif = 'S' ;
017803101115            D57iksco =vkscb  ;
017804101115            D57itfto =vtftb  ;
017805101115            D57iffto =vfftb  ;
017806101115            D57ikscb =vkscf  ;
017807101115            D57ikscf =vkscfi ;
017808101115            D57iunib =vfunif ;
017809101115            D57itftb =vtftf  ;
017810101115            D57ifftb =vfftf  ;
017811101115            D57idftb =vtdff  ;
017812101115            D57iunif =vfunifi;
017813101115            D57itftf =vtftfi ;
017814101115            D57ifftf =vfftfi ;
017815101115            D57idftf =vtdffi ;
017816101115         else ;
017817101115            D57iksco =vkscb  ;
017818101115            D57itfto =vtftb  ;
017819101115            D57iffto =vfftb  ;
017820101115            D57ikscb =vkscb  ;
017821101115            D57ikscf =vkscf  ;
017822101115            D57iunib =vfunib ;
017823101115            D57itftb =vtftb  ;
017824101115            D57ifftb =vfftb  ;
017825101115            D57idftb =vtdfb  ;
017826101115            D57iunif =vfunif ;
017827101115            D57itftf =vtftf  ;
017828101115            D57ifftf =vfftf  ;
017829101115            D57idftf =vtdff  ;
017830101115         Endif ;
017831101115
017832101115         trul57rpr (Kpjba:trul57ds);
017833101115
017834101115
017835101115       ENDSR;
017836101115
017837101115      /end-free
017838101115
017839101115** - $Msg
017840101115Obbligatorio codice di bollettazione
017841101115Codice errato inesistente
