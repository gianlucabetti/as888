000100070801      * FIOR37R1 *----------------------------------------------------*
000200070801      *?MANUTENZIONE ANAGRAFICA CLIENTI RITIRO (CON GIRO)            ?*
000300070730      *---------------------------------------------------------------*
000400070730
000500070801     h decedit('0,') datedit(*YMD.) option(*nodebugio)
000600070730
000700070730      *---------------------------------------------------------------*
000800000927
000900070801     fAZORG01L  if   e           k disk
001000070730     fFNORS02L  if   e           k disk    rename(FNORS000:FNORS002)
001100070730     fFNORS03L  if   e           k disk    rename(FNORS000:FNORS003)
001200070730     fFNORM18L  if   e           k disk    rename(FNORM000:FNORM018)
001300070730     fFNORM09L  if   e           k disk    rename(FNORM000:FNORM009)
001400070730     fTNTAM01L  if   e           k disk
001500121220     fTNTBE01L  if   e           k disk    usropn
001600121220     f                                     extfile(wLibTbe)
001700070730      *
001800070730     fFNACR01L  Uf A e           k disk
001900071030     fFNACR13L  Uf A e           k disk
002000151012     fFNACRE1L  Uf A e           k disk
002100070730      *
002200070801     fFIOR37D1  cf   e             workstn
002300151104     f                                     sfile(OR37S02 : S02nrr)
002400000927
002500070730      *
002600070730      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - -?*
002700070730      *
002800070801      * - Attributi di visualizzazione
002900070801     d C_HI_RI         c                   const(x'23')
003000070801     d C_ND            c                   const(x'27')
003100071105      * - Num. Max di codici giro ritiro inseribili per cliente ritiro
003200071105     d C_MaxCGI        c                   const(03)
003300131113
003400131113       // -?Costante per controllo "caratteri solo numerici"?
003500131113     d c_Digits        c                   const('0123456789')
003600070730      *
003700070730      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - -?*
003800070730      *
003900070801      * - Testi di decodifica funzione richiesta
004000071105     d $Fun            s             17    dim( 6)  ctdata  perrcd(1)
004100070730      * - Messaggi di errore
004200121220     d $Msg            s             78    dim(35)  ctdata  perrcd(1)           MSG VIDEO
004300151104
004400151104      // - Sk Frequenza Ritiro
004500151104     d Frequenza       s              1a   dim(04) ctdata perrcd(1)
004600151104     d DesFreq         s             20a   dim(04) alt(Frequenza)
004700070730      *
004800070730      *?  S T R U T T U R E   D A T I   - - - - - - - - - - - - - - -?*
004900070730      *
005000070730      * - Parametri
005100070730     d KPJBA         e ds
005200070730     d FIOR37ds      e ds                  inz
005300070730      *
005400060203      * - Parametri x Controllo profilo utenti
005500060203     d TIBS34ds      e ds
005600060203      * - Ds di riferimento al file esterno AZUTE00F
005700060203     d AZUTEds       e ds                  extname(AZUTE00F)
005800060203      * - Ds per dati organigramma
005900070801     d dDatiUte      e ds
006000070801      *
006100070801      * - Lettura anagrafiche cliente
006200070801     d TIBS69ds      e ds                  inz
006300070802     d ds_CNACO      e ds                  inz  extname(CNACO00F)
006400070802     d ds_CNIND      e ds                  inz  extname(CNIND00F)
006500070802     d ds_CNCLP      e ds                  inz  extname(CNCLP00F)
006600070802     d ds_FNCLS      e ds                  inz  extname(FNCLS00F)
006700070802      *
006800070802      * - Controllo CAP/Località
006900070802     d TISI95ds      e ds                  inz
007000070802      * - Pretrullo TISI95R
007100070802     d FNLV13ds      e ds                  inz
007200070802      * - Controllo indirizzo completo
007300070802     d FNLV14ds      e ds                  inz
007400060203      *
007500070730      * - Gestione anagrafica giri
007600070730     d FIDG09ds      e ds                  inz
007700121130
007800070802      * - Parametri x gestione anagrafica ritiri fissi
007900070802     d FIOR02ds      e ds                  inz
008000070802      *
008100070802      * - Controllo validita n° tel/fax
008200070802     d TRUL42ds      e ds                  inz
008300121130
008400121130      // - Parametri per ricerca Programmi chiamanti
008500121130     d dsStack         ds
008600121130     d  skStack                      10    Dim(100)
008700070730      *
008800070802      * - 143ª descrizione dell'organigramma
008900070802     d Og143         e ds                  inz
009000131113      *
009100131113      * - tabella OSR
009200121220     d dOSR          e ds                  inz
009300131113      *
009400131113      * - Orari ritiro ed altro in FNACR00F
009500131113     d dACR01        e ds                  inz  qualified
009600070730      *
009700070730     d Status         sds           333
009800070730     d   SDSpgm          *proc
009900070801      *
010000070801      * - Codice cliente ritiro
010100070801     d ds_CRO          ds            10    inz
010200070801     d  dsCRO1                        3  0 inz
010300070801     d  dsCRO2                        4  0 inz
010400070801     d  dsCRO3                        3  0 inz
010500070802      *
010600070802      * - Ora
010700070802     d ds_Time         ds             4    inz
010800070802     d  dsHH                          2  0 inz
010900070802     d  dsMM                          2  0 inz
011000070730      *
011100070730      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
011200070730      *
011300070801      * - Flags
011400070801     d $InzD01         s              1    inz(*on)
011500070801     d $Fine           s              1    inz(*off)
011600070802     d $Tam            s              1    inz(*off)
011700071030      * - Indici di schiera / Contatori
011800071030     d wNrCGI          s              3  0 inz
011900070802      * - Comodo
012000071030     d Wpocgi          s                   inz  like(V2Cpocgi1)
012100071030     d Wcgi            s                   inz  like(V2Ccgi1)
012200071030     d WcgiD           s                   inz  like(V2Dcgi1)
012300070802     d SAVcar          s                   inz  like(V2Ccar)
012400070802     d SAVlor          s                   inz  like(V2Clor)
012500070802     d SAVprr          s                   inz  like(V2Cprr)
012600070802     d SAVksc          s                   inz  like(V2Cksc)
012700070802     d SAVpocgi        s                   inz  like(ACR1pocgi)
012800070802     d SAVcgi          s                   inz  like(ACR1cgi)
012900110121     d wdecod          s              1    inz
013000121130     d wGiaChiamato    s               n   inz(*off)
013100121220     d wLibTbe         s             21a   inz
013200151104     d FineTcr         s               n   inz(*off)
013300151104     d S02nrr          s              4s 0 inz
013400151104     d savfreq         s                   like(W02tcr) inz
013500151104     d xx              s              4s 0 inz
013600140114
013700140114     d sav_§ACRstcr    s                   like(dACR01.§ACRstcr)
013800140114     d sav_§ACRctcr    s                   like(dACR01.§ACRctcr)
013900121130
014000121130      // - Campi per procedura esterna
014100121130     d  pStack         s           1000
014200121130     d  pStartPoint    s              3  0
014300121130     d  pEntries       s              3  0
014400121130
014500121130      //---------------------------------------------------------------
014600121130      //?Definizione procedure esterne.
014700121130      //---------------------------------------------------------------
014800121130
014900121130       // - Pgm per ricerca Programmi chiamati
015000121130     d XRTVSTKR        pr                  extpgm('XRTVSTKR')
015100121130     d  pStack                     1000
015200121130     d  pStartPoint                   3  0
015300121130     d  pEntries                      3  0
015400131113
015500131113       // -?Controllo orari apertura del cliente?
015600131113     d TRUL03ds      e ds                  qualified  inz
015700131113     d trul03r         pr                  extpgm('TRUL03R')
015800131113     d   trul03ds...
015900131113     d                                     likeds(trul03ds)
016000151012
016100151012       // -?Controllo indirizzo e-mail?
016200151012     d dsEmail       e ds                  inz
016300151012      /copy gaitrasrc/srcProtoPR,XEMAIL
016400131113
016500121130      //---------------------------------------------------------------
016600070730      *
016700070730      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
016800070730      *
016900151012       // -?File FNACRE1L?
017000151012     d keyFNACRE1    e ds                  extname( FNACRE1L : *key )
017100151012     d                                     prefix( k_ )   inz
017200071030      * - FNACR13L
017300071030     c     k02acr13      klist
017400071023     c                   kfld                    I37cro
017500071023     c                   kfld                    I37fgs
017600071030     c     k03acr13      klist
017700071030     c                   kfld                    I37cro
017800071030     c                   kfld                    I37fgs
017900071030     c                   kfld                    ACR1ain
018000070801      * - TNTAM01L
018100070802     c     k02tam01      klist
018200070802     c                   kfld                    TAMksc
018300070801     c                   kfld                    TAMctr
018400121130
018500070801      *---------------------------------------------------------------*
018600070801      *?RIEPILOGO INDICATORI                                         ?*
018700070801      *---------------------------------------------------------------*
018800070801      *  02    - Abilitazione tasto funzionale F2=Anagr.Fissi   - D01 *
018900070801      *  03    - Abilitazione tasto funzionale F3=Fine          - D01 *
019000070801      *  05    - Abilitazione tasto funzionale F5=Ripristino    - D01 *
019100070801      *  06    - Abilitazione tasto funzionale F6=Conferma      - D01 *
019200070801      *  16    - Abilitazione tasto funzionale F15=Annullamento - D01 *
019300070801      *  25    - Comodo                                               *
019400070801      *  28    - Emissione del messaggio di errore a video            *
019500140113      *  39    - Visualizzo Frequenza ritiro salvata x EDP
019600140113      *  40    - Proteggo Frequenza ritiro
019700070801      *  41    - Protezione campi chiave in modifica            - D01 *
019800070809      *  42    - Protezione campi codice PdC e Tariffa          - D01 *
019900070809      *  43    - Visualizzazione "ANNULLATO"                    - D01 *
020000070801      *  51    - Cliente ritiro           errato                - D01 *
020100150909      *  52    - Filiale ritiro forzata   errata                - D01 *
020200070801      *  53    - Ragione sociale          errata                - D01 *
020300070801      *  54    - Indirizzo                errato                - D01 *
020400070801      *  55    - CAP                      errato                - D01 *
020500070801      *  56    - Località                 errata                - D01 *
020600070801      *  57    - Provincia                errata                - D01 *
020700070801      *  58    - Nazione                  errata                - D01 *
020800071030      *  59    - Fil. Giro Ritiro ("=")   errata                - D01 *
020900071030      *  60    - Giro di Ritiro   ("=")   errato                - D01 *
021000070801      *  61    - Ordinante                errato                - D01 *
021100070801      *  62    - Codice PdC               errato                - D01 *
021200070801      *  63    - Tariffa                  errata                - D01 *
021300070801      *  64    - Frequenza ritiro         errata                - D01 *
021400070801      *  65    - Ora       ritiro         errata                - D01 *
021500070801      *  67    - Telefono referente       errato                - D01 *
021600070801      *  68    - E-Mail   referente       errata                - D01 *
021700070801      *  69    - Flag immissione colli    errato                - D01 *
021800070801      *  70    - Flag immissione bancali  errato                - D01 *
021900070801      *  71    - Flag immissione peso     errato                - D01 *
022000070801      *  72    - Flag immissione volume   errato                - D01 *
022100070801      *  73    - Flag immissione bilico   errato                - D01 *
022200070801      *  74    - Flag immissione motrice  errato                - D01 *
022300070801      *  75    - Flag immissione autotr.  errato                - D01 *
022400071030      *  76    - Fil. Giro Ritiro (">")   errata                - D01 *
022500071030      *  77    - Giro di Ritiro   (">")   errato                - D01 *
022600071030      *  78    - Fil. Giro Ritiro ("<")   errata                - D01 *
022700071030      *  79    - Giro di Ritiro   ("<")   errato                - D01 *
022800070801      *  90    - Generico di errore                                   *
022900070803      *  91    - Fallito aggiornamento di FNACR00F                    *
023000070803      *  92    - Fallito aggiornamento di FNACR10F                    *
023100000927      *---------------------------------------------------------------*
023200070730
023300070730     c     *Entry        plist
023400070730     c                   parm                    KPJBA
023500070801     c                   movel     KPJBU         FIOR37ds
023600070730      *
023700070730      * Operazioni iniziali
023800070801     c                   exsr      RoutInz
023900070801      *
024000071024      * Gestione video
024100070801do  1c                   DOW       $Fine   = *off
024200070801     c                   exsr      GesD01
024300070801e   1c                   ENDDO
024400070801      *
024500071024      * Operazioni finali
024600070801     c                   movel(p)  FIOR37ds      KPJBU
024700070801     c                   eval      *inLR   = *on
024800070801
024900070801      *---------------------------------------------------------------*
025000070801      *?Operazioni Iniziali                                          ?*
025100070801      *---------------------------------------------------------------*
025200070801     c     RoutInz       BEGSR
025300070801      *
025400070801      * Pulizia campi di output
025500070801     c                   clear                   O37cro
025600070801     c                   clear                   O37err
025700070801     c                   clear                   O37msg
025800071030     c                   eval      O37ret  = *off
025900070801      *
026000070801      * Controllo parametri ricevuti
026100070801sel 1c                   SELECT
026200070801      * - Controllo inserimento filiale gestione per Immissione/Variaz.
026300070801w   1c                   WHEN           I37opz  <> '1'
026400070801     c                             and  I37opz  <> '2'
026500070801     c                             and  I37opz  <> '3'
026600070801     c                             and  I37opz  <> '4'
026700070801     c                             and  I37opz  <> '5'
026800070801     c                             and  I37opz  <> '7'
026900070809     c                             and  I37opz  <> 'A'
027000070809     c                             and  I37opz  <> 'C'
027100071030     c                   eval      O37err  = 'E'
027200071030     c                   eval      O37msg  = $Msg(1)
027300071030     c                   eval      $Fine   = *on
027400070801     c                   leavesr
027500070807w   1c                   WHEN           I37opz  <> '1'
027600070801     c                             and  I37cro   = *zeros
027700071030     c                   eval      O37err  = 'E'
027800071030     c                   eval      O37msg  = $Msg(1)
027900071030     c                   eval      $Fine   = *on
028000070801     c                   leavesr
028100070802w   1c                   WHEN           I37opz  <> '5'
028200070802     c                             and  I37fgs   = *zeros
028300071030     c                   eval      O37err  = 'E'
028400071030     c                   eval      O37msg  = $Msg(1)
028500071030     c                   eval      $Fine   = *on
028600070801     c                   leavesr
028700070801e   1c                   ENDSL
028800070801      *
028900070801      * Reperimento dati job
029000070801     c                   exsr      DatiJob
029100070801      *
029200070801      * Impostazione nome programma a video
029300070801     c                   movel     SDSpgm        V1Tpgm
029400070801      *
029500070801      * Impostazione attributi di visualizzazione in testata
029600071030     c                   eval      DA01    = C_HI_RI
029700070801sel 1c                   select
029800071030w   1c                   when      I37opz  = '1'
029900071030     c                   eval      V1Topz  = $Fun(1)
030000071030w   1c                   when      I37opz  = '2'
030100071030     c                   eval      V1Topz  = $Fun(2)
030200071030w   1c                   when      I37opz  = '3'
030300071030     c                   eval      V1Topz  = $Fun(3)
030400071030w   1c                   when      I37opz  = '4'
030500071030     c                   eval      V1Topz  = $Fun(4)
030600071030w   1c                   when      I37opz  = '5'
030700071030     c                   eval      V1Topz  = $Fun(5)
030800071030w   1c                   when      I37opz  = '7'
030900071030     c                   eval      V1Topz  = $Fun(6)
031000071030w   1c                   when      I37opz  = 'A'
031100071030     c                   eval      V1Topz  = $Fun(1)
031200071030w   1c                   when      I37opz  = 'C'
031300071030     c                   eval      V1Topz  = $Fun(3)
031400070801x   1c                   other
031500071030     c                   eval      DA01    = C_ND
031600070801e   1c                   endsl
031700070801      *
031800070801      * Impostazione automatica della filiale di gestione
031900070801sel 1c                   select
032000070801      * - ricevuta
032100070801w   1c                   when           I37opz  <> *blanks
032200070801     c                             and  I37fgs  <> *zeros
032300070801      * - impostazione di default per filiali di 2° livello
032400070801w   1c                   when           DUTlpo   = '2'
032500070801     c                              or  DUTlpo   = *blanks
032600071030     c                   eval      I37fgs  = DUTpou
032700070801      * - impostazione di default per filiali di 1° livello
032800070801x   1c                   other
032900071030     c                   eval      I37fgs  = DUTtfp
033000070801e   1c                   endsl
033100121130
033200121130      /free
033300121130         //?Verifico se anagrafica ritiri fissi già richiamata
033400121130         //?in questo caso non abilito il tasto funzione F2=Ritiro Fissi
033500121130         //?per evitare la chiamata ricorsiva
033600121130         wGiaChiamato = *off;
033700121130         XRTVSTKR (pStack:pStartPoint:pEntries);
033800121130         dsStack = pStack;
033900121130         IF  %lookup('FIOR02R   ':skStack) > 0;
034000121130           wgiachiamato = *on;
034100121130         ENDIF;
034200130731         //?Se richiamato da nuova interrogazione anagrafica ritiri
034300130731         //?non abilito F2 in quanto non serve e se lo lascio abilitato
034400130731         //?cliente, in queso caso non emette niente perchè F2 va per FGS
034500130731         IF not wGiaChiamato;
034600130731           XRTVSTKR (pStack:pStartPoint:pEntries);
034700130731           dsStack = pStack;
034800130731           IF  %lookup('FIOR81R   ':skStack) > 0;
034900130731             wgiachiamato = *on;
035000130731           ENDIF;
035100130731         ENDIF;
035200121130      /end-free
035300070801      *
035400070801     c                   ENDSR
035500070801
035600070801      *---------------------------------------------------------------*
035700070801      *?Reperimento Dati del job (Utente/Operativi)                  ?*
035800070801      *---------------------------------------------------------------*
035900070801     c     DatiJob       BEGSR
036000070801      *
036100070801     c     *dtaara       define    §azute        azuteds
036200070801     c     *dtaara       define    §datiute      ddatiute
036300070801      *
036400070801     c                   in(E)     *dtaara
036500070801     c                   IF        %ERROR or RSUT = *blanks
036600070801     c                   clear                   Tibs34Ds
036700070801     c                   call      'TIBS34R'
036800070801     c                   parm                    Tibs34Ds
036900070801     c                   in        *dtaara
037000070801     c                   ENDIF
037100070801      *
037200070801     c                   ENDSR
037300070801
037400070801      *---------------------------------------------------------------*
037500070801      *?Gestione videata D01                                         ?*
037600070801      *---------------------------------------------------------------*
037700070801     c     GesD01        BEGSR
037800070801      *
037900070801      * Inizializzazione videata
038000070801if  1c                   if        $InzD01 = *on
038100070801     c                   exsr      InzD01
038200070801     c                   eval      $InzD01 = (*in90 = *on)
038300070801e   1c                   endif
038400070801      *
038500070801      * Scrittura di testata e di riga tasti funzionali abilitati
038600070801     c                   write     OR37T01
038700070801     c                   write     OR37Z01
038800070801      *
038900070801      * Emissione videata
039000070801if  1c                   if             I37opz <> '4'
039100070801     c                             and  I37opz <> '5'
039200070801     c                   exfmt     OR37D01
039300070801x   1c                   else
039400070801     c                   write     OR37D01
039500070801     c                   exfmt     PROTECT
039600070801e   1c                   endif
039700070801     c                   setoff                                       28  90
039800070801     c                   clear                   V1Dmsg
039900070801      *
040000070801sel 1c                   SELECT
040100070802      * F2=Anagrafica ritiri fissi
040200070802w   1c                   WHEN      *inKB
040300070802     c                   exsr      F02D01
040400070801      * F3=Fine
040500070801w   1c                   WHEN      *inKC
040600070801     c                   exsr      F03D01
040700070801      * F12=Ritorno
040800070801w   1c                   WHEN      *inKL
040900070801     c                   exsr      F12D01
041000070806      * Visualizzazione (nessun controllo, ma decodifiche)
041100070801w   1c                   WHEN      I37opz  = '5'
041200070802      * Inserimento/Modifica/Ripristino o Annullamento
041300070801x   1c                   OTHER
041400070802      * - Annullamento
041500070802if  2c                   if             I37opz  = '4'
041600070802     c                             or   *inKQ
041700070802     c                   exsr      Ctr_Annull
041800070802x   2c                   else
041900070802      * - Inserimento/Modifica/Ripristino
042000070801     c                   exsr      CtrD01
042100070802e   2c                   endif
042200070809if  2c                   if        NOT  *in90
042300070809      * - F5=Ripristino
042400070809     c                             and (*inKE
042500070802      * - F6=Conferma
042600070809     c                              or  *inKF
042700070802      * - F16=Annullamento
042800070802     c                              or  *inKQ)
042900070801     c                   exsr      F06D01
043000070801e   2c                   endif
043100070801e   1c                   ENDSL
043200070801      *
043300070801     c                   ENDSR
043400070801
043500070801      *---------------------------------------------------------------*
043600070801      *?Inizializzazione videata D01                                 ?*
043700070801      *---------------------------------------------------------------*
043800070801     c     InzD01        BEGSR
043900070801      *
044000070801     c                   clear                   OR37D01
044100070801     c                   clear                   FNACR000
044200070801     c                   clear                   FNACR100
044300151012     c                   clear                   FNACRE00
044400070903     c                   clear                   SAVpocgi
044500070910     c                   clear                   SAVcgi
044600070910     c                   clear                   SAVcar
044700070910     c                   clear                   SAVlor
044800070910     c                   clear                   SAVprr
044900071030     c                   eval      SAVksc  = *hival
045000140113     c                   eval      *in41 = *off
045100140114
045200140114      /free
045300140114         //?Utente EDP
045400140114         *in39 = *off;
045500140114         IF  %subst(knmus:1:3) = 'EDP';
045600140114           *in39 = *on;
045700140114         ENDIF;
045800151012
045900151012         // -?Impostazione chiave d'accesso al file FNACRE1L per il?
046000151012         //  ?reperimento della "e-Mail Conferma Accettazione ORM"?
046100151012         clear  keyFNACRE1;
046200151012         k_ACREcro = I37cro;
046300151012         k_ACREtrc = 'MC';
046400140114      /end-free
046500140114
046600070801      *
046700151012      * Reperimento dati
046800151012      *
046900070801sel 1c                   SELECT
047000070802      *
047100070801      * - Se richiesto inserimento:
047200070801      *   il controllo di effettiva inesistenza verrà eseguito nella
047300070801      *   subr. CTRD01.
047400070809w   1c                   WHEN           I37opz = '1'
047500070809     c                             or   I37opz = 'A'
047600071030     c                   eval      V2Ccd1  = I37fgs
047700130417     c                   eval      V2Ctcr  = 'R'
047800150909     c                   eval      V2Cpoa  = I37fgs
047900070802      *
048000070801      * - Se richiesta modifica/annullamento/ripristino:
048100070801      *   il record da aggiornare va allocato
048200070801w   1c                   WHEN           I37opz = '2'
048300070801     c                             or   I37opz = '4'
048400070801     c                             or   I37opz = '7'
048500070801     c     I37cro        chain     FNACR000
048600070801if  2c                   if        NOT  %found(FNACR01L)
048700070801     c                   seton                                        28  90
048800071030     c                   eval      V1Dmsg  = $Msg(2)
048900070801e   2c                   endif
049000071030     c     k02acr13      setll     FNACR100
049100151012      /free
049200151012         chain  %kds( keyFNACRE1 )  FNACRE00;
049300151012      /end-free
049400070802      *
049500070801      * - Se richiesta visualizzazione/copia
049600070801      *   il record da visualizzare NON va allocato
049700070801w   1c                   WHEN           I37opz = '3'
049800070801     c                             or   I37opz = '5'
049900070809     c                             or   I37opz = 'C'
050000070801     c     I37cro        chain(n)  FNACR000
050100070801if  2c                   if        NOT  %found(FNACR01L)
050200070801     c                   seton                                        28  90
050300071030     c                   eval      V1Dmsg  = $Msg(2)
050400070801e   2c                   endif
050500071030     c     k02acr13      setll     FNACR100
050600151012      /free
050700151012         chain(N)  %kds( keyFNACRE1 )  FNACRE00;
050800151012      /end-free
050900070802      *
051000070801e   1c                   ENDSL
051100070801      *
051200070809      * Eventuali forzature
051300070802if  1c                   if             %found(FNACR01L)
051400071030     c                             and  I37opz  = '2'
051500070801     c                             and  I37fgs <> *zeros
051600150909     c                             and  I37fgs <>
051700150909     c                             %dec(%subst(%editc(ACRcro:'X'):1:3):3:0)
051800071030     c                   eval      I37opz  = '5'
051900070802e   1c                   endif
052000070801      *
052100070801      * Impostazione campi chiave a video
052200070809sel 1c                   select
052300070809w   1c                   when           I37opz  = '1'
052400071030     c                   eval      V2Ccd1    = I37fgs
052500071030     c                   eval      V2Cfcc    = 'S'
052600150909     c                   eval      V2Cpoa    = I37fgs
052700070809w   1c                   when           I37opz  = 'A'
052800070809     c                   movel     I37cro        ds_CRO
052900071030     c                   eval      V2Ccd1    = dsCRO1
053000071030     c                   eval      V2Ccd2    = dsCRO2
053100071030     c                   eval      V2Ccd3    = dsCRO3
053200071030     c                   eval      V2Cksc    = I37ksc
053300071030     c                   eval      V2Cfcc    = 'S'
053400150909     c                   eval      V2Cpoa    = V2Ccd1
053500070809x   1c                   other
053600150910     c                   movel     I37cro        ds_CRO
053700071030     c                   eval      V2Ccd1    = dsCRO1
053800071030     c                   eval      V2Ccd2    = dsCRO2
053900071030     c                   eval      V2Ccd3    = dsCRO3
054000070809e   1c                   endsl
054100070801      *
054200070801      * Impostazione altri campi a video
054300070802if  1c                   IF            (I37opz = '2'
054400070801     c                             or   I37opz = '3'
054500070801     c                             or   I37opz = '4'
054600070801     c                             or   I37opz = '5'
054700070809     c                             or   I37opz = '7'
054800070809     c                             or   I37opz = 'C')
054900070801     c                             and  %found(FNACR01L)
055000071030     c                   eval      V2Crsr    = ACRrsr
055100071030     c                   eval      V2Cinr    = ACRinr
055200071030     c                   eval      V2Ccar    = ACRcar
055300071030     c                   eval      V2Ccar    = ACRcar
055400071030     c                   eval      V2Clor    = ACRlor
055500071030     c                   eval      V2Cprr    = ACRprr
055600071030     c                   eval      V2Cnar    = ACRnar
055700071030if  2c                   IF        %equal(FNACR13L)
055800071030if  3c                   if             I37opz = '3'
055900071030     c                             or   I37opz = '5'
056000071030     c                             or   I37opz = 'C'
056100071030     c     k02acr13      reade(n)  FNACR100
056200071030x   3c                   else
056300071030     c     k02acr13      reade     FNACR100
056400071030e   3c                   endif
056500071030do  3c                   DoW       NOT  %eof(FNACR13L)
056600071030sel 4c                   select
056700071030w   4c                   when      ACR1atb  <> ACRatb
056800071030w   4c                   when      ACR1ain   = '='
056900071030     c                   eval      V2Cpocgi1 = ACR1pocgi
057000071030     c                   eval      V2Ccgi1   = ACR1cgi
057100071030w   4c                   when      ACR1ain   = '>'
057200071030     c                   eval      V2Cpocgi2 = ACR1pocgi
057300071030     c                   eval      V2Ccgi2   = ACR1cgi
057400071030w   4c                   when      ACR1ain   = '<'
057500071030     c                   eval      V2Cpocgi3 = ACR1pocgi
057600071030     c                   eval      V2Ccgi3   = ACR1cgi
057700071030e   4c                   endsl
057800071030if  4c                   if             I37opz = '3'
057900071030     c                             or   I37opz = '5'
058000071030     c                             or   I37opz = 'C'
058100071030     c     k02acr13      reade(n)  FNACR100
058200071030x   4c                   else
058300071030     c     k02acr13      reade     FNACR100
058400071030e   4c                   endif
058500071030e   3c                   EndDo
058600071030e   2c                   ENDIF
058700070801     c                   movel     ACRcco        ds_CRO
058800071030     c                   eval      V2Cco1    = dsCRO1
058900071030     c                   eval      V2Cco2    = dsCRO2
059000071030     c                   eval      V2Cco3    = dsCRO3
059100071030     c                   eval      V2Cksc    = ACRksc
059200091009if  2c                   if        ACRccc   = 999
059300091009     c                   clear                   v2cctr
059400091009     c                   else
059500091009     c                   move      ACRccc        V2Cctr
059600070801e   2c                   endif
059700071030     c                   eval      V2Ctcr    = ACRtcr
059800071030if  2c                   if        ACRfcc    = 'N'
059900071030     c                   eval      V2Cfcc    = ACRfcc
060000070802x   2c                   else
060100071030     c                   eval      V2Cfcc    = 'S'
060200070802e   2c                   endif
060300071030     c                   eval      V2Corr    = ACRorr
060400071030     c                   eval      V2Cper    = ACRper
060500071030     c                   eval      V2Ctel    = ACRtel
060600131113     c                   eval      dACR01    = ACRmai
060700140114     c                   eval      sav_§ACRstcr = dACR01.§ACRstcr
060800140114     c                   eval      sav_§ACRctcr = dACR01.§ACrctcr
060900131113      * - Valorizzazione degli orari solo SE effettivamente numerici
061000131113     c                   if        %check( c_Digits : %subst(
061100131113     c                                     ACRmai : 1
061200131113     c                                            : %len(dACR01.§ACRoa1) )
061300131113     c                                   ) = *zero
061400131113     c                   eval      V2ChmAmDa = %int( dACR01.§ACRoa1 )
061500131113     c                   endif
061600131113     c                   if        %check( c_Digits : %subst(
061700131113     c                                     ACRmai : %len(dACR01.§ACRoa1) + 1
061800131113     c                                            : %len(dACR01.§ACRoa2) )
061900131113     c                                   ) = *zero
062000131113     c                   eval      V2ChmAmA  = %int( dACR01.§ACRoa2 )
062100131113     c                   endif
062200131113     c                   if        %check( c_Digits : %subst(
062300131113     c                                     ACRmai : %len(dACR01.§ACRoa1) +
062400131113     c                                              %len(dACR01.§ACRoa2) + 1
062500131113     c                                            : %len(dACR01.§ACRoa3) )
062600131113     c                                   ) = *zero
062700131113     c                   eval      V2ChmPmDa = %int( dACR01.§ACRoa3 )
062800131113     c                   endif
062900131113     c                   if        %check( c_Digits : %subst(
063000131113     c                                     ACRmai : %len(dACR01.§ACRoa1) +
063100131113     c                                              %len(dACR01.§ACRoa2) +
063200131113     c                                              %len(dACR01.§ACRoa3) + 1
063300131113     c                                            : %len(dACR01.§ACRoa4) )
063400131113     c                                   ) = *zero
063500131113     c                   eval      V2ChmPmA  = %int( dACR01.§ACRoa4 )
063600131113     c                   endif
063700071030     c                   eval      V2Cfcl    = ACRfcl
063800071030     c                   eval      V2Cfbn    = ACRfbn
063900071030     c                   eval      V2Cfpk    = ACRfpk
064000071030     c                   eval      V2Cfmc    = ACRfmc
064100071030     c                   eval      V2Cfbl    = ACRfbl
064200071030     c                   eval      V2Cfmt    = ACRfmt
064300071030     c                   eval      V2Cfat    = ACRfat
064400071030     c                   eval      V2Cspi    = ACRspi
064500071030     c                   eval      V2Cntm    = ACRntm
064600071030     c                   eval      V2Cno1    = ACRno1
064700071030     c                   eval      V2Cno2    = ACRno2
064800140113      /free
064900140113       //?Se frequenza ritiro impostata in automatico da pgm contabili
065000140113       //?non posso toccarla quindi la proteggo
065100140113         IF  dACR01.§ACRctcr = 'C';
065200140113           *in40 = *on;
065300140113         ENDIF;
065400140113       //?Imposto la frequenza ritiro salvata
065500140113         V2Cstcr = dACR01.§ACRstcr;
065600150909       //?Filiale ritiro forzata
065700150909         V2Cpoa = ACRpoa;
065800151104       // -?E-Mail Conferma Accettazione ORM + flag?
065900151104         V2Cfca = dacr01.§ACRfmar;
066000151012         V2Cmca = ACREdati;
066100140113      /end-free
066200140113
066300070802e   1c                   ENDIF
066400070801      *
066500070801      * Abilitazione tasti funzionali:
066600070801      * - F2=Anagr.Fissi
066700070802     c                   eval      *in02   = *on
066800121130      /free
066900121130         //?Se anagrafica ritiri fissi già richiamata non abilito F2
067000121130           IF  wGiaChiamato;
067100121130             *in02 = *off;
067200121130           ENDIF;
067300121130      /end-free
067400070801      * - F3=Fine
067500070809     c                   eval      *in03   = (I37opz <> 'A'    and
067600070809     c                                        I37opz <> 'C')
067700070801      * - F5=Riattivazione
067800070809     c                   eval      *in05   = (I37opz  = '2'    and
067900070801     c                                        %found(FNACR01L) and
068000070801     c                                        ACRatb <> *blanks)
068100070801      * - F6=Conferma
068200070809     c                   eval      *in06   = (I37opz <> '5')
068300070828      * - F7=Interrogazione Giri di Ritiro
068400070828     c                   eval      *in07   = (I37opz <> '4'    and
068500070828     c                                        I37opz <> '5')
068600070801      * - F16=Annullamento
068700070809     c                   eval      *in16   = (I37opz  = '2'    and
068800070801     c                                        %found(FNACR01L) and
068900140723     c                                        ACRatb  = *blanks and
069000140807     c                                        (V2Ccd3 > 0 or
069100140807     c                                        (V2Ccd3 = 0 and
069200140807     c                                        (V2Ccd2 = 8888 or
069300140807     c                                         V2Ccd2 = 9999))))
069400070809      * - F12=Ritorno
069500070809     c                   eval      *in12   = (I37opz <> 'A'    and
069600070809     c                                        I37opz <> 'C')
069700070801      *
069800070801      * Protezione campo chiave a video
069900070802     c                   eval      *in41   = (I37opz <> '1'    and
070000070809     c                                        I37opz <> '3'    and
070100070809     c                                        I37opz <> 'A'    and
070200070809     c                                        I37opz <> 'C')
070300070802      * Protezione codice P.d.C.  e Tariffa
070400070801     c                   eval      *in42   = (I37ksc <> *zeros)
070500070802      * Evidenziazione record annullato
070600070801     c                   eval      *in43   = (ACRatb <> *blanks)
070700070802      *
070800070802      * Impostazione F5=Annullamento o F16=Ripristino
070900070802sel 1c                   select
071000070802w   1c                   when      *in05
071100070802     c                   eval      Z1Df16  = 'F5=Riattivazione'
071200070802w   1c                   when      *in16
071300070802     c                   eval      Z1Df16  = 'F16=Annullamento'
071400070802e   1c                   endsl
071500070911      *
071600070911      * Decodifiche
071700070911if  1c                   if             %found(FNACR01L)
071800070911     c                             or   I37opz  = 'A'
071900110121     c                   eval      wdecod='S'
072000070911     c                   exsr      CtrD01
072100110121     c                   eval      wdecod=' '
072200070911     c                   setoff                                       28  90
072300070911     c                   clear                   V1Dmsg
072400070911     c                   movea     *zeros        *in(50)
072500070911e   1c                   endif
072600070801      *
072700070801     c                   ENDSR
072800070801
072900070801      *---------------------------------------------------------------*
073000070801      *?Gestione tasto funzionale F2 da videata D01                  ?*
073100070801      *?F2=Gestione anagrafica ritiri fissi                          ?*
073200070801      *---------------------------------------------------------------*
073300070801     c     F02D01        BEGSR
073400070801      *
073500070801     c                   clear                   FIOR02ds
073600070801if  1c                   if        I37opz  = '5'
073700070801     c                   eval      I02ric  = '1'
073800070801x   1c                   else
073900070801     c                   eval      I02ric  = '2'
074000070801e   1c                   endif
074100070801     c                   eval      I02cro  = V2Ccd1 * 10000000
074200070801     c                                     + V2Ccd2 * 1000
074300070801     c                                     + V2Ccd3
074400070801     c                   eval      I02poa  = I37fgs
074500070801     c                   call      'FIOR02R'
074600070801     c                   parm                    KPJBA
074700070801     c                   parm                    FIOR02ds
074800070801      *
074900070828if  1c                   if        O02err <> *blanks
075000070801     c                   seton                                        28  90
075100070801     c                   eval      V1Dmsg  = O02msg
075200070801e   1c                   endif
075300070801      *
075400070801     c                   ENDSR
075500070801
075600070801      *---------------------------------------------------------------*
075700070801      *?Gestione tasto funzionale F3 da videata D01                  ?*
075800070801      *?F3=Fine                                                      ?*
075900070801      *---------------------------------------------------------------*
076000070801     c     F03D01        BEGSR
076100070801      *
076200070801      * Disallocazione del record
076300070801if  1c                   if             I37opz = '2'
076400070801     c                             or   I37opz = '4'
076500070801     c                             or   I37opz = '7'
076600070801     c                   UNLOCK    FNACR01L
076700071030     c                   UNLOCK    FNACR13L
076800151012     c                   UNLOCK    FNACRE1L
076900070801e   1c                   endif
077000070802      *
077100070803      * Impostazione avviso di ritorno con F3
077200070803     c                   eval      O37ret  = *on
077300070801      *
077400070801      * Chiusura del programma
077500070801     c                   eval      $Fine   = *on
077600070801      *
077700070801     c                   ENDSR
077800070801
077900070801      *---------------------------------------------------------------*
078000070801      *?Gestione tasto funzionale F6 da videata D01                  ?*
078100070801      *?F6=Conferma                                                  ?*
078200070801      *---------------------------------------------------------------*
078300070801     c     F06D01        BEGSR
078400070801      *
078500070801      * Aggiornamento record
078600070801     c                   exsr      Upd_FNACR
078700070801      *
078800070801      * Chiusura del programma
078900070801if  1c                   if        NOT  *in90
079000070910if  2c                   if             I37opz = '1'
079100070910     c                             or   I37opz = '3'
079200070910     c                             or   I37opz = 'A'
079300070910     c                             or   I37opz = 'C'
079400070910     c                   eval      O37cro  = I37cro
079500070910e   2c                   endif
079600070801     c                   eval      $Fine   = *on
079700070801e   1c                   endif
079800070801      *
079900070801     c                   ENDSR
080000070801
080100070801      *---------------------------------------------------------------*
080200070801      *?Gestione tasto funzionale F12 da videata D01                 ?*
080300070801      *?F12=Ritorno                                                  ?*
080400070801      *---------------------------------------------------------------*
080500070801     c     F12D01        BEGSR
080600070801      *
080700070801      * Disallocazione del record
080800070801if  1c                   if             I37opz = '2'
080900070801     c                             or   I37opz = '4'
081000070801     c                             or   I37opz = '7'
081100070801     c                   UNLOCK    FNACR01L
081200071030     c                   UNLOCK    FNACR13L
081300151012     c                   UNLOCK    FNACRE1L
081400070801e   1c                   endif
081500070803      *
081600070803      * Impostazione avviso di ritorno con F12
081700070803     c                   eval      O37ret  = '2'
081800070801      *
081900070801      * Chiusura del programma
082000070801     c                   eval      $Fine   = *on
082100070801      *
082200070801     c                   ENDSR
082300070911
082400070911      *---------------------------------------------------------------*
082500070911      *?Controllo e decodifica codice giro di ritiro                 ?*
082600070911      *---------------------------------------------------------------*
082700070911     c     Ctr_Giro      BEGSR
082800071030      *
082900071030      * - immessa solo la filiale (senza codice giro)
083000071030if  1c                   if             Wpocgi  <> *zeros
083100071030     c                             and  Wcgi     = *blanks
083200071105     c                   clear                   Wpocgi
083300071105     c                   clear                   WcgiD
083400071030e   1c                   endif
083500071030      *
083600071030      * - immesso solo il codice giro (senza filiale)
083700071030if  1c                   if             Wpocgi   = *zeros
083800071030     c                             and  Wcgi    <> *blanks
083900071105     c                   eval      Wpocgi   =  I37fgs
084000071030e   1c                   endif
084100071030      *
084200071030      * - controllo codice filiale del giro ritiro
084300071105      *   (filiale giro diversa dalla filiale gestione)
084400071105if  1c                   if             Wpocgi  <> I37fgs
084500071105     c                             and  Wcgi    <> *blanks
084600071105     c                   seton                                        286090
084700071030     c                   eval      V1Dmsg  = $Msg(18)
084800071030     c                   leavesr
084900071030e   1c                   endif
085000070911      *
085100071030      * - controllo giro CON data validità ("bypassabile")
085200071031if  1c                   IF             Wpocgi  <> D09ifgs
085300071031     c                             or   Wcgi    <> D09icgi
085400070911     c                   clear                   FIDG09ds
085500070911     c                   eval      D09iop0   = '001'
085600071030     c                   eval      D09ifgs   = Wpocgi
085700071030     c                   eval      D09icgi   = Wcgi
085800070911     c                   eval      D09idat   = *date
085900070911     c                   eval      D09itug   = 'R'
086000070911     c                   movel(p)  FIDG09ds      KPJBU
086100070911     c                   call      'FIDG09R'
086200070911     c                   parm                    KPJBA
086300070911     c                   movel     KPJBU         FIDG09ds
086400071105e   1c                   ENDIF
086500071105      *
086600071105sel 1c                   select
086700071105w   1c                   when      D09oerr   = *on
086800071105     c                   movel     D09odes       WcgiD
086900070911     c                   seton                                        286090
087000070911     c                   eval      V1Dmsg    = $Msg(29)
087100071112     c                   leavesr
087200071105w   1c                   when      D09ocgi  <> *blanks
087300071030     c                   movel     D09odes       WcgiD
087400071105e   1c                   endsl
087500070911      *
087600070911     c                   ENDSR
087700070801
087800070801      *---------------------------------------------------------------*
087900070801      *?Controllo videata D01                                        ?*
088000070801      *---------------------------------------------------------------*
088100070801     c     CtrD01        BEGSR
088200070801      *
088300070806     c                   movea     *zeros        *in(50)
088400070806      *
088500150909     c                   clear                   V2Dpoa
088600070802     c                   clear                   ds_CRO
088700070802     c                   eval      dsCRO1  = V2Ccd1
088800070802     c                   eval      dsCRO2  = V2Ccd2
088900070802     c                   eval      dsCRO3  = V2Ccd3
089000070802      *
089100070802      * Codice cliente ritiro
089200070802      * - Se IMMISSIONE (NON COPIA):
089300070802      *   i primi 7 caratteri del codice devono esistere in CNACO
089400070802if  1c                   if             I37opz  = '1'
089500070809     c                             or   I37opz  = 'A'
089600070802     c                   clear                   TIBS69ds
089700070802     c                   moveL     ds_CRO        I69kac
089800070809     c                   if            (I37opz  = '1'
089900070809     c                              or  I37opz  = 'A')
090000070809     c                             and  $InzD01 = *on
090100070809     c                   moveL     ds_CRO        I69kin
090200070809     c                   endif
090300070802     c                   exsr      Call_TIBS69
090400070809sel 2c                   select
090500070809w   2c                   when      *in90
090600070802     c                   eval      *in51   = *on
090700070802     c                   eval      V1Dmsg  = $Msg(7)
090800070802     c                   leavesr
090900070809x   2c                   when          (I37opz  = '1'
091000070809     c                              or  I37opz  = 'A')
091100070809     c                             and  $InzD01 = *on
091200070809     c                   movel     ACOrag        V2Crsr
091300070809     c                   movel     INDvia        V2Cinr
091400070809     c                   movel     INDcae        V2Ccar
091500070809     c                   movel     INDcit        V2Clor
091600070809     c                   movel     INDprv        V2Cprr
091700070809     c                   movel     INDsta        V2Cnar
091800080421     c                   movel     INDtel        V2Ctel
091900070809e   2c                   endsl
092000070802e   1c                   endif
092100070802      * - Se IMMISSIONE O COPIA:
092200070802if  1c                   if             I37opz  = '1'
092300070802     c                             or   I37opz  = '3'
092400070809     c                             or   I37opz  = 'A'
092500070809     c                             or   I37opz  = 'C'
092600070802      * - - controllo completo inserimento
092700070802if  2c                   IF        V2Ccd2  = *zeros
092800070802     c                   seton                                        285190
092900070802     c                   eval      V1Dmsg  = $Msg(8)
093000070802     c                   leavesr
093100070802      * - - controllo se già esistente
093200070802x   2C                   ELSE
093300070802     c                   move      ds_CRO        ACRcro
093400070802     c     ACRcro        chain(n)  FNACR000
093500070802if  3c                   if        %found(FNACR01L)
093600070802     c                   seton                                        285190
093700070802     c                   eval      V1Dmsg  = $Msg(9)
093800070802     c                   leavesr
093900070802e   3c                   endif
094000070802e   2c                   ENDIF
094100140723      /free
094200140723       //?Se NON esiste il luogo '000' devo per forza creare quello
094300140723         IF  dscro3 <> 000;
094400140723           clear ACRcro;
094500140806           ACRcro = (dscro1 * 10000000) + (dscro2 * 1000);
094600140723           chain ACRcro FNACR01L;
094700140723           IF  not %found(FNACR01L);
094800140723             *in28 = *on;
094900140723             *in51 = *on;
095000140723             *in90 = *on;
095100140723             V1Dmsg = $msg(32);
095200140723             leavesr;
095300140723           ENDIF;
095400140723         ENDIF;
095500140723      /end-free
095600070802e   1c                   endif
095700070802      *
095800070802      * Ragione sociale, Indirizzo, Località, CAP, Prov., Naz.
095900110121     c                   if        wdecod=' '
096000070802     c                   clear                   FNLV14ds
096100070802     c                   eval      I14dri  = *date
096200070802     c                   movel     V2Crsr        I14rsc
096300070802     c                   movel     V2Cinr        I14ind
096400070802     c                   movel     V2Clor        E14loc
096500070802     c                   movel     V2Cprr        E14prv
096600070802     c                   movel     V2Ccar        E14cap
096700070802     c                   movel     V2Cnar        E14nar
096800070802     c                   call      'FNLV14R'
096900070802     c                   parm                    KPJBA
097000070802     c                   parm                    FNLV14ds
097100070802     c                   movel     I14ind        V2Cinr
097200070802     c                   movel     E14loc        V2Clor
097300070802     c                   movel     E14prv        V2Cprr
097400070802     c                   movel     E14cap        V2Ccar
097500070802     c                   movel     E14nar        V2Cnar
097600070802if  1c                   if        O14msg <> *blanks
097700070802     c                   eval      *in53   = (O14err = '1')                     Ragione Sociale
097800070802     c                   eval      *in54   = (O14err = '2')                     Indirizzo
097900070802     c                   eval      *in55   = (O14err = '5')                     CAP
098000070802     c                   eval      *in56   = (O14err = '3')                     Località
098100070802     c                   eval      *in57   = (O14err = '4')                     Provincia
098200070802     c                   eval      *in58   = (O14err = '6')                     Nazione
098300070802     c                   seton                                        28  90
098400070802     c                   eval      V1Dmsg  = O14msg
098500070802     c                   leavesr
098600070802e   1c                   endif
098700070802      * - controllo specifico CAP
098800070802     c                   exsr      Ctr_CAP
098900070802if  1c                   if        *in90
099000070802     c                   leavesr
099100070802e   1c                   endif
099200110121     c                   endif
099300070801      *
099400070802      * Giro di ritiro
099500071105do  1c                   DO        C_MaxCGI      wNrCGI
099600071030      *
099700071031     c                   clear                   WcgiD
099800071030sel 2c                   select
099900071030w   2c                   when      wNrCGI  = 1
100000071030     c                   eval      Wpocgi  = V2Cpocgi1
100100071030     c                   eval      Wcgi    = V2Ccgi1
100200071030w   2c                   when      wNrCGI  = 2
100300071030     c                   eval      Wpocgi  = V2Cpocgi2
100400071030     c                   eval      Wcgi    = V2Ccgi2
100500071030w   2c                   when      wNrCGI  = 3
100600071030     c                   eval      Wpocgi  = V2Cpocgi3
100700071030     c                   eval      Wcgi    = V2Ccgi3
100800071030e   2c                   endsl
100900070911      *
101000071030      * - controllo codici giri ritiro
101100071031sel 2c                   Select
101200071105w   2c                   When      Wcgi    = *blanks
101300071105     c                   clear                   Wpocgi
101400071031      *   - interrogazione giri di ritiro
101500071031w   2c                   When      %scan('?' : %trimr(Wcgi)) > *zeros
101600071031     c                   exsr      Search_Giro
101700071030      *   - controllo codici giri ritiro
101800071031x   2c                   Other
101900070911     c                   exsr      Ctr_Giro
102000071031e   2c                   EndSl
102100071031      *
102200071031sel 2c                   Select
102300071031w   2c                   When      wNrCGI  = 1
102400071030     c                   eval      V2Cpocgi1 = Wpocgi
102500071031     c                   eval      V2Ccgi1   = Wcgi
102600071030     c                   eval      V2Dcgi1   = WcgiD
102700071031w   2c                   When      wNrCGI  = 2
102800071030     c                   eval      V2Cpocgi2 = Wpocgi
102900071031     c                   eval      V2Ccgi2   = Wcgi
103000071030     c                   eval      V2Dcgi2   = WcgiD
103100071105     c                   eval      *in76   = (*in59 = *on)
103200071030     c                   eval      *in77   = (*in60 = *on)
103300071030     c                   eval      *in59   = *off
103400071030     c                   eval      *in60   = *off
103500071031w   2c                   When      wNrCGI  = 3
103600071030     c                   eval      V2Cpocgi3 = Wpocgi
103700071031     c                   eval      V2Ccgi3   = Wcgi
103800071030     c                   eval      V2Dcgi3   = WcgiD
103900071105     c                   eval      *in78   = (*in59 = *on)
104000071030     c                   eval      *in79   = (*in60 = *on)
104100071030     c                   eval      *in59   = *off
104200071030     c                   eval      *in60   = *off
104300071031e   2c                   EndSl
104400071031if  2c                   if        *in90
104500070911     c                   leavesr
104600071031e   2c                   endif
104700071030      *
104800071030e   1c                   ENDDO
104900070802      *
105000070802      * Codice cliente Ordinante
105100070802     c                   clear                   V2Ddeo
105200070802if  1c                   IF             V2Cco1 <> *zeros
105300070802     c                             or   V2Cco2 <> *zeros
105400070802     c                             or   V2Cco3 <> *zeros
105500070802      * - Tipo anagrafica "R" = cliente Ritiro
105600070809     c                   eval      ACRcro  = V2Cco1 * 10000000
105700070809     c                                     + V2Cco2 * 1000
105800070809     c                                     + V2Cco3
105900070802     c     ACRcro        chain(n)  FNACR000
106000071030if  2c                   if             %found(FNACR01L)
106100070802     c                             and  ACRatb  = *blanks
106200070802     c                   movel     ACRrsr        V2Ddeo
106300071030x   2c                   else
106400070802     c                   seton                                        286190
106500070802     c                   eval      V1Dmsg  = $Msg(20)
106600070802     c                   leavesr
106700071030e   2c                   endif
106800071030if  2c                   if             V2Cco1  = V2Ccd1
106900070802     c                             and  V2Cco2  = V2Ccd2
107000070802     c                             and  V2Cco3  = V2Ccd3
107100070802     c                   seton                                        286190
107200070802     c                   eval      V1Dmsg  = $Msg(20)
107300070802     c                   leavesr
107400071030e   2c                   endif
107500070802      *
107600070802e   1c                   ENDIF
107700070802      *
107800070802      * Codice P.d.C.
107900070802     c                   clear                   V2Dksc
108000070802if  1c                   IF        V2Cksc <> *zeros
108100070802     c                   clear                   TIBS69ds
108200070802     c                   eval      I69kac  = V2Cksc
108300070802     c                   exsr      Call_TIBS69
108400070802if  2c                   if        *in90
108500070802     c                   seton                                        286290
108600070802     c                   eval      V1Dmsg  = $Msg(22)
108700070802     c                   leavesr
108800070802e   2c                   endif
108900070802     c                   clear                   Og143
109000070802     c                   moveL     V2Cksc        ORGfil
109100070802     c     ORGfil        chain     AZORG
109200070802if  2c                   if        %found(AZORG01L)
109300070802     c                   eval      Og143   = ORGde3
109400070802e   2c                   endif
109500070802if  2c                   if             §OGntw  = 'LOG'
109600070802     c                             or   §OGntw  = 'XXX'
109700070802     c                   seton                                        286290
109800070802     c                   eval      V1Dmsg  = $Msg(22)
109900070802     c                   leavesr
110000070802e   2c                   endif
110100070802     c                   movel     ACOrag        V2Dksc
110200070802e   1c                   ENDIF
110300070802      *
110400070802      * Tariffa
110500091009if  1c                   IF        V2Cctr <> *blanks
110600070802if  2c                   if        V2Cksc  = *zeros
110700070802     c                   seton                                        286390
110800070802     c                   eval      V1Dmsg  = $Msg(23)
110900070802     c                   leavesr
111000070802e   2c                   endif
111100070802      * - verifica esistenza tariffe per il cliente
111200070802     c                   eval      $Tam    = *off
111300070802     c                   eval      TAMksc  = V2Cksc
111400091009     c                   move      v2cctr        TAMctr
111500070802     c     k02tam01      setgt     TNTAM000
111600070802     c     k02tam01      readpe    TNTAM000
111700070802do  1c                   dow       NOT  %eof(TNTAM01L)
111800070802if  2c                   if        TAMatb  = *blanks
111900070802     c                   eval      $Tam    = *on
112000070802     c                   leave
112100070802e   2c                   endif
112200070802     c     k02tam01      readpe    TNTAM000
112300070802e   1c                   enddo
112400070802if  1c                   if        $Tam    = *off
112500070802     c                   seton                                        286390
112600070802     c                   eval      V1Dmsg  = $Msg(24)
112700070802     c                   leavesr
112800070802e   1c                   endif
112900070802e   1c                   ENDIF
113000070802      *
113100070802      * Frequenza Ritiro
113200070802      * - F = Fisso
113300070802      * - R = Ricorrente
113400151104      * - O = No Internet
113500070802      * - M = Mai
113600151104      /free
113700151104        IF  %scan('?':V2Ctcr) > 0;
113800151104          FineTcr = *off;
113900151104          clear savfreq;
114000151104          exsr RicTcr;
114100151104          V2Ctcr = savfreq;
114200151104        ENDIF;
114300151104      /end-free
114400070802if  1c                   if             V2Ctcr <> 'F'
114500070802     c                             and  V2Ctcr <> 'R'
114600070802     c                             and  V2Ctcr <> 'O'
114700070802     c                             and  V2Ctcr <> 'M'
114800070802     c                   seton                                        286490
114900070802     c                   eval      V1Dmsg  = $Msg(25)
115000070802     c                   leavesr
115100070802e   1c                   endif
115200150909
115300150909      /free
115400150909       //?Filiale ritiro forzata
115500150909         IF  V2Cpoa = 0;
115600150909           V1Dmsg = $msg(33);
115700150909           *in28 = *on;
115800150909           *in52 = *on;
115900150909           *in90 = *on;
116000150909           leavesr;
116100150909         ENDIF;
116200150909         chain (V2Cpoa) AZORG01L;
116300150909         IF  not %found(AZORG01L) or ORGfva <> *blanks;
116400150909           V1Dmsg = $msg(33);
116500150909           *in28 = *on;
116600150909           *in52 = *on;
116700150909           *in90 = *on;
116800150909           leavesr;
116900150909         ENDIF;
117000150909         V2Dpoa = ORGdes;
117100150909      /end-free
117200070802      *
117300070802      * Ora ritiro
117400070802if  1c                   if        V2Corr <> *zeros
117500070802     c                   movel     V2Corr        ds_Time
117600070802if  2c                   if             dsHH    < 1
117700070802     c                             or   dsHH    > 24
117800070802     c                             or   dsMM    > 59
117900070802     c                   seton                                        286590
118000070802     c                   eval      V1Dmsg  = $Msg(26)
118100070802     c                   leavesr
118200070802e   2c                   endif
118300070802e   1c                   endif
118400070802      *
118500070802      * Referente: numero di telefono
118600070802if  1c                   IF        V2Ctel <> *blanks
118700070802     c                   clear                   TRUL42ds
118800070802     c                   eval      D42fax  = V2Ctel
118900070802     c                   call      'TRUL42R'
119000070802     c                   parm                    TRUL42ds
119100070802if  2c                   if        D42err  = *on
119200070802     c                   seton                                        286790
119300070802     c                   eval      V1Dmsg  = D42msg
119400070802     c                   leavesr
119500070802e   2c                   endif
119600070802e   1c                   ENDIF
119700070802      *
119800131113      * Orari Apertura
119900131113     c                   exsr      CtrlOrariApert
120000131113     c                   if        *in28
120100131113     c                   leavesr
120200131113     c                   endif
120300070802      *
120400070802      * Flags immissione
120500070802     c                   select
120600070802      * - Colli
120700070802     c                   when           V2Cfcl <> *blanks
120800070802     c                             and  V2Cfcl <> 'F'
120900070802     c                             and  V2Cfcl <> 'O'
121000070802     c                   seton                                        286990
121100070802      * - Bancali
121200070802     c                   when           V2Cfbn <> *blanks
121300070802     c                             and  V2Cfbn <> 'F'
121400070802     c                             and  V2Cfbn <> 'O'
121500070802     c                   seton                                        287090
121600070802      * - Peso   (kg)
121700070802     c                   when           V2Cfpk <> *blanks
121800070802     c                             and  V2Cfpk <> 'F'
121900070802     c                             and  V2Cfpk <> 'O'
122000070802     c                   seton                                        287190
122100070802      * - Volume (m³)
122200070802     c                   when           V2Cfmc <> *blanks
122300070802     c                             and  V2Cfmc <> 'F'
122400070802     c                             and  V2Cfmc <> 'O'
122500070802     c                   seton                                        287290
122600070802      * - Bilico
122700070802     c                   when           V2Cfbl <> *blanks
122800070802     c                             and  V2Cfbl <> 'F'
122900070802     c                             and  V2Cfbl <> 'O'
123000070802     c                   seton                                        287390
123100070802      * - Motrice
123200070802     c                   when           V2Cfmt <> *blanks
123300070802     c                             and  V2Cfmt <> 'F'
123400070802     c                             and  V2Cfmt <> 'O'
123500070802     c                   seton                                        287490
123600070802      * - Autotreno
123700070802     c                   when           V2Cfat <> *blanks
123800070802     c                             and  V2Cfat <> 'F'
123900070802     c                             and  V2Cfat <> 'O'
124000070802     c                   seton                                        287590
124100070802     c                   endsl
124200070802     c                   if        *in28  and  *in90
124300070802     c                   eval      V1Dmsg  = $Msg(27)
124400070802     c                   leavesr
124500070802     c                   endif
124600070802      *
124700070802      * Se COPIA   &   codice piano dei conti non cambiato
124800070802      * => messaggio d'avvertimento
124900070809if  1c                   if            (I37opz  = '3'
125000070809     c                              or  I37opz  = 'C')
125100070910     c                             and (ACRksc <> *zeros
125200070910     c                             and  V2Cksc  = ACRksc
125300070910     c                             and  V2Cksc <> SAVksc)
125400070809     c                   eval      SAVksc  = V2Cksc
125500070802     c                   seton                                        286290
125600070802     c                   eval      V1Dmsg  = $Msg(28)
125700070802     c                   leavesr
125800070809e   1c                   endif
125900151012      *
126000151012      /free
126100151012
126200151012         // -?Controllo e-Mail Conferma Accettazione?
126300151012         If  V2Cmca <> *blank;
126400151012           clear dsEmail;
126500151012           §emlIndI = V2Cmca;
126600151012           xEmail( dsEmail );
126700151012           if §emlErrO = *on;
126800151012             *in66  = *on;
126900151012             *in28  = *on;
127000151012             *in90  = *on;
127100151012             V1Dmsg = §emlMsgO;
127200151012             leavesr;
127300151012           endif;
127400151012           V2Cmca = §emlIndO;
127500151012         EndIf;
127600151012
127700151012      /end-free
127800070801      *
127900070801     c                   ENDSR
128000131113
128100131113      /free
128200131113
128300131113       //--------------------------------------------------------------
128400131113       //?Controllo orari di apertura.                                 ?
128500131113       //--------------------------------------------------------------
128600131113       BEGSR  CtrlOrariApert;
128700131113
128800131113         // -?Richiamo *pgm per controllo orari di apertura del cliente:?
128900131113         //  ?si deve anche controllare che ci sia abbastanza spazio?
129000131113         //  ?nella Nota-1 e nella Nota-2 per poterli inserire in questi?
129100131113         //  ?campi per poi stamparli?
129200131113         If  (V2ChmAMda + V2ChmAMa + V2ChmPMda + V2ChmPMa) > *zero;
129300131113
129400131113           clear  TRUL03ds;
129500131113           TRUL03ds.i03hm1 = V2ChmAMda;
129600131113           TRUL03ds.i03hm2 = V2ChmAMa;
129700131113           TRUL03ds.i03hm3 = V2ChmPMda;
129800131113           TRUL03ds.i03hm4 = V2ChmPMa;
129900131113           TRUL03ds.i03nt1 = V2Cno1;
130000131113           TRUL03ds.i03nt2 = V2Cno2;
130100131113
130200131113           trul03r ( TRUL03ds );
130300131113
130400131113           If  TRUL03ds.o03err <> *off;
130500131113             V1Dmsg = TRUL03ds.o03msg;
130600131113             *in28 = *on;
130700131113             *in(79 + TRUL03ds.o03errpos) = *on;
130800131113           EndIf;
130900131113
131000131113         EndIf;
131100131113
131200131113       ENDSR;
131300151104
131400151104       //--------------------------------------------------------------
131500151104       //?Ricerca Frequenza Ritiro.                                    ?
131600151104       //--------------------------------------------------------------
131700151104       BEGSR RicTcr;
131800151104
131900151104       //?Inizializzazione videata
132000151104         exsr InzS02;
132100151104
132200151104       //?Visualizzazione del SFL
132300151104         *in30 = (S02nrr <= *zeros);
132400151104
132500151104         DOW not FineTcr;
132600151104
132700151104         //?Emissione videata
132800151104           exfmt  OR37C02;
132900151104           *in28 = *off;
133000151104
133100151104         //?- Enter Controllo ed esco
133200151104           exsr OpzS02;
133300151104           IF  *in28;
133400151104             iter;
133500151104           ENDIF;
133600151104           FineTcr = *on;
133700151104
133800151104         ENDDO;
133900151104
134000151104       ENDSR;
134100151104
134200151104       //--------------------------------------------------------------
134300151104       //?Inizializzazione Videata S02.
134400151104       //--------------------------------------------------------------
134500151104       BEGSR InzS02;
134600151104
134700151104       //?Pulizia subfile
134800151104         exsr PulS02;
134900151104
135000151104       //?Caricamento subfile
135100151104         exsr RieS02;
135200151104
135300151104         *in33 = *on;
135400151104
135500151104       //?Imposto il posizionamento al primo rcd
135600151104         IF  S02nrr > 0;
135700151104           W02rcd = 1;
135800151104         ELSE;
135900151104           clear W02rcd;
136000151104         ENDIF;
136100151104
136200151104         W02csr = W02rcd;
136300151104
136400151104       ENDSR;
136500151104
136600151104       //--------------------------------------------------------------
136700151104       //?Pulizia Subfile S02.
136800151104       //--------------------------------------------------------------
136900151104       BEGSR PulS02;
137000151104
137100151104       //?Pulizia subfile
137200151104         *in30 = *on;
137300151104         *in31 = *on;
137400151104         write OR37C02;
137500151104         *in31 = *off;
137600151104         *in33 = *off;
137700151104
137800151104         clear W02rcd;
137900151104         clear W02csr;
138000151104         clear S02nrr;
138100151104         clear W02msg;
138200151104
138300151104         *in28 = *off;
138400151104
138500151104       ENDSR;
138600151104
138700151104       //--------------------------------------------------------------
138800151104       //?Caricamento Subfile S02.
138900151104       //--------------------------------------------------------------
139000151104       BEGSR RieS02;
139100151104
139200151104         xx = 1;
139300151104         FOR xx by 1 to 4;
139400151104           clear  OR37S02;
139500151104           W02tcr = Frequenza(xx);
139600151104           W02des = Frequenza(xx) + DesFreq(xx);
139700151104           S02nrr += 1;
139800151104           write  OR37S02;
139900151104         ENDFOR;
140000151104
140100151104       ENDSR;
140200151104
140300151104       //--------------------------------------------------------------
140400151104       //?Gestione opzioni Subfile S02.
140500151104       //--------------------------------------------------------------
140600151104       BEGSR OpzS02;
140700151104
140800151104         xx = 1;
140900151104         FOR xx by 1 to 4;
141000151104           S02nrr = xx;
141100151104           chain S02nrr OR37S02;
141200151104           IF  not %found;
141300151104             leave;
141400151104           ENDIF;
141500151104
141600151104           SELECT;
141700151104         //?- Nessuna opzione
141800151104           WHEN  W02opz = *blank;
141900151104
142000151104         //?- 1 = Scelta
142100151104           WHEN  W02opz = '1';
142200151104             savfreq = W02tcr;
142300151104             leave;
142400151104
142500151104           OTHER;
142600151104             *in28 = *on;
142700151104             W02msg = 'Opzione errata';
142800151104             update OR37S02;
142900151104             leave;
143000151104           ENDSL;
143100151104
143200151104         ENDFOR;
143300151104
143400151104       ENDSR;
143500131113
143600131113      /end-free
143700071030
143800071030      *---------------------------------------------------------------*
143900071030      *?Interrogazione Giri di Ritiro                                ?*
144000071030      *---------------------------------------------------------------*
144100071031     c     Search_Giro   BEGSR
144200071030      *
144300071031     c                   clear                   Wpocgi
144400071031     c                   clear                   Wcgi
144500071030     c                   clear                   WcgiD
144600071030      *
144700071030     c                   clear                   FIDG09ds
144800071030     c                   eval      D09iop0   = 'P01'
144900071030     c                   eval      D09ifgs   = I37fgs
145000071030     c                   eval      D09idat   = *date
145100071030     c                   eval      D09itug   = 'R'
145200071030      *
145300071030     c                   movel(p)  FIDG09ds      KPJBU
145400071030     c                   call      'FIDG09R'
145500071030     c                   parm                    KPJBA
145600071030     c                   movel     KPJBU         FIDG09ds
145700071030      *
145800081211sel 1c                   select
145900081211w   1c                   when           D09Of03 <> *on
146000071030     c                             and  D09Of12 <> *on
146100071030     c                             and  D09Oerr <> *on
146200071030     c                   eval      Wpocgi    = D09ofgs
146300071030     c                   eval      Wcgi      = D09ocgi
146400071030     c                   movel     D09odes       WcgiD
146500081211w   1c                   when      D09Of03   = *on
146600081211     c                   eval      *in60     = *on
146700081211     c                   eval      *in90     = *on
146800081211w   1c                   when      D09Of12   = *on
146900081211e   1c                   endsl
147000071030      *
147100071030     c                   ENDSR
147200070802
147300070802      *---------------------------------------------------------------*
147400070802      *?Verifica se record annullabile                               ?*
147500070802      *---------------------------------------------------------------*
147600070802     c     Ctr_Annull    BEGSR
147700070802      *
147800071030     c                   eval      dsCRO1  = V2Ccd1
147900071030     c                   eval      dsCRO2  = V2Ccd2
148000071030     c                   eval      dsCRO3  = V2Ccd3
148100070802      *
148200070802      * Non deve esistere un Ritiro Fisso con questo codice
148300070802     c                   exsr      Ctr_RFissi
148400070802if  1c                   if        *in90   = *on
148500070802     c                   leavesr
148600070802e   1c                   endif
148700070802      *
148800070802      * Non deve esistere un O.R.M. con questo codice
148900070802     c                   exsr      Ctr_ORM
149000070802if  1c                   if        *in90
149100070802     c                   leavesr
149200070802e   1c                   endif
149300121220      *
149400121220      * Non deve esistere l'abilitazione OSR per questo codice
149500121220     c                   exsr      Ctr_OSR
149600121220if  1c                   if        *in90
149700121220     c                   leavesr
149800121220e   1c                   endif
149900070802      *
150000070802     c                   ENDSR
150100070802
150200070802      *---------------------------------------------------------------*
150300070802      *?Non deve esistere un Ritiro Fisso con il codice da annullare ?*
150400070802      *---------------------------------------------------------------*
150500070802     c     Ctr_RFissi    BEGSR
150600070802      *
150700070802      * Ricerca per codice ritiro
150800070802     c                   move      ds_CRO        ORScra
150900070802     c     ORScra        setll     FNORS002
151000070802     c     ORScra        reade     FNORS002
151100070802do  1c                   dow       NOT  %eof(FNORS02L)
151200071030if  2c                   if        ORSatb  = *blanks
151300070803     c                   seton                                        285190
151400071030     c                   eval      V1Dmsg  = $Msg(5)
151500070802     c                   leavesr
151600070802e   2c                   endif
151700070802     c     ORScra        reade     FNORS002
151800070802e   1c                   enddo
151900070802      *
152000070802      * Ricerca per codice ordinante
152100070802     c                   move      ds_CRO        ORScor
152200070802     c     ORScor        setll     FNORS003
152300070802     c     ORScor        reade     FNORS003
152400070802do  1c                   dow       NOT  %eof(FNORS03L)
152500071030if  2c                   if        ORSatb  = *blanks
152600070803     c                   seton                                        285190
152700071030     c                   eval      V1Dmsg  = $Msg(5)
152800070802     c                   leavesr
152900070802e   2c                   endif
153000070802     c     ORScor        reade     FNORS003
153100070802e   1c                   enddo
153200070802      *
153300070802     c                   ENDSR
153400070802
153500070802      *---------------------------------------------------------------*
153600070802      *?Non deve esistere un O.R.M. con il codice da annullare       ?*
153700070802      *---------------------------------------------------------------*
153800070802     c     Ctr_ORM       BEGSR
153900070802      *
154000070802      * Ricerca per codice ritiro
154100070802     c                   move      ds_CRO        ORMcra
154200070802     c     ORMcra        setll     FNORM018
154300070802     c     ORMcra        reade     FNORM018
154400070802do  1c                   dow       NOT  %eof(FNORM18L)
154500070807if  2c                   if             ORMatb  = *blanks
154600070807     C                             and  ORMfao  < 900
154700070803     c                   seton                                        285190
154800071030     c                   eval      V1Dmsg  = $Msg(6)
154900070802     c                   leavesr
155000070802e   2c                   endif
155100070807     c     ORScra        reade     FNORM018
155200070802e   1c                   enddo
155300070802      *
155400070802      * Ricerca per codice ordinante
155500070802     c                   move      ds_CRO        ORMcor
155600070802     c     ORMcor        setll     FNORM009
155700070802     c     ORMcor        reade     FNORM009
155800070802do  1c                   dow       NOT  %eof(FNORM09L)
155900070802if  2c                   if             ORMatb  = *blanks
156000070802     C                             and  ORMfao  < 900
156100070803     c                   seton                                        285190
156200071030     c                   eval      V1Dmsg  = $Msg(6)
156300070802     c                   leavesr
156400070802e   2c                   endif
156500070802     c     ORMcor        reade     FNORM009
156600070802e   1c                   enddo
156700070802      *
156800070802     c                   ENDSR
156900121220
157000121220      *---------------------------------------------------------------*
157100121220      *?Non deve esistere tab. OSR  con il codice da annullare       ?*
157200121220      *---------------------------------------------------------------*
157300121220     c     Ctr_OSR       BEGSR
157400121220
157500121220      * se non è aperto il file lo devo aprire
157600121220     c                   IF        not %open(TNTBE01L)
157700121220     c                   IF        %subst(knsif:7:1) = 'P'
157800121220     c                   eval      wLibTbe = 'GAITRAGRPS/TNTBE01L'
157900121220     c                   ELSE
158000121220     c                   eval      wLibTbe = 'GAITRAGRU/TNTBE01L'
158100121220     c                   ENDIF
158200121220     c                   open      TNTBE01L
158300121220     c                   ENDIF
158400121220
158500121220     c     'OSR'         setll     TNTBE01L
158600121220     c                   DO        *hival
158700121220     c     'OSR'         reade     TNTBE01L
158800121220     c                   IF        %eof(TNTBE01L)
158900121220     c                   leave
159000121220     c                   ENDIF
159100121220     c                   IF        TBEatb <> *blanks
159200121220     c                   iter
159300121220     c                   ENDIF
159400121220     c                   eval      dOSR = TBEuni
159500121220     c                   SELECT
159600121220     c                   WHEN      %subst(%editc(I37cro:'X'):8:3) = '333' or
159700121220     c                             %subst(%editc(I37cro:'X'):8:3) = '444'
159800121220     c                   IF        %subst(%editc(I37cro:'X'):1:7) =
159900121220     c                             %editc(d§OSRcli:'X') or
160000121220     c                             %subst(%editc(I37cro:'X'):1:7) =
160100121220     c                             %editc(d§OSRcl2:'X') or
160200121220     c                             %subst(%editc(I37cro:'X'):1:7) =
160300121220     c                             %editc(d§OSRcl3:'X') or
160400121220     c                             %subst(%editc(I37cro:'X'):1:7) =
160500121220     c                             %editc(d§OSRcl4:'X') or
160600121220     c                             %subst(%editc(I37cro:'X'):1:7) =
160700121220     c                             %editc(d§OSRcl5:'X') or
160800121220     c                             %subst(%editc(I37cro:'X'):1:7) =
160900121220     c                             %editc(d§OSRcl6:'X') or
161000121220     c                             %subst(%editc(I37cro:'X'):1:7) =
161100121220     c                             %editc(d§OSRcl7:'X') or
161200121220     c                             %subst(%editc(I37cro:'X'):1:7) =
161300121220     c                             %editc(d§OSRcl8:'X') or
161400121220     c                             %subst(%editc(I37cro:'X'):1:7) =
161500121220     c                             %editc(d§OSRcl9:'X') or
161600121220     c                             %subst(%editc(I37cro:'X'):1:7) =
161700121220     c                             %editc(d§OSRcl13:'X') or
161800121220     c                             %subst(%editc(I37cro:'X'):1:7) =
161900121220     c                             %editc(d§OSRcl11:'X') or
162000121220     c                             %subst(%editc(I37cro:'X'):1:7) =
162100121220     c                             %editc(d§OSRcl12:'X') or
162200121220     c                             %subst(%editc(I37cro:'X'):1:7) =
162300121220     c                             %editc(d§OSRcl13:'X') or
162400121220     c                             %subst(%editc(I37cro:'X'):1:7) =
162500121220     c                             %editc(d§OSRcl14:'X') or
162600121220     c                             %subst(%editc(I37cro:'X'):1:7) =
162700121220     c                             %editc(d§OSRcl15:'X') or
162800121220     c                             %subst(%editc(I37cro:'X'):1:7) =
162900121220     c                             %editc(d§OSRcl16:'X')
163000121220     c                   seton                                        285190
163100121220     c                   eval      V1Dmsg  = $Msg(31)
163200121220     c                   eval      V1Dmsg = %trim(V1Dmsg) + ' LNP ' +
163300121220     c                             %subst(TBEke1:1:3) + ' Serie ' +
163400121220     c                             %subst(TBEke1:4:2)
163500121220     c                   leavesr
163600121220     c                   ENDIF
163700121220     c                   WHEN      %subst(%editc(I37cro:'X'):8:3) = '333' and
163800121220     c                             %subst(%editc(I37cro:'X'):1:7) =
163900121220     c                             %editc(d§OSRcor:'X')
164000121220     c                   seton                                        285190
164100121220     c                   eval      V1Dmsg  = $Msg(31)
164200121220     c                   eval      V1Dmsg = %trim(V1Dmsg) + ' LNP ' +
164300121220     c                             %subst(TBEke1:1:3) + ' Serie ' +
164400121220     c                             %subst(TBEke1:4:2)
164500121220     c                   leavesr
164600121220     c                   WHEN      %subst(%editc(I37cro:'X'):8:3) = '444' and
164700121220     c                             %subst(%editc(I37cro:'X'):1:7) =
164800121220     c                             %editc(d§OSRcrc:'X')
164900121220     c                   seton                                        285190
165000121220     c                   eval      V1Dmsg  = $Msg(31)
165100121220     c                   eval      V1Dmsg = %trim(V1Dmsg) + ' LNP ' +
165200121220     c                             %subst(TBEke1:1:3) + ' Serie ' +
165300121220     c                             %subst(TBEke1:4:2)
165400121220     c                   leavesr
165500121220     c                   ENDSL
165600121220     c                   ENDDO
165700121220
165800121220     c                   ENDSR
165900070802
166000070802      *---------------------------------------------------------------*
166100070802      *?Controllo C.A.P.                                             ?*
166200070802      *---------------------------------------------------------------*
166300070802     c     Ctr_CAP       BEGSR
166400070802      *
166500070802     c                   clear                   TISI95ds
166600070802     c                   eval      I95tcn  = '7'
166700070802     c                   eval      I95nar  = V2Cnar
166800070802     c                   eval      I95cap  = V2Ccar
166900070802     c                   eval      I95loc  = V2Clor
167000070802     c                   eval      I95prv  = V2Cprr
167100070802     c                   eval      I95ind  = V2Cinr
167200070802     c                   clear                   FNLV13ds
167300070802     c                   eval      I13af0  = 'S'
167400070802     c                   eval      I13cnv  = 'S'
167500070802     c                   eval      I13af1  = 'S'
167600070802     c                   eval      I13sz2  = 'S'
167700070802     c                   eval      I13la3  = 'S'
167800070802     c                   eval      I13sz3  = 'S'
167900070802      *
168000070802      * Gestione errori forzabili:
168100070802      * se cambiato CAP/Località riazzero i valori di errori forzabili
168200070802      *
168300070802      * - Modifica CAP
168400070802if  1c                   if        V2Ccar <> SAVcar
168500070802     c                   eval      SAVcar  = V2Ccar
168600070802     c                   clear                   E13fzv
168700070802     c                   clear                   E13fz1
168800070802     c                   clear                   E13fz2
168900070802     c                   clear                   E13fz3
169000070802e   1c                   endif
169100070802      * - Modifica località
169200070802if  1c                   if        V2Clor <> SAVlor
169300070802     c                   eval      SAVlor  = V2Clor
169400070802     c                   clear                   E13fz1
169500070802     c                   clear                   E13fz2
169600070802     c                   clear                   E13fz3
169700070802e   1c                   endif
169800070802      * - Modifica PROVINCIA
169900070802if  1c                   if        V2Cprr <> SAVprr
170000070802     c                   eval      SAVprr  = V2Cprr
170100070802     c                   clear                   E13fz3
170200070802e   1c                   endif
170300070802      *
170400070802      * Richiamo programma
170500070802     c                   call      'FNLV13R'
170600070802     c                   parm                    KPJBA
170700070802     c                   parm                    FNLV13ds
170800070802     c                   parm                    TISI95ds
170900070802      *
171000070802      * Reimpostazione dei dati ricevuti in TISI95ds
171100070802if  1c                   if             O13ron  = 'S'
171200110121     c                   eval      V2Cnar  = O95nar
171300110121e   1c                   endif
171400110121     c                   if             O13roc  = 'S'
171500110121     c                   eval      V2Ccar  = O95cap
171600110121e   1c                   endif
171700110121     c                   if             O13rop  = 'S'
171800110121     c                   eval      V2Cprr  = O95prv
171900110121e   1c                   endif
172000110121     c                   if             O13rol  = 'S'
172100070802     c                   eval      V2Clor  = O95loc
172200070802e   1c                   endif
172300070802      *
172400070802      * Riscontrati errori o reimpostati dati
172500070802sel 1c                   select
172600070802      * - Emissione messaggio con l'errore rilevato
172700081022w   1c                   when      O13msg <> *blanks
172800070802     c                   eval      *in55   = (O13err = '5')                     CAP
172900070802     c                   eval      *in56   = (O13err = '3')                     Località
173000070802     c                   eval      *in57   = (O13err = '4')                     Provincia
173100070802     c                   eval      *in58   = (O13err = '6')                     Nazione
173200070802     c                   seton                                        28  90
173300070802     c                   eval      V1Dmsg  = O13msg
173400070802     c                   leavesr
173500070802      * - C.A.P. reimpostato
173600070802w   1c                   when      O13roc  = 'S'
173700070802     c                   seton                                        285590
173800070802     c                   eval      V1Dmsg  = $Msg(12)
173900070802     c                   leavesr
174000070802      * - Località reimpostata
174100070802w   1c                   when      O13rol  = 'S'
174200070802     c                   seton                                        285690
174300070802     c                   eval      V1Dmsg  = $Msg(13)
174400070802     c                   leavesr
174500070802      * - Provincia reimpostata
174600070802w   1c                   when      O13rop  = 'S'
174700070802     c                   seton                                        285790
174800070802     c                   eval      V1Dmsg  = $Msg(14)
174900070802     c                   leavesr
175000070802      * - Nazione reimpostata
175100070802w   1c                   when      O13ron  = 'S'
175200070802     c                   seton                                        285890
175300070802     c                   eval      V1Dmsg  = $Msg(15)
175400070802     c                   leavesr
175500070802e   1c                   endsl
175600070802      *
175700070802     c                   ENDSR
175800070801
175900070801      *---------------------------------------------------------------*
176000070801      *?Reperimento dati anagrafici                                  ?*
176100070801      *---------------------------------------------------------------*
176200070801     c     Call_TIBS69   BEGSR
176300070801      *
176400070801     c                   clear                   ds_CNACO
176500070801     c                   clear                   ds_CNIND
176600070801     c                   clear                   ds_CNCLP
176700070801     c                   clear                   ds_FNCLS
176800070801      *
176900070801     c                   call      'TIBS69R'
177000070801     c                   parm                    TIBS69ds
177100070801     c                   parm                    ds_CNACO
177200070801     c                   parm                    ds_CNIND
177300070801     c                   parm                    ds_CNCLP
177400070801     c                   parm                    ds_FNCLS
177500070801      *
177600071023     c                   eval      *in28   = (O69err = *on)
177700070801if  1c                   if        *in28
177800071023     c                   eval      *in90   = *on
177900071023     c                   eval      V1Dmsg  = O69msg
178000070801e   1c                   endif
178100070801      *
178200070801     c                   ENDSR
178300070801
178400070801      *---------------------------------------------------------------*
178500070801      *?Aggiornamento dati file FNACR00F                             ?*
178600070801      *---------------------------------------------------------------*
178700070801     c     Upd_FNACR     BEGSR
178800070801      *
178900071023     c                   eval      *in91   = *off
179000071023     c                   eval      I37cro  = V2Ccd1 * 10000000
179100071023     c                                     + V2Ccd2 * 1000
179200070807     c                                     + V2Ccd3
179300071030     c     I37cro        chain     FNACR000
179400070803      *
179500070801     c                   select
179600070801      *
179700070801      * Inserimento / Copia
179800070801w   1c                   when          (I37opz = '1'
179900070809     c                             or   I37opz = '3'
180000070809     c                             or   I37opz = 'A'
180100070809     c                             or   I37opz = 'C')
180200070801     c                             and  *inKF
180300070801     c                   exsr      RieRec
180400070803     c                   WRITE     FNACR000                             91
180500070801      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
180600070801      * Modifica
180700070801w   1c                   when           I37opz  = '2'
180800070801     c                             and  *inKF
180900070801     c                   exsr      RieRec
181000070801     c                   UPDATE    FNACR000
181100070801      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
181200070801      * Annullamento
181300070801w   1c                   when          (I37opz  = '4'
181400070801     c                             and  *inKF)
181500070801     c                             or   *inKQ
181600070801     c                   eval      ACRatb  = 'A'
181700070801     c                   UPDATE    FNACR000
181800070801      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
181900070801      * Ripristino
182000070801w   1c                   when          (I37opz  = '7'
182100070801     c                             and  *inKF)
182200070801     c                             or   *inKE
182300070802     c                   exsr      RieRec
182400070801     c                   UPDATE    FNACR000
182500070801      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
182600070801e   1c                   endsl
182700070801      *
182800070803if  1c                   if        *in91
182900070801     c                   seton                                        28  90
183000071030     c                   eval      V1Dmsg  = %trimr($Msg(4))
183100071030     c                                     + ' FNACR00F'
183200070801e   1c                   endif
183300070801      *
183400071030      * Aggiornamento ambito "standard" di intradamento codice giro
183500071030     c                   eval      ACR1ain = '='
183600071030     c                   eval      Wpocgi  = V2Cpocgi1
183700071030     c                   eval      Wcgi    = V2Ccgi1
183800070801     c                   exsr      Upd_FNACR1
183900071030      *
184000071030      * Aggiornamento ambito "oltre" di intradamento codice giro
184100071030     c                   eval      ACR1ain = '>'
184200071030     c                   eval      Wpocgi  = V2Cpocgi2
184300071030     c                   eval      Wcgi    = V2Ccgi2
184400071030     c                   exsr      Upd_FNACR1
184500071030      *
184600071030      * Aggiornamento ambito "sotto" di intradamento codice giro
184700071030     c                   eval      ACR1ain = '<'
184800071030     c                   eval      Wpocgi  = V2Cpocgi3
184900071030     c                   eval      Wcgi    = V2Ccgi3
185000071030     c                   exsr      Upd_FNACR1
185100151012      *
185200151012      /free
185300151012
185400151012         // -?Aggiornamento dati file FNACRE0F?
185500151012
185600151012         // -?Impostazione chiave d'accesso al file FNACRE1L per l'?
185700151012         //  ?aggionamento della "e-Mail Conferma Accettazione ORM"?
185800151012         clear  keyFNACRE1;
185900151012         k_ACREcro = I37cro;
186000151012         k_ACREtrc = 'MC';
186100151012         chain  %kds( keyFNACRE1 )  FNACRE00;
186200151012
186300151012         Select;
186400151012
186500151012           // -?Inserimento / Copia?
186600151012           When  Not %found(FNACRE1L)  and  V2Cmca <> *blank;
186700151012             clear  FNACRE00;
186800151012             ACREatb  = ACRatb;
186900151012             ACREcro  = k_ACREcro;
187000151012             ACREtrc  = k_ACREtrc;
187100151012             ACREdati = V2Cmca;
187200151012             Write  FNACRE00;
187300151012
187400151012           // -?Modifica / Annullamento / Ripristino?
187500151012           When  %found(FNACRE1L)  and  V2Cmca <> *blank;
187600151012             ACREatb  = ACRatb;
187700151012             ACREdati = V2Cmca;
187800151012             Update  FNACRE00;
187900151012
188000151012           // -?Cancellazione?
188100151012           When  %found(FNACRE1L)  and  V2Cmca = *blank;
188200151012             Delete  FNACRE00;
188300151012
188400151012           // -?Disallocazione?
188500151012           Other;
188600151012             UnLock  FNACRE1L;
188700151012
188800151012         EndSl;
188900151012
189000151012      /end-free
189100070801      *
189200070801     c                   ENDSR
189300070801
189400070801      *---------------------------------------------------------------*
189500070801      *?Aggiornamento dati file FNACR10F                             ?*
189600070801      *---------------------------------------------------------------*
189700070801     c     Upd_FNACR1    BEGSR
189800070801      *
189900071030     c                   eval      *in92   = *off
190000070807     c                   eval      I37cro  = V2Ccd1*10000000
190100070807     c                                     + V2Ccd2*1000
190200070807     c                                     + V2Ccd3
190300071024      *
190400071024sel 1c                   select
190500070801      *
190600070802      * Inserimento / Modifica / Copia
190700070803w   1c                   when          (I37opz  = '1'
190800070803     c                             or   I37opz  = '2'
190900070809     c                             or   I37opz  = '3'
191000070809     c                             or   I37opz  = 'A'
191100070809     c                             or   I37opz  = 'C')
191200070801     c                             and  *inKF
191300071030     c     k03acr13      chain     FNACR100
191400071105     c                   clear                   SAVpocgi
191500071105     c                   clear                   SAVcgi
191600071105if  2c                   if        %found(FNACR13L)
191700071105     c                   eval      SAVpocgi     = ACR1pocgi
191800071105     c                   eval      SAVcgi       = ACR1cgi
191900071105e   2c                   endif
192000071105      *
192100071030if  2c                   if        Wcgi   <> *blanks
192200070801     c                   exsr      RieRec1
192300071030e   2c                   endif
192400071105      *
192500070803sel 2c                   select
192600071030w   2c                   when      NOT  %found(FNACR13L)
192700071030     c                             and  Wcgi   <> *blanks
192800070801     c                   WRITE     FNACR100                             92
192900070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
193000071030w   2c                   when           %found(FNACR13L)
193100071030     c                             and  Wcgi   <> *blanks
193200071105     c                             and  Wcgi   <> SAVcgi
193300070801     c                   UPDATE    FNACR100
193400070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
193500071030w   2c                   when           %found(FNACR13L)
193600071030     c                             and  Wcgi    = *blanks
193700071024     c                   DELETE    FNACR100
193800071024      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
193900071030w   2c                   when           %found(FNACR13L)
194000071105     c                             and  Wcgi    = SAVcgi
194100071030     c                   UNLOCK    FNACR13L
194200071030      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
194300070803e   2c                   endsl
194400070801      *
194500070801      * Annullamento
194600070807w   1c                   when         ((I37opz  = '4'
194700070807     c                             and  *inKF)
194800070807     c                             or   *inKQ)
194900071024     c     I37cro        setll     FNACR100
195000071024     c     I37cro        reade     FNACR100
195100071030do  2c                   dow       NOT  %eof(FNACR13L)
195200080115     c                   DELETE    FNACR100
195300070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
195400071024     c     I37cro        reade     FNACR100
195500071024e   2c                   enddo
195600070801      * Ripristino
195700070807w   1c                   when         ((I37opz  = '7'
195800070801     c                             and  *inKF)
195900070807     c                             or   *inKE)
196000071024     c     I37cro        setll     FNACR100
196100071024     c     I37cro        reade     FNACR100
196200071030do  2c                   dow       NOT  %eof(FNACR13L)
196300071030if  3c                   if        I37fgs  = ACR1pocgi
196400070801     c                   exsr      RieRec1
196500071024x   3c                   else
196600071030     c                   eval      ACR1atb = *blanks
196700071030     c                   eval      ACR1dtv = *date
196800071030     c                   eval      ACR1ute = KNMUS
196900071024e   3c                   endif
197000070807     c                   UPDATE    FNACR100
197100070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
197200071024     c     I37cro        reade     FNACR100
197300071024e   2c                   enddo
197400070801e   1c                   endsl
197500070801      *
197600070801if  1c                   if        *in92
197700070801     c                   seton                                        28  90
197800071030     c                   eval      V1Dmsg  = %trimr($Msg(4))
197900071030     c                                     + ' FNACR10F'
198000070801e   1c                   endif
198100070801      *
198200070801     c                   ENDSR
198300070801
198400070801      *---------------------------------------------------------------*
198500070801      *?Impostazione valori nei campi del record FNACR00F            ?*
198600070801      *---------------------------------------------------------------*
198700070801     c     RieRec        BEGSR
198800070801      *
198900131113     c                   clear                   dACR01
199000070801     c                   clear                   FNACR000
199100070801      *
199200070809      * Lascia record annullato se lo era già (*in43)
199300070809      * e non ne è stato chiesto il ripristino
199400070807     c                   if             *in43
199500070807     c                             and  NOT *inKE
199600070807     c                             and  I37opz <> '7'
199700070807     c                   eval      ACRatb  = 'A'
199800070807     c                   endif
199900070809      *
200000070809      * Imposta tutti gli altri campi ("chiave" compresi)
200100071030     c                   eval      ACRtac  = 'R'
200200070801     c                   eval      ACRcro  = V2Ccd1*10000000
200300070801     c                                     + V2Ccd2*1000
200400070801     c                                     + V2Ccd3
200500070801     c                   eval      ACRrsr  = V2Crsr
200600070801     c                   eval      ACRinr  = V2Cinr
200700070801     c                   eval      ACRcar  = V2Ccar
200800070801     c                   eval      ACRlor  = V2Clor
200900070801     c                   eval      ACRprr  = V2Cprr
201000070801     c                   eval      ACRnar  = V2Cnar
201100070801     c                   eval      ACRksc  = V2Cksc
201200070801     c                   eval      ACRcco  = V2Cco1*10000000
201300070801     c                                     + V2Cco2*1000
201400070801     c                                     + V2Cco3
201500070801     c                   eval      ACRper  = V2Cper
201600070801     c                   eval      ACRtel  = V2Ctel
201700131113     c                   eval      dACR01.§ACRoa1 = %editc( V2ChmAmDa : 'X' )
201800131113     c                   eval      dACR01.§ACRoa2 = %editc( V2ChmAmA  : 'X' )
201900131113     c                   eval      dACR01.§ACRoa3 = %editc( V2ChmPmDa : 'X' )
202000131113     c                   eval      dACR01.§ACRoa4 = %editc( V2ChmPmA  : 'X' )
202100140114     c                   eval      dACR01.§ACRstcr = sav_§ACRstcr
202200140114     c                   eval      dACR01.§ACRctcr = sav_§ACrctcr
202300151104     c                   eval      dACR01.§ACRfmar = V2Cfca
202400131113     c                   eval      ACRmai  = dACR01
202500070801     c                   eval      ACRno1  = V2Cno1
202600070801     c                   eval      ACRno2  = V2Cno2
202700070801     c                   eval      ACRntm  = V2Cntm
202800150909     c                   eval      ACRpoa  = V2Cpoa
202900070801     c                   eval      ACRtcr  = V2Ctcr
203000070802if  1c                   if        V2Cfcc  = 'N'
203100070801     c                   eval      ACRfcc  = V2Cfcc
203200070802x   1c                   else
203300070802     c                   clear                   ACRfcc
203400070802e   1c                   endif
203500091009if  1c                   if        V2Cctr  = *blanks
203600070801     c                   eval      ACRccc  = 999
203700070801x   1c                   else
203800091009     c                   move      v2cctr        ACRccc
203900070801e   1c                   endif
204000070801     c                   eval      ACRfcl  = V2Cfcl
204100070802     c                   eval      ACRfpk  = V2Cfpk
204200070801     c                   eval      ACRfmc  = V2Cfmc
204300070801     c                   eval      ACRfbn  = V2Cfbn
204400070801     c                   eval      ACRfbl  = V2Cfbl
204500070801     c                   eval      ACRfmt  = V2Cfmt
204600070801     c                   eval      ACRfat  = V2Cfat
204700070801     c                   eval      ACRspi  = V2Cspi
204800070801     c                   eval      ACRorr  = V2Corr
204900070801     c                   eval      ACRdtr  = *date
205000070801      *
205100070801     c                   ENDSR
205200070801
205300070801      *---------------------------------------------------------------*
205400070801      *?Impostazione valori nei campi del record FNACR10F            ?*
205500070801      *---------------------------------------------------------------*
205600070801     c     RieRec1       BEGSR
205700070801      *
205800070913     c                   eval      ACR1atb   = ACRatb
205900070801     c                   eval      ACR1cro   = ACRcro
206000071030     c                   eval      ACR1pocgi = Wpocgi
206100071030     c                   eval      ACR1cgi   = Wcgi
206200071030if  1c                   if        NOT  %found(FNACR13L)
206300071030     c                             or  (SAVpocgi <> Wpocgi
206400071030     c                             or   SAVcgi   <> Wcgi)
206500070801     c                   eval      ACR1dtv   = *date
206600070802     c                   eval      ACR1ute   = KNMUS
206700070802e   1c                   endif
206800070801      *
206900070801     c                   ENDSR
207000070801
207100070801** - $Fun ------*
207200070801   INSERIMENTO      1
207300070801     MODIFICA       2
207400070801      COPIA         3
207500070801   ANNULLAMENTO     4
207600070801 VISUALIZZAZIONE    5
207700070801    RIPRISTINO      6
207800070801** - $Msg -------------------------------------------------------------------*
207900070801PARAMETRI ERRATI                                                                1
208000070801Richiesta funzione errata per record inesistente                                2
208100070801Record già esistente                                                            3
208200070801Errore in fase di scrittura record nel file                                     4
208300070802NON ANNULLABILE: reperite anagrafiche ritiri fissi con questo codice            5
208400070802NON ANNULLABILE: reperiti ritiri con questo codice                              6
208500070802Codice cliente ritiro non valido: NON esiste sul piano dei conti                7
208600070802Codice cliente ritiro obbligatorio                                              8
208700070802Codice cliente ritiro già esistente                                             9
208800070802Tipo anagrafica errato                                                         10
208900070802Frequenza ritiro errato                                                        11
209000070802ATTENZIONE: modificato C.A.P.                                                  12
209100070802ATTENZIONE: modificata località                                                13
209200070802ATTENZIONE: modificata provincia                                               14
209300070802ATTENZIONE: modificata nazione                                                 15
209400070808Immettere anche il giro di ritiro  o  togliere la filiale del giro             16
209500070808Immettere anche la filiale del giro  o  togliere il giro di ritiro             17
209600070802Filiale errata per codice giro di ritiro                                       18
209700070911Codice giro di ritiro inesistente o non valido                                 19
209800070802Codice cliente ordinante errato                                                20
209900070802Non è possibile inserire un codice ordinante per un cliente di tipo ordinante  21
210000070802Codice cliente piano dei conti errato                                          22
210100070802Inserire il codice tariffa solo se inserito anche il codice P.d.C.             23
210200091009Codice tariffa inesistente                                                     24
210300070802Frequenza ritiro errata                                                        25
210400070802Ora ritiro errata                                                              26
210500070802Valore flag immissione errato                                                  27
210600070802NON è stato modificato il codice Piano dei Conti  --  Enter per forzare        28
210700071112Codice giro di ritiro errato o NON in vigore                                   29
210800071030PRIMA posizionare il cursore sul campo per cui interrogare e/o selezionare     30
210900121220NON ANNULLABILE: Codice presente su tabella OSR                                31
211000140723Creare il luogo '000' per il codice cliente in gestione.                       32
211100150909Filiale Ritiro Forzata errata                                                  33
211200151104
211300151104** -- Frequenza / DesFreq ---------------------------------------------------*
211400151104F = Fisso
211500151104R = Ricorrente
211600151104O = No Internet
211700151104M = Mai
