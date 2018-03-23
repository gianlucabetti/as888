000100070801      * FIOR37R1 *----------------------------------------------------*
000200070801      *?MANUTENZIONE ANAGRAFICA CLIENTI RITIRO (CON GIRO)            ?*
000300070730      *---------------------------------------------------------------*
000400070730
000500070801     h decedit('0,') datedit(*YMD.) option(*nodebugio)
000600160317     h dftactgrp(*no) bnddir('UBBNDDIR')
000700070730
000800070730      *---------------------------------------------------------------*
000900000927
001000070801     fAZORG01L  if   e           k disk
001100070730     fFNORS02L  if   e           k disk    rename(FNORS000:FNORS002)
001200070730     fFNORS03L  if   e           k disk    rename(FNORS000:FNORS003)
001300070730     fFNORM18L  if   e           k disk    rename(FNORM000:FNORM018)
001400070730     fFNORM09L  if   e           k disk    rename(FNORM000:FNORM009)
001500070730     fTNTAM01L  if   e           k disk
001600121220     fTNTBE01L  if   e           k disk    usropn
001700121220     f                                     extfile(wLibTbe)
001800070730      *
001900070730     fFNACR01L  Uf A e           k disk
002000071030     fFNACR13L  Uf A e           k disk
002100151012     fFNACRE1L  Uf A e           k disk
002200070730      *
002300070801     fFIOR37D1  cf   e             workstn
002400151104     f                                     sfile(OR37S02 : S02nrr)
002500000927
002600070730      *
002700070730      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - -?*
002800070730      *
002900070801      * - Attributi di visualizzazione
003000070801     d C_HI_RI         c                   const(x'23')
003100070801     d C_ND            c                   const(x'27')
003200071105      * - Num. Max di codici giro ritiro inseribili per cliente ritiro
003300071105     d C_MaxCGI        c                   const(03)
003400131113
003500131113       // -?Costante per controllo "caratteri solo numerici"?
003600131113     d c_Digits        c                   const('0123456789')
003700070730      *
003800070730      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - -?*
003900070730      *
004000070801      * - Testi di decodifica funzione richiesta
004100071105     d $Fun            s             17    dim( 6)  ctdata  perrcd(1)
004200070730      * - Messaggi di errore
004300121220     d $Msg            s             78    dim(35)  ctdata  perrcd(1)           MSG VIDEO
004400151104
004500151104      // - Sk Frequenza Ritiro
004600151104     d Frequenza       s              1a   dim(04) ctdata perrcd(1)
004700151104     d DesFreq         s             20a   dim(04) alt(Frequenza)
004800070730      *
004900070730      *?  S T R U T T U R E   D A T I   - - - - - - - - - - - - - - -?*
005000070730      *
005100070730      * - Parametri
005200070730     d KPJBA         e ds
005300070730     d FIOR37ds      e ds                  inz
005400070730      *
005500060203      * - Parametri x Controllo profilo utenti
005600060203     d TIBS34ds      e ds
005700060203      * - Ds di riferimento al file esterno AZUTE00F
005800060203     d AZUTEds       e ds                  extname(AZUTE00F)
005900060203      * - Ds per dati organigramma
006000070801     d dDatiUte      e ds
006100070801      *
006200070801      * - Lettura anagrafiche cliente
006300070801     d TIBS69ds      e ds                  inz
006400070802     d ds_CNACO      e ds                  inz  extname(CNACO00F)
006500070802     d ds_CNIND      e ds                  inz  extname(CNIND00F)
006600070802     d ds_CNCLP      e ds                  inz  extname(CNCLP00F)
006700070802     d ds_FNCLS      e ds                  inz  extname(FNCLS00F)
006800070802      *
006900070802      * - Controllo CAP/Località
007000070802     d TISI95ds      e ds                  inz
007100070802      * - Pretrullo TISI95R
007200070802     d FNLV13ds      e ds                  inz
007300070802      * - Controllo indirizzo completo
007400070802     d FNLV14ds      e ds                  inz
007500060203      *
007600070730      * - Gestione anagrafica giri
007700070730     d FIDG09ds      e ds                  inz
007800121130
007900070802      * - Parametri x gestione anagrafica ritiri fissi
008000070802     d FIOR02ds      e ds                  inz
008100070802      *
008200070802      * - Controllo validita n° tel/fax
008300070802     d TRUL42ds      e ds                  inz
008400121130
008500121130      // - Parametri per ricerca Programmi chiamanti
008600121130     d dsStack         ds
008700121130     d  skStack                      10    Dim(100)
008800070730      *
008900070802      * - 143ª descrizione dell'organigramma
009000070802     d Og143         e ds                  inz
009100131113      *
009200131113      * - tabella OSR
009300121220     d dOSR          e ds                  inz
009400131113      *
009500131113      * - Orari ritiro ed altro in FNACR00F
009600131113     d dACR01        e ds                  inz  qualified
009700160713
009800160713      //?ds per rcd 'AA' FNORE - Variazione dati alert conferma ritiro
009900160713     d dOREana       e ds
010000070730      *
010100070730     d Status         sds           333
010200070730     d   SDSpgm          *proc
010300070801      *
010400070801      * - Codice cliente ritiro
010500070801     d ds_CRO          ds            10    inz
010600070801     d  dsCRO1                        3  0 inz
010700070801     d  dsCRO2                        4  0 inz
010800070801     d  dsCRO3                        3  0 inz
010900070802      *
011000070802      * - Ora
011100070802     d ds_Time         ds             4    inz
011200070802     d  dsHH                          2  0 inz
011300070802     d  dsMM                          2  0 inz
011400070730      *
011500070730      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
011600070730      *
011700070801      * - Flags
011800070801     d $InzD01         s              1    inz(*on)
011900070801     d $Fine           s              1    inz(*off)
012000070802     d $Tam            s              1    inz(*off)
012100160713     d wOkRcdAA        s               n   inz
012200071030      * - Indici di schiera / Contatori
012300071030     d wNrCGI          s              3  0 inz
012400070802      * - Comodo
012500071030     d Wpocgi          s                   inz  like(V2Cpocgi1)
012600071030     d Wcgi            s                   inz  like(V2Ccgi1)
012700071030     d WcgiD           s                   inz  like(V2Dcgi1)
012800070802     d SAVcar          s                   inz  like(V2Ccar)
012900070802     d SAVlor          s                   inz  like(V2Clor)
013000070802     d SAVprr          s                   inz  like(V2Cprr)
013100070802     d SAVksc          s                   inz  like(V2Cksc)
013200070802     d SAVpocgi        s                   inz  like(ACR1pocgi)
013300070802     d SAVcgi          s                   inz  like(ACR1cgi)
013400110121     d wdecod          s              1    inz
013500121130     d wGiaChiamato    s               n   inz(*off)
013600121220     d wLibTbe         s             21a   inz
013700151104     d FineTcr         s               n   inz(*off)
013800151104     d S02nrr          s              4s 0 inz
013900151104     d savfreq         s                   like(W02tcr) inz
014000151104     d xx              s              4s 0 inz
014100160713     d FineW03         s               n   inz(*off)
014200160713     d savfca          s                   like(V2Cfca) inz
014300160713     d savmca          s                   like(V2Cmca) inz
014400160713     d savsca          s                   like(V2Csca) inz
014500140114
014600140114     d sav_§ACRstcr    s                   like(dACR01.§ACRstcr)
014700140114     d sav_§ACRctcr    s                   like(dACR01.§ACRctcr)
014800160713
014900160713      // - Campi di comodo data
015000160713     d Data_EUR        s               d   datfmt(*eur)
015100160713     d Data_ISO        s               d   datfmt(*iso)
015200121130
015300121130      // - Campi per procedura esterna
015400121130     d  pStack         s           1000
015500121130     d  pStartPoint    s              3  0
015600121130     d  pEntries       s              3  0
015700121130
015800121130      //---------------------------------------------------------------
015900121130      //?Definizione procedure esterne.
016000121130      //---------------------------------------------------------------
016100121130
016200121130       // - Pgm per ricerca Programmi chiamati
016300121130     d XRTVSTKR        pr                  extpgm('XRTVSTKR')
016400121130     d  pStack                     1000
016500121130     d  pStartPoint                   3  0
016600121130     d  pEntries                      3  0
016700131113
016800131113       // -?Controllo orari apertura del cliente?
016900131113     d TRUL03ds      e ds                  qualified  inz
017000131113     d trul03r         pr                  extpgm('TRUL03R')
017100131113     d   trul03ds...
017200131113     d                                     likeds(trul03ds)
017300151012
017400151012       // -?Controllo indirizzo e-mail?
017500151012     d dsEmail       e ds                  inz
017600151012      /copy gaitrasrc/srcProtoPR,XEMAIL
017700160317
017800160317       // -?Controllo cellulare
017900160317      /copy gaitrasrc/srcprotopi,ubchkcel
018000160317      /copy gaitrasrc/srcprotopr,ubchkcel
018100131113
018200121130      //---------------------------------------------------------------
018300070730      *
018400070730      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
018500070730      *
018600151012       // -?File FNACRE1L?
018700151012     d keyFNACRE1    e ds                  extname( FNACRE1L : *key )
018800151012     d                                     prefix( k_ )   inz
018900071030      * - FNACR13L
019000071030     c     k02acr13      klist
019100071023     c                   kfld                    I37cro
019200071023     c                   kfld                    I37fgs
019300071030     c     k03acr13      klist
019400071030     c                   kfld                    I37cro
019500071030     c                   kfld                    I37fgs
019600071030     c                   kfld                    ACR1ain
019700070801      * - TNTAM01L
019800070802     c     k02tam01      klist
019900070802     c                   kfld                    TAMksc
020000070801     c                   kfld                    TAMctr
020100121130
020200070801      *---------------------------------------------------------------*
020300070801      *?RIEPILOGO INDICATORI                                         ?*
020400070801      *---------------------------------------------------------------*
020500070801      *  02    - Abilitazione tasto funzionale F2=Anagr.Fissi   - D01 *
020600070801      *  03    - Abilitazione tasto funzionale F3=Fine          - D01 *
020700070801      *  05    - Abilitazione tasto funzionale F5=Ripristino    - D01 *
020800070801      *  06    - Abilitazione tasto funzionale F6=Conferma      - D01 *
020900070801      *  16    - Abilitazione tasto funzionale F15=Annullamento - D01 *
021000160713      *  19    - Abilitazione tasto funzionale F19=Mod.Dati Alert     *
021100070801      *  25    - Comodo                                               *
021200070801      *  28    - Emissione del messaggio di errore a video            *
021300140113      *  39    - Visualizzo Frequenza ritiro salvata x EDP
021400140113      *  40    - Proteggo Frequenza ritiro
021500070801      *  41    - Protezione campi chiave in modifica            - D01 *
021600070809      *  42    - Protezione campi codice PdC e Tariffa          - D01 *
021700070809      *  43    - Visualizzazione "ANNULLATO"                    - D01 *
021800070801      *  51    - Cliente ritiro           errato                - D01 *
021900150909      *  52    - Filiale ritiro forzata   errata                - D01 *
022000070801      *  53    - Ragione sociale          errata                - D01 *
022100070801      *  54    - Indirizzo                errato                - D01 *
022200070801      *  55    - CAP                      errato                - D01 *
022300070801      *  56    - Località                 errata                - D01 *
022400070801      *  57    - Provincia                errata                - D01 *
022500070801      *  58    - Nazione                  errata                - D01 *
022600071030      *  59    - Fil. Giro Ritiro ("=")   errata                - D01 *
022700071030      *  60    - Giro di Ritiro   ("=")   errato                - D01 *
022800070801      *  61    - Ordinante                errato                - D01 *
022900070801      *  62    - Codice PdC               errato                - D01 *
023000070801      *  63    - Tariffa                  errata                - D01 *
023100070801      *  64    - Frequenza ritiro         errata                - D01 *
023200070801      *  65    - Ora       ritiro         errata                - D01 *
023300070801      *  67    - Telefono referente       errato                - D01 *
023400070801      *  68    - E-Mail   referente       errata                - D01 *
023500070801      *  69    - Flag immissione colli    errato                - D01 *
023600070801      *  70    - Flag immissione bancali  errato                - D01 *
023700070801      *  71    - Flag immissione peso     errato                - D01 *
023800070801      *  72    - Flag immissione volume   errato                - D01 *
023900070801      *  73    - Flag immissione bilico   errato                - D01 *
024000070801      *  74    - Flag immissione motrice  errato                - D01 *
024100070801      *  75    - Flag immissione autotr.  errato                - D01 *
024200071030      *  76    - Fil. Giro Ritiro (">")   errata                - D01 *
024300071030      *  77    - Giro di Ritiro   (">")   errato                - D01 *
024400071030      *  78    - Fil. Giro Ritiro ("<")   errata                - D01 *
024500071030      *  79    - Giro di Ritiro   ("<")   errato                - D01 *
024600160317      *  80/83 - Orari apertura                                 - D01 *
024700160317      *  84    - SMS per invio conferma prenotazione ritiro     - D01 *
024800070801      *  90    - Generico di errore                                   *
024900070803      *  91    - Fallito aggiornamento di FNACR00F                    *
025000070803      *  92    - Fallito aggiornamento di FNACR10F                    *
025100000927      *---------------------------------------------------------------*
025200070730
025300070730     c     *Entry        plist
025400070730     c                   parm                    KPJBA
025500070801     c                   movel     KPJBU         FIOR37ds
025600070730      *
025700070730      * Operazioni iniziali
025800070801     c                   exsr      RoutInz
025900070801      *
026000071024      * Gestione video
026100070801do  1c                   DOW       $Fine   = *off
026200070801     c                   exsr      GesD01
026300070801e   1c                   ENDDO
026400070801      *
026500071024      * Operazioni finali
026600070801     c                   movel(p)  FIOR37ds      KPJBU
026700070801     c                   eval      *inLR   = *on
026800070801
026900070801      *---------------------------------------------------------------*
027000070801      *?Operazioni Iniziali                                          ?*
027100070801      *---------------------------------------------------------------*
027200070801     c     RoutInz       BEGSR
027300070801      *
027400070801      * Pulizia campi di output
027500070801     c                   clear                   O37cro
027600070801     c                   clear                   O37err
027700070801     c                   clear                   O37msg
027800071030     c                   eval      O37ret  = *off
027900070801      *
028000070801      * Controllo parametri ricevuti
028100070801sel 1c                   SELECT
028200070801      * - Controllo inserimento filiale gestione per Immissione/Variaz.
028300070801w   1c                   WHEN           I37opz  <> '1'
028400070801     c                             and  I37opz  <> '2'
028500070801     c                             and  I37opz  <> '3'
028600070801     c                             and  I37opz  <> '4'
028700070801     c                             and  I37opz  <> '5'
028800070801     c                             and  I37opz  <> '7'
028900070809     c                             and  I37opz  <> 'A'
029000070809     c                             and  I37opz  <> 'C'
029100071030     c                   eval      O37err  = 'E'
029200071030     c                   eval      O37msg  = $Msg(1)
029300071030     c                   eval      $Fine   = *on
029400070801     c                   leavesr
029500070807w   1c                   WHEN           I37opz  <> '1'
029600070801     c                             and  I37cro   = *zeros
029700071030     c                   eval      O37err  = 'E'
029800071030     c                   eval      O37msg  = $Msg(1)
029900071030     c                   eval      $Fine   = *on
030000070801     c                   leavesr
030100070802w   1c                   WHEN           I37opz  <> '5'
030200070802     c                             and  I37fgs   = *zeros
030300071030     c                   eval      O37err  = 'E'
030400071030     c                   eval      O37msg  = $Msg(1)
030500071030     c                   eval      $Fine   = *on
030600070801     c                   leavesr
030700070801e   1c                   ENDSL
030800070801      *
030900070801      * Reperimento dati job
031000070801     c                   exsr      DatiJob
031100070801      *
031200070801      * Impostazione nome programma a video
031300070801     c                   movel     SDSpgm        V1Tpgm
031400070801      *
031500070801      * Impostazione attributi di visualizzazione in testata
031600071030     c                   eval      DA01    = C_HI_RI
031700070801sel 1c                   select
031800071030w   1c                   when      I37opz  = '1'
031900071030     c                   eval      V1Topz  = $Fun(1)
032000071030w   1c                   when      I37opz  = '2'
032100071030     c                   eval      V1Topz  = $Fun(2)
032200071030w   1c                   when      I37opz  = '3'
032300071030     c                   eval      V1Topz  = $Fun(3)
032400071030w   1c                   when      I37opz  = '4'
032500071030     c                   eval      V1Topz  = $Fun(4)
032600071030w   1c                   when      I37opz  = '5'
032700071030     c                   eval      V1Topz  = $Fun(5)
032800071030w   1c                   when      I37opz  = '7'
032900071030     c                   eval      V1Topz  = $Fun(6)
033000071030w   1c                   when      I37opz  = 'A'
033100071030     c                   eval      V1Topz  = $Fun(1)
033200071030w   1c                   when      I37opz  = 'C'
033300071030     c                   eval      V1Topz  = $Fun(3)
033400070801x   1c                   other
033500071030     c                   eval      DA01    = C_ND
033600070801e   1c                   endsl
033700070801      *
033800070801      * Impostazione automatica della filiale di gestione
033900070801sel 1c                   select
034000070801      * - ricevuta
034100070801w   1c                   when           I37opz  <> *blanks
034200070801     c                             and  I37fgs  <> *zeros
034300070801      * - impostazione di default per filiali di 2° livello
034400070801w   1c                   when           DUTlpo   = '2'
034500070801     c                              or  DUTlpo   = *blanks
034600071030     c                   eval      I37fgs  = DUTpou
034700070801      * - impostazione di default per filiali di 1° livello
034800070801x   1c                   other
034900071030     c                   eval      I37fgs  = DUTtfp
035000070801e   1c                   endsl
035100121130
035200121130      /free
035300121130         //?Verifico se anagrafica ritiri fissi già richiamata
035400121130         //?in questo caso non abilito il tasto funzione F2=Ritiro Fissi
035500121130         //?per evitare la chiamata ricorsiva
035600121130         wGiaChiamato = *off;
035700121130         XRTVSTKR (pStack:pStartPoint:pEntries);
035800121130         dsStack = pStack;
035900121130         IF  %lookup('FIOR02R   ':skStack) > 0;
036000121130           wgiachiamato = *on;
036100121130         ENDIF;
036200130731         //?Se richiamato da nuova interrogazione anagrafica ritiri
036300130731         //?non abilito F2 in quanto non serve e se lo lascio abilitato
036400130731         //?cliente, in queso caso non emette niente perchè F2 va per FGS
036500130731         IF not wGiaChiamato;
036600130731           XRTVSTKR (pStack:pStartPoint:pEntries);
036700130731           dsStack = pStack;
036800130731           IF  %lookup('FIOR81R   ':skStack) > 0;
036900130731             wgiachiamato = *on;
037000130731           ENDIF;
037100130731         ENDIF;
037200121130      /end-free
037300070801      *
037400070801     c                   ENDSR
037500070801
037600070801      *---------------------------------------------------------------*
037700070801      *?Reperimento Dati del job (Utente/Operativi)                  ?*
037800070801      *---------------------------------------------------------------*
037900070801     c     DatiJob       BEGSR
038000070801      *
038100070801     c     *dtaara       define    §azute        azuteds
038200070801     c     *dtaara       define    §datiute      ddatiute
038300070801      *
038400070801     c                   in(E)     *dtaara
038500070801     c                   IF        %ERROR or RSUT = *blanks
038600070801     c                   clear                   Tibs34Ds
038700070801     c                   call      'TIBS34R'
038800070801     c                   parm                    Tibs34Ds
038900070801     c                   in        *dtaara
039000070801     c                   ENDIF
039100070801      *
039200070801     c                   ENDSR
039300070801
039400070801      *---------------------------------------------------------------*
039500070801      *?Gestione videata D01                                         ?*
039600070801      *---------------------------------------------------------------*
039700070801     c     GesD01        BEGSR
039800070801      *
039900070801      * Inizializzazione videata
040000070801if  1c                   if        $InzD01 = *on
040100070801     c                   exsr      InzD01
040200070801     c                   eval      $InzD01 = (*in90 = *on)
040300070801e   1c                   endif
040400070801      *
040500070801      * Scrittura di testata e di riga tasti funzionali abilitati
040600070801     c                   write     OR37T01
040700070801     c                   write     OR37Z01
040800070801      *
040900070801      * Emissione videata
041000070801if  1c                   if             I37opz <> '4'
041100070801     c                             and  I37opz <> '5'
041200070801     c                   exfmt     OR37D01
041300070801x   1c                   else
041400070801     c                   write     OR37D01
041500070801     c                   exfmt     PROTECT
041600070801e   1c                   endif
041700070801     c                   setoff                                       28  90
041800070801     c                   clear                   V1Dmsg
041900070801      *
042000070801sel 1c                   SELECT
042100070802      * F2=Anagrafica ritiri fissi
042200070802w   1c                   WHEN      *inKB
042300070802     c                   exsr      F02D01
042400070801      * F3=Fine
042500070801w   1c                   WHEN      *inKC
042600070801     c                   exsr      F03D01
042700070801      * F12=Ritorno
042800070801w   1c                   WHEN      *inKL
042900070801     c                   exsr      F12D01
043000160713      * F19=DatiVariati
043100160713w   1c                   WHEN      *inKT
043200160713     c                   exsr      F19D01
043300070806      * Visualizzazione (nessun controllo, ma decodifiche)
043400070801w   1c                   WHEN      I37opz  = '5'
043500070802      * Inserimento/Modifica/Ripristino o Annullamento
043600070801x   1c                   OTHER
043700070802      * - Annullamento
043800070802if  2c                   if             I37opz  = '4'
043900070802     c                             or   *inKQ
044000070802     c                   exsr      Ctr_Annull
044100070802x   2c                   else
044200070802      * - Inserimento/Modifica/Ripristino
044300070801     c                   exsr      CtrD01
044400070802e   2c                   endif
044500070809if  2c                   if        NOT  *in90
044600070809      * - F5=Ripristino
044700070809     c                             and (*inKE
044800070802      * - F6=Conferma
044900070809     c                              or  *inKF
045000070802      * - F16=Annullamento
045100070802     c                              or  *inKQ)
045200070801     c                   exsr      F06D01
045300070801e   2c                   endif
045400070801e   1c                   ENDSL
045500070801      *
045600070801     c                   ENDSR
045700070801
045800070801      *---------------------------------------------------------------*
045900070801      *?Inizializzazione videata D01                                 ?*
046000070801      *---------------------------------------------------------------*
046100070801     c     InzD01        BEGSR
046200070801      *
046300070801     c                   clear                   OR37D01
046400070801     c                   clear                   FNACR000
046500070801     c                   clear                   FNACR100
046600070903     c                   clear                   SAVpocgi
046700070910     c                   clear                   SAVcgi
046800070910     c                   clear                   SAVcar
046900070910     c                   clear                   SAVlor
047000070910     c                   clear                   SAVprr
047100160713     c                   clear                   savfca
047200160713     c                   clear                   savmca
047300160713     c                   clear                   savsca
047400071030     c                   eval      SAVksc  = *hival
047500140113     c                   eval      *in41 = *off
047600140114
047700140114      /free
047800140114         //?Utente EDP
047900140114         *in39 = *off;
048000140114         IF  %subst(knmus:1:3) = 'EDP';
048100140114           *in39 = *on;
048200140114         ENDIF;
048300151012
048400151012         // -?Impostazione chiave d'accesso al file FNACRE1L per il?
048500160317         //  ?reperimento dei dati Alert Conferma Accettazione ORM?
048600151012         clear  keyFNACRE1;
048700151012         k_ACREcro = I37cro;
048800140114      /end-free
048900140114
049000070801      *
049100151012      * Reperimento dati
049200151012      *
049300070801sel 1c                   SELECT
049400070802      *
049500070801      * - Se richiesto inserimento:
049600070801      *   il controllo di effettiva inesistenza verrà eseguito nella
049700070801      *   subr. CTRD01.
049800070809w   1c                   WHEN           I37opz = '1'
049900070809     c                             or   I37opz = 'A'
050000071030     c                   eval      V2Ccd1  = I37fgs
050100130417     c                   eval      V2Ctcr  = 'R'
050200150909     c                   eval      V2Cpoa  = I37fgs
050300070802      *
050400070801      * - Se richiesta modifica/annullamento/ripristino:
050500070801      *   il record da aggiornare va allocato
050600070801w   1c                   WHEN           I37opz = '2'
050700070801     c                             or   I37opz = '4'
050800070801     c                             or   I37opz = '7'
050900070801     c     I37cro        chain     FNACR000
051000070801if  2c                   if        NOT  %found(FNACR01L)
051100070801     c                   seton                                        28  90
051200071030     c                   eval      V1Dmsg  = $Msg(2)
051300070801e   2c                   endif
051400071030     c     k02acr13      setll     FNACR100
051500070802      *
051600070801      * - Se richiesta visualizzazione/copia
051700070801      *   il record da visualizzare NON va allocato
051800070801w   1c                   WHEN           I37opz = '3'
051900070801     c                             or   I37opz = '5'
052000070809     c                             or   I37opz = 'C'
052100070801     c     I37cro        chain(n)  FNACR000
052200070801if  2c                   if        NOT  %found(FNACR01L)
052300070801     c                   seton                                        28  90
052400071030     c                   eval      V1Dmsg  = $Msg(2)
052500070801e   2c                   endif
052600071030     c     k02acr13      setll     FNACR100
052700070802      *
052800070801e   1c                   ENDSL
052900070801      *
053000070809      * Eventuali forzature
053100070802if  1c                   if             %found(FNACR01L)
053200071030     c                             and  I37opz  = '2'
053300070801     c                             and  I37fgs <> *zeros
053400150909     c                             and  I37fgs <>
053500150909     c                             %dec(%subst(%editc(ACRcro:'X'):1:3):3:0)
053600071030     c                   eval      I37opz  = '5'
053700070802e   1c                   endif
053800070801      *
053900070801      * Impostazione campi chiave a video
054000070809sel 1c                   select
054100070809w   1c                   when           I37opz  = '1'
054200071030     c                   eval      V2Ccd1    = I37fgs
054300071030     c                   eval      V2Cfcc    = 'S'
054400150909     c                   eval      V2Cpoa    = I37fgs
054500070809w   1c                   when           I37opz  = 'A'
054600070809     c                   movel     I37cro        ds_CRO
054700071030     c                   eval      V2Ccd1    = dsCRO1
054800071030     c                   eval      V2Ccd2    = dsCRO2
054900071030     c                   eval      V2Ccd3    = dsCRO3
055000071030     c                   eval      V2Cksc    = I37ksc
055100071030     c                   eval      V2Cfcc    = 'S'
055200150909     c                   eval      V2Cpoa    = V2Ccd1
055300070809x   1c                   other
055400150910     c                   movel     I37cro        ds_CRO
055500071030     c                   eval      V2Ccd1    = dsCRO1
055600071030     c                   eval      V2Ccd2    = dsCRO2
055700071030     c                   eval      V2Ccd3    = dsCRO3
055800070809e   1c                   endsl
055900070801      *
056000070801      * Impostazione altri campi a video
056100070802if  1c                   IF            (I37opz = '2'
056200070801     c                             or   I37opz = '3'
056300070801     c                             or   I37opz = '4'
056400070801     c                             or   I37opz = '5'
056500070809     c                             or   I37opz = '7'
056600070809     c                             or   I37opz = 'C')
056700070801     c                             and  %found(FNACR01L)
056800071030     c                   eval      V2Crsr    = ACRrsr
056900071030     c                   eval      V2Cinr    = ACRinr
057000071030     c                   eval      V2Ccar    = ACRcar
057100071030     c                   eval      V2Ccar    = ACRcar
057200071030     c                   eval      V2Clor    = ACRlor
057300071030     c                   eval      V2Cprr    = ACRprr
057400071030     c                   eval      V2Cnar    = ACRnar
057500071030if  2c                   IF        %equal(FNACR13L)
057600071030if  3c                   if             I37opz = '3'
057700071030     c                             or   I37opz = '5'
057800071030     c                             or   I37opz = 'C'
057900071030     c     k02acr13      reade(n)  FNACR100
058000071030x   3c                   else
058100071030     c     k02acr13      reade     FNACR100
058200071030e   3c                   endif
058300071030do  3c                   DoW       NOT  %eof(FNACR13L)
058400071030sel 4c                   select
058500071030w   4c                   when      ACR1atb  <> ACRatb
058600071030w   4c                   when      ACR1ain   = '='
058700071030     c                   eval      V2Cpocgi1 = ACR1pocgi
058800071030     c                   eval      V2Ccgi1   = ACR1cgi
058900071030w   4c                   when      ACR1ain   = '>'
059000071030     c                   eval      V2Cpocgi2 = ACR1pocgi
059100071030     c                   eval      V2Ccgi2   = ACR1cgi
059200071030w   4c                   when      ACR1ain   = '<'
059300071030     c                   eval      V2Cpocgi3 = ACR1pocgi
059400071030     c                   eval      V2Ccgi3   = ACR1cgi
059500071030e   4c                   endsl
059600071030if  4c                   if             I37opz = '3'
059700071030     c                             or   I37opz = '5'
059800071030     c                             or   I37opz = 'C'
059900071030     c     k02acr13      reade(n)  FNACR100
060000071030x   4c                   else
060100071030     c     k02acr13      reade     FNACR100
060200071030e   4c                   endif
060300071030e   3c                   EndDo
060400071030e   2c                   ENDIF
060500070801     c                   movel     ACRcco        ds_CRO
060600071030     c                   eval      V2Cco1    = dsCRO1
060700071030     c                   eval      V2Cco2    = dsCRO2
060800071030     c                   eval      V2Cco3    = dsCRO3
060900071030     c                   eval      V2Cksc    = ACRksc
061000091009if  2c                   if        ACRccc   = 999
061100091009     c                   clear                   v2cctr
061200091009     c                   else
061300091009     c                   move      ACRccc        V2Cctr
061400070801e   2c                   endif
061500071030     c                   eval      V2Ctcr    = ACRtcr
061600071030if  2c                   if        ACRfcc    = 'N'
061700071030     c                   eval      V2Cfcc    = ACRfcc
061800070802x   2c                   else
061900071030     c                   eval      V2Cfcc    = 'S'
062000070802e   2c                   endif
062100071030     c                   eval      V2Corr    = ACRorr
062200071030     c                   eval      V2Cper    = ACRper
062300071030     c                   eval      V2Ctel    = ACRtel
062400131113     c                   eval      dACR01    = ACRmai
062500140114     c                   eval      sav_§ACRstcr = dACR01.§ACRstcr
062600140114     c                   eval      sav_§ACRctcr = dACR01.§ACrctcr
062700131113      * - Valorizzazione degli orari solo SE effettivamente numerici
062800131113     c                   if        %check( c_Digits : %subst(
062900131113     c                                     ACRmai : 1
063000131113     c                                            : %len(dACR01.§ACRoa1) )
063100131113     c                                   ) = *zero
063200131113     c                   eval      V2ChmAmDa = %int( dACR01.§ACRoa1 )
063300131113     c                   endif
063400131113     c                   if        %check( c_Digits : %subst(
063500131113     c                                     ACRmai : %len(dACR01.§ACRoa1) + 1
063600131113     c                                            : %len(dACR01.§ACRoa2) )
063700131113     c                                   ) = *zero
063800131113     c                   eval      V2ChmAmA  = %int( dACR01.§ACRoa2 )
063900131113     c                   endif
064000131113     c                   if        %check( c_Digits : %subst(
064100131113     c                                     ACRmai : %len(dACR01.§ACRoa1) +
064200131113     c                                              %len(dACR01.§ACRoa2) + 1
064300131113     c                                            : %len(dACR01.§ACRoa3) )
064400131113     c                                   ) = *zero
064500131113     c                   eval      V2ChmPmDa = %int( dACR01.§ACRoa3 )
064600131113     c                   endif
064700131113     c                   if        %check( c_Digits : %subst(
064800131113     c                                     ACRmai : %len(dACR01.§ACRoa1) +
064900131113     c                                              %len(dACR01.§ACRoa2) +
065000131113     c                                              %len(dACR01.§ACRoa3) + 1
065100131113     c                                            : %len(dACR01.§ACRoa4) )
065200131113     c                                   ) = *zero
065300131113     c                   eval      V2ChmPmA  = %int( dACR01.§ACRoa4 )
065400131113     c                   endif
065500071030     c                   eval      V2Cfcl    = ACRfcl
065600071030     c                   eval      V2Cfbn    = ACRfbn
065700071030     c                   eval      V2Cfpk    = ACRfpk
065800071030     c                   eval      V2Cfmc    = ACRfmc
065900071030     c                   eval      V2Cfbl    = ACRfbl
066000071030     c                   eval      V2Cfmt    = ACRfmt
066100071030     c                   eval      V2Cfat    = ACRfat
066200071030     c                   eval      V2Cspi    = ACRspi
066300071030     c                   eval      V2Cntm    = ACRntm
066400071030     c                   eval      V2Cno1    = ACRno1
066500071030     c                   eval      V2Cno2    = ACRno2
066600140113      /free
066700140113       //?Se frequenza ritiro impostata in automatico da pgm contabili
066800140113       //?non posso toccarla quindi la proteggo
066900140113         IF  dACR01.§ACRctcr = 'C';
067000140113           *in40 = *on;
067100140113         ENDIF;
067200140113       //?Imposto la frequenza ritiro salvata
067300140113         V2Cstcr = dACR01.§ACRstcr;
067400150909       //?Filiale ritiro forzata
067500150909         V2Cpoa = ACRpoa;
067600160317       // -?Flag x Conferma Accettazione ORM?
067700151104         V2Cfca = dacr01.§ACRfmar;
067800160713         savfca = dacr01.§ACRfmar;
067900160317       // -?E-Mail Conferma Accettazione ORM?
068000160317         k_ACREtrc = 'MC';
068100160317         chain(n)  %kds( keyFNACRE1 )  FNACRE00;
068200160317         IF  %found(FNACRE1L);
068300151012         V2Cmca = ACREdati;
068400160713         savmca = ACREdati;
068500160317         ENDIF;
068600160317       // -?E-Mail Conferma Accettazione ORM?
068700160317         k_ACREtrc = 'SC';
068800160317         chain(n)  %kds( keyFNACRE1 )  FNACRE00;
068900160317         IF  %found(FNACRE1L);
069000160317           V2Csca = ACREdati;
069100160713           savsca = ACREdati;
069200160317         ENDIF;
069300140113      /end-free
069400140113
069500070802e   1c                   ENDIF
069600070801      *
069700070801      * Abilitazione tasti funzionali:
069800070801      * - F2=Anagr.Fissi
069900070802     c                   eval      *in02   = *on
070000121130      /free
070100121130         //?Se anagrafica ritiri fissi già richiamata non abilito F2
070200121130           IF  wGiaChiamato;
070300121130             *in02 = *off;
070400121130           ENDIF;
070500121130      /end-free
070600070801      * - F3=Fine
070700070809     c                   eval      *in03   = (I37opz <> 'A'    and
070800070809     c                                        I37opz <> 'C')
070900070801      * - F5=Riattivazione
071000070809     c                   eval      *in05   = (I37opz  = '2'    and
071100070801     c                                        %found(FNACR01L) and
071200070801     c                                        ACRatb <> *blanks)
071300070801      * - F6=Conferma
071400070809     c                   eval      *in06   = (I37opz <> '5')
071500070828      * - F7=Interrogazione Giri di Ritiro
071600070828     c                   eval      *in07   = (I37opz <> '4'    and
071700070828     c                                        I37opz <> '5')
071800070801      * - F16=Annullamento
071900070809     c                   eval      *in16   = (I37opz  = '2'    and
072000070801     c                                        %found(FNACR01L) and
072100140723     c                                        ACRatb  = *blanks and
072200140807     c                                        (V2Ccd3 > 0 or
072300140807     c                                        (V2Ccd3 = 0 and
072400140807     c                                        (V2Ccd2 = 8888 or
072500140807     c                                         V2Ccd2 = 9999))))
072600070809      * - F12=Ritorno
072700070809     c                   eval      *in12   = (I37opz <> 'A'    and
072800070809     c                                        I37opz <> 'C')
072900160713      /free
073000160713       //?F19=Variazione dati Alert Conferma Ritiro
073100160713         *in19 = *off;
073200160713         clear dOREana;
073300160713         k_ACREtrc = 'AA';
073400160713         chain(n)  %kds( keyFNACRE1 )  FNACRE00;
073500160713         IF  %found(FNACRE1L) and *in39;
073600160713           dOREana = ACREdati;
073700160713           *in19 = *on;
073800160713         ENDIF;
073900160713      /end-free
074000070801      *
074100070801      * Protezione campo chiave a video
074200070802     c                   eval      *in41   = (I37opz <> '1'    and
074300070809     c                                        I37opz <> '3'    and
074400070809     c                                        I37opz <> 'A'    and
074500070809     c                                        I37opz <> 'C')
074600070802      * Protezione codice P.d.C.  e Tariffa
074700070801     c                   eval      *in42   = (I37ksc <> *zeros)
074800070802      * Evidenziazione record annullato
074900070801     c                   eval      *in43   = (ACRatb <> *blanks)
075000070802      *
075100070802      * Impostazione F5=Annullamento o F16=Ripristino
075200070802sel 1c                   select
075300070802w   1c                   when      *in05
075400070802     c                   eval      Z1Df16  = 'F5=Riattivazione'
075500070802w   1c                   when      *in16
075600070802     c                   eval      Z1Df16  = 'F16=Annullamento'
075700070802e   1c                   endsl
075800070911      *
075900070911      * Decodifiche
076000070911if  1c                   if             %found(FNACR01L)
076100070911     c                             or   I37opz  = 'A'
076200110121     c                   eval      wdecod='S'
076300070911     c                   exsr      CtrD01
076400110121     c                   eval      wdecod=' '
076500070911     c                   setoff                                       28  90
076600070911     c                   clear                   V1Dmsg
076700070911     c                   movea     *zeros        *in(50)
076800070911e   1c                   endif
076900070801      *
077000070801     c                   ENDSR
077100070801
077200070801      *---------------------------------------------------------------*
077300070801      *?Gestione tasto funzionale F2 da videata D01                  ?*
077400070801      *?F2=Gestione anagrafica ritiri fissi                          ?*
077500070801      *---------------------------------------------------------------*
077600070801     c     F02D01        BEGSR
077700070801      *
077800070801     c                   clear                   FIOR02ds
077900070801if  1c                   if        I37opz  = '5'
078000070801     c                   eval      I02ric  = '1'
078100070801x   1c                   else
078200070801     c                   eval      I02ric  = '2'
078300070801e   1c                   endif
078400070801     c                   eval      I02cro  = V2Ccd1 * 10000000
078500070801     c                                     + V2Ccd2 * 1000
078600070801     c                                     + V2Ccd3
078700070801     c                   eval      I02poa  = I37fgs
078800070801     c                   call      'FIOR02R'
078900070801     c                   parm                    KPJBA
079000070801     c                   parm                    FIOR02ds
079100070801      *
079200070828if  1c                   if        O02err <> *blanks
079300070801     c                   seton                                        28  90
079400070801     c                   eval      V1Dmsg  = O02msg
079500070801e   1c                   endif
079600070801      *
079700070801     c                   ENDSR
079800070801
079900070801      *---------------------------------------------------------------*
080000070801      *?Gestione tasto funzionale F3 da videata D01                  ?*
080100070801      *?F3=Fine                                                      ?*
080200070801      *---------------------------------------------------------------*
080300070801     c     F03D01        BEGSR
080400070801      *
080500070801      * Disallocazione del record
080600070801if  1c                   if             I37opz = '2'
080700070801     c                             or   I37opz = '4'
080800070801     c                             or   I37opz = '7'
080900070801     c                   UNLOCK    FNACR01L
081000071030     c                   UNLOCK    FNACR13L
081100070801e   1c                   endif
081200070802      *
081300070803      * Impostazione avviso di ritorno con F3
081400070803     c                   eval      O37ret  = *on
081500070801      *
081600070801      * Chiusura del programma
081700070801     c                   eval      $Fine   = *on
081800070801      *
081900070801     c                   ENDSR
082000070801
082100070801      *---------------------------------------------------------------*
082200070801      *?Gestione tasto funzionale F6 da videata D01                  ?*
082300070801      *?F6=Conferma                                                  ?*
082400070801      *---------------------------------------------------------------*
082500070801     c     F06D01        BEGSR
082600070801      *
082700070801      * Aggiornamento record
082800070801     c                   exsr      Upd_FNACR
082900070801      *
083000070801      * Chiusura del programma
083100070801if  1c                   if        NOT  *in90
083200070910if  2c                   if             I37opz = '1'
083300070910     c                             or   I37opz = '3'
083400070910     c                             or   I37opz = 'A'
083500070910     c                             or   I37opz = 'C'
083600070910     c                   eval      O37cro  = I37cro
083700070910e   2c                   endif
083800070801     c                   eval      $Fine   = *on
083900070801e   1c                   endif
084000070801      *
084100070801     c                   ENDSR
084200070801
084300070801      *---------------------------------------------------------------*
084400070801      *?Gestione tasto funzionale F12 da videata D01                 ?*
084500070801      *?F12=Ritorno                                                  ?*
084600070801      *---------------------------------------------------------------*
084700070801     c     F12D01        BEGSR
084800070801      *
084900070801      * Disallocazione del record
085000070801if  1c                   if             I37opz = '2'
085100070801     c                             or   I37opz = '4'
085200070801     c                             or   I37opz = '7'
085300070801     c                   UNLOCK    FNACR01L
085400071030     c                   UNLOCK    FNACR13L
085500070801e   1c                   endif
085600070803      *
085700070803      * Impostazione avviso di ritorno con F12
085800070803     c                   eval      O37ret  = '2'
085900070801      *
086000070801      * Chiusura del programma
086100070801     c                   eval      $Fine   = *on
086200070801      *
086300070801     c                   ENDSR
086400160713
086500160713      /free
086600160713       //--------------------------------------------------------------
086700160713       //?Gestione tasto funzionale F19 da videata D01
086800160713       //?F19=Variazione dati Alert
086900160713       //--------------------------------------------------------------
087000160713       BEGSR F19D01;
087100160713
087200160713       //?Inizializzazione videata
087300160713         exsr InzW03;
087400160713
087500160713         DOW not FineW03;
087600160713
087700160713         //?Emissione videata
087800160713           exfmt  OR37W03;
087900160713
088000160713         //?- F12=Ritorno
088100160713           IF  *inkl;
088200160713             FineW03 = *on;
088300160713           ENDIF;
088400160713
088500160713         ENDDO;
088600160713
088700160713       ENDSR;
088800160713
088900160713       //--------------------------------------------------------------
089000160713       //?Inizializzazione Videata W03.
089100160713       //--------------------------------------------------------------
089200160713       BEGSR InzW03;
089300160920
089400160920         FineW03 = *off;
089500160713
089600160713         clear  OR37W03;
089700160713         Data_EUR = %date(§OREdtv:*ISO);
089800160713         W03data = %dec(Data_EUR);
089900160713         W03ora  = §OREorv;
090000160713         IF  §OREtipo = 'I';
090100160713           W03who = 'Cliente: ' + %trim(§OREute);
090200160713           W03who = %trim(W03who) + ' IP: ' + %trim(§OREjob);
090300160713         ELSE;
090400160713           W03who = 'Utente: ' + %trim(§OREute);
090500160713           W03who = %trim(W03who) + ' Video: ' + %trim(§OREjob);
090600160713         ENDIF;
090700160713
090800160713       ENDSR;
090900160713      /end-free
091000070911
091100070911      *---------------------------------------------------------------*
091200070911      *?Controllo e decodifica codice giro di ritiro                 ?*
091300070911      *---------------------------------------------------------------*
091400070911     c     Ctr_Giro      BEGSR
091500071030      *
091600071030      * - immessa solo la filiale (senza codice giro)
091700071030if  1c                   if             Wpocgi  <> *zeros
091800071030     c                             and  Wcgi     = *blanks
091900071105     c                   clear                   Wpocgi
092000071105     c                   clear                   WcgiD
092100071030e   1c                   endif
092200071030      *
092300071030      * - immesso solo il codice giro (senza filiale)
092400071030if  1c                   if             Wpocgi   = *zeros
092500071030     c                             and  Wcgi    <> *blanks
092600071105     c                   eval      Wpocgi   =  I37fgs
092700071030e   1c                   endif
092800071030      *
092900071030      * - controllo codice filiale del giro ritiro
093000071105      *   (filiale giro diversa dalla filiale gestione)
093100071105if  1c                   if             Wpocgi  <> I37fgs
093200071105     c                             and  Wcgi    <> *blanks
093300071105     c                   seton                                        286090
093400071030     c                   eval      V1Dmsg  = $Msg(18)
093500071030     c                   leavesr
093600071030e   1c                   endif
093700070911      *
093800071030      * - controllo giro CON data validità ("bypassabile")
093900071031if  1c                   IF             Wpocgi  <> D09ifgs
094000071031     c                             or   Wcgi    <> D09icgi
094100070911     c                   clear                   FIDG09ds
094200070911     c                   eval      D09iop0   = '001'
094300071030     c                   eval      D09ifgs   = Wpocgi
094400071030     c                   eval      D09icgi   = Wcgi
094500070911     c                   eval      D09idat   = *date
094600070911     c                   eval      D09itug   = 'R'
094700070911     c                   movel(p)  FIDG09ds      KPJBU
094800070911     c                   call      'FIDG09R'
094900070911     c                   parm                    KPJBA
095000070911     c                   movel     KPJBU         FIDG09ds
095100071105e   1c                   ENDIF
095200071105      *
095300071105sel 1c                   select
095400071105w   1c                   when      D09oerr   = *on
095500071105     c                   movel     D09odes       WcgiD
095600070911     c                   seton                                        286090
095700070911     c                   eval      V1Dmsg    = $Msg(29)
095800071112     c                   leavesr
095900071105w   1c                   when      D09ocgi  <> *blanks
096000071030     c                   movel     D09odes       WcgiD
096100071105e   1c                   endsl
096200070911      *
096300070911     c                   ENDSR
096400070801
096500070801      *---------------------------------------------------------------*
096600070801      *?Controllo videata D01                                        ?*
096700070801      *---------------------------------------------------------------*
096800070801     c     CtrD01        BEGSR
096900070801      *
097000070806     c                   movea     *zeros        *in(50)
097100070806      *
097200150909     c                   clear                   V2Dpoa
097300070802     c                   clear                   ds_CRO
097400070802     c                   eval      dsCRO1  = V2Ccd1
097500070802     c                   eval      dsCRO2  = V2Ccd2
097600070802     c                   eval      dsCRO3  = V2Ccd3
097700070802      *
097800070802      * Codice cliente ritiro
097900070802      * - Se IMMISSIONE (NON COPIA):
098000070802      *   i primi 7 caratteri del codice devono esistere in CNACO
098100070802if  1c                   if             I37opz  = '1'
098200070809     c                             or   I37opz  = 'A'
098300070802     c                   clear                   TIBS69ds
098400070802     c                   moveL     ds_CRO        I69kac
098500070809     c                   if            (I37opz  = '1'
098600070809     c                              or  I37opz  = 'A')
098700070809     c                             and  $InzD01 = *on
098800070809     c                   moveL     ds_CRO        I69kin
098900070809     c                   endif
099000070802     c                   exsr      Call_TIBS69
099100070809sel 2c                   select
099200070809w   2c                   when      *in90
099300070802     c                   eval      *in51   = *on
099400070802     c                   eval      V1Dmsg  = $Msg(7)
099500070802     c                   leavesr
099600070809x   2c                   when          (I37opz  = '1'
099700070809     c                              or  I37opz  = 'A')
099800070809     c                             and  $InzD01 = *on
099900070809     c                   movel     ACOrag        V2Crsr
100000070809     c                   movel     INDvia        V2Cinr
100100070809     c                   movel     INDcae        V2Ccar
100200070809     c                   movel     INDcit        V2Clor
100300070809     c                   movel     INDprv        V2Cprr
100400070809     c                   movel     INDsta        V2Cnar
100500080421     c                   movel     INDtel        V2Ctel
100600070809e   2c                   endsl
100700070802e   1c                   endif
100800070802      * - Se IMMISSIONE O COPIA:
100900070802if  1c                   if             I37opz  = '1'
101000070802     c                             or   I37opz  = '3'
101100070809     c                             or   I37opz  = 'A'
101200070809     c                             or   I37opz  = 'C'
101300070802      * - - controllo completo inserimento
101400070802if  2c                   IF        V2Ccd2  = *zeros
101500070802     c                   seton                                        285190
101600070802     c                   eval      V1Dmsg  = $Msg(8)
101700070802     c                   leavesr
101800070802      * - - controllo se già esistente
101900070802x   2C                   ELSE
102000070802     c                   move      ds_CRO        ACRcro
102100070802     c     ACRcro        chain(n)  FNACR000
102200070802if  3c                   if        %found(FNACR01L)
102300070802     c                   seton                                        285190
102400070802     c                   eval      V1Dmsg  = $Msg(9)
102500070802     c                   leavesr
102600070802e   3c                   endif
102700070802e   2c                   ENDIF
102800140723      /free
102900140723       //?Se NON esiste il luogo '000' devo per forza creare quello
103000140723         IF  dscro3 <> 000;
103100140723           clear ACRcro;
103200140806           ACRcro = (dscro1 * 10000000) + (dscro2 * 1000);
103300140723           chain ACRcro FNACR01L;
103400140723           IF  not %found(FNACR01L);
103500140723             *in28 = *on;
103600140723             *in51 = *on;
103700140723             *in90 = *on;
103800140723             V1Dmsg = $msg(32);
103900140723             leavesr;
104000140723           ENDIF;
104100140723         ENDIF;
104200140723      /end-free
104300070802e   1c                   endif
104400070802      *
104500070802      * Ragione sociale, Indirizzo, Località, CAP, Prov., Naz.
104600110121     c                   if        wdecod=' '
104700070802     c                   clear                   FNLV14ds
104800070802     c                   eval      I14dri  = *date
104900070802     c                   movel     V2Crsr        I14rsc
105000070802     c                   movel     V2Cinr        I14ind
105100070802     c                   movel     V2Clor        E14loc
105200070802     c                   movel     V2Cprr        E14prv
105300070802     c                   movel     V2Ccar        E14cap
105400070802     c                   movel     V2Cnar        E14nar
105500070802     c                   call      'FNLV14R'
105600070802     c                   parm                    KPJBA
105700070802     c                   parm                    FNLV14ds
105800070802     c                   movel     I14ind        V2Cinr
105900070802     c                   movel     E14loc        V2Clor
106000070802     c                   movel     E14prv        V2Cprr
106100070802     c                   movel     E14cap        V2Ccar
106200070802     c                   movel     E14nar        V2Cnar
106300070802if  1c                   if        O14msg <> *blanks
106400070802     c                   eval      *in53   = (O14err = '1')                     Ragione Sociale
106500070802     c                   eval      *in54   = (O14err = '2')                     Indirizzo
106600070802     c                   eval      *in55   = (O14err = '5')                     CAP
106700070802     c                   eval      *in56   = (O14err = '3')                     Località
106800070802     c                   eval      *in57   = (O14err = '4')                     Provincia
106900070802     c                   eval      *in58   = (O14err = '6')                     Nazione
107000070802     c                   seton                                        28  90
107100070802     c                   eval      V1Dmsg  = O14msg
107200070802     c                   leavesr
107300070802e   1c                   endif
107400070802      * - controllo specifico CAP
107500070802     c                   exsr      Ctr_CAP
107600070802if  1c                   if        *in90
107700070802     c                   leavesr
107800070802e   1c                   endif
107900110121     c                   endif
108000070801      *
108100070802      * Giro di ritiro
108200071105do  1c                   DO        C_MaxCGI      wNrCGI
108300071030      *
108400071031     c                   clear                   WcgiD
108500071030sel 2c                   select
108600071030w   2c                   when      wNrCGI  = 1
108700071030     c                   eval      Wpocgi  = V2Cpocgi1
108800071030     c                   eval      Wcgi    = V2Ccgi1
108900071030w   2c                   when      wNrCGI  = 2
109000071030     c                   eval      Wpocgi  = V2Cpocgi2
109100071030     c                   eval      Wcgi    = V2Ccgi2
109200071030w   2c                   when      wNrCGI  = 3
109300071030     c                   eval      Wpocgi  = V2Cpocgi3
109400071030     c                   eval      Wcgi    = V2Ccgi3
109500071030e   2c                   endsl
109600070911      *
109700071030      * - controllo codici giri ritiro
109800071031sel 2c                   Select
109900071105w   2c                   When      Wcgi    = *blanks
110000071105     c                   clear                   Wpocgi
110100071031      *   - interrogazione giri di ritiro
110200071031w   2c                   When      %scan('?' : %trimr(Wcgi)) > *zeros
110300071031     c                   exsr      Search_Giro
110400071030      *   - controllo codici giri ritiro
110500071031x   2c                   Other
110600070911     c                   exsr      Ctr_Giro
110700071031e   2c                   EndSl
110800071031      *
110900071031sel 2c                   Select
111000071031w   2c                   When      wNrCGI  = 1
111100071030     c                   eval      V2Cpocgi1 = Wpocgi
111200071031     c                   eval      V2Ccgi1   = Wcgi
111300071030     c                   eval      V2Dcgi1   = WcgiD
111400071031w   2c                   When      wNrCGI  = 2
111500071030     c                   eval      V2Cpocgi2 = Wpocgi
111600071031     c                   eval      V2Ccgi2   = Wcgi
111700071030     c                   eval      V2Dcgi2   = WcgiD
111800071105     c                   eval      *in76   = (*in59 = *on)
111900071030     c                   eval      *in77   = (*in60 = *on)
112000071030     c                   eval      *in59   = *off
112100071030     c                   eval      *in60   = *off
112200071031w   2c                   When      wNrCGI  = 3
112300071030     c                   eval      V2Cpocgi3 = Wpocgi
112400071031     c                   eval      V2Ccgi3   = Wcgi
112500071030     c                   eval      V2Dcgi3   = WcgiD
112600071105     c                   eval      *in78   = (*in59 = *on)
112700071030     c                   eval      *in79   = (*in60 = *on)
112800071030     c                   eval      *in59   = *off
112900071030     c                   eval      *in60   = *off
113000071031e   2c                   EndSl
113100071031if  2c                   if        *in90
113200070911     c                   leavesr
113300071031e   2c                   endif
113400071030      *
113500071030e   1c                   ENDDO
113600070802      *
113700070802      * Codice cliente Ordinante
113800070802     c                   clear                   V2Ddeo
113900070802if  1c                   IF             V2Cco1 <> *zeros
114000070802     c                             or   V2Cco2 <> *zeros
114100070802     c                             or   V2Cco3 <> *zeros
114200070802      * - Tipo anagrafica "R" = cliente Ritiro
114300070809     c                   eval      ACRcro  = V2Cco1 * 10000000
114400070809     c                                     + V2Cco2 * 1000
114500070809     c                                     + V2Cco3
114600070802     c     ACRcro        chain(n)  FNACR000
114700071030if  2c                   if             %found(FNACR01L)
114800070802     c                             and  ACRatb  = *blanks
114900070802     c                   movel     ACRrsr        V2Ddeo
115000071030x   2c                   else
115100070802     c                   seton                                        286190
115200070802     c                   eval      V1Dmsg  = $Msg(20)
115300070802     c                   leavesr
115400071030e   2c                   endif
115500071030if  2c                   if             V2Cco1  = V2Ccd1
115600070802     c                             and  V2Cco2  = V2Ccd2
115700070802     c                             and  V2Cco3  = V2Ccd3
115800070802     c                   seton                                        286190
115900070802     c                   eval      V1Dmsg  = $Msg(20)
116000070802     c                   leavesr
116100071030e   2c                   endif
116200070802      *
116300070802e   1c                   ENDIF
116400070802      *
116500070802      * Codice P.d.C.
116600070802     c                   clear                   V2Dksc
116700070802if  1c                   IF        V2Cksc <> *zeros
116800070802     c                   clear                   TIBS69ds
116900070802     c                   eval      I69kac  = V2Cksc
117000070802     c                   exsr      Call_TIBS69
117100070802if  2c                   if        *in90
117200070802     c                   seton                                        286290
117300070802     c                   eval      V1Dmsg  = $Msg(22)
117400070802     c                   leavesr
117500070802e   2c                   endif
117600070802     c                   clear                   Og143
117700070802     c                   moveL     V2Cksc        ORGfil
117800070802     c     ORGfil        chain     AZORG
117900070802if  2c                   if        %found(AZORG01L)
118000070802     c                   eval      Og143   = ORGde3
118100070802e   2c                   endif
118200070802if  2c                   if             §OGntw  = 'LOG'
118300070802     c                             or   §OGntw  = 'XXX'
118400070802     c                   seton                                        286290
118500070802     c                   eval      V1Dmsg  = $Msg(22)
118600070802     c                   leavesr
118700070802e   2c                   endif
118800070802     c                   movel     ACOrag        V2Dksc
118900070802e   1c                   ENDIF
119000070802      *
119100070802      * Tariffa
119200091009if  1c                   IF        V2Cctr <> *blanks
119300070802if  2c                   if        V2Cksc  = *zeros
119400070802     c                   seton                                        286390
119500070802     c                   eval      V1Dmsg  = $Msg(23)
119600070802     c                   leavesr
119700070802e   2c                   endif
119800070802      * - verifica esistenza tariffe per il cliente
119900070802     c                   eval      $Tam    = *off
120000070802     c                   eval      TAMksc  = V2Cksc
120100091009     c                   move      v2cctr        TAMctr
120200070802     c     k02tam01      setgt     TNTAM000
120300070802     c     k02tam01      readpe    TNTAM000
120400070802do  1c                   dow       NOT  %eof(TNTAM01L)
120500070802if  2c                   if        TAMatb  = *blanks
120600070802     c                   eval      $Tam    = *on
120700070802     c                   leave
120800070802e   2c                   endif
120900070802     c     k02tam01      readpe    TNTAM000
121000070802e   1c                   enddo
121100070802if  1c                   if        $Tam    = *off
121200070802     c                   seton                                        286390
121300070802     c                   eval      V1Dmsg  = $Msg(24)
121400070802     c                   leavesr
121500070802e   1c                   endif
121600070802e   1c                   ENDIF
121700070802      *
121800070802      * Frequenza Ritiro
121900070802      * - F = Fisso
122000070802      * - R = Ricorrente
122100151104      * - O = No Internet
122200070802      * - M = Mai
122300151104      /free
122400151104        IF  %scan('?':V2Ctcr) > 0;
122500151104          FineTcr = *off;
122600151104          clear savfreq;
122700151104          exsr RicTcr;
122800151104          V2Ctcr = savfreq;
122900151104        ENDIF;
123000151104      /end-free
123100070802if  1c                   if             V2Ctcr <> 'F'
123200070802     c                             and  V2Ctcr <> 'R'
123300070802     c                             and  V2Ctcr <> 'O'
123400070802     c                             and  V2Ctcr <> 'M'
123500070802     c                   seton                                        286490
123600070802     c                   eval      V1Dmsg  = $Msg(25)
123700070802     c                   leavesr
123800070802e   1c                   endif
123900150909
124000150909      /free
124100150909       //?Filiale ritiro forzata
124200150909         IF  V2Cpoa = 0;
124300150909           V1Dmsg = $msg(33);
124400150909           *in28 = *on;
124500150909           *in52 = *on;
124600150909           *in90 = *on;
124700150909           leavesr;
124800150909         ENDIF;
124900150909         chain (V2Cpoa) AZORG01L;
125000150909         IF  not %found(AZORG01L) or ORGfva <> *blanks;
125100150909           V1Dmsg = $msg(33);
125200150909           *in28 = *on;
125300150909           *in52 = *on;
125400150909           *in90 = *on;
125500150909           leavesr;
125600150909         ENDIF;
125700150909         V2Dpoa = ORGdes;
125800150909      /end-free
125900070802      *
126000070802      * Ora ritiro
126100070802if  1c                   if        V2Corr <> *zeros
126200070802     c                   movel     V2Corr        ds_Time
126300070802if  2c                   if             dsHH    < 1
126400070802     c                             or   dsHH    > 24
126500070802     c                             or   dsMM    > 59
126600070802     c                   seton                                        286590
126700070802     c                   eval      V1Dmsg  = $Msg(26)
126800070802     c                   leavesr
126900070802e   2c                   endif
127000070802e   1c                   endif
127100070802      *
127200070802      * Referente: numero di telefono
127300070802if  1c                   IF        V2Ctel <> *blanks
127400070802     c                   clear                   TRUL42ds
127500070802     c                   eval      D42fax  = V2Ctel
127600070802     c                   call      'TRUL42R'
127700070802     c                   parm                    TRUL42ds
127800070802if  2c                   if        D42err  = *on
127900070802     c                   seton                                        286790
128000070802     c                   eval      V1Dmsg  = D42msg
128100070802     c                   leavesr
128200070802e   2c                   endif
128300070802e   1c                   ENDIF
128400070802      *
128500131113      * Orari Apertura
128600131113     c                   exsr      CtrlOrariApert
128700131113     c                   if        *in28
128800131113     c                   leavesr
128900131113     c                   endif
129000070802      *
129100070802      * Flags immissione
129200070802     c                   select
129300070802      * - Colli
129400070802     c                   when           V2Cfcl <> *blanks
129500070802     c                             and  V2Cfcl <> 'F'
129600070802     c                             and  V2Cfcl <> 'O'
129700070802     c                   seton                                        286990
129800070802      * - Bancali
129900070802     c                   when           V2Cfbn <> *blanks
130000070802     c                             and  V2Cfbn <> 'F'
130100070802     c                             and  V2Cfbn <> 'O'
130200070802     c                   seton                                        287090
130300070802      * - Peso   (kg)
130400070802     c                   when           V2Cfpk <> *blanks
130500070802     c                             and  V2Cfpk <> 'F'
130600070802     c                             and  V2Cfpk <> 'O'
130700070802     c                   seton                                        287190
130800070802      * - Volume (m³)
130900070802     c                   when           V2Cfmc <> *blanks
131000070802     c                             and  V2Cfmc <> 'F'
131100070802     c                             and  V2Cfmc <> 'O'
131200070802     c                   seton                                        287290
131300070802      * - Bilico
131400070802     c                   when           V2Cfbl <> *blanks
131500070802     c                             and  V2Cfbl <> 'F'
131600070802     c                             and  V2Cfbl <> 'O'
131700070802     c                   seton                                        287390
131800070802      * - Motrice
131900070802     c                   when           V2Cfmt <> *blanks
132000070802     c                             and  V2Cfmt <> 'F'
132100070802     c                             and  V2Cfmt <> 'O'
132200070802     c                   seton                                        287490
132300070802      * - Autotreno
132400070802     c                   when           V2Cfat <> *blanks
132500070802     c                             and  V2Cfat <> 'F'
132600070802     c                             and  V2Cfat <> 'O'
132700070802     c                   seton                                        287590
132800070802     c                   endsl
132900070802     c                   if        *in28  and  *in90
133000070802     c                   eval      V1Dmsg  = $Msg(27)
133100070802     c                   leavesr
133200070802     c                   endif
133300070802      *
133400070802      * Se COPIA   &   codice piano dei conti non cambiato
133500070802      * => messaggio d'avvertimento
133600070809if  1c                   if            (I37opz  = '3'
133700070809     c                              or  I37opz  = 'C')
133800070910     c                             and (ACRksc <> *zeros
133900070910     c                             and  V2Cksc  = ACRksc
134000070910     c                             and  V2Cksc <> SAVksc)
134100070809     c                   eval      SAVksc  = V2Cksc
134200070802     c                   seton                                        286290
134300070802     c                   eval      V1Dmsg  = $Msg(28)
134400070802     c                   leavesr
134500070809e   1c                   endif
134600151012      *
134700151012      /free
134800151012
134900151012         // -?Controllo e-Mail Conferma Accettazione?
135000151012         If  V2Cmca <> *blank;
135100151012           clear dsEmail;
135200151012           §emlIndI = V2Cmca;
135300151012           xEmail( dsEmail );
135400151012           if §emlErrO = *on;
135500151012             *in66  = *on;
135600151012             *in28  = *on;
135700151012             *in90  = *on;
135800151012             V1Dmsg = §emlMsgO;
135900151012             leavesr;
136000151012           endif;
136100151012           V2Cmca = §emlIndO;
136200151012         EndIf;
136300160317       //?SMS
136400160317         IF  V2Csca <> *blanks;
136500160317           pInCell = %trim(V2Csca);
136600160317           clear pOutErr;
136700160317           clear pOutCell;
136800160317           IF  UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
136900160317             *in84 = *on;
137000160317             *in28 = *on;
137100160317             *in90 = *on;
137200160317             V1Dmsg = $msg(34);
137300160317             leavesr;
137400160317           ENDIF;
137500160317           IF  pOutCell <> *blanks;
137600160317             V2Csca = pOutCell;
137700160317           ENDIF;
137800160317         ENDIF;
137900151012
138000151012      /end-free
138100070801      *
138200070801     c                   ENDSR
138300131113
138400131113      /free
138500131113
138600131113       //--------------------------------------------------------------
138700131113       //?Controllo orari di apertura.                                 ?
138800131113       //--------------------------------------------------------------
138900131113       BEGSR  CtrlOrariApert;
139000131113
139100131113         // -?Richiamo *pgm per controllo orari di apertura del cliente:?
139200131113         //  ?si deve anche controllare che ci sia abbastanza spazio?
139300131113         //  ?nella Nota-1 e nella Nota-2 per poterli inserire in questi?
139400131113         //  ?campi per poi stamparli?
139500131113         If  (V2ChmAMda + V2ChmAMa + V2ChmPMda + V2ChmPMa) > *zero;
139600131113
139700131113           clear  TRUL03ds;
139800131113           TRUL03ds.i03hm1 = V2ChmAMda;
139900131113           TRUL03ds.i03hm2 = V2ChmAMa;
140000131113           TRUL03ds.i03hm3 = V2ChmPMda;
140100131113           TRUL03ds.i03hm4 = V2ChmPMa;
140200131113           TRUL03ds.i03nt1 = V2Cno1;
140300131113           TRUL03ds.i03nt2 = V2Cno2;
140400131113
140500131113           trul03r ( TRUL03ds );
140600131113
140700131113           If  TRUL03ds.o03err <> *off;
140800131113             V1Dmsg = TRUL03ds.o03msg;
140900131113             *in28 = *on;
141000131113             *in(79 + TRUL03ds.o03errpos) = *on;
141100131113           EndIf;
141200131113
141300131113         EndIf;
141400131113
141500131113       ENDSR;
141600151104
141700151104       //--------------------------------------------------------------
141800151104       //?Ricerca Frequenza Ritiro.                                    ?
141900151104       //--------------------------------------------------------------
142000151104       BEGSR RicTcr;
142100151104
142200151104       //?Inizializzazione videata
142300151104         exsr InzS02;
142400151104
142500151104       //?Visualizzazione del SFL
142600151104         *in30 = (S02nrr <= *zeros);
142700151104
142800151104         DOW not FineTcr;
142900151104
143000151104         //?Emissione videata
143100151104           exfmt  OR37C02;
143200151104           *in28 = *off;
143300151104
143400151104         //?- Enter Controllo ed esco
143500151104           exsr OpzS02;
143600151104           IF  *in28;
143700151104             iter;
143800151104           ENDIF;
143900151104           FineTcr = *on;
144000151104
144100151104         ENDDO;
144200151104
144300151104       ENDSR;
144400151104
144500151104       //--------------------------------------------------------------
144600151104       //?Inizializzazione Videata S02.
144700151104       //--------------------------------------------------------------
144800151104       BEGSR InzS02;
144900151104
145000151104       //?Pulizia subfile
145100151104         exsr PulS02;
145200151104
145300151104       //?Caricamento subfile
145400151104         exsr RieS02;
145500151104
145600151104         *in33 = *on;
145700151104
145800151104       //?Imposto il posizionamento al primo rcd
145900151104         IF  S02nrr > 0;
146000151104           W02rcd = 1;
146100151104         ELSE;
146200151104           clear W02rcd;
146300151104         ENDIF;
146400151104
146500151104         W02csr = W02rcd;
146600151104
146700151104       ENDSR;
146800151104
146900151104       //--------------------------------------------------------------
147000151104       //?Pulizia Subfile S02.
147100151104       //--------------------------------------------------------------
147200151104       BEGSR PulS02;
147300151104
147400151104       //?Pulizia subfile
147500151104         *in30 = *on;
147600151104         *in31 = *on;
147700151104         write OR37C02;
147800151104         *in31 = *off;
147900151104         *in33 = *off;
148000151104
148100151104         clear W02rcd;
148200151104         clear W02csr;
148300151104         clear S02nrr;
148400151104         clear W02msg;
148500151104
148600151104         *in28 = *off;
148700151104
148800151104       ENDSR;
148900151104
149000151104       //--------------------------------------------------------------
149100151104       //?Caricamento Subfile S02.
149200151104       //--------------------------------------------------------------
149300151104       BEGSR RieS02;
149400151104
149500151104         xx = 1;
149600151104         FOR xx by 1 to 4;
149700151104           clear  OR37S02;
149800151104           W02tcr = Frequenza(xx);
149900151104           W02des = Frequenza(xx) + DesFreq(xx);
150000151104           S02nrr += 1;
150100151104           write  OR37S02;
150200151104         ENDFOR;
150300151104
150400151104       ENDSR;
150500151104
150600151104       //--------------------------------------------------------------
150700151104       //?Gestione opzioni Subfile S02.
150800151104       //--------------------------------------------------------------
150900151104       BEGSR OpzS02;
151000151104
151100151104         xx = 1;
151200151104         FOR xx by 1 to 4;
151300151104           S02nrr = xx;
151400151104           chain S02nrr OR37S02;
151500151104           IF  not %found;
151600151104             leave;
151700151104           ENDIF;
151800151104
151900151104           SELECT;
152000151104         //?- Nessuna opzione
152100151104           WHEN  W02opz = *blank;
152200151104
152300151104         //?- 1 = Scelta
152400151104           WHEN  W02opz = '1';
152500151104             savfreq = W02tcr;
152600151104             leave;
152700151104
152800151104           OTHER;
152900151104             *in28 = *on;
153000151104             W02msg = 'Opzione errata';
153100151104             update OR37S02;
153200151104             leave;
153300151104           ENDSL;
153400151104
153500151104         ENDFOR;
153600151104
153700151104       ENDSR;
153800131113
153900131113      /end-free
154000071030
154100071030      *---------------------------------------------------------------*
154200071030      *?Interrogazione Giri di Ritiro                                ?*
154300071030      *---------------------------------------------------------------*
154400071031     c     Search_Giro   BEGSR
154500071030      *
154600071031     c                   clear                   Wpocgi
154700071031     c                   clear                   Wcgi
154800071030     c                   clear                   WcgiD
154900071030      *
155000071030     c                   clear                   FIDG09ds
155100071030     c                   eval      D09iop0   = 'P01'
155200071030     c                   eval      D09ifgs   = I37fgs
155300071030     c                   eval      D09idat   = *date
155400071030     c                   eval      D09itug   = 'R'
155500071030      *
155600071030     c                   movel(p)  FIDG09ds      KPJBU
155700071030     c                   call      'FIDG09R'
155800071030     c                   parm                    KPJBA
155900071030     c                   movel     KPJBU         FIDG09ds
156000071030      *
156100081211sel 1c                   select
156200081211w   1c                   when           D09Of03 <> *on
156300071030     c                             and  D09Of12 <> *on
156400071030     c                             and  D09Oerr <> *on
156500071030     c                   eval      Wpocgi    = D09ofgs
156600071030     c                   eval      Wcgi      = D09ocgi
156700071030     c                   movel     D09odes       WcgiD
156800081211w   1c                   when      D09Of03   = *on
156900081211     c                   eval      *in60     = *on
157000081211     c                   eval      *in90     = *on
157100081211w   1c                   when      D09Of12   = *on
157200081211e   1c                   endsl
157300071030      *
157400071030     c                   ENDSR
157500070802
157600070802      *---------------------------------------------------------------*
157700070802      *?Verifica se record annullabile                               ?*
157800070802      *---------------------------------------------------------------*
157900070802     c     Ctr_Annull    BEGSR
158000070802      *
158100071030     c                   eval      dsCRO1  = V2Ccd1
158200071030     c                   eval      dsCRO2  = V2Ccd2
158300071030     c                   eval      dsCRO3  = V2Ccd3
158400070802      *
158500070802      * Non deve esistere un Ritiro Fisso con questo codice
158600070802     c                   exsr      Ctr_RFissi
158700070802if  1c                   if        *in90   = *on
158800070802     c                   leavesr
158900070802e   1c                   endif
159000070802      *
159100070802      * Non deve esistere un O.R.M. con questo codice
159200070802     c                   exsr      Ctr_ORM
159300070802if  1c                   if        *in90
159400070802     c                   leavesr
159500070802e   1c                   endif
159600121220      *
159700121220      * Non deve esistere l'abilitazione OSR per questo codice
159800121220     c                   exsr      Ctr_OSR
159900121220if  1c                   if        *in90
160000121220     c                   leavesr
160100121220e   1c                   endif
160200070802      *
160300070802     c                   ENDSR
160400070802
160500070802      *---------------------------------------------------------------*
160600070802      *?Non deve esistere un Ritiro Fisso con il codice da annullare ?*
160700070802      *---------------------------------------------------------------*
160800070802     c     Ctr_RFissi    BEGSR
160900070802      *
161000070802      * Ricerca per codice ritiro
161100070802     c                   move      ds_CRO        ORScra
161200070802     c     ORScra        setll     FNORS002
161300070802     c     ORScra        reade     FNORS002
161400070802do  1c                   dow       NOT  %eof(FNORS02L)
161500071030if  2c                   if        ORSatb  = *blanks
161600070803     c                   seton                                        285190
161700071030     c                   eval      V1Dmsg  = $Msg(5)
161800070802     c                   leavesr
161900070802e   2c                   endif
162000070802     c     ORScra        reade     FNORS002
162100070802e   1c                   enddo
162200070802      *
162300070802      * Ricerca per codice ordinante
162400070802     c                   move      ds_CRO        ORScor
162500070802     c     ORScor        setll     FNORS003
162600070802     c     ORScor        reade     FNORS003
162700070802do  1c                   dow       NOT  %eof(FNORS03L)
162800071030if  2c                   if        ORSatb  = *blanks
162900070803     c                   seton                                        285190
163000071030     c                   eval      V1Dmsg  = $Msg(5)
163100070802     c                   leavesr
163200070802e   2c                   endif
163300070802     c     ORScor        reade     FNORS003
163400070802e   1c                   enddo
163500070802      *
163600070802     c                   ENDSR
163700070802
163800070802      *---------------------------------------------------------------*
163900070802      *?Non deve esistere un O.R.M. con il codice da annullare       ?*
164000070802      *---------------------------------------------------------------*
164100070802     c     Ctr_ORM       BEGSR
164200070802      *
164300070802      * Ricerca per codice ritiro
164400070802     c                   move      ds_CRO        ORMcra
164500070802     c     ORMcra        setll     FNORM018
164600070802     c     ORMcra        reade     FNORM018
164700070802do  1c                   dow       NOT  %eof(FNORM18L)
164800070807if  2c                   if             ORMatb  = *blanks
164900070807     C                             and  ORMfao  < 900
165000070803     c                   seton                                        285190
165100071030     c                   eval      V1Dmsg  = $Msg(6)
165200070802     c                   leavesr
165300070802e   2c                   endif
165400070807     c     ORScra        reade     FNORM018
165500070802e   1c                   enddo
165600070802      *
165700070802      * Ricerca per codice ordinante
165800070802     c                   move      ds_CRO        ORMcor
165900070802     c     ORMcor        setll     FNORM009
166000070802     c     ORMcor        reade     FNORM009
166100070802do  1c                   dow       NOT  %eof(FNORM09L)
166200070802if  2c                   if             ORMatb  = *blanks
166300070802     C                             and  ORMfao  < 900
166400070803     c                   seton                                        285190
166500071030     c                   eval      V1Dmsg  = $Msg(6)
166600070802     c                   leavesr
166700070802e   2c                   endif
166800070802     c     ORMcor        reade     FNORM009
166900070802e   1c                   enddo
167000070802      *
167100070802     c                   ENDSR
167200121220
167300121220      *---------------------------------------------------------------*
167400121220      *?Non deve esistere tab. OSR  con il codice da annullare       ?*
167500121220      *---------------------------------------------------------------*
167600121220     c     Ctr_OSR       BEGSR
167700121220
167800121220      * se non è aperto il file lo devo aprire
167900121220     c                   IF        not %open(TNTBE01L)
168000121220     c                   IF        %subst(knsif:7:1) = 'P'
168100121220     c                   eval      wLibTbe = 'GAITRAGRPS/TNTBE01L'
168200121220     c                   ELSE
168300121220     c                   eval      wLibTbe = 'GAITRAGRU/TNTBE01L'
168400121220     c                   ENDIF
168500121220     c                   open      TNTBE01L
168600121220     c                   ENDIF
168700121220
168800121220     c     'OSR'         setll     TNTBE01L
168900121220     c                   DO        *hival
169000121220     c     'OSR'         reade     TNTBE01L
169100121220     c                   IF        %eof(TNTBE01L)
169200121220     c                   leave
169300121220     c                   ENDIF
169400121220     c                   IF        TBEatb <> *blanks
169500121220     c                   iter
169600121220     c                   ENDIF
169700121220     c                   eval      dOSR = TBEuni
169800121220     c                   SELECT
169900121220     c                   WHEN      %subst(%editc(I37cro:'X'):8:3) = '333' or
170000121220     c                             %subst(%editc(I37cro:'X'):8:3) = '444'
170100121220     c                   IF        %subst(%editc(I37cro:'X'):1:7) =
170200121220     c                             %editc(d§OSRcli:'X') or
170300121220     c                             %subst(%editc(I37cro:'X'):1:7) =
170400121220     c                             %editc(d§OSRcl2:'X') or
170500121220     c                             %subst(%editc(I37cro:'X'):1:7) =
170600121220     c                             %editc(d§OSRcl3:'X') or
170700121220     c                             %subst(%editc(I37cro:'X'):1:7) =
170800121220     c                             %editc(d§OSRcl4:'X') or
170900121220     c                             %subst(%editc(I37cro:'X'):1:7) =
171000121220     c                             %editc(d§OSRcl5:'X') or
171100121220     c                             %subst(%editc(I37cro:'X'):1:7) =
171200121220     c                             %editc(d§OSRcl6:'X') or
171300121220     c                             %subst(%editc(I37cro:'X'):1:7) =
171400121220     c                             %editc(d§OSRcl7:'X') or
171500121220     c                             %subst(%editc(I37cro:'X'):1:7) =
171600121220     c                             %editc(d§OSRcl8:'X') or
171700121220     c                             %subst(%editc(I37cro:'X'):1:7) =
171800121220     c                             %editc(d§OSRcl9:'X') or
171900121220     c                             %subst(%editc(I37cro:'X'):1:7) =
172000121220     c                             %editc(d§OSRcl13:'X') or
172100121220     c                             %subst(%editc(I37cro:'X'):1:7) =
172200121220     c                             %editc(d§OSRcl11:'X') or
172300121220     c                             %subst(%editc(I37cro:'X'):1:7) =
172400121220     c                             %editc(d§OSRcl12:'X') or
172500121220     c                             %subst(%editc(I37cro:'X'):1:7) =
172600121220     c                             %editc(d§OSRcl13:'X') or
172700121220     c                             %subst(%editc(I37cro:'X'):1:7) =
172800121220     c                             %editc(d§OSRcl14:'X') or
172900121220     c                             %subst(%editc(I37cro:'X'):1:7) =
173000121220     c                             %editc(d§OSRcl15:'X') or
173100121220     c                             %subst(%editc(I37cro:'X'):1:7) =
173200121220     c                             %editc(d§OSRcl16:'X')
173300121220     c                   seton                                        285190
173400121220     c                   eval      V1Dmsg  = $Msg(31)
173500121220     c                   eval      V1Dmsg = %trim(V1Dmsg) + ' LNP ' +
173600121220     c                             %subst(TBEke1:1:3) + ' Serie ' +
173700121220     c                             %subst(TBEke1:4:2)
173800121220     c                   leavesr
173900121220     c                   ENDIF
174000121220     c                   WHEN      %subst(%editc(I37cro:'X'):8:3) = '333' and
174100121220     c                             %subst(%editc(I37cro:'X'):1:7) =
174200121220     c                             %editc(d§OSRcor:'X')
174300121220     c                   seton                                        285190
174400121220     c                   eval      V1Dmsg  = $Msg(31)
174500121220     c                   eval      V1Dmsg = %trim(V1Dmsg) + ' LNP ' +
174600121220     c                             %subst(TBEke1:1:3) + ' Serie ' +
174700121220     c                             %subst(TBEke1:4:2)
174800121220     c                   leavesr
174900121220     c                   WHEN      %subst(%editc(I37cro:'X'):8:3) = '444' and
175000121220     c                             %subst(%editc(I37cro:'X'):1:7) =
175100121220     c                             %editc(d§OSRcrc:'X')
175200121220     c                   seton                                        285190
175300121220     c                   eval      V1Dmsg  = $Msg(31)
175400121220     c                   eval      V1Dmsg = %trim(V1Dmsg) + ' LNP ' +
175500121220     c                             %subst(TBEke1:1:3) + ' Serie ' +
175600121220     c                             %subst(TBEke1:4:2)
175700121220     c                   leavesr
175800121220     c                   ENDSL
175900121220     c                   ENDDO
176000121220
176100121220     c                   ENDSR
176200070802
176300070802      *---------------------------------------------------------------*
176400070802      *?Controllo C.A.P.                                             ?*
176500070802      *---------------------------------------------------------------*
176600070802     c     Ctr_CAP       BEGSR
176700070802      *
176800070802     c                   clear                   TISI95ds
176900070802     c                   eval      I95tcn  = '7'
177000070802     c                   eval      I95nar  = V2Cnar
177100070802     c                   eval      I95cap  = V2Ccar
177200070802     c                   eval      I95loc  = V2Clor
177300070802     c                   eval      I95prv  = V2Cprr
177400070802     c                   eval      I95ind  = V2Cinr
177500070802     c                   clear                   FNLV13ds
177600070802     c                   eval      I13af0  = 'S'
177700070802     c                   eval      I13cnv  = 'S'
177800070802     c                   eval      I13af1  = 'S'
177900070802     c                   eval      I13sz2  = 'S'
178000070802     c                   eval      I13la3  = 'S'
178100070802     c                   eval      I13sz3  = 'S'
178200070802      *
178300070802      * Gestione errori forzabili:
178400070802      * se cambiato CAP/Località riazzero i valori di errori forzabili
178500070802      *
178600070802      * - Modifica CAP
178700070802if  1c                   if        V2Ccar <> SAVcar
178800070802     c                   eval      SAVcar  = V2Ccar
178900070802     c                   clear                   E13fzv
179000070802     c                   clear                   E13fz1
179100070802     c                   clear                   E13fz2
179200070802     c                   clear                   E13fz3
179300070802e   1c                   endif
179400070802      * - Modifica località
179500070802if  1c                   if        V2Clor <> SAVlor
179600070802     c                   eval      SAVlor  = V2Clor
179700070802     c                   clear                   E13fz1
179800070802     c                   clear                   E13fz2
179900070802     c                   clear                   E13fz3
180000070802e   1c                   endif
180100070802      * - Modifica PROVINCIA
180200070802if  1c                   if        V2Cprr <> SAVprr
180300070802     c                   eval      SAVprr  = V2Cprr
180400070802     c                   clear                   E13fz3
180500070802e   1c                   endif
180600070802      *
180700070802      * Richiamo programma
180800070802     c                   call      'FNLV13R'
180900070802     c                   parm                    KPJBA
181000070802     c                   parm                    FNLV13ds
181100070802     c                   parm                    TISI95ds
181200070802      *
181300070802      * Reimpostazione dei dati ricevuti in TISI95ds
181400070802if  1c                   if             O13ron  = 'S'
181500110121     c                   eval      V2Cnar  = O95nar
181600110121e   1c                   endif
181700110121     c                   if             O13roc  = 'S'
181800110121     c                   eval      V2Ccar  = O95cap
181900110121e   1c                   endif
182000110121     c                   if             O13rop  = 'S'
182100110121     c                   eval      V2Cprr  = O95prv
182200110121e   1c                   endif
182300110121     c                   if             O13rol  = 'S'
182400070802     c                   eval      V2Clor  = O95loc
182500070802e   1c                   endif
182600070802      *
182700070802      * Riscontrati errori o reimpostati dati
182800070802sel 1c                   select
182900070802      * - Emissione messaggio con l'errore rilevato
183000081022w   1c                   when      O13msg <> *blanks
183100070802     c                   eval      *in55   = (O13err = '5')                     CAP
183200070802     c                   eval      *in56   = (O13err = '3')                     Località
183300070802     c                   eval      *in57   = (O13err = '4')                     Provincia
183400070802     c                   eval      *in58   = (O13err = '6')                     Nazione
183500070802     c                   seton                                        28  90
183600070802     c                   eval      V1Dmsg  = O13msg
183700070802     c                   leavesr
183800070802      * - C.A.P. reimpostato
183900070802w   1c                   when      O13roc  = 'S'
184000070802     c                   seton                                        285590
184100070802     c                   eval      V1Dmsg  = $Msg(12)
184200070802     c                   leavesr
184300070802      * - Località reimpostata
184400070802w   1c                   when      O13rol  = 'S'
184500070802     c                   seton                                        285690
184600070802     c                   eval      V1Dmsg  = $Msg(13)
184700070802     c                   leavesr
184800070802      * - Provincia reimpostata
184900070802w   1c                   when      O13rop  = 'S'
185000070802     c                   seton                                        285790
185100070802     c                   eval      V1Dmsg  = $Msg(14)
185200070802     c                   leavesr
185300070802      * - Nazione reimpostata
185400070802w   1c                   when      O13ron  = 'S'
185500070802     c                   seton                                        285890
185600070802     c                   eval      V1Dmsg  = $Msg(15)
185700070802     c                   leavesr
185800070802e   1c                   endsl
185900070802      *
186000070802     c                   ENDSR
186100070801
186200070801      *---------------------------------------------------------------*
186300070801      *?Reperimento dati anagrafici                                  ?*
186400070801      *---------------------------------------------------------------*
186500070801     c     Call_TIBS69   BEGSR
186600070801      *
186700070801     c                   clear                   ds_CNACO
186800070801     c                   clear                   ds_CNIND
186900070801     c                   clear                   ds_CNCLP
187000070801     c                   clear                   ds_FNCLS
187100070801      *
187200070801     c                   call      'TIBS69R'
187300070801     c                   parm                    TIBS69ds
187400070801     c                   parm                    ds_CNACO
187500070801     c                   parm                    ds_CNIND
187600070801     c                   parm                    ds_CNCLP
187700070801     c                   parm                    ds_FNCLS
187800070801      *
187900071023     c                   eval      *in28   = (O69err = *on)
188000070801if  1c                   if        *in28
188100071023     c                   eval      *in90   = *on
188200071023     c                   eval      V1Dmsg  = O69msg
188300070801e   1c                   endif
188400070801      *
188500070801     c                   ENDSR
188600070801
188700070801      *---------------------------------------------------------------*
188800070801      *?Aggiornamento dati file FNACR00F                             ?*
188900070801      *---------------------------------------------------------------*
189000070801     c     Upd_FNACR     BEGSR
189100070801      *
189200071023     c                   eval      *in91   = *off
189300071023     c                   eval      I37cro  = V2Ccd1 * 10000000
189400071023     c                                     + V2Ccd2 * 1000
189500070807     c                                     + V2Ccd3
189600071030     c     I37cro        chain     FNACR000
189700070803      *
189800070801     c                   select
189900070801      *
190000070801      * Inserimento / Copia
190100070801w   1c                   when          (I37opz = '1'
190200070809     c                             or   I37opz = '3'
190300070809     c                             or   I37opz = 'A'
190400070809     c                             or   I37opz = 'C')
190500070801     c                             and  *inKF
190600070801     c                   exsr      RieRec
190700070803     c                   WRITE     FNACR000                             91
190800070801      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
190900070801      * Modifica
191000070801w   1c                   when           I37opz  = '2'
191100070801     c                             and  *inKF
191200070801     c                   exsr      RieRec
191300070801     c                   UPDATE    FNACR000
191400070801      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
191500070801      * Annullamento
191600070801w   1c                   when          (I37opz  = '4'
191700070801     c                             and  *inKF)
191800070801     c                             or   *inKQ
191900070801     c                   eval      ACRatb  = 'A'
192000070801     c                   UPDATE    FNACR000
192100070801      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
192200070801      * Ripristino
192300070801w   1c                   when          (I37opz  = '7'
192400070801     c                             and  *inKF)
192500070801     c                             or   *inKE
192600070802     c                   exsr      RieRec
192700070801     c                   UPDATE    FNACR000
192800070801      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
192900070801e   1c                   endsl
193000070801      *
193100070803if  1c                   if        *in91
193200070801     c                   seton                                        28  90
193300071030     c                   eval      V1Dmsg  = %trimr($Msg(4))
193400071030     c                                     + ' FNACR00F'
193500070801e   1c                   endif
193600070801      *
193700071030      * Aggiornamento ambito "standard" di intradamento codice giro
193800071030     c                   eval      ACR1ain = '='
193900071030     c                   eval      Wpocgi  = V2Cpocgi1
194000071030     c                   eval      Wcgi    = V2Ccgi1
194100070801     c                   exsr      Upd_FNACR1
194200071030      *
194300071030      * Aggiornamento ambito "oltre" di intradamento codice giro
194400071030     c                   eval      ACR1ain = '>'
194500071030     c                   eval      Wpocgi  = V2Cpocgi2
194600071030     c                   eval      Wcgi    = V2Ccgi2
194700071030     c                   exsr      Upd_FNACR1
194800071030      *
194900071030      * Aggiornamento ambito "sotto" di intradamento codice giro
195000071030     c                   eval      ACR1ain = '<'
195100071030     c                   eval      Wpocgi  = V2Cpocgi3
195200071030     c                   eval      Wcgi    = V2Ccgi3
195300071030     c                   exsr      Upd_FNACR1
195400151012      *
195500151012      /free
195600151012
195700151012         // -?Aggiornamento dati file FNACRE0F?
195800151012
195900151012         // -?Impostazione chiave d'accesso al file FNACRE1L per l'?
196000151012         //  ?aggionamento della "e-Mail Conferma Accettazione ORM"?
196100151012         clear  keyFNACRE1;
196200151012         k_ACREcro = I37cro;
196300151012         k_ACREtrc = 'MC';
196400151012         chain  %kds( keyFNACRE1 )  FNACRE00;
196500151012
196600151012         Select;
196700151012
196800151012           // -?Inserimento / Copia?
196900151012           When  Not %found(FNACRE1L)  and  V2Cmca <> *blank;
197000151012             clear  FNACRE00;
197100151012             ACREatb  = ACRatb;
197200151012             ACREcro  = k_ACREcro;
197300151012             ACREtrc  = k_ACREtrc;
197400151012             ACREdati = V2Cmca;
197500151012             Write  FNACRE00;
197600151012
197700151012           // -?Modifica / Annullamento / Ripristino?
197800151012           When  %found(FNACRE1L)  and  V2Cmca <> *blank;
197900151012             ACREatb  = ACRatb;
198000151012             ACREdati = V2Cmca;
198100151012             Update  FNACRE00;
198200151012
198300151012           // -?Cancellazione?
198400151012           When  %found(FNACRE1L)  and  V2Cmca = *blank;
198500151012             Delete  FNACRE00;
198600151012
198700151012         EndSl;
198800160317
198900160317         // -?Impostazione chiave d'accesso al file FNACRE1L per l'?
199000160317         //  ?aggionamento del "SMS Conferma Accettazione ORM"?
199100160317         clear  keyFNACRE1;
199200160317         k_ACREcro = I37cro;
199300160317         k_ACREtrc = 'SC';
199400160317         chain  %kds( keyFNACRE1 )  FNACRE00;
199500160317
199600160317         Select;
199700160317
199800160317           // -?Inserimento / Copia?
199900160317           When  Not %found(FNACRE1L)  and  V2Csca <> *blank;
200000160317             clear  FNACRE00;
200100160317             ACREatb  = ACRatb;
200200160317             ACREcro  = k_ACREcro;
200300160317             ACREtrc  = k_ACREtrc;
200400160317             ACREdati = V2Csca;
200500160317             Write  FNACRE00;
200600160317
200700160317           // -?Modifica / Annullamento / Ripristino?
200800160317           When  %found(FNACRE1L)  and  V2Csca <> *blank;
200900160317             ACREatb  = ACRatb;
201000160317             ACREdati = V2Csca;
201100160317             Update  FNACRE00;
201200160317
201300160317           // -?Cancellazione?
201400160317           When  %found(FNACRE1L)  and  V2Csca = *blank;
201500160317             Delete  FNACRE00;
201600160317
201700160317         EndSl;
201800160713
201900160713       //?Se devo scrivere il rcd 'AA' lo faccio ora
202000160713         IF  wOkRcdAA;
202100160713           clear  keyFNACRE1;
202200160713           k_ACREcro = I37cro;
202300160713           k_ACREtrc = 'AA';
202400160713           chain  %kds( keyFNACRE1 )  FNACRE00;
202500160713           clear dOREana;
202600160713           §OREtipo = 'A';
202700160713           §OREdtv = %dec(%date());
202800160713           §OREorv = %dec(%time());
202900160713           §OREute = knmus;
203000160713           §OREjob = knmeb;
203100160713           ACREatb = ACRatb;
203200160713           IF  %found(FNACRE1L);
203300160713             ACREdati = dOREana;
203400160713             update FNACRE00;
203500160713           ENDIF;
203600160713           IF  not %found(FNACRE1L);
203700160713             ACREcro = k_ACREcro;
203800160713             ACREtrc = k_ACREtrc;
203900160713             ACREdati = dOREana;
204000160713             write FNACRE00;
204100160713           ENDIF;
204200160713         ENDIF;
204300151012
204400151012      /end-free
204500070801      *
204600070801     c                   ENDSR
204700070801
204800070801      *---------------------------------------------------------------*
204900070801      *?Aggiornamento dati file FNACR10F                             ?*
205000070801      *---------------------------------------------------------------*
205100070801     c     Upd_FNACR1    BEGSR
205200070801      *
205300071030     c                   eval      *in92   = *off
205400070807     c                   eval      I37cro  = V2Ccd1*10000000
205500070807     c                                     + V2Ccd2*1000
205600070807     c                                     + V2Ccd3
205700071024      *
205800071024sel 1c                   select
205900070801      *
206000070802      * Inserimento / Modifica / Copia
206100070803w   1c                   when          (I37opz  = '1'
206200070803     c                             or   I37opz  = '2'
206300070809     c                             or   I37opz  = '3'
206400070809     c                             or   I37opz  = 'A'
206500070809     c                             or   I37opz  = 'C')
206600070801     c                             and  *inKF
206700071030     c     k03acr13      chain     FNACR100
206800071105     c                   clear                   SAVpocgi
206900071105     c                   clear                   SAVcgi
207000071105if  2c                   if        %found(FNACR13L)
207100071105     c                   eval      SAVpocgi     = ACR1pocgi
207200071105     c                   eval      SAVcgi       = ACR1cgi
207300071105e   2c                   endif
207400071105      *
207500071030if  2c                   if        Wcgi   <> *blanks
207600070801     c                   exsr      RieRec1
207700071030e   2c                   endif
207800071105      *
207900070803sel 2c                   select
208000071030w   2c                   when      NOT  %found(FNACR13L)
208100071030     c                             and  Wcgi   <> *blanks
208200070801     c                   WRITE     FNACR100                             92
208300070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
208400071030w   2c                   when           %found(FNACR13L)
208500071030     c                             and  Wcgi   <> *blanks
208600071105     c                             and  Wcgi   <> SAVcgi
208700070801     c                   UPDATE    FNACR100
208800070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
208900071030w   2c                   when           %found(FNACR13L)
209000071030     c                             and  Wcgi    = *blanks
209100071024     c                   DELETE    FNACR100
209200071024      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
209300071030w   2c                   when           %found(FNACR13L)
209400071105     c                             and  Wcgi    = SAVcgi
209500071030     c                   UNLOCK    FNACR13L
209600071030      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
209700070803e   2c                   endsl
209800070801      *
209900070801      * Annullamento
210000070807w   1c                   when         ((I37opz  = '4'
210100070807     c                             and  *inKF)
210200070807     c                             or   *inKQ)
210300071024     c     I37cro        setll     FNACR100
210400071024     c     I37cro        reade     FNACR100
210500071030do  2c                   dow       NOT  %eof(FNACR13L)
210600080115     c                   DELETE    FNACR100
210700070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
210800071024     c     I37cro        reade     FNACR100
210900071024e   2c                   enddo
211000070801      * Ripristino
211100070807w   1c                   when         ((I37opz  = '7'
211200070801     c                             and  *inKF)
211300070807     c                             or   *inKE)
211400071024     c     I37cro        setll     FNACR100
211500071024     c     I37cro        reade     FNACR100
211600071030do  2c                   dow       NOT  %eof(FNACR13L)
211700071030if  3c                   if        I37fgs  = ACR1pocgi
211800070801     c                   exsr      RieRec1
211900071024x   3c                   else
212000071030     c                   eval      ACR1atb = *blanks
212100071030     c                   eval      ACR1dtv = *date
212200071030     c                   eval      ACR1ute = KNMUS
212300071024e   3c                   endif
212400070807     c                   UPDATE    FNACR100
212500070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
212600071024     c     I37cro        reade     FNACR100
212700071024e   2c                   enddo
212800070801e   1c                   endsl
212900070801      *
213000070801if  1c                   if        *in92
213100070801     c                   seton                                        28  90
213200071030     c                   eval      V1Dmsg  = %trimr($Msg(4))
213300071030     c                                     + ' FNACR10F'
213400070801e   1c                   endif
213500070801      *
213600070801     c                   ENDSR
213700070801
213800070801      *---------------------------------------------------------------*
213900070801      *?Impostazione valori nei campi del record FNACR00F            ?*
214000070801      *---------------------------------------------------------------*
214100070801     c     RieRec        BEGSR
214200070801      *
214300131113     c                   clear                   dACR01
214400070801     c                   clear                   FNACR000
214500160713      /free
214600160713       //?Se ho flag e/o mail e/o sms alert diversi devo scrivere anche rcd 'AA'
214700160713         wOkRcdAA = *off;
214800160713         IF  V2Cfca <> savfca or V2Cmca <> savmca or V2Csca <> savsca;
214900160713           wOkRcdAA = *on;
215000160713         ENDIF;
215100160713      /end-free
215200070801      *
215300070809      * Lascia record annullato se lo era già (*in43)
215400070809      * e non ne è stato chiesto il ripristino
215500070807     c                   if             *in43
215600070807     c                             and  NOT *inKE
215700070807     c                             and  I37opz <> '7'
215800070807     c                   eval      ACRatb  = 'A'
215900070807     c                   endif
216000070809      *
216100070809      * Imposta tutti gli altri campi ("chiave" compresi)
216200071030     c                   eval      ACRtac  = 'R'
216300070801     c                   eval      ACRcro  = V2Ccd1*10000000
216400070801     c                                     + V2Ccd2*1000
216500070801     c                                     + V2Ccd3
216600070801     c                   eval      ACRrsr  = V2Crsr
216700070801     c                   eval      ACRinr  = V2Cinr
216800070801     c                   eval      ACRcar  = V2Ccar
216900070801     c                   eval      ACRlor  = V2Clor
217000070801     c                   eval      ACRprr  = V2Cprr
217100070801     c                   eval      ACRnar  = V2Cnar
217200070801     c                   eval      ACRksc  = V2Cksc
217300070801     c                   eval      ACRcco  = V2Cco1*10000000
217400070801     c                                     + V2Cco2*1000
217500070801     c                                     + V2Cco3
217600070801     c                   eval      ACRper  = V2Cper
217700070801     c                   eval      ACRtel  = V2Ctel
217800131113     c                   eval      dACR01.§ACRoa1 = %editc( V2ChmAmDa : 'X' )
217900131113     c                   eval      dACR01.§ACRoa2 = %editc( V2ChmAmA  : 'X' )
218000131113     c                   eval      dACR01.§ACRoa3 = %editc( V2ChmPmDa : 'X' )
218100131113     c                   eval      dACR01.§ACRoa4 = %editc( V2ChmPmA  : 'X' )
218200140114     c                   eval      dACR01.§ACRstcr = sav_§ACRstcr
218300140114     c                   eval      dACR01.§ACRctcr = sav_§ACrctcr
218400151104     c                   eval      dACR01.§ACRfmar = V2Cfca
218500131113     c                   eval      ACRmai  = dACR01
218600070801     c                   eval      ACRno1  = V2Cno1
218700070801     c                   eval      ACRno2  = V2Cno2
218800070801     c                   eval      ACRntm  = V2Cntm
218900150909     c                   eval      ACRpoa  = V2Cpoa
219000070801     c                   eval      ACRtcr  = V2Ctcr
219100070802if  1c                   if        V2Cfcc  = 'N'
219200070801     c                   eval      ACRfcc  = V2Cfcc
219300070802x   1c                   else
219400070802     c                   clear                   ACRfcc
219500070802e   1c                   endif
219600091009if  1c                   if        V2Cctr  = *blanks
219700070801     c                   eval      ACRccc  = 999
219800070801x   1c                   else
219900091009     c                   move      v2cctr        ACRccc
220000070801e   1c                   endif
220100070801     c                   eval      ACRfcl  = V2Cfcl
220200070802     c                   eval      ACRfpk  = V2Cfpk
220300070801     c                   eval      ACRfmc  = V2Cfmc
220400070801     c                   eval      ACRfbn  = V2Cfbn
220500070801     c                   eval      ACRfbl  = V2Cfbl
220600070801     c                   eval      ACRfmt  = V2Cfmt
220700070801     c                   eval      ACRfat  = V2Cfat
220800070801     c                   eval      ACRspi  = V2Cspi
220900070801     c                   eval      ACRorr  = V2Corr
221000070801     c                   eval      ACRdtr  = *date
221100070801      *
221200070801     c                   ENDSR
221300070801
221400070801      *---------------------------------------------------------------*
221500070801      *?Impostazione valori nei campi del record FNACR10F            ?*
221600070801      *---------------------------------------------------------------*
221700070801     c     RieRec1       BEGSR
221800070801      *
221900070913     c                   eval      ACR1atb   = ACRatb
222000070801     c                   eval      ACR1cro   = ACRcro
222100071030     c                   eval      ACR1pocgi = Wpocgi
222200071030     c                   eval      ACR1cgi   = Wcgi
222300071030if  1c                   if        NOT  %found(FNACR13L)
222400071030     c                             or  (SAVpocgi <> Wpocgi
222500071030     c                             or   SAVcgi   <> Wcgi)
222600070801     c                   eval      ACR1dtv   = *date
222700070802     c                   eval      ACR1ute   = KNMUS
222800070802e   1c                   endif
222900070801      *
223000070801     c                   ENDSR
223100070801
223200070801** - $Fun ------*
223300070801   INSERIMENTO      1
223400070801     MODIFICA       2
223500070801      COPIA         3
223600070801   ANNULLAMENTO     4
223700070801 VISUALIZZAZIONE    5
223800070801    RIPRISTINO      6
223900070801** - $Msg -------------------------------------------------------------------*
224000070801PARAMETRI ERRATI                                                                1
224100070801Richiesta funzione errata per record inesistente                                2
224200070801Record già esistente                                                            3
224300070801Errore in fase di scrittura record nel file                                     4
224400070802NON ANNULLABILE: reperite anagrafiche ritiri fissi con questo codice            5
224500070802NON ANNULLABILE: reperiti ritiri con questo codice                              6
224600070802Codice cliente ritiro non valido: NON esiste sul piano dei conti                7
224700070802Codice cliente ritiro obbligatorio                                              8
224800070802Codice cliente ritiro già esistente                                             9
224900070802Tipo anagrafica errato                                                         10
225000070802Frequenza ritiro errato                                                        11
225100070802ATTENZIONE: modificato C.A.P.                                                  12
225200070802ATTENZIONE: modificata località                                                13
225300070802ATTENZIONE: modificata provincia                                               14
225400070802ATTENZIONE: modificata nazione                                                 15
225500070808Immettere anche il giro di ritiro  o  togliere la filiale del giro             16
225600070808Immettere anche la filiale del giro  o  togliere il giro di ritiro             17
225700070802Filiale errata per codice giro di ritiro                                       18
225800070911Codice giro di ritiro inesistente o non valido                                 19
225900070802Codice cliente ordinante errato                                                20
226000070802Non è possibile inserire un codice ordinante per un cliente di tipo ordinante  21
226100070802Codice cliente piano dei conti errato                                          22
226200070802Inserire il codice tariffa solo se inserito anche il codice P.d.C.             23
226300091009Codice tariffa inesistente                                                     24
226400070802Frequenza ritiro errata                                                        25
226500070802Ora ritiro errata                                                              26
226600070802Valore flag immissione errato                                                  27
226700070802NON è stato modificato il codice Piano dei Conti  --  Enter per forzare        28
226800071112Codice giro di ritiro errato o NON in vigore                                   29
226900071030PRIMA posizionare il cursore sul campo per cui interrogare e/o selezionare     30
227000121220NON ANNULLABILE: Codice presente su tabella OSR                                31
227100140723Creare il luogo '000' per il codice cliente in gestione.                       32
227200150909Filiale Ritiro Forzata errata                                                  33
227300160317Immettere un numero di telefono valido                                         34
227400151104
227500151104** -- Frequenza / DesFreq ---------------------------------------------------*
227600151104F = Fisso
227700151104R = Ricorrente
227800151104O = No Internet
227900151104M = Mai
