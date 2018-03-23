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
010000170330
010100170330      //?ds per campo D09OFLO - FLO anagrafica giri
010200170330     d dGRGflo       e ds
010300070730      *
010400070730     d Status         sds           333
010500070730     d   SDSpgm          *proc
010600070801      *
010700070801      * - Codice cliente ritiro
010800070801     d ds_CRO          ds            10    inz
010900070801     d  dsCRO1                        3  0 inz
011000070801     d  dsCRO2                        4  0 inz
011100070801     d  dsCRO3                        3  0 inz
011200070802      *
011300070802      * - Ora
011400070802     d ds_Time         ds             4    inz
011500070802     d  dsHH                          2  0 inz
011600070802     d  dsMM                          2  0 inz
011700070730      *
011800070730      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
011900070730      *
012000070801      * - Flags
012100070801     d $InzD01         s              1    inz(*on)
012200070801     d $Fine           s              1    inz(*off)
012300070802     d $Tam            s              1    inz(*off)
012400160713     d wOkRcdAA        s               n   inz
012500071030      * - Indici di schiera / Contatori
012600071030     d wNrCGI          s              3  0 inz
012700070802      * - Comodo
012800071030     d Wpocgi          s                   inz  like(V2Cpocgi1)
012900071030     d Wcgi            s                   inz  like(V2Ccgi1)
013000071030     d WcgiD           s                   inz  like(V2Dcgi1)
013100070802     d SAVcar          s                   inz  like(V2Ccar)
013200070802     d SAVlor          s                   inz  like(V2Clor)
013300070802     d SAVprr          s                   inz  like(V2Cprr)
013400070802     d SAVksc          s                   inz  like(V2Cksc)
013500070802     d SAVpocgi        s                   inz  like(ACR1pocgi)
013600070802     d SAVcgi          s                   inz  like(ACR1cgi)
013700110121     d wdecod          s              1    inz
013800121130     d wGiaChiamato    s               n   inz(*off)
013900121220     d wLibTbe         s             21a   inz
014000151104     d FineTcr         s               n   inz(*off)
014100151104     d S02nrr          s              4s 0 inz
014200151104     d savfreq         s                   like(W02tcr) inz
014300151104     d xx              s              4s 0 inz
014400160713     d FineW03         s               n   inz(*off)
014500160713     d savfca          s                   like(V2Cfca) inz
014600160713     d savmca          s                   like(V2Cmca) inz
014700160713     d savsca          s                   like(V2Csca) inz
014800140114
014900140114     d sav_§ACRstcr    s                   like(dACR01.§ACRstcr)
015000140114     d sav_§ACRctcr    s                   like(dACR01.§ACRctcr)
015100160713
015200160713      // - Campi di comodo data
015300160713     d Data_EUR        s               d   datfmt(*eur)
015400160713     d Data_ISO        s               d   datfmt(*iso)
015500121130
015600121130      // - Campi per procedura esterna
015700121130     d  pStack         s           1000
015800121130     d  pStartPoint    s              3  0
015900121130     d  pEntries       s              3  0
016000121130
016100121130      //---------------------------------------------------------------
016200121130      //?Definizione procedure esterne.
016300121130      //---------------------------------------------------------------
016400121130
016500121130       // - Pgm per ricerca Programmi chiamati
016600121130     d XRTVSTKR        pr                  extpgm('XRTVSTKR')
016700121130     d  pStack                     1000
016800121130     d  pStartPoint                   3  0
016900121130     d  pEntries                      3  0
017000131113
017100131113       // -?Controllo orari apertura del cliente?
017200131113     d TRUL03ds      e ds                  qualified  inz
017300131113     d trul03r         pr                  extpgm('TRUL03R')
017400131113     d   trul03ds...
017500131113     d                                     likeds(trul03ds)
017600151012
017700151012       // -?Controllo indirizzo e-mail?
017800151012     d dsEmail       e ds                  inz
017900151012      /copy gaitrasrc/srcProtoPR,XEMAIL
018000160317
018100160317       // -?Controllo cellulare
018200160317      /copy gaitrasrc/srcprotopi,ubchkcel
018300160317      /copy gaitrasrc/srcprotopr,ubchkcel
018400131113
018500121130      //---------------------------------------------------------------
018600070730      *
018700070730      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
018800070730      *
018900151012       // -?File FNACRE1L?
019000151012     d keyFNACRE1    e ds                  extname( FNACRE1L : *key )
019100151012     d                                     prefix( k_ )   inz
019200071030      * - FNACR13L
019300071030     c     k02acr13      klist
019400071023     c                   kfld                    I37cro
019500071023     c                   kfld                    I37fgs
019600071030     c     k03acr13      klist
019700071030     c                   kfld                    I37cro
019800071030     c                   kfld                    I37fgs
019900071030     c                   kfld                    ACR1ain
020000070801      * - TNTAM01L
020100070802     c     k02tam01      klist
020200070802     c                   kfld                    TAMksc
020300070801     c                   kfld                    TAMctr
020400121130
020500070801      *---------------------------------------------------------------*
020600070801      *?RIEPILOGO INDICATORI                                         ?*
020700070801      *---------------------------------------------------------------*
020800070801      *  02    - Abilitazione tasto funzionale F2=Anagr.Fissi   - D01 *
020900070801      *  03    - Abilitazione tasto funzionale F3=Fine          - D01 *
021000070801      *  05    - Abilitazione tasto funzionale F5=Ripristino    - D01 *
021100070801      *  06    - Abilitazione tasto funzionale F6=Conferma      - D01 *
021200070801      *  16    - Abilitazione tasto funzionale F15=Annullamento - D01 *
021300160713      *  19    - Abilitazione tasto funzionale F19=Mod.Dati Alert     *
021400070801      *  25    - Comodo                                               *
021500070801      *  28    - Emissione del messaggio di errore a video            *
021600140113      *  39    - Visualizzo Frequenza ritiro salvata x EDP
021700140113      *  40    - Proteggo Frequenza ritiro
021800070801      *  41    - Protezione campi chiave in modifica            - D01 *
021900070809      *  42    - Protezione campi codice PdC e Tariffa          - D01 *
022000070809      *  43    - Visualizzazione "ANNULLATO"                    - D01 *
022100070801      *  51    - Cliente ritiro           errato                - D01 *
022200150909      *  52    - Filiale ritiro forzata   errata                - D01 *
022300070801      *  53    - Ragione sociale          errata                - D01 *
022400070801      *  54    - Indirizzo                errato                - D01 *
022500070801      *  55    - CAP                      errato                - D01 *
022600070801      *  56    - Località                 errata                - D01 *
022700070801      *  57    - Provincia                errata                - D01 *
022800070801      *  58    - Nazione                  errata                - D01 *
022900071030      *  59    - Fil. Giro Ritiro ("=")   errata                - D01 *
023000071030      *  60    - Giro di Ritiro   ("=")   errato                - D01 *
023100070801      *  61    - Ordinante                errato                - D01 *
023200070801      *  62    - Codice PdC               errato                - D01 *
023300070801      *  63    - Tariffa                  errata                - D01 *
023400070801      *  64    - Frequenza ritiro         errata                - D01 *
023500070801      *  65    - Ora       ritiro         errata                - D01 *
023600070801      *  67    - Telefono referente       errato                - D01 *
023700070801      *  68    - E-Mail   referente       errata                - D01 *
023800070801      *  69    - Flag immissione colli    errato                - D01 *
023900070801      *  70    - Flag immissione bancali  errato                - D01 *
024000070801      *  71    - Flag immissione peso     errato                - D01 *
024100070801      *  72    - Flag immissione volume   errato                - D01 *
024200070801      *  73    - Flag immissione bilico   errato                - D01 *
024300070801      *  74    - Flag immissione motrice  errato                - D01 *
024400070801      *  75    - Flag immissione autotr.  errato                - D01 *
024500071030      *  76    - Fil. Giro Ritiro (">")   errata                - D01 *
024600071030      *  77    - Giro di Ritiro   (">")   errato                - D01 *
024700071030      *  78    - Fil. Giro Ritiro ("<")   errata                - D01 *
024800071030      *  79    - Giro di Ritiro   ("<")   errato                - D01 *
024900160317      *  80/83 - Orari apertura                                 - D01 *
025000160317      *  84    - SMS per invio conferma prenotazione ritiro     - D01 *
025100070801      *  90    - Generico di errore                                   *
025200070803      *  91    - Fallito aggiornamento di FNACR00F                    *
025300070803      *  92    - Fallito aggiornamento di FNACR10F                    *
025400000927      *---------------------------------------------------------------*
025500070730
025600070730     c     *Entry        plist
025700070730     c                   parm                    KPJBA
025800070801     c                   movel     KPJBU         FIOR37ds
025900070730      *
026000070730      * Operazioni iniziali
026100070801     c                   exsr      RoutInz
026200070801      *
026300071024      * Gestione video
026400070801do  1c                   DOW       $Fine   = *off
026500070801     c                   exsr      GesD01
026600070801e   1c                   ENDDO
026700070801      *
026800071024      * Operazioni finali
026900070801     c                   movel(p)  FIOR37ds      KPJBU
027000070801     c                   eval      *inLR   = *on
027100070801
027200070801      *---------------------------------------------------------------*
027300070801      *?Operazioni Iniziali                                          ?*
027400070801      *---------------------------------------------------------------*
027500070801     c     RoutInz       BEGSR
027600070801      *
027700070801      * Pulizia campi di output
027800070801     c                   clear                   O37cro
027900070801     c                   clear                   O37err
028000070801     c                   clear                   O37msg
028100071030     c                   eval      O37ret  = *off
028200070801      *
028300070801      * Controllo parametri ricevuti
028400070801sel 1c                   SELECT
028500070801      * - Controllo inserimento filiale gestione per Immissione/Variaz.
028600070801w   1c                   WHEN           I37opz  <> '1'
028700070801     c                             and  I37opz  <> '2'
028800070801     c                             and  I37opz  <> '3'
028900070801     c                             and  I37opz  <> '4'
029000070801     c                             and  I37opz  <> '5'
029100070801     c                             and  I37opz  <> '7'
029200070809     c                             and  I37opz  <> 'A'
029300070809     c                             and  I37opz  <> 'C'
029400071030     c                   eval      O37err  = 'E'
029500071030     c                   eval      O37msg  = $Msg(1)
029600071030     c                   eval      $Fine   = *on
029700070801     c                   leavesr
029800070807w   1c                   WHEN           I37opz  <> '1'
029900070801     c                             and  I37cro   = *zeros
030000071030     c                   eval      O37err  = 'E'
030100071030     c                   eval      O37msg  = $Msg(1)
030200071030     c                   eval      $Fine   = *on
030300070801     c                   leavesr
030400070802w   1c                   WHEN           I37opz  <> '5'
030500070802     c                             and  I37fgs   = *zeros
030600071030     c                   eval      O37err  = 'E'
030700071030     c                   eval      O37msg  = $Msg(1)
030800071030     c                   eval      $Fine   = *on
030900070801     c                   leavesr
031000070801e   1c                   ENDSL
031100070801      *
031200070801      * Reperimento dati job
031300070801     c                   exsr      DatiJob
031400070801      *
031500070801      * Impostazione nome programma a video
031600070801     c                   movel     SDSpgm        V1Tpgm
031700070801      *
031800070801      * Impostazione attributi di visualizzazione in testata
031900071030     c                   eval      DA01    = C_HI_RI
032000070801sel 1c                   select
032100071030w   1c                   when      I37opz  = '1'
032200071030     c                   eval      V1Topz  = $Fun(1)
032300071030w   1c                   when      I37opz  = '2'
032400071030     c                   eval      V1Topz  = $Fun(2)
032500071030w   1c                   when      I37opz  = '3'
032600071030     c                   eval      V1Topz  = $Fun(3)
032700071030w   1c                   when      I37opz  = '4'
032800071030     c                   eval      V1Topz  = $Fun(4)
032900071030w   1c                   when      I37opz  = '5'
033000071030     c                   eval      V1Topz  = $Fun(5)
033100071030w   1c                   when      I37opz  = '7'
033200071030     c                   eval      V1Topz  = $Fun(6)
033300071030w   1c                   when      I37opz  = 'A'
033400071030     c                   eval      V1Topz  = $Fun(1)
033500071030w   1c                   when      I37opz  = 'C'
033600071030     c                   eval      V1Topz  = $Fun(3)
033700070801x   1c                   other
033800071030     c                   eval      DA01    = C_ND
033900070801e   1c                   endsl
034000070801      *
034100070801      * Impostazione automatica della filiale di gestione
034200070801sel 1c                   select
034300070801      * - ricevuta
034400070801w   1c                   when           I37opz  <> *blanks
034500070801     c                             and  I37fgs  <> *zeros
034600070801      * - impostazione di default per filiali di 2° livello
034700070801w   1c                   when           DUTlpo   = '2'
034800070801     c                              or  DUTlpo   = *blanks
034900071030     c                   eval      I37fgs  = DUTpou
035000070801      * - impostazione di default per filiali di 1° livello
035100070801x   1c                   other
035200071030     c                   eval      I37fgs  = DUTtfp
035300070801e   1c                   endsl
035400121130
035500121130      /free
035600121130         //?Verifico se anagrafica ritiri fissi già richiamata
035700121130         //?in questo caso non abilito il tasto funzione F2=Ritiro Fissi
035800121130         //?per evitare la chiamata ricorsiva
035900121130         wGiaChiamato = *off;
036000121130         XRTVSTKR (pStack:pStartPoint:pEntries);
036100121130         dsStack = pStack;
036200121130         IF  %lookup('FIOR02R   ':skStack) > 0;
036300121130           wgiachiamato = *on;
036400121130         ENDIF;
036500130731         //?Se richiamato da nuova interrogazione anagrafica ritiri
036600130731         //?non abilito F2 in quanto non serve e se lo lascio abilitato
036700130731         //?cliente, in queso caso non emette niente perchè F2 va per FGS
036800130731         IF not wGiaChiamato;
036900130731           XRTVSTKR (pStack:pStartPoint:pEntries);
037000130731           dsStack = pStack;
037100130731           IF  %lookup('FIOR81R   ':skStack) > 0;
037200130731             wgiachiamato = *on;
037300130731           ENDIF;
037400130731         ENDIF;
037500121130      /end-free
037600070801      *
037700070801     c                   ENDSR
037800070801
037900070801      *---------------------------------------------------------------*
038000070801      *?Reperimento Dati del job (Utente/Operativi)                  ?*
038100070801      *---------------------------------------------------------------*
038200070801     c     DatiJob       BEGSR
038300070801      *
038400070801     c     *dtaara       define    §azute        azuteds
038500070801     c     *dtaara       define    §datiute      ddatiute
038600070801      *
038700070801     c                   in(E)     *dtaara
038800070801     c                   IF        %ERROR or RSUT = *blanks
038900070801     c                   clear                   Tibs34Ds
039000070801     c                   call      'TIBS34R'
039100070801     c                   parm                    Tibs34Ds
039200070801     c                   in        *dtaara
039300070801     c                   ENDIF
039400070801      *
039500070801     c                   ENDSR
039600070801
039700070801      *---------------------------------------------------------------*
039800070801      *?Gestione videata D01                                         ?*
039900070801      *---------------------------------------------------------------*
040000070801     c     GesD01        BEGSR
040100070801      *
040200070801      * Inizializzazione videata
040300070801if  1c                   if        $InzD01 = *on
040400070801     c                   exsr      InzD01
040500070801     c                   eval      $InzD01 = (*in90 = *on)
040600070801e   1c                   endif
040700070801      *
040800070801      * Scrittura di testata e di riga tasti funzionali abilitati
040900070801     c                   write     OR37T01
041000070801     c                   write     OR37Z01
041100070801      *
041200070801      * Emissione videata
041300070801if  1c                   if             I37opz <> '4'
041400070801     c                             and  I37opz <> '5'
041500070801     c                   exfmt     OR37D01
041600070801x   1c                   else
041700070801     c                   write     OR37D01
041800070801     c                   exfmt     PROTECT
041900070801e   1c                   endif
042000070801     c                   setoff                                       28  90
042100070801     c                   clear                   V1Dmsg
042200070801      *
042300070801sel 1c                   SELECT
042400070802      * F2=Anagrafica ritiri fissi
042500070802w   1c                   WHEN      *inKB
042600070802     c                   exsr      F02D01
042700070801      * F3=Fine
042800070801w   1c                   WHEN      *inKC
042900070801     c                   exsr      F03D01
043000070801      * F12=Ritorno
043100070801w   1c                   WHEN      *inKL
043200070801     c                   exsr      F12D01
043300160713      * F19=DatiVariati
043400160713w   1c                   WHEN      *inKT
043500160713     c                   exsr      F19D01
043600070806      * Visualizzazione (nessun controllo, ma decodifiche)
043700070801w   1c                   WHEN      I37opz  = '5'
043800070802      * Inserimento/Modifica/Ripristino o Annullamento
043900070801x   1c                   OTHER
044000070802      * - Annullamento
044100070802if  2c                   if             I37opz  = '4'
044200070802     c                             or   *inKQ
044300070802     c                   exsr      Ctr_Annull
044400070802x   2c                   else
044500070802      * - Inserimento/Modifica/Ripristino
044600070801     c                   exsr      CtrD01
044700070802e   2c                   endif
044800070809if  2c                   if        NOT  *in90
044900070809      * - F5=Ripristino
045000070809     c                             and (*inKE
045100070802      * - F6=Conferma
045200070809     c                              or  *inKF
045300070802      * - F16=Annullamento
045400070802     c                              or  *inKQ)
045500070801     c                   exsr      F06D01
045600070801e   2c                   endif
045700070801e   1c                   ENDSL
045800070801      *
045900070801     c                   ENDSR
046000070801
046100070801      *---------------------------------------------------------------*
046200070801      *?Inizializzazione videata D01                                 ?*
046300070801      *---------------------------------------------------------------*
046400070801     c     InzD01        BEGSR
046500070801      *
046600070801     c                   clear                   OR37D01
046700070801     c                   clear                   FNACR000
046800070801     c                   clear                   FNACR100
046900070903     c                   clear                   SAVpocgi
047000070910     c                   clear                   SAVcgi
047100070910     c                   clear                   SAVcar
047200070910     c                   clear                   SAVlor
047300070910     c                   clear                   SAVprr
047400160713     c                   clear                   savfca
047500160713     c                   clear                   savmca
047600160713     c                   clear                   savsca
047700071030     c                   eval      SAVksc  = *hival
047800140113     c                   eval      *in41 = *off
047900140114
048000140114      /free
048100140114         //?Utente EDP
048200140114         *in39 = *off;
048300140114         IF  %subst(knmus:1:3) = 'EDP';
048400140114           *in39 = *on;
048500140114         ENDIF;
048600151012
048700151012         // -?Impostazione chiave d'accesso al file FNACRE1L per il?
048800160317         //  ?reperimento dei dati Alert Conferma Accettazione ORM?
048900151012         clear  keyFNACRE1;
049000151012         k_ACREcro = I37cro;
049100140114      /end-free
049200140114
049300070801      *
049400151012      * Reperimento dati
049500151012      *
049600070801sel 1c                   SELECT
049700070802      *
049800070801      * - Se richiesto inserimento:
049900070801      *   il controllo di effettiva inesistenza verrà eseguito nella
050000070801      *   subr. CTRD01.
050100070809w   1c                   WHEN           I37opz = '1'
050200070809     c                             or   I37opz = 'A'
050300071030     c                   eval      V2Ccd1  = I37fgs
050400130417     c                   eval      V2Ctcr  = 'R'
050500150909     c                   eval      V2Cpoa  = I37fgs
050600070802      *
050700070801      * - Se richiesta modifica/annullamento/ripristino:
050800070801      *   il record da aggiornare va allocato
050900070801w   1c                   WHEN           I37opz = '2'
051000070801     c                             or   I37opz = '4'
051100070801     c                             or   I37opz = '7'
051200070801     c     I37cro        chain     FNACR000
051300070801if  2c                   if        NOT  %found(FNACR01L)
051400070801     c                   seton                                        28  90
051500071030     c                   eval      V1Dmsg  = $Msg(2)
051600070801e   2c                   endif
051700071030     c     k02acr13      setll     FNACR100
051800070802      *
051900070801      * - Se richiesta visualizzazione/copia
052000070801      *   il record da visualizzare NON va allocato
052100070801w   1c                   WHEN           I37opz = '3'
052200070801     c                             or   I37opz = '5'
052300070809     c                             or   I37opz = 'C'
052400070801     c     I37cro        chain(n)  FNACR000
052500070801if  2c                   if        NOT  %found(FNACR01L)
052600070801     c                   seton                                        28  90
052700071030     c                   eval      V1Dmsg  = $Msg(2)
052800070801e   2c                   endif
052900071030     c     k02acr13      setll     FNACR100
053000070802      *
053100070801e   1c                   ENDSL
053200070801      *
053300070809      * Eventuali forzature
053400070802if  1c                   if             %found(FNACR01L)
053500071030     c                             and  I37opz  = '2'
053600070801     c                             and  I37fgs <> *zeros
053700150909     c                             and  I37fgs <>
053800150909     c                             %dec(%subst(%editc(ACRcro:'X'):1:3):3:0)
053900071030     c                   eval      I37opz  = '5'
054000070802e   1c                   endif
054100070801      *
054200070801      * Impostazione campi chiave a video
054300070809sel 1c                   select
054400070809w   1c                   when           I37opz  = '1'
054500071030     c                   eval      V2Ccd1    = I37fgs
054600071030     c                   eval      V2Cfcc    = 'S'
054700150909     c                   eval      V2Cpoa    = I37fgs
054800070809w   1c                   when           I37opz  = 'A'
054900070809     c                   movel     I37cro        ds_CRO
055000071030     c                   eval      V2Ccd1    = dsCRO1
055100071030     c                   eval      V2Ccd2    = dsCRO2
055200071030     c                   eval      V2Ccd3    = dsCRO3
055300071030     c                   eval      V2Cksc    = I37ksc
055400071030     c                   eval      V2Cfcc    = 'S'
055500150909     c                   eval      V2Cpoa    = V2Ccd1
055600070809x   1c                   other
055700150910     c                   movel     I37cro        ds_CRO
055800071030     c                   eval      V2Ccd1    = dsCRO1
055900071030     c                   eval      V2Ccd2    = dsCRO2
056000071030     c                   eval      V2Ccd3    = dsCRO3
056100070809e   1c                   endsl
056200070801      *
056300070801      * Impostazione altri campi a video
056400070802if  1c                   IF            (I37opz = '2'
056500070801     c                             or   I37opz = '3'
056600070801     c                             or   I37opz = '4'
056700070801     c                             or   I37opz = '5'
056800070809     c                             or   I37opz = '7'
056900070809     c                             or   I37opz = 'C')
057000070801     c                             and  %found(FNACR01L)
057100071030     c                   eval      V2Crsr    = ACRrsr
057200071030     c                   eval      V2Cinr    = ACRinr
057300071030     c                   eval      V2Ccar    = ACRcar
057400071030     c                   eval      V2Ccar    = ACRcar
057500071030     c                   eval      V2Clor    = ACRlor
057600071030     c                   eval      V2Cprr    = ACRprr
057700071030     c                   eval      V2Cnar    = ACRnar
057800071030if  2c                   IF        %equal(FNACR13L)
057900071030if  3c                   if             I37opz = '3'
058000071030     c                             or   I37opz = '5'
058100071030     c                             or   I37opz = 'C'
058200071030     c     k02acr13      reade(n)  FNACR100
058300071030x   3c                   else
058400071030     c     k02acr13      reade     FNACR100
058500071030e   3c                   endif
058600071030do  3c                   DoW       NOT  %eof(FNACR13L)
058700071030sel 4c                   select
058800071030w   4c                   when      ACR1atb  <> ACRatb
058900071030w   4c                   when      ACR1ain   = '='
059000071030     c                   eval      V2Cpocgi1 = ACR1pocgi
059100071030     c                   eval      V2Ccgi1   = ACR1cgi
059200071030w   4c                   when      ACR1ain   = '>'
059300071030     c                   eval      V2Cpocgi2 = ACR1pocgi
059400071030     c                   eval      V2Ccgi2   = ACR1cgi
059500071030w   4c                   when      ACR1ain   = '<'
059600071030     c                   eval      V2Cpocgi3 = ACR1pocgi
059700071030     c                   eval      V2Ccgi3   = ACR1cgi
059800071030e   4c                   endsl
059900071030if  4c                   if             I37opz = '3'
060000071030     c                             or   I37opz = '5'
060100071030     c                             or   I37opz = 'C'
060200071030     c     k02acr13      reade(n)  FNACR100
060300071030x   4c                   else
060400071030     c     k02acr13      reade     FNACR100
060500071030e   4c                   endif
060600071030e   3c                   EndDo
060700071030e   2c                   ENDIF
060800070801     c                   movel     ACRcco        ds_CRO
060900071030     c                   eval      V2Cco1    = dsCRO1
061000071030     c                   eval      V2Cco2    = dsCRO2
061100071030     c                   eval      V2Cco3    = dsCRO3
061200071030     c                   eval      V2Cksc    = ACRksc
061300091009if  2c                   if        ACRccc   = 999
061400091009     c                   clear                   v2cctr
061500091009     c                   else
061600091009     c                   move      ACRccc        V2Cctr
061700070801e   2c                   endif
061800071030     c                   eval      V2Ctcr    = ACRtcr
061900071030if  2c                   if        ACRfcc    = 'N'
062000071030     c                   eval      V2Cfcc    = ACRfcc
062100070802x   2c                   else
062200071030     c                   eval      V2Cfcc    = 'S'
062300070802e   2c                   endif
062400071030     c                   eval      V2Corr    = ACRorr
062500071030     c                   eval      V2Cper    = ACRper
062600071030     c                   eval      V2Ctel    = ACRtel
062700131113     c                   eval      dACR01    = ACRmai
062800140114     c                   eval      sav_§ACRstcr = dACR01.§ACRstcr
062900140114     c                   eval      sav_§ACRctcr = dACR01.§ACrctcr
063000131113      * - Valorizzazione degli orari solo SE effettivamente numerici
063100131113     c                   if        %check( c_Digits : %subst(
063200131113     c                                     ACRmai : 1
063300131113     c                                            : %len(dACR01.§ACRoa1) )
063400131113     c                                   ) = *zero
063500131113     c                   eval      V2ChmAmDa = %int( dACR01.§ACRoa1 )
063600131113     c                   endif
063700131113     c                   if        %check( c_Digits : %subst(
063800131113     c                                     ACRmai : %len(dACR01.§ACRoa1) + 1
063900131113     c                                            : %len(dACR01.§ACRoa2) )
064000131113     c                                   ) = *zero
064100131113     c                   eval      V2ChmAmA  = %int( dACR01.§ACRoa2 )
064200131113     c                   endif
064300131113     c                   if        %check( c_Digits : %subst(
064400131113     c                                     ACRmai : %len(dACR01.§ACRoa1) +
064500131113     c                                              %len(dACR01.§ACRoa2) + 1
064600131113     c                                            : %len(dACR01.§ACRoa3) )
064700131113     c                                   ) = *zero
064800131113     c                   eval      V2ChmPmDa = %int( dACR01.§ACRoa3 )
064900131113     c                   endif
065000131113     c                   if        %check( c_Digits : %subst(
065100131113     c                                     ACRmai : %len(dACR01.§ACRoa1) +
065200131113     c                                              %len(dACR01.§ACRoa2) +
065300131113     c                                              %len(dACR01.§ACRoa3) + 1
065400131113     c                                            : %len(dACR01.§ACRoa4) )
065500131113     c                                   ) = *zero
065600131113     c                   eval      V2ChmPmA  = %int( dACR01.§ACRoa4 )
065700131113     c                   endif
065800071030     c                   eval      V2Cfcl    = ACRfcl
065900071030     c                   eval      V2Cfbn    = ACRfbn
066000071030     c                   eval      V2Cfpk    = ACRfpk
066100071030     c                   eval      V2Cfmc    = ACRfmc
066200071030     c                   eval      V2Cfbl    = ACRfbl
066300071030     c                   eval      V2Cfmt    = ACRfmt
066400071030     c                   eval      V2Cfat    = ACRfat
066500071030     c                   eval      V2Cspi    = ACRspi
066600071030     c                   eval      V2Cntm    = ACRntm
066700071030     c                   eval      V2Cno1    = ACRno1
066800071030     c                   eval      V2Cno2    = ACRno2
066900140113      /free
067000140113       //?Se frequenza ritiro impostata in automatico da pgm contabili
067100140113       //?non posso toccarla quindi la proteggo
067200140113         IF  dACR01.§ACRctcr = 'C';
067300140113           *in40 = *on;
067400140113         ENDIF;
067500140113       //?Imposto la frequenza ritiro salvata
067600140113         V2Cstcr = dACR01.§ACRstcr;
067700150909       //?Filiale ritiro forzata
067800150909         V2Cpoa = ACRpoa;
067900160317       // -?Flag x Conferma Accettazione ORM?
068000151104         V2Cfca = dacr01.§ACRfmar;
068100160713         savfca = dacr01.§ACRfmar;
068200160317       // -?E-Mail Conferma Accettazione ORM?
068300160317         k_ACREtrc = 'MC';
068400160317         chain(n)  %kds( keyFNACRE1 )  FNACRE00;
068500160317         IF  %found(FNACRE1L);
068600151012         V2Cmca = ACREdati;
068700160713         savmca = ACREdati;
068800160317         ENDIF;
068900160317       // -?E-Mail Conferma Accettazione ORM?
069000160317         k_ACREtrc = 'SC';
069100160317         chain(n)  %kds( keyFNACRE1 )  FNACRE00;
069200160317         IF  %found(FNACRE1L);
069300160317           V2Csca = ACREdati;
069400160713           savsca = ACREdati;
069500160317         ENDIF;
069600140113      /end-free
069700140113
069800070802e   1c                   ENDIF
069900070801      *
070000070801      * Abilitazione tasti funzionali:
070100070801      * - F2=Anagr.Fissi
070200070802     c                   eval      *in02   = *on
070300121130      /free
070400121130         //?Se anagrafica ritiri fissi già richiamata non abilito F2
070500121130           IF  wGiaChiamato;
070600121130             *in02 = *off;
070700121130           ENDIF;
070800121130      /end-free
070900070801      * - F3=Fine
071000070809     c                   eval      *in03   = (I37opz <> 'A'    and
071100070809     c                                        I37opz <> 'C')
071200070801      * - F5=Riattivazione
071300070809     c                   eval      *in05   = (I37opz  = '2'    and
071400070801     c                                        %found(FNACR01L) and
071500070801     c                                        ACRatb <> *blanks)
071600070801      * - F6=Conferma
071700070809     c                   eval      *in06   = (I37opz <> '5')
071800070828      * - F7=Interrogazione Giri di Ritiro
071900070828     c                   eval      *in07   = (I37opz <> '4'    and
072000070828     c                                        I37opz <> '5')
072100070801      * - F16=Annullamento
072200070809     c                   eval      *in16   = (I37opz  = '2'    and
072300070801     c                                        %found(FNACR01L) and
072400140723     c                                        ACRatb  = *blanks and
072500140807     c                                        (V2Ccd3 > 0 or
072600140807     c                                        (V2Ccd3 = 0 and
072700140807     c                                        (V2Ccd2 = 8888 or
072800140807     c                                         V2Ccd2 = 9999))))
072900070809      * - F12=Ritorno
073000070809     c                   eval      *in12   = (I37opz <> 'A'    and
073100070809     c                                        I37opz <> 'C')
073200160713      /free
073300160713       //?F19=Variazione dati Alert Conferma Ritiro
073400160713         *in19 = *off;
073500160713         clear dOREana;
073600160713         k_ACREtrc = 'AA';
073700160713         chain(n)  %kds( keyFNACRE1 )  FNACRE00;
073800160713         IF  %found(FNACRE1L) and *in39;
073900160713           dOREana = ACREdati;
074000160713           *in19 = *on;
074100160713         ENDIF;
074200160713      /end-free
074300070801      *
074400070801      * Protezione campo chiave a video
074500070802     c                   eval      *in41   = (I37opz <> '1'    and
074600070809     c                                        I37opz <> '3'    and
074700070809     c                                        I37opz <> 'A'    and
074800070809     c                                        I37opz <> 'C')
074900070802      * Protezione codice P.d.C.  e Tariffa
075000070801     c                   eval      *in42   = (I37ksc <> *zeros)
075100070802      * Evidenziazione record annullato
075200070801     c                   eval      *in43   = (ACRatb <> *blanks)
075300070802      *
075400070802      * Impostazione F5=Annullamento o F16=Ripristino
075500070802sel 1c                   select
075600070802w   1c                   when      *in05
075700070802     c                   eval      Z1Df16  = 'F5=Riattivazione'
075800070802w   1c                   when      *in16
075900070802     c                   eval      Z1Df16  = 'F16=Annullamento'
076000070802e   1c                   endsl
076100070911      *
076200070911      * Decodifiche
076300070911if  1c                   if             %found(FNACR01L)
076400070911     c                             or   I37opz  = 'A'
076500110121     c                   eval      wdecod='S'
076600070911     c                   exsr      CtrD01
076700110121     c                   eval      wdecod=' '
076800070911     c                   setoff                                       28  90
076900070911     c                   clear                   V1Dmsg
077000070911     c                   movea     *zeros        *in(50)
077100070911e   1c                   endif
077200070801      *
077300070801     c                   ENDSR
077400070801
077500070801      *---------------------------------------------------------------*
077600070801      *?Gestione tasto funzionale F2 da videata D01                  ?*
077700070801      *?F2=Gestione anagrafica ritiri fissi                          ?*
077800070801      *---------------------------------------------------------------*
077900070801     c     F02D01        BEGSR
078000070801      *
078100070801     c                   clear                   FIOR02ds
078200070801if  1c                   if        I37opz  = '5'
078300070801     c                   eval      I02ric  = '1'
078400070801x   1c                   else
078500070801     c                   eval      I02ric  = '2'
078600070801e   1c                   endif
078700070801     c                   eval      I02cro  = V2Ccd1 * 10000000
078800070801     c                                     + V2Ccd2 * 1000
078900070801     c                                     + V2Ccd3
079000070801     c                   eval      I02poa  = I37fgs
079100070801     c                   call      'FIOR02R'
079200070801     c                   parm                    KPJBA
079300070801     c                   parm                    FIOR02ds
079400070801      *
079500070828if  1c                   if        O02err <> *blanks
079600070801     c                   seton                                        28  90
079700070801     c                   eval      V1Dmsg  = O02msg
079800070801e   1c                   endif
079900070801      *
080000070801     c                   ENDSR
080100070801
080200070801      *---------------------------------------------------------------*
080300070801      *?Gestione tasto funzionale F3 da videata D01                  ?*
080400070801      *?F3=Fine                                                      ?*
080500070801      *---------------------------------------------------------------*
080600070801     c     F03D01        BEGSR
080700070801      *
080800070801      * Disallocazione del record
080900070801if  1c                   if             I37opz = '2'
081000070801     c                             or   I37opz = '4'
081100070801     c                             or   I37opz = '7'
081200070801     c                   UNLOCK    FNACR01L
081300071030     c                   UNLOCK    FNACR13L
081400070801e   1c                   endif
081500070802      *
081600070803      * Impostazione avviso di ritorno con F3
081700070803     c                   eval      O37ret  = *on
081800070801      *
081900070801      * Chiusura del programma
082000070801     c                   eval      $Fine   = *on
082100070801      *
082200070801     c                   ENDSR
082300070801
082400070801      *---------------------------------------------------------------*
082500070801      *?Gestione tasto funzionale F6 da videata D01                  ?*
082600070801      *?F6=Conferma                                                  ?*
082700070801      *---------------------------------------------------------------*
082800070801     c     F06D01        BEGSR
082900070801      *
083000070801      * Aggiornamento record
083100070801     c                   exsr      Upd_FNACR
083200070801      *
083300070801      * Chiusura del programma
083400070801if  1c                   if        NOT  *in90
083500070910if  2c                   if             I37opz = '1'
083600070910     c                             or   I37opz = '3'
083700070910     c                             or   I37opz = 'A'
083800070910     c                             or   I37opz = 'C'
083900070910     c                   eval      O37cro  = I37cro
084000070910e   2c                   endif
084100070801     c                   eval      $Fine   = *on
084200070801e   1c                   endif
084300070801      *
084400070801     c                   ENDSR
084500070801
084600070801      *---------------------------------------------------------------*
084700070801      *?Gestione tasto funzionale F12 da videata D01                 ?*
084800070801      *?F12=Ritorno                                                  ?*
084900070801      *---------------------------------------------------------------*
085000070801     c     F12D01        BEGSR
085100070801      *
085200070801      * Disallocazione del record
085300070801if  1c                   if             I37opz = '2'
085400070801     c                             or   I37opz = '4'
085500070801     c                             or   I37opz = '7'
085600070801     c                   UNLOCK    FNACR01L
085700071030     c                   UNLOCK    FNACR13L
085800070801e   1c                   endif
085900070803      *
086000070803      * Impostazione avviso di ritorno con F12
086100070803     c                   eval      O37ret  = '2'
086200070801      *
086300070801      * Chiusura del programma
086400070801     c                   eval      $Fine   = *on
086500070801      *
086600070801     c                   ENDSR
086700160713
086800160713      /free
086900160713       //--------------------------------------------------------------
087000160713       //?Gestione tasto funzionale F19 da videata D01
087100160713       //?F19=Variazione dati Alert
087200160713       //--------------------------------------------------------------
087300160713       BEGSR F19D01;
087400160713
087500160713       //?Inizializzazione videata
087600160713         exsr InzW03;
087700160713
087800160713         DOW not FineW03;
087900160713
088000160713         //?Emissione videata
088100160713           exfmt  OR37W03;
088200160713
088300160713         //?- F12=Ritorno
088400160713           IF  *inkl;
088500160713             FineW03 = *on;
088600160713           ENDIF;
088700160713
088800160713         ENDDO;
088900160713
089000160713       ENDSR;
089100160713
089200160713       //--------------------------------------------------------------
089300160713       //?Inizializzazione Videata W03.
089400160713       //--------------------------------------------------------------
089500160713       BEGSR InzW03;
089600160920
089700160920         FineW03 = *off;
089800160713
089900160713         clear  OR37W03;
090000160713         Data_EUR = %date(§OREdtv:*ISO);
090100160713         W03data = %dec(Data_EUR);
090200160713         W03ora  = §OREorv;
090300160713         IF  §OREtipo = 'I';
090400160713           W03who = 'Cliente: ' + %trim(§OREute);
090500160713           W03who = %trim(W03who) + ' IP: ' + %trim(§OREjob);
090600160713         ELSE;
090700160713           W03who = 'Utente: ' + %trim(§OREute);
090800160713           W03who = %trim(W03who) + ' Video: ' + %trim(§OREjob);
090900160713         ENDIF;
091000160713
091100160713       ENDSR;
091200160713      /end-free
091300070911
091400070911      *---------------------------------------------------------------*
091500070911      *?Controllo e decodifica codice giro di ritiro                 ?*
091600070911      *---------------------------------------------------------------*
091700070911     c     Ctr_Giro      BEGSR
091800071030      *
091900071030      * - immessa solo la filiale (senza codice giro)
092000071030if  1c                   if             Wpocgi  <> *zeros
092100071030     c                             and  Wcgi     = *blanks
092200071105     c                   clear                   Wpocgi
092300071105     c                   clear                   WcgiD
092400071030e   1c                   endif
092500071030      *
092600071030      * - immesso solo il codice giro (senza filiale)
092700071030if  1c                   if             Wpocgi   = *zeros
092800071030     c                             and  Wcgi    <> *blanks
092900071105     c                   eval      Wpocgi   =  I37fgs
093000071030e   1c                   endif
093100071030      *
093200071030      * - controllo codice filiale del giro ritiro
093300071105      *   (filiale giro diversa dalla filiale gestione)
093400071105if  1c                   if             Wpocgi  <> I37fgs
093500071105     c                             and  Wcgi    <> *blanks
093600071105     c                   seton                                        286090
093700071030     c                   eval      V1Dmsg  = $Msg(18)
093800071030     c                   leavesr
093900071030e   1c                   endif
094000070911      *
094100071030      * - controllo giro CON data validità ("bypassabile")
094200071031if  1c                   IF             Wpocgi  <> D09ifgs
094300071031     c                             or   Wcgi    <> D09icgi
094400070911     c                   clear                   FIDG09ds
094500070911     c                   eval      D09iop0   = '001'
094600071030     c                   eval      D09ifgs   = Wpocgi
094700071030     c                   eval      D09icgi   = Wcgi
094800070911     c                   eval      D09idat   = *date
094900170330     c****               eval      D09itug   = 'R'
095000070911     c                   movel(p)  FIDG09ds      KPJBU
095100070911     c                   call      'FIDG09R'
095200070911     c                   parm                    KPJBA
095300070911     c                   movel     KPJBU         FIDG09ds
095400071105e   1c                   ENDIF
095500071105      *
095600170330     c                   eval      dgrgflo = d09oflo
095700071105sel 1c                   select
095800071105w   1c                   when      D09oerr   = *on
095900071105     c                   movel     D09odes       WcgiD
096000070911     c                   seton                                        286090
096100070911     c                   eval      V1Dmsg    = $Msg(29)
096200071112     c                   leavesr
096300170330     c                   when      §GRGfit <> 'S'
096400170330     c                   movel     D09odes       WcgiD
096500170330     c                   seton                                        286090
096600170330     c                   eval      V1Dmsg    = $Msg(35)
096700170330     c                   leavesr
096800071105w   1c                   when      D09ocgi  <> *blanks
096900071030     c                   movel     D09odes       WcgiD
097000071105e   1c                   endsl
097100070911      *
097200070911     c                   ENDSR
097300070801
097400070801      *---------------------------------------------------------------*
097500070801      *?Controllo videata D01                                        ?*
097600070801      *---------------------------------------------------------------*
097700070801     c     CtrD01        BEGSR
097800070801      *
097900070806     c                   movea     *zeros        *in(50)
098000070806      *
098100150909     c                   clear                   V2Dpoa
098200070802     c                   clear                   ds_CRO
098300070802     c                   eval      dsCRO1  = V2Ccd1
098400070802     c                   eval      dsCRO2  = V2Ccd2
098500070802     c                   eval      dsCRO3  = V2Ccd3
098600070802      *
098700170330      * Codice cliente ritiro
098800070802      * - Se IMMISSIONE (NON COPIA):
098900070802      *   i primi 7 caratteri del codice devono esistere in CNACO
099000070802if  1c                   if             I37opz  = '1'
099100070809     c                             or   I37opz  = 'A'
099200070802     c                   clear                   TIBS69ds
099300070802     c                   moveL     ds_CRO        I69kac
099400070809     c                   if            (I37opz  = '1'
099500070809     c                              or  I37opz  = 'A')
099600070809     c                             and  $InzD01 = *on
099700070809     c                   moveL     ds_CRO        I69kin
099800070809     c                   endif
099900070802     c                   exsr      Call_TIBS69
100000070809sel 2c                   select
100100070809w   2c                   when      *in90
100200070802     c                   eval      *in51   = *on
100300070802     c                   eval      V1Dmsg  = $Msg(7)
100400070802     c                   leavesr
100500070809x   2c                   when          (I37opz  = '1'
100600070809     c                              or  I37opz  = 'A')
100700070809     c                             and  $InzD01 = *on
100800070809     c                   movel     ACOrag        V2Crsr
100900070809     c                   movel     INDvia        V2Cinr
101000070809     c                   movel     INDcae        V2Ccar
101100070809     c                   movel     INDcit        V2Clor
101200070809     c                   movel     INDprv        V2Cprr
101300070809     c                   movel     INDsta        V2Cnar
101400080421     c                   movel     INDtel        V2Ctel
101500070809e   2c                   endsl
101600070802e   1c                   endif
101700070802      * - Se IMMISSIONE O COPIA:
101800070802if  1c                   if             I37opz  = '1'
101900070802     c                             or   I37opz  = '3'
102000070809     c                             or   I37opz  = 'A'
102100070809     c                             or   I37opz  = 'C'
102200070802      * - - controllo completo inserimento
102300070802if  2c                   IF        V2Ccd2  = *zeros
102400070802     c                   seton                                        285190
102500070802     c                   eval      V1Dmsg  = $Msg(8)
102600070802     c                   leavesr
102700070802      * - - controllo se già esistente
102800070802x   2C                   ELSE
102900070802     c                   move      ds_CRO        ACRcro
103000070802     c     ACRcro        chain(n)  FNACR000
103100070802if  3c                   if        %found(FNACR01L)
103200070802     c                   seton                                        285190
103300070802     c                   eval      V1Dmsg  = $Msg(9)
103400070802     c                   leavesr
103500070802e   3c                   endif
103600070802e   2c                   ENDIF
103700140723      /free
103800140723       //?Se NON esiste il luogo '000' devo per forza creare quello
103900140723         IF  dscro3 <> 000;
104000140723           clear ACRcro;
104100140806           ACRcro = (dscro1 * 10000000) + (dscro2 * 1000);
104200140723           chain ACRcro FNACR01L;
104300140723           IF  not %found(FNACR01L);
104400140723             *in28 = *on;
104500140723             *in51 = *on;
104600140723             *in90 = *on;
104700140723             V1Dmsg = $msg(32);
104800140723             leavesr;
104900140723           ENDIF;
105000140723         ENDIF;
105100140723      /end-free
105200070802e   1c                   endif
105300070802      *
105400070802      * Ragione sociale, Indirizzo, Località, CAP, Prov., Naz.
105500110121     c                   if        wdecod=' '
105600070802     c                   clear                   FNLV14ds
105700070802     c                   eval      I14dri  = *date
105800070802     c                   movel     V2Crsr        I14rsc
105900070802     c                   movel     V2Cinr        I14ind
106000070802     c                   movel     V2Clor        E14loc
106100070802     c                   movel     V2Cprr        E14prv
106200070802     c                   movel     V2Ccar        E14cap
106300070802     c                   movel     V2Cnar        E14nar
106400070802     c                   call      'FNLV14R'
106500070802     c                   parm                    KPJBA
106600070802     c                   parm                    FNLV14ds
106700070802     c                   movel     I14ind        V2Cinr
106800070802     c                   movel     E14loc        V2Clor
106900070802     c                   movel     E14prv        V2Cprr
107000070802     c                   movel     E14cap        V2Ccar
107100070802     c                   movel     E14nar        V2Cnar
107200070802if  1c                   if        O14msg <> *blanks
107300070802     c                   eval      *in53   = (O14err = '1')                     Ragione Sociale
107400070802     c                   eval      *in54   = (O14err = '2')                     Indirizzo
107500070802     c                   eval      *in55   = (O14err = '5')                     CAP
107600070802     c                   eval      *in56   = (O14err = '3')                     Località
107700070802     c                   eval      *in57   = (O14err = '4')                     Provincia
107800070802     c                   eval      *in58   = (O14err = '6')                     Nazione
107900070802     c                   seton                                        28  90
108000070802     c                   eval      V1Dmsg  = O14msg
108100070802     c                   leavesr
108200070802e   1c                   endif
108300070802      * - controllo specifico CAP
108400070802     c                   exsr      Ctr_CAP
108500070802if  1c                   if        *in90
108600070802     c                   leavesr
108700070802e   1c                   endif
108800110121     c                   endif
108900070801      *
109000070802      * Giro di ritiro
109100071105do  1c                   DO        C_MaxCGI      wNrCGI
109200071030      *
109300071031     c                   clear                   WcgiD
109400071030sel 2c                   select
109500071030w   2c                   when      wNrCGI  = 1
109600071030     c                   eval      Wpocgi  = V2Cpocgi1
109700071030     c                   eval      Wcgi    = V2Ccgi1
109800071030w   2c                   when      wNrCGI  = 2
109900071030     c                   eval      Wpocgi  = V2Cpocgi2
110000071030     c                   eval      Wcgi    = V2Ccgi2
110100071030w   2c                   when      wNrCGI  = 3
110200071030     c                   eval      Wpocgi  = V2Cpocgi3
110300071030     c                   eval      Wcgi    = V2Ccgi3
110400071030e   2c                   endsl
110500070911      *
110600071030      * - controllo codici giri ritiro
110700071031sel 2c                   Select
110800071105w   2c                   When      Wcgi    = *blanks
110900071105     c                   clear                   Wpocgi
111000071031      *   - interrogazione giri di ritiro
111100071031w   2c                   When      %scan('?' : %trimr(Wcgi)) > *zeros
111200071031     c                   exsr      Search_Giro
111300071030      *   - controllo codici giri ritiro
111400071031x   2c                   Other
111500070911     c                   exsr      Ctr_Giro
111600071031e   2c                   EndSl
111700071031      *
111800071031sel 2c                   Select
111900071031w   2c                   When      wNrCGI  = 1
112000071030     c                   eval      V2Cpocgi1 = Wpocgi
112100071031     c                   eval      V2Ccgi1   = Wcgi
112200071030     c                   eval      V2Dcgi1   = WcgiD
112300071031w   2c                   When      wNrCGI  = 2
112400071030     c                   eval      V2Cpocgi2 = Wpocgi
112500071031     c                   eval      V2Ccgi2   = Wcgi
112600071030     c                   eval      V2Dcgi2   = WcgiD
112700071105     c                   eval      *in76   = (*in59 = *on)
112800071030     c                   eval      *in77   = (*in60 = *on)
112900071030     c                   eval      *in59   = *off
113000071030     c                   eval      *in60   = *off
113100071031w   2c                   When      wNrCGI  = 3
113200071030     c                   eval      V2Cpocgi3 = Wpocgi
113300071031     c                   eval      V2Ccgi3   = Wcgi
113400071030     c                   eval      V2Dcgi3   = WcgiD
113500071105     c                   eval      *in78   = (*in59 = *on)
113600071030     c                   eval      *in79   = (*in60 = *on)
113700071030     c                   eval      *in59   = *off
113800071030     c                   eval      *in60   = *off
113900071031e   2c                   EndSl
114000071031if  2c                   if        *in90
114100070911     c                   leavesr
114200071031e   2c                   endif
114300071030      *
114400071030e   1c                   ENDDO
114500070802      *
114600070802      * Codice cliente Ordinante
114700070802     c                   clear                   V2Ddeo
114800070802if  1c                   IF             V2Cco1 <> *zeros
114900070802     c                             or   V2Cco2 <> *zeros
115000070802     c                             or   V2Cco3 <> *zeros
115100070802      * - Tipo anagrafica "R" = cliente Ritiro
115200070809     c                   eval      ACRcro  = V2Cco1 * 10000000
115300070809     c                                     + V2Cco2 * 1000
115400070809     c                                     + V2Cco3
115500070802     c     ACRcro        chain(n)  FNACR000
115600071030if  2c                   if             %found(FNACR01L)
115700070802     c                             and  ACRatb  = *blanks
115800070802     c                   movel     ACRrsr        V2Ddeo
115900071030x   2c                   else
116000070802     c                   seton                                        286190
116100070802     c                   eval      V1Dmsg  = $Msg(20)
116200070802     c                   leavesr
116300071030e   2c                   endif
116400071030if  2c                   if             V2Cco1  = V2Ccd1
116500070802     c                             and  V2Cco2  = V2Ccd2
116600070802     c                             and  V2Cco3  = V2Ccd3
116700070802     c                   seton                                        286190
116800070802     c                   eval      V1Dmsg  = $Msg(20)
116900070802     c                   leavesr
117000071030e   2c                   endif
117100070802      *
117200070802e   1c                   ENDIF
117300070802      *
117400070802      * Codice P.d.C.
117500070802     c                   clear                   V2Dksc
117600070802if  1c                   IF        V2Cksc <> *zeros
117700070802     c                   clear                   TIBS69ds
117800070802     c                   eval      I69kac  = V2Cksc
117900070802     c                   exsr      Call_TIBS69
118000070802if  2c                   if        *in90
118100070802     c                   seton                                        286290
118200070802     c                   eval      V1Dmsg  = $Msg(22)
118300070802     c                   leavesr
118400070802e   2c                   endif
118500070802     c                   clear                   Og143
118600070802     c                   moveL     V2Cksc        ORGfil
118700070802     c     ORGfil        chain     AZORG
118800070802if  2c                   if        %found(AZORG01L)
118900070802     c                   eval      Og143   = ORGde3
119000070802e   2c                   endif
119100070802if  2c                   if             §OGntw  = 'LOG'
119200070802     c                             or   §OGntw  = 'XXX'
119300070802     c                   seton                                        286290
119400070802     c                   eval      V1Dmsg  = $Msg(22)
119500070802     c                   leavesr
119600070802e   2c                   endif
119700070802     c                   movel     ACOrag        V2Dksc
119800070802e   1c                   ENDIF
119900070802      *
120000070802      * Tariffa
120100091009if  1c                   IF        V2Cctr <> *blanks
120200070802if  2c                   if        V2Cksc  = *zeros
120300070802     c                   seton                                        286390
120400070802     c                   eval      V1Dmsg  = $Msg(23)
120500070802     c                   leavesr
120600070802e   2c                   endif
120700070802      * - verifica esistenza tariffe per il cliente
120800070802     c                   eval      $Tam    = *off
120900070802     c                   eval      TAMksc  = V2Cksc
121000091009     c                   move      v2cctr        TAMctr
121100070802     c     k02tam01      setgt     TNTAM000
121200070802     c     k02tam01      readpe    TNTAM000
121300070802do  1c                   dow       NOT  %eof(TNTAM01L)
121400070802if  2c                   if        TAMatb  = *blanks
121500070802     c                   eval      $Tam    = *on
121600070802     c                   leave
121700070802e   2c                   endif
121800070802     c     k02tam01      readpe    TNTAM000
121900070802e   1c                   enddo
122000070802if  1c                   if        $Tam    = *off
122100070802     c                   seton                                        286390
122200070802     c                   eval      V1Dmsg  = $Msg(24)
122300070802     c                   leavesr
122400070802e   1c                   endif
122500070802e   1c                   ENDIF
122600070802      *
122700070802      * Frequenza Ritiro
122800070802      * - F = Fisso
122900070802      * - R = Ricorrente
123000151104      * - O = No Internet
123100070802      * - M = Mai
123200151104      /free
123300151104        IF  %scan('?':V2Ctcr) > 0;
123400151104          FineTcr = *off;
123500151104          clear savfreq;
123600151104          exsr RicTcr;
123700151104          V2Ctcr = savfreq;
123800151104        ENDIF;
123900151104      /end-free
124000070802if  1c                   if             V2Ctcr <> 'F'
124100070802     c                             and  V2Ctcr <> 'R'
124200070802     c                             and  V2Ctcr <> 'O'
124300070802     c                             and  V2Ctcr <> 'M'
124400070802     c                   seton                                        286490
124500070802     c                   eval      V1Dmsg  = $Msg(25)
124600070802     c                   leavesr
124700070802e   1c                   endif
124800150909
124900150909      /free
125000150909       //?Filiale ritiro forzata
125100150909         IF  V2Cpoa = 0;
125200150909           V1Dmsg = $msg(33);
125300150909           *in28 = *on;
125400150909           *in52 = *on;
125500150909           *in90 = *on;
125600150909           leavesr;
125700150909         ENDIF;
125800150909         chain (V2Cpoa) AZORG01L;
125900150909         IF  not %found(AZORG01L) or ORGfva <> *blanks;
126000150909           V1Dmsg = $msg(33);
126100150909           *in28 = *on;
126200150909           *in52 = *on;
126300150909           *in90 = *on;
126400150909           leavesr;
126500150909         ENDIF;
126600150909         V2Dpoa = ORGdes;
126700150909      /end-free
126800070802      *
126900070802      * Ora ritiro
127000070802if  1c                   if        V2Corr <> *zeros
127100070802     c                   movel     V2Corr        ds_Time
127200070802if  2c                   if             dsHH    < 1
127300070802     c                             or   dsHH    > 24
127400070802     c                             or   dsMM    > 59
127500070802     c                   seton                                        286590
127600070802     c                   eval      V1Dmsg  = $Msg(26)
127700070802     c                   leavesr
127800070802e   2c                   endif
127900070802e   1c                   endif
128000070802      *
128100070802      * Referente: numero di telefono
128200070802if  1c                   IF        V2Ctel <> *blanks
128300070802     c                   clear                   TRUL42ds
128400070802     c                   eval      D42fax  = V2Ctel
128500070802     c                   call      'TRUL42R'
128600070802     c                   parm                    TRUL42ds
128700070802if  2c                   if        D42err  = *on
128800070802     c                   seton                                        286790
128900070802     c                   eval      V1Dmsg  = D42msg
129000070802     c                   leavesr
129100070802e   2c                   endif
129200070802e   1c                   ENDIF
129300070802      *
129400131113      * Orari Apertura
129500131113     c                   exsr      CtrlOrariApert
129600131113     c                   if        *in28
129700131113     c                   leavesr
129800131113     c                   endif
129900070802      *
130000070802      * Flags immissione
130100070802     c                   select
130200070802      * - Colli
130300070802     c                   when           V2Cfcl <> *blanks
130400070802     c                             and  V2Cfcl <> 'F'
130500070802     c                             and  V2Cfcl <> 'O'
130600070802     c                   seton                                        286990
130700070802      * - Bancali
130800070802     c                   when           V2Cfbn <> *blanks
130900070802     c                             and  V2Cfbn <> 'F'
131000070802     c                             and  V2Cfbn <> 'O'
131100070802     c                   seton                                        287090
131200070802      * - Peso   (kg)
131300070802     c                   when           V2Cfpk <> *blanks
131400070802     c                             and  V2Cfpk <> 'F'
131500070802     c                             and  V2Cfpk <> 'O'
131600070802     c                   seton                                        287190
131700070802      * - Volume (m³)
131800070802     c                   when           V2Cfmc <> *blanks
131900070802     c                             and  V2Cfmc <> 'F'
132000070802     c                             and  V2Cfmc <> 'O'
132100070802     c                   seton                                        287290
132200070802      * - Bilico
132300070802     c                   when           V2Cfbl <> *blanks
132400070802     c                             and  V2Cfbl <> 'F'
132500070802     c                             and  V2Cfbl <> 'O'
132600070802     c                   seton                                        287390
132700070802      * - Motrice
132800070802     c                   when           V2Cfmt <> *blanks
132900070802     c                             and  V2Cfmt <> 'F'
133000070802     c                             and  V2Cfmt <> 'O'
133100070802     c                   seton                                        287490
133200070802      * - Autotreno
133300070802     c                   when           V2Cfat <> *blanks
133400070802     c                             and  V2Cfat <> 'F'
133500070802     c                             and  V2Cfat <> 'O'
133600070802     c                   seton                                        287590
133700070802     c                   endsl
133800070802     c                   if        *in28  and  *in90
133900070802     c                   eval      V1Dmsg  = $Msg(27)
134000070802     c                   leavesr
134100070802     c                   endif
134200070802      *
134300070802      * Se COPIA   &   codice piano dei conti non cambiato
134400070802      * => messaggio d'avvertimento
134500070809if  1c                   if            (I37opz  = '3'
134600070809     c                              or  I37opz  = 'C')
134700070910     c                             and (ACRksc <> *zeros
134800070910     c                             and  V2Cksc  = ACRksc
134900070910     c                             and  V2Cksc <> SAVksc)
135000070809     c                   eval      SAVksc  = V2Cksc
135100070802     c                   seton                                        286290
135200070802     c                   eval      V1Dmsg  = $Msg(28)
135300070802     c                   leavesr
135400070809e   1c                   endif
135500151012      *
135600151012      /free
135700151012
135800151012         // -?Controllo e-Mail Conferma Accettazione?
135900151012         If  V2Cmca <> *blank;
136000151012           clear dsEmail;
136100151012           §emlIndI = V2Cmca;
136200151012           xEmail( dsEmail );
136300151012           if §emlErrO = *on;
136400151012             *in66  = *on;
136500151012             *in28  = *on;
136600151012             *in90  = *on;
136700151012             V1Dmsg = §emlMsgO;
136800151012             leavesr;
136900151012           endif;
137000151012           V2Cmca = §emlIndO;
137100151012         EndIf;
137200160317       //?SMS
137300160317         IF  V2Csca <> *blanks;
137400160317           pInCell = %trim(V2Csca);
137500160317           clear pOutErr;
137600160317           clear pOutCell;
137700160317           IF  UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
137800160317             *in84 = *on;
137900160317             *in28 = *on;
138000160317             *in90 = *on;
138100160317             V1Dmsg = $msg(34);
138200160317             leavesr;
138300160317           ENDIF;
138400160317           IF  pOutCell <> *blanks;
138500160317             V2Csca = pOutCell;
138600160317           ENDIF;
138700160317         ENDIF;
138800151012
138900151012      /end-free
139000070801      *
139100070801     c                   ENDSR
139200131113
139300131113      /free
139400131113
139500131113       //--------------------------------------------------------------
139600131113       //?Controllo orari di apertura.                                 ?
139700131113       //--------------------------------------------------------------
139800131113       BEGSR  CtrlOrariApert;
139900131113
140000131113         // -?Richiamo *pgm per controllo orari di apertura del cliente:?
140100131113         //  ?si deve anche controllare che ci sia abbastanza spazio?
140200131113         //  ?nella Nota-1 e nella Nota-2 per poterli inserire in questi?
140300131113         //  ?campi per poi stamparli?
140400131113         If  (V2ChmAMda + V2ChmAMa + V2ChmPMda + V2ChmPMa) > *zero;
140500131113
140600131113           clear  TRUL03ds;
140700131113           TRUL03ds.i03hm1 = V2ChmAMda;
140800131113           TRUL03ds.i03hm2 = V2ChmAMa;
140900131113           TRUL03ds.i03hm3 = V2ChmPMda;
141000131113           TRUL03ds.i03hm4 = V2ChmPMa;
141100131113           TRUL03ds.i03nt1 = V2Cno1;
141200131113           TRUL03ds.i03nt2 = V2Cno2;
141300131113
141400131113           trul03r ( TRUL03ds );
141500131113
141600131113           If  TRUL03ds.o03err <> *off;
141700131113             V1Dmsg = TRUL03ds.o03msg;
141800131113             *in28 = *on;
141900131113             *in(79 + TRUL03ds.o03errpos) = *on;
142000131113           EndIf;
142100131113
142200131113         EndIf;
142300131113
142400131113       ENDSR;
142500151104
142600151104       //--------------------------------------------------------------
142700151104       //?Ricerca Frequenza Ritiro.                                    ?
142800151104       //--------------------------------------------------------------
142900151104       BEGSR RicTcr;
143000151104
143100151104       //?Inizializzazione videata
143200151104         exsr InzS02;
143300151104
143400151104       //?Visualizzazione del SFL
143500151104         *in30 = (S02nrr <= *zeros);
143600151104
143700151104         DOW not FineTcr;
143800151104
143900151104         //?Emissione videata
144000151104           exfmt  OR37C02;
144100151104           *in28 = *off;
144200151104
144300151104         //?- Enter Controllo ed esco
144400151104           exsr OpzS02;
144500151104           IF  *in28;
144600151104             iter;
144700151104           ENDIF;
144800151104           FineTcr = *on;
144900151104
145000151104         ENDDO;
145100151104
145200151104       ENDSR;
145300151104
145400151104       //--------------------------------------------------------------
145500151104       //?Inizializzazione Videata S02.
145600151104       //--------------------------------------------------------------
145700151104       BEGSR InzS02;
145800151104
145900151104       //?Pulizia subfile
146000151104         exsr PulS02;
146100151104
146200151104       //?Caricamento subfile
146300151104         exsr RieS02;
146400151104
146500151104         *in33 = *on;
146600151104
146700151104       //?Imposto il posizionamento al primo rcd
146800151104         IF  S02nrr > 0;
146900151104           W02rcd = 1;
147000151104         ELSE;
147100151104           clear W02rcd;
147200151104         ENDIF;
147300151104
147400151104         W02csr = W02rcd;
147500151104
147600151104       ENDSR;
147700151104
147800151104       //--------------------------------------------------------------
147900151104       //?Pulizia Subfile S02.
148000151104       //--------------------------------------------------------------
148100151104       BEGSR PulS02;
148200151104
148300151104       //?Pulizia subfile
148400151104         *in30 = *on;
148500151104         *in31 = *on;
148600151104         write OR37C02;
148700151104         *in31 = *off;
148800151104         *in33 = *off;
148900151104
149000151104         clear W02rcd;
149100151104         clear W02csr;
149200151104         clear S02nrr;
149300151104         clear W02msg;
149400151104
149500151104         *in28 = *off;
149600151104
149700151104       ENDSR;
149800151104
149900151104       //--------------------------------------------------------------
150000151104       //?Caricamento Subfile S02.
150100151104       //--------------------------------------------------------------
150200151104       BEGSR RieS02;
150300151104
150400151104         xx = 1;
150500151104         FOR xx by 1 to 4;
150600151104           clear  OR37S02;
150700151104           W02tcr = Frequenza(xx);
150800151104           W02des = Frequenza(xx) + DesFreq(xx);
150900151104           S02nrr += 1;
151000151104           write  OR37S02;
151100151104         ENDFOR;
151200151104
151300151104       ENDSR;
151400151104
151500151104       //--------------------------------------------------------------
151600151104       //?Gestione opzioni Subfile S02.
151700151104       //--------------------------------------------------------------
151800151104       BEGSR OpzS02;
151900151104
152000151104         xx = 1;
152100151104         FOR xx by 1 to 4;
152200151104           S02nrr = xx;
152300151104           chain S02nrr OR37S02;
152400151104           IF  not %found;
152500151104             leave;
152600151104           ENDIF;
152700151104
152800151104           SELECT;
152900151104         //?- Nessuna opzione
153000151104           WHEN  W02opz = *blank;
153100151104
153200151104         //?- 1 = Scelta
153300151104           WHEN  W02opz = '1';
153400151104             savfreq = W02tcr;
153500151104             leave;
153600151104
153700151104           OTHER;
153800151104             *in28 = *on;
153900151104             W02msg = 'Opzione errata';
154000151104             update OR37S02;
154100151104             leave;
154200151104           ENDSL;
154300151104
154400151104         ENDFOR;
154500151104
154600151104       ENDSR;
154700131113
154800131113      /end-free
154900071030
155000071030      *---------------------------------------------------------------*
155100071030      *?Interrogazione Giri di Ritiro                                ?*
155200071030      *---------------------------------------------------------------*
155300071031     c     Search_Giro   BEGSR
155400071030      *
155500071031     c                   clear                   Wpocgi
155600071031     c                   clear                   Wcgi
155700071030     c                   clear                   WcgiD
155800071030      *
155900071030     c                   clear                   FIDG09ds
156000071030     c                   eval      D09iop0   = 'P01'
156100071030     c                   eval      D09ifgs   = I37fgs
156200071030     c                   eval      D09idat   = *date
156300071030     c                   eval      D09itug   = 'R'
156400071030      *
156500071030     c                   movel(p)  FIDG09ds      KPJBU
156600071030     c                   call      'FIDG09R'
156700071030     c                   parm                    KPJBA
156800071030     c                   movel     KPJBU         FIDG09ds
156900071030      *
157000081211sel 1c                   select
157100081211w   1c                   when           D09Of03 <> *on
157200071030     c                             and  D09Of12 <> *on
157300071030     c                             and  D09Oerr <> *on
157400071030     c                   eval      Wpocgi    = D09ofgs
157500071030     c                   eval      Wcgi      = D09ocgi
157600071030     c                   movel     D09odes       WcgiD
157700081211w   1c                   when      D09Of03   = *on
157800081211     c                   eval      *in60     = *on
157900081211     c                   eval      *in90     = *on
158000081211w   1c                   when      D09Of12   = *on
158100081211e   1c                   endsl
158200071030      *
158300071030     c                   ENDSR
158400070802
158500070802      *---------------------------------------------------------------*
158600070802      *?Verifica se record annullabile                               ?*
158700070802      *---------------------------------------------------------------*
158800070802     c     Ctr_Annull    BEGSR
158900070802      *
159000071030     c                   eval      dsCRO1  = V2Ccd1
159100071030     c                   eval      dsCRO2  = V2Ccd2
159200071030     c                   eval      dsCRO3  = V2Ccd3
159300070802      *
159400070802      * Non deve esistere un Ritiro Fisso con questo codice
159500070802     c                   exsr      Ctr_RFissi
159600070802if  1c                   if        *in90   = *on
159700070802     c                   leavesr
159800070802e   1c                   endif
159900070802      *
160000070802      * Non deve esistere un O.R.M. con questo codice
160100070802     c                   exsr      Ctr_ORM
160200070802if  1c                   if        *in90
160300070802     c                   leavesr
160400070802e   1c                   endif
160500121220      *
160600121220      * Non deve esistere l'abilitazione OSR per questo codice
160700121220     c                   exsr      Ctr_OSR
160800121220if  1c                   if        *in90
160900121220     c                   leavesr
161000121220e   1c                   endif
161100070802      *
161200070802     c                   ENDSR
161300070802
161400070802      *---------------------------------------------------------------*
161500070802      *?Non deve esistere un Ritiro Fisso con il codice da annullare ?*
161600070802      *---------------------------------------------------------------*
161700070802     c     Ctr_RFissi    BEGSR
161800070802      *
161900070802      * Ricerca per codice ritiro
162000070802     c                   move      ds_CRO        ORScra
162100070802     c     ORScra        setll     FNORS002
162200070802     c     ORScra        reade     FNORS002
162300070802do  1c                   dow       NOT  %eof(FNORS02L)
162400071030if  2c                   if        ORSatb  = *blanks
162500070803     c                   seton                                        285190
162600071030     c                   eval      V1Dmsg  = $Msg(5)
162700070802     c                   leavesr
162800070802e   2c                   endif
162900070802     c     ORScra        reade     FNORS002
163000070802e   1c                   enddo
163100070802      *
163200070802      * Ricerca per codice ordinante
163300070802     c                   move      ds_CRO        ORScor
163400070802     c     ORScor        setll     FNORS003
163500070802     c     ORScor        reade     FNORS003
163600070802do  1c                   dow       NOT  %eof(FNORS03L)
163700071030if  2c                   if        ORSatb  = *blanks
163800070803     c                   seton                                        285190
163900071030     c                   eval      V1Dmsg  = $Msg(5)
164000070802     c                   leavesr
164100070802e   2c                   endif
164200070802     c     ORScor        reade     FNORS003
164300070802e   1c                   enddo
164400070802      *
164500070802     c                   ENDSR
164600070802
164700070802      *---------------------------------------------------------------*
164800070802      *?Non deve esistere un O.R.M. con il codice da annullare       ?*
164900070802      *---------------------------------------------------------------*
165000070802     c     Ctr_ORM       BEGSR
165100070802      *
165200070802      * Ricerca per codice ritiro
165300070802     c                   move      ds_CRO        ORMcra
165400070802     c     ORMcra        setll     FNORM018
165500070802     c     ORMcra        reade     FNORM018
165600070802do  1c                   dow       NOT  %eof(FNORM18L)
165700070807if  2c                   if             ORMatb  = *blanks
165800070807     C                             and  ORMfao  < 900
165900070803     c                   seton                                        285190
166000071030     c                   eval      V1Dmsg  = $Msg(6)
166100070802     c                   leavesr
166200070802e   2c                   endif
166300070807     c     ORScra        reade     FNORM018
166400070802e   1c                   enddo
166500070802      *
166600070802      * Ricerca per codice ordinante
166700070802     c                   move      ds_CRO        ORMcor
166800070802     c     ORMcor        setll     FNORM009
166900070802     c     ORMcor        reade     FNORM009
167000070802do  1c                   dow       NOT  %eof(FNORM09L)
167100070802if  2c                   if             ORMatb  = *blanks
167200070802     C                             and  ORMfao  < 900
167300070803     c                   seton                                        285190
167400071030     c                   eval      V1Dmsg  = $Msg(6)
167500070802     c                   leavesr
167600070802e   2c                   endif
167700070802     c     ORMcor        reade     FNORM009
167800070802e   1c                   enddo
167900070802      *
168000070802     c                   ENDSR
168100121220
168200121220      *---------------------------------------------------------------*
168300121220      *?Non deve esistere tab. OSR  con il codice da annullare       ?*
168400121220      *---------------------------------------------------------------*
168500121220     c     Ctr_OSR       BEGSR
168600121220
168700121220      * se non è aperto il file lo devo aprire
168800121220     c                   IF        not %open(TNTBE01L)
168900121220     c                   IF        %subst(knsif:7:1) = 'P'
169000121220     c                   eval      wLibTbe = 'GAITRAGRPS/TNTBE01L'
169100121220     c                   ELSE
169200121220     c                   eval      wLibTbe = 'GAITRAGRU/TNTBE01L'
169300121220     c                   ENDIF
169400121220     c                   open      TNTBE01L
169500121220     c                   ENDIF
169600121220
169700121220     c     'OSR'         setll     TNTBE01L
169800121220     c                   DO        *hival
169900121220     c     'OSR'         reade     TNTBE01L
170000121220     c                   IF        %eof(TNTBE01L)
170100121220     c                   leave
170200121220     c                   ENDIF
170300121220     c                   IF        TBEatb <> *blanks
170400121220     c                   iter
170500121220     c                   ENDIF
170600121220     c                   eval      dOSR = TBEuni
170700121220     c                   SELECT
170800121220     c                   WHEN      %subst(%editc(I37cro:'X'):8:3) = '333' or
170900121220     c                             %subst(%editc(I37cro:'X'):8:3) = '444'
171000121220     c                   IF        %subst(%editc(I37cro:'X'):1:7) =
171100121220     c                             %editc(d§OSRcli:'X') or
171200121220     c                             %subst(%editc(I37cro:'X'):1:7) =
171300121220     c                             %editc(d§OSRcl2:'X') or
171400121220     c                             %subst(%editc(I37cro:'X'):1:7) =
171500121220     c                             %editc(d§OSRcl3:'X') or
171600121220     c                             %subst(%editc(I37cro:'X'):1:7) =
171700121220     c                             %editc(d§OSRcl4:'X') or
171800121220     c                             %subst(%editc(I37cro:'X'):1:7) =
171900121220     c                             %editc(d§OSRcl5:'X') or
172000121220     c                             %subst(%editc(I37cro:'X'):1:7) =
172100121220     c                             %editc(d§OSRcl6:'X') or
172200121220     c                             %subst(%editc(I37cro:'X'):1:7) =
172300121220     c                             %editc(d§OSRcl7:'X') or
172400121220     c                             %subst(%editc(I37cro:'X'):1:7) =
172500121220     c                             %editc(d§OSRcl8:'X') or
172600121220     c                             %subst(%editc(I37cro:'X'):1:7) =
172700121220     c                             %editc(d§OSRcl9:'X') or
172800121220     c                             %subst(%editc(I37cro:'X'):1:7) =
172900121220     c                             %editc(d§OSRcl13:'X') or
173000121220     c                             %subst(%editc(I37cro:'X'):1:7) =
173100121220     c                             %editc(d§OSRcl11:'X') or
173200121220     c                             %subst(%editc(I37cro:'X'):1:7) =
173300121220     c                             %editc(d§OSRcl12:'X') or
173400121220     c                             %subst(%editc(I37cro:'X'):1:7) =
173500121220     c                             %editc(d§OSRcl13:'X') or
173600121220     c                             %subst(%editc(I37cro:'X'):1:7) =
173700121220     c                             %editc(d§OSRcl14:'X') or
173800121220     c                             %subst(%editc(I37cro:'X'):1:7) =
173900121220     c                             %editc(d§OSRcl15:'X') or
174000121220     c                             %subst(%editc(I37cro:'X'):1:7) =
174100121220     c                             %editc(d§OSRcl16:'X')
174200121220     c                   seton                                        285190
174300121220     c                   eval      V1Dmsg  = $Msg(31)
174400121220     c                   eval      V1Dmsg = %trim(V1Dmsg) + ' LNP ' +
174500121220     c                             %subst(TBEke1:1:3) + ' Serie ' +
174600121220     c                             %subst(TBEke1:4:2)
174700121220     c                   leavesr
174800121220     c                   ENDIF
174900121220     c                   WHEN      %subst(%editc(I37cro:'X'):8:3) = '333' and
175000121220     c                             %subst(%editc(I37cro:'X'):1:7) =
175100121220     c                             %editc(d§OSRcor:'X')
175200121220     c                   seton                                        285190
175300121220     c                   eval      V1Dmsg  = $Msg(31)
175400121220     c                   eval      V1Dmsg = %trim(V1Dmsg) + ' LNP ' +
175500121220     c                             %subst(TBEke1:1:3) + ' Serie ' +
175600121220     c                             %subst(TBEke1:4:2)
175700121220     c                   leavesr
175800121220     c                   WHEN      %subst(%editc(I37cro:'X'):8:3) = '444' and
175900121220     c                             %subst(%editc(I37cro:'X'):1:7) =
176000121220     c                             %editc(d§OSRcrc:'X')
176100121220     c                   seton                                        285190
176200121220     c                   eval      V1Dmsg  = $Msg(31)
176300121220     c                   eval      V1Dmsg = %trim(V1Dmsg) + ' LNP ' +
176400121220     c                             %subst(TBEke1:1:3) + ' Serie ' +
176500121220     c                             %subst(TBEke1:4:2)
176600121220     c                   leavesr
176700121220     c                   ENDSL
176800121220     c                   ENDDO
176900121220
177000121220     c                   ENDSR
177100070802
177200070802      *---------------------------------------------------------------*
177300070802      *?Controllo C.A.P.                                             ?*
177400070802      *---------------------------------------------------------------*
177500070802     c     Ctr_CAP       BEGSR
177600070802      *
177700070802     c                   clear                   TISI95ds
177800070802     c                   eval      I95tcn  = '7'
177900070802     c                   eval      I95nar  = V2Cnar
178000070802     c                   eval      I95cap  = V2Ccar
178100070802     c                   eval      I95loc  = V2Clor
178200070802     c                   eval      I95prv  = V2Cprr
178300070802     c                   eval      I95ind  = V2Cinr
178400070802     c                   clear                   FNLV13ds
178500070802     c                   eval      I13af0  = 'S'
178600070802     c                   eval      I13cnv  = 'S'
178700070802     c                   eval      I13af1  = 'S'
178800070802     c                   eval      I13sz2  = 'S'
178900070802     c                   eval      I13la3  = 'S'
179000070802     c                   eval      I13sz3  = 'S'
179100070802      *
179200070802      * Gestione errori forzabili:
179300070802      * se cambiato CAP/Località riazzero i valori di errori forzabili
179400070802      *
179500070802      * - Modifica CAP
179600070802if  1c                   if        V2Ccar <> SAVcar
179700070802     c                   eval      SAVcar  = V2Ccar
179800070802     c                   clear                   E13fzv
179900070802     c                   clear                   E13fz1
180000070802     c                   clear                   E13fz2
180100070802     c                   clear                   E13fz3
180200070802e   1c                   endif
180300070802      * - Modifica località
180400070802if  1c                   if        V2Clor <> SAVlor
180500070802     c                   eval      SAVlor  = V2Clor
180600070802     c                   clear                   E13fz1
180700070802     c                   clear                   E13fz2
180800070802     c                   clear                   E13fz3
180900070802e   1c                   endif
181000070802      * - Modifica PROVINCIA
181100070802if  1c                   if        V2Cprr <> SAVprr
181200070802     c                   eval      SAVprr  = V2Cprr
181300070802     c                   clear                   E13fz3
181400070802e   1c                   endif
181500070802      *
181600070802      * Richiamo programma
181700070802     c                   call      'FNLV13R'
181800070802     c                   parm                    KPJBA
181900070802     c                   parm                    FNLV13ds
182000070802     c                   parm                    TISI95ds
182100070802      *
182200070802      * Reimpostazione dei dati ricevuti in TISI95ds
182300070802if  1c                   if             O13ron  = 'S'
182400110121     c                   eval      V2Cnar  = O95nar
182500110121e   1c                   endif
182600110121     c                   if             O13roc  = 'S'
182700110121     c                   eval      V2Ccar  = O95cap
182800110121e   1c                   endif
182900110121     c                   if             O13rop  = 'S'
183000110121     c                   eval      V2Cprr  = O95prv
183100110121e   1c                   endif
183200110121     c                   if             O13rol  = 'S'
183300070802     c                   eval      V2Clor  = O95loc
183400070802e   1c                   endif
183500070802      *
183600070802      * Riscontrati errori o reimpostati dati
183700070802sel 1c                   select
183800070802      * - Emissione messaggio con l'errore rilevato
183900081022w   1c                   when      O13msg <> *blanks
184000070802     c                   eval      *in55   = (O13err = '5')                     CAP
184100070802     c                   eval      *in56   = (O13err = '3')                     Località
184200070802     c                   eval      *in57   = (O13err = '4')                     Provincia
184300070802     c                   eval      *in58   = (O13err = '6')                     Nazione
184400070802     c                   seton                                        28  90
184500070802     c                   eval      V1Dmsg  = O13msg
184600070802     c                   leavesr
184700070802      * - C.A.P. reimpostato
184800070802w   1c                   when      O13roc  = 'S'
184900070802     c                   seton                                        285590
185000070802     c                   eval      V1Dmsg  = $Msg(12)
185100070802     c                   leavesr
185200070802      * - Località reimpostata
185300070802w   1c                   when      O13rol  = 'S'
185400070802     c                   seton                                        285690
185500070802     c                   eval      V1Dmsg  = $Msg(13)
185600070802     c                   leavesr
185700070802      * - Provincia reimpostata
185800070802w   1c                   when      O13rop  = 'S'
185900070802     c                   seton                                        285790
186000070802     c                   eval      V1Dmsg  = $Msg(14)
186100070802     c                   leavesr
186200070802      * - Nazione reimpostata
186300070802w   1c                   when      O13ron  = 'S'
186400070802     c                   seton                                        285890
186500070802     c                   eval      V1Dmsg  = $Msg(15)
186600070802     c                   leavesr
186700070802e   1c                   endsl
186800070802      *
186900070802     c                   ENDSR
187000070801
187100070801      *---------------------------------------------------------------*
187200070801      *?Reperimento dati anagrafici                                  ?*
187300070801      *---------------------------------------------------------------*
187400070801     c     Call_TIBS69   BEGSR
187500070801      *
187600070801     c                   clear                   ds_CNACO
187700070801     c                   clear                   ds_CNIND
187800070801     c                   clear                   ds_CNCLP
187900070801     c                   clear                   ds_FNCLS
188000070801      *
188100070801     c                   call      'TIBS69R'
188200070801     c                   parm                    TIBS69ds
188300070801     c                   parm                    ds_CNACO
188400070801     c                   parm                    ds_CNIND
188500070801     c                   parm                    ds_CNCLP
188600070801     c                   parm                    ds_FNCLS
188700070801      *
188800071023     c                   eval      *in28   = (O69err = *on)
188900070801if  1c                   if        *in28
189000071023     c                   eval      *in90   = *on
189100071023     c                   eval      V1Dmsg  = O69msg
189200070801e   1c                   endif
189300070801      *
189400070801     c                   ENDSR
189500070801
189600070801      *---------------------------------------------------------------*
189700070801      *?Aggiornamento dati file FNACR00F                             ?*
189800070801      *---------------------------------------------------------------*
189900070801     c     Upd_FNACR     BEGSR
190000070801      *
190100071023     c                   eval      *in91   = *off
190200071023     c                   eval      I37cro  = V2Ccd1 * 10000000
190300071023     c                                     + V2Ccd2 * 1000
190400070807     c                                     + V2Ccd3
190500071030     c     I37cro        chain     FNACR000
190600070803      *
190700070801     c                   select
190800070801      *
190900070801      * Inserimento / Copia
191000070801w   1c                   when          (I37opz = '1'
191100070809     c                             or   I37opz = '3'
191200070809     c                             or   I37opz = 'A'
191300070809     c                             or   I37opz = 'C')
191400070801     c                             and  *inKF
191500070801     c                   exsr      RieRec
191600070803     c                   WRITE     FNACR000                             91
191700070801      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
191800070801      * Modifica
191900070801w   1c                   when           I37opz  = '2'
192000070801     c                             and  *inKF
192100070801     c                   exsr      RieRec
192200070801     c                   UPDATE    FNACR000
192300070801      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
192400070801      * Annullamento
192500070801w   1c                   when          (I37opz  = '4'
192600070801     c                             and  *inKF)
192700070801     c                             or   *inKQ
192800070801     c                   eval      ACRatb  = 'A'
192900070801     c                   UPDATE    FNACR000
193000070801      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
193100070801      * Ripristino
193200070801w   1c                   when          (I37opz  = '7'
193300070801     c                             and  *inKF)
193400070801     c                             or   *inKE
193500070802     c                   exsr      RieRec
193600070801     c                   UPDATE    FNACR000
193700070801      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
193800070801e   1c                   endsl
193900070801      *
194000070803if  1c                   if        *in91
194100070801     c                   seton                                        28  90
194200071030     c                   eval      V1Dmsg  = %trimr($Msg(4))
194300071030     c                                     + ' FNACR00F'
194400070801e   1c                   endif
194500070801      *
194600071030      * Aggiornamento ambito "standard" di intradamento codice giro
194700071030     c                   eval      ACR1ain = '='
194800071030     c                   eval      Wpocgi  = V2Cpocgi1
194900071030     c                   eval      Wcgi    = V2Ccgi1
195000070801     c                   exsr      Upd_FNACR1
195100071030      *
195200071030      * Aggiornamento ambito "oltre" di intradamento codice giro
195300071030     c                   eval      ACR1ain = '>'
195400071030     c                   eval      Wpocgi  = V2Cpocgi2
195500071030     c                   eval      Wcgi    = V2Ccgi2
195600071030     c                   exsr      Upd_FNACR1
195700071030      *
195800071030      * Aggiornamento ambito "sotto" di intradamento codice giro
195900071030     c                   eval      ACR1ain = '<'
196000071030     c                   eval      Wpocgi  = V2Cpocgi3
196100071030     c                   eval      Wcgi    = V2Ccgi3
196200071030     c                   exsr      Upd_FNACR1
196300151012      *
196400151012      /free
196500151012
196600151012         // -?Aggiornamento dati file FNACRE0F?
196700151012
196800151012         // -?Impostazione chiave d'accesso al file FNACRE1L per l'?
196900151012         //  ?aggionamento della "e-Mail Conferma Accettazione ORM"?
197000151012         clear  keyFNACRE1;
197100151012         k_ACREcro = I37cro;
197200151012         k_ACREtrc = 'MC';
197300151012         chain  %kds( keyFNACRE1 )  FNACRE00;
197400151012
197500151012         Select;
197600151012
197700151012           // -?Inserimento / Copia?
197800151012           When  Not %found(FNACRE1L)  and  V2Cmca <> *blank;
197900151012             clear  FNACRE00;
198000151012             ACREatb  = ACRatb;
198100151012             ACREcro  = k_ACREcro;
198200151012             ACREtrc  = k_ACREtrc;
198300151012             ACREdati = V2Cmca;
198400151012             Write  FNACRE00;
198500151012
198600151012           // -?Modifica / Annullamento / Ripristino?
198700151012           When  %found(FNACRE1L)  and  V2Cmca <> *blank;
198800151012             ACREatb  = ACRatb;
198900151012             ACREdati = V2Cmca;
199000151012             Update  FNACRE00;
199100151012
199200151012           // -?Cancellazione?
199300151012           When  %found(FNACRE1L)  and  V2Cmca = *blank;
199400151012             Delete  FNACRE00;
199500151012
199600151012         EndSl;
199700160317
199800160317         // -?Impostazione chiave d'accesso al file FNACRE1L per l'?
199900160317         //  ?aggionamento del "SMS Conferma Accettazione ORM"?
200000160317         clear  keyFNACRE1;
200100160317         k_ACREcro = I37cro;
200200160317         k_ACREtrc = 'SC';
200300160317         chain  %kds( keyFNACRE1 )  FNACRE00;
200400160317
200500160317         Select;
200600160317
200700160317           // -?Inserimento / Copia?
200800160317           When  Not %found(FNACRE1L)  and  V2Csca <> *blank;
200900160317             clear  FNACRE00;
201000160317             ACREatb  = ACRatb;
201100160317             ACREcro  = k_ACREcro;
201200160317             ACREtrc  = k_ACREtrc;
201300160317             ACREdati = V2Csca;
201400160317             Write  FNACRE00;
201500160317
201600160317           // -?Modifica / Annullamento / Ripristino?
201700160317           When  %found(FNACRE1L)  and  V2Csca <> *blank;
201800160317             ACREatb  = ACRatb;
201900160317             ACREdati = V2Csca;
202000160317             Update  FNACRE00;
202100160317
202200160317           // -?Cancellazione?
202300160317           When  %found(FNACRE1L)  and  V2Csca = *blank;
202400160317             Delete  FNACRE00;
202500160317
202600160317         EndSl;
202700160713
202800160713       //?Se devo scrivere il rcd 'AA' lo faccio ora
202900160713         IF  wOkRcdAA;
203000160713           clear  keyFNACRE1;
203100160713           k_ACREcro = I37cro;
203200160713           k_ACREtrc = 'AA';
203300160713           chain  %kds( keyFNACRE1 )  FNACRE00;
203400160713           clear dOREana;
203500160713           §OREtipo = 'A';
203600160713           §OREdtv = %dec(%date());
203700160713           §OREorv = %dec(%time());
203800160713           §OREute = knmus;
203900160713           §OREjob = knmeb;
204000160713           ACREatb = ACRatb;
204100160713           IF  %found(FNACRE1L);
204200160713             ACREdati = dOREana;
204300160713             update FNACRE00;
204400160713           ENDIF;
204500160713           IF  not %found(FNACRE1L);
204600160713             ACREcro = k_ACREcro;
204700160713             ACREtrc = k_ACREtrc;
204800160713             ACREdati = dOREana;
204900160713             write FNACRE00;
205000160713           ENDIF;
205100160713         ENDIF;
205200170222
205300170222         // -?"Conferma" degli aggiornamenti appena eseguiti?
205400170222         if  Not *in90;
205500170222           feod  FNACR01L;
205600170222           feod  FNACR13L;
205700170222           feod  FNACRE1L;
205800170222         endif;
205900151012
206000151012      /end-free
206100070801      *
206200070801     c                   ENDSR
206300070801
206400070801      *---------------------------------------------------------------*
206500070801      *?Aggiornamento dati file FNACR10F                             ?*
206600070801      *---------------------------------------------------------------*
206700070801     c     Upd_FNACR1    BEGSR
206800070801      *
206900071030     c                   eval      *in92   = *off
207000070807     c                   eval      I37cro  = V2Ccd1*10000000
207100070807     c                                     + V2Ccd2*1000
207200070807     c                                     + V2Ccd3
207300071024      *
207400071024sel 1c                   select
207500070801      *
207600070802      * Inserimento / Modifica / Copia
207700070803w   1c                   when          (I37opz  = '1'
207800070803     c                             or   I37opz  = '2'
207900070809     c                             or   I37opz  = '3'
208000070809     c                             or   I37opz  = 'A'
208100070809     c                             or   I37opz  = 'C')
208200070801     c                             and  *inKF
208300071030     c     k03acr13      chain     FNACR100
208400071105     c                   clear                   SAVpocgi
208500071105     c                   clear                   SAVcgi
208600071105if  2c                   if        %found(FNACR13L)
208700071105     c                   eval      SAVpocgi     = ACR1pocgi
208800071105     c                   eval      SAVcgi       = ACR1cgi
208900071105e   2c                   endif
209000071105      *
209100071030if  2c                   if        Wcgi   <> *blanks
209200070801     c                   exsr      RieRec1
209300071030e   2c                   endif
209400071105      *
209500070803sel 2c                   select
209600071030w   2c                   when      NOT  %found(FNACR13L)
209700071030     c                             and  Wcgi   <> *blanks
209800070801     c                   WRITE     FNACR100                             92
209900070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
210000071030w   2c                   when           %found(FNACR13L)
210100071030     c                             and  Wcgi   <> *blanks
210200071105     c                             and  Wcgi   <> SAVcgi
210300070801     c                   UPDATE    FNACR100
210400070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
210500071030w   2c                   when           %found(FNACR13L)
210600071030     c                             and  Wcgi    = *blanks
210700071024     c                   DELETE    FNACR100
210800071024      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
210900071030w   2c                   when           %found(FNACR13L)
211000071105     c                             and  Wcgi    = SAVcgi
211100071030     c                   UNLOCK    FNACR13L
211200071030      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
211300070803e   2c                   endsl
211400070801      *
211500070801      * Annullamento
211600070807w   1c                   when         ((I37opz  = '4'
211700070807     c                             and  *inKF)
211800070807     c                             or   *inKQ)
211900071024     c     I37cro        setll     FNACR100
212000071024     c     I37cro        reade     FNACR100
212100071030do  2c                   dow       NOT  %eof(FNACR13L)
212200080115     c                   DELETE    FNACR100
212300070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
212400071024     c     I37cro        reade     FNACR100
212500071024e   2c                   enddo
212600070801      * Ripristino
212700070807w   1c                   when         ((I37opz  = '7'
212800070801     c                             and  *inKF)
212900070807     c                             or   *inKE)
213000071024     c     I37cro        setll     FNACR100
213100071024     c     I37cro        reade     FNACR100
213200071030do  2c                   dow       NOT  %eof(FNACR13L)
213300071030if  3c                   if        I37fgs  = ACR1pocgi
213400070801     c                   exsr      RieRec1
213500071024x   3c                   else
213600071030     c                   eval      ACR1atb = *blanks
213700071030     c                   eval      ACR1dtv = *date
213800071030     c                   eval      ACR1ute = KNMUS
213900071024e   3c                   endif
214000070807     c                   UPDATE    FNACR100
214100070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
214200071024     c     I37cro        reade     FNACR100
214300071024e   2c                   enddo
214400070801e   1c                   endsl
214500070801      *
214600070801if  1c                   if        *in92
214700070801     c                   seton                                        28  90
214800071030     c                   eval      V1Dmsg  = %trimr($Msg(4))
214900071030     c                                     + ' FNACR10F'
215000070801e   1c                   endif
215100070801      *
215200070801     c                   ENDSR
215300070801
215400070801      *---------------------------------------------------------------*
215500070801      *?Impostazione valori nei campi del record FNACR00F            ?*
215600070801      *---------------------------------------------------------------*
215700070801     c     RieRec        BEGSR
215800070801      *
215900131113     c                   clear                   dACR01
216000070801     c                   clear                   FNACR000
216100160713      /free
216200160713       //?Se ho flag e/o mail e/o sms alert diversi devo scrivere anche rcd 'AA'
216300160713         wOkRcdAA = *off;
216400160713         IF  V2Cfca <> savfca or V2Cmca <> savmca or V2Csca <> savsca;
216500160713           wOkRcdAA = *on;
216600160713         ENDIF;
216700160713      /end-free
216800070801      *
216900070809      * Lascia record annullato se lo era già (*in43)
217000070809      * e non ne è stato chiesto il ripristino
217100070807     c                   if             *in43
217200070807     c                             and  NOT *inKE
217300070807     c                             and  I37opz <> '7'
217400070807     c                   eval      ACRatb  = 'A'
217500070807     c                   endif
217600070809      *
217700070809      * Imposta tutti gli altri campi ("chiave" compresi)
217800071030     c                   eval      ACRtac  = 'R'
217900070801     c                   eval      ACRcro  = V2Ccd1*10000000
218000070801     c                                     + V2Ccd2*1000
218100070801     c                                     + V2Ccd3
218200070801     c                   eval      ACRrsr  = V2Crsr
218300070801     c                   eval      ACRinr  = V2Cinr
218400070801     c                   eval      ACRcar  = V2Ccar
218500070801     c                   eval      ACRlor  = V2Clor
218600070801     c                   eval      ACRprr  = V2Cprr
218700070801     c                   eval      ACRnar  = V2Cnar
218800070801     c                   eval      ACRksc  = V2Cksc
218900070801     c                   eval      ACRcco  = V2Cco1*10000000
219000070801     c                                     + V2Cco2*1000
219100070801     c                                     + V2Cco3
219200070801     c                   eval      ACRper  = V2Cper
219300070801     c                   eval      ACRtel  = V2Ctel
219400131113     c                   eval      dACR01.§ACRoa1 = %editc( V2ChmAmDa : 'X' )
219500131113     c                   eval      dACR01.§ACRoa2 = %editc( V2ChmAmA  : 'X' )
219600131113     c                   eval      dACR01.§ACRoa3 = %editc( V2ChmPmDa : 'X' )
219700131113     c                   eval      dACR01.§ACRoa4 = %editc( V2ChmPmA  : 'X' )
219800140114     c                   eval      dACR01.§ACRstcr = sav_§ACRstcr
219900140114     c                   eval      dACR01.§ACRctcr = sav_§ACrctcr
220000151104     c                   eval      dACR01.§ACRfmar = V2Cfca
220100131113     c                   eval      ACRmai  = dACR01
220200070801     c                   eval      ACRno1  = V2Cno1
220300070801     c                   eval      ACRno2  = V2Cno2
220400070801     c                   eval      ACRntm  = V2Cntm
220500150909     c                   eval      ACRpoa  = V2Cpoa
220600070801     c                   eval      ACRtcr  = V2Ctcr
220700070802if  1c                   if        V2Cfcc  = 'N'
220800070801     c                   eval      ACRfcc  = V2Cfcc
220900070802x   1c                   else
221000070802     c                   clear                   ACRfcc
221100070802e   1c                   endif
221200091009if  1c                   if        V2Cctr  = *blanks
221300070801     c                   eval      ACRccc  = 999
221400070801x   1c                   else
221500091009     c                   move      v2cctr        ACRccc
221600070801e   1c                   endif
221700070801     c                   eval      ACRfcl  = V2Cfcl
221800070802     c                   eval      ACRfpk  = V2Cfpk
221900070801     c                   eval      ACRfmc  = V2Cfmc
222000070801     c                   eval      ACRfbn  = V2Cfbn
222100070801     c                   eval      ACRfbl  = V2Cfbl
222200070801     c                   eval      ACRfmt  = V2Cfmt
222300070801     c                   eval      ACRfat  = V2Cfat
222400070801     c                   eval      ACRspi  = V2Cspi
222500070801     c                   eval      ACRorr  = V2Corr
222600070801     c                   eval      ACRdtr  = *date
222700070801      *
222800070801     c                   ENDSR
222900070801
223000070801      *---------------------------------------------------------------*
223100070801      *?Impostazione valori nei campi del record FNACR10F            ?*
223200070801      *---------------------------------------------------------------*
223300070801     c     RieRec1       BEGSR
223400070801      *
223500070913     c                   eval      ACR1atb   = ACRatb
223600070801     c                   eval      ACR1cro   = ACRcro
223700071030     c                   eval      ACR1pocgi = Wpocgi
223800071030     c                   eval      ACR1cgi   = Wcgi
223900071030if  1c                   if        NOT  %found(FNACR13L)
224000071030     c                             or  (SAVpocgi <> Wpocgi
224100071030     c                             or   SAVcgi   <> Wcgi)
224200070801     c                   eval      ACR1dtv   = *date
224300070802     c                   eval      ACR1ute   = KNMUS
224400070802e   1c                   endif
224500070801      *
224600070801     c                   ENDSR
224700070801
224800070801** - $Fun ------*
224900070801   INSERIMENTO      1
225000070801     MODIFICA       2
225100070801      COPIA         3
225200070801   ANNULLAMENTO     4
225300070801 VISUALIZZAZIONE    5
225400070801    RIPRISTINO      6
225500070801** - $Msg -------------------------------------------------------------------*
225600070801PARAMETRI ERRATI                                                                1
225700070801Richiesta funzione errata per record inesistente                                2
225800170330---Libero                                                                       3
225900070801Errore in fase di scrittura record nel file                                     4
226000070802NON ANNULLABILE: reperite anagrafiche ritiri fissi con questo codice            5
226100070802NON ANNULLABILE: reperiti ritiri con questo codice                              6
226200070802Codice cliente ritiro non valido: NON esiste sul piano dei conti                7
226300070802Codice cliente ritiro obbligatorio                                              8
226400070802Codice cliente ritiro già esistente                                             9
226500170330---Libero                                                                      10
226600170330---Libero                                                                      11
226700070802ATTENZIONE: modificato C.A.P.                                                  12
226800070802ATTENZIONE: modificata località                                                13
226900070802ATTENZIONE: modificata provincia                                               14
227000070802ATTENZIONE: modificata nazione                                                 15
227100170330---Libero                                                                      16
227200170330---Libero                                                                      17
227300070802Filiale errata per codice giro di ritiro                                       18
227400170330---Libero                                                                      19
227500070802Codice cliente ordinante errato                                                20
227600170330---Libero                                                                      21
227700070802Codice cliente piano dei conti errato                                          22
227800070802Inserire il codice tariffa solo se inserito anche il codice P.d.C.             23
227900091009Codice tariffa inesistente                                                     24
228000070802Frequenza ritiro errata                                                        25
228100070802Ora ritiro errata                                                              26
228200070802Valore flag immissione errato                                                  27
228300070802NON è stato modificato il codice Piano dei Conti  --  Enter per forzare        28
228400071112Codice giro di ritiro errato o NON in vigore                                   29
228500170330---Libero                                                                      30
228600121220NON ANNULLABILE: Codice presente su tabella OSR                                31
228700140723Creare il luogo '000' per il codice cliente in gestione.                       32
228800150909Filiale Ritiro Forzata errata                                                  33
228900160317Immettere un numero di telefono valido                                         34
229000170330Codice Giro di ritiro NON FITTIZIO.                                            35
229100151104** -- Frequenza / DesFreq ---------------------------------------------------*
229200151104F = Fisso
229300151104R = Ricorrente
229400151104O = No Internet
229500151104M = Mai
