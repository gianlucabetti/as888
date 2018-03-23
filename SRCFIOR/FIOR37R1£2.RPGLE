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
009700070730      *
009800070730     d Status         sds           333
009900070730     d   SDSpgm          *proc
010000070801      *
010100070801      * - Codice cliente ritiro
010200070801     d ds_CRO          ds            10    inz
010300070801     d  dsCRO1                        3  0 inz
010400070801     d  dsCRO2                        4  0 inz
010500070801     d  dsCRO3                        3  0 inz
010600070802      *
010700070802      * - Ora
010800070802     d ds_Time         ds             4    inz
010900070802     d  dsHH                          2  0 inz
011000070802     d  dsMM                          2  0 inz
011100070730      *
011200070730      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
011300070730      *
011400070801      * - Flags
011500070801     d $InzD01         s              1    inz(*on)
011600070801     d $Fine           s              1    inz(*off)
011700070802     d $Tam            s              1    inz(*off)
011800071030      * - Indici di schiera / Contatori
011900071030     d wNrCGI          s              3  0 inz
012000070802      * - Comodo
012100071030     d Wpocgi          s                   inz  like(V2Cpocgi1)
012200071030     d Wcgi            s                   inz  like(V2Ccgi1)
012300071030     d WcgiD           s                   inz  like(V2Dcgi1)
012400070802     d SAVcar          s                   inz  like(V2Ccar)
012500070802     d SAVlor          s                   inz  like(V2Clor)
012600070802     d SAVprr          s                   inz  like(V2Cprr)
012700070802     d SAVksc          s                   inz  like(V2Cksc)
012800070802     d SAVpocgi        s                   inz  like(ACR1pocgi)
012900070802     d SAVcgi          s                   inz  like(ACR1cgi)
013000110121     d wdecod          s              1    inz
013100121130     d wGiaChiamato    s               n   inz(*off)
013200121220     d wLibTbe         s             21a   inz
013300151104     d FineTcr         s               n   inz(*off)
013400151104     d S02nrr          s              4s 0 inz
013500151104     d savfreq         s                   like(W02tcr) inz
013600151104     d xx              s              4s 0 inz
013700140114
013800140114     d sav_§ACRstcr    s                   like(dACR01.§ACRstcr)
013900140114     d sav_§ACRctcr    s                   like(dACR01.§ACRctcr)
014000121130
014100121130      // - Campi per procedura esterna
014200121130     d  pStack         s           1000
014300121130     d  pStartPoint    s              3  0
014400121130     d  pEntries       s              3  0
014500121130
014600121130      //---------------------------------------------------------------
014700121130      //?Definizione procedure esterne.
014800121130      //---------------------------------------------------------------
014900121130
015000121130       // - Pgm per ricerca Programmi chiamati
015100121130     d XRTVSTKR        pr                  extpgm('XRTVSTKR')
015200121130     d  pStack                     1000
015300121130     d  pStartPoint                   3  0
015400121130     d  pEntries                      3  0
015500131113
015600131113       // -?Controllo orari apertura del cliente?
015700131113     d TRUL03ds      e ds                  qualified  inz
015800131113     d trul03r         pr                  extpgm('TRUL03R')
015900131113     d   trul03ds...
016000131113     d                                     likeds(trul03ds)
016100151012
016200151012       // -?Controllo indirizzo e-mail?
016300151012     d dsEmail       e ds                  inz
016400151012      /copy gaitrasrc/srcProtoPR,XEMAIL
016500160317
016600160317       // -?Controllo cellulare
016700160317      /copy gaitrasrc/srcprotopi,ubchkcel
016800160317      /copy gaitrasrc/srcprotopr,ubchkcel
016900131113
017000121130      //---------------------------------------------------------------
017100070730      *
017200070730      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
017300070730      *
017400151012       // -?File FNACRE1L?
017500151012     d keyFNACRE1    e ds                  extname( FNACRE1L : *key )
017600151012     d                                     prefix( k_ )   inz
017700071030      * - FNACR13L
017800071030     c     k02acr13      klist
017900071023     c                   kfld                    I37cro
018000071023     c                   kfld                    I37fgs
018100071030     c     k03acr13      klist
018200071030     c                   kfld                    I37cro
018300071030     c                   kfld                    I37fgs
018400071030     c                   kfld                    ACR1ain
018500070801      * - TNTAM01L
018600070802     c     k02tam01      klist
018700070802     c                   kfld                    TAMksc
018800070801     c                   kfld                    TAMctr
018900121130
019000070801      *---------------------------------------------------------------*
019100070801      *?RIEPILOGO INDICATORI                                         ?*
019200070801      *---------------------------------------------------------------*
019300070801      *  02    - Abilitazione tasto funzionale F2=Anagr.Fissi   - D01 *
019400070801      *  03    - Abilitazione tasto funzionale F3=Fine          - D01 *
019500070801      *  05    - Abilitazione tasto funzionale F5=Ripristino    - D01 *
019600070801      *  06    - Abilitazione tasto funzionale F6=Conferma      - D01 *
019700070801      *  16    - Abilitazione tasto funzionale F15=Annullamento - D01 *
019800070801      *  25    - Comodo                                               *
019900070801      *  28    - Emissione del messaggio di errore a video            *
020000140113      *  39    - Visualizzo Frequenza ritiro salvata x EDP
020100140113      *  40    - Proteggo Frequenza ritiro
020200070801      *  41    - Protezione campi chiave in modifica            - D01 *
020300070809      *  42    - Protezione campi codice PdC e Tariffa          - D01 *
020400070809      *  43    - Visualizzazione "ANNULLATO"                    - D01 *
020500070801      *  51    - Cliente ritiro           errato                - D01 *
020600150909      *  52    - Filiale ritiro forzata   errata                - D01 *
020700070801      *  53    - Ragione sociale          errata                - D01 *
020800070801      *  54    - Indirizzo                errato                - D01 *
020900070801      *  55    - CAP                      errato                - D01 *
021000070801      *  56    - Località                 errata                - D01 *
021100070801      *  57    - Provincia                errata                - D01 *
021200070801      *  58    - Nazione                  errata                - D01 *
021300071030      *  59    - Fil. Giro Ritiro ("=")   errata                - D01 *
021400071030      *  60    - Giro di Ritiro   ("=")   errato                - D01 *
021500070801      *  61    - Ordinante                errato                - D01 *
021600070801      *  62    - Codice PdC               errato                - D01 *
021700070801      *  63    - Tariffa                  errata                - D01 *
021800070801      *  64    - Frequenza ritiro         errata                - D01 *
021900070801      *  65    - Ora       ritiro         errata                - D01 *
022000070801      *  67    - Telefono referente       errato                - D01 *
022100070801      *  68    - E-Mail   referente       errata                - D01 *
022200070801      *  69    - Flag immissione colli    errato                - D01 *
022300070801      *  70    - Flag immissione bancali  errato                - D01 *
022400070801      *  71    - Flag immissione peso     errato                - D01 *
022500070801      *  72    - Flag immissione volume   errato                - D01 *
022600070801      *  73    - Flag immissione bilico   errato                - D01 *
022700070801      *  74    - Flag immissione motrice  errato                - D01 *
022800070801      *  75    - Flag immissione autotr.  errato                - D01 *
022900071030      *  76    - Fil. Giro Ritiro (">")   errata                - D01 *
023000071030      *  77    - Giro di Ritiro   (">")   errato                - D01 *
023100071030      *  78    - Fil. Giro Ritiro ("<")   errata                - D01 *
023200071030      *  79    - Giro di Ritiro   ("<")   errato                - D01 *
023300160317      *  80/83 - Orari apertura                                 - D01 *
023400160317      *  84    - SMS per invio conferma prenotazione ritiro     - D01 *
023500070801      *  90    - Generico di errore                                   *
023600070803      *  91    - Fallito aggiornamento di FNACR00F                    *
023700070803      *  92    - Fallito aggiornamento di FNACR10F                    *
023800000927      *---------------------------------------------------------------*
023900070730
024000070730     c     *Entry        plist
024100070730     c                   parm                    KPJBA
024200070801     c                   movel     KPJBU         FIOR37ds
024300070730      *
024400070730      * Operazioni iniziali
024500070801     c                   exsr      RoutInz
024600070801      *
024700071024      * Gestione video
024800070801do  1c                   DOW       $Fine   = *off
024900070801     c                   exsr      GesD01
025000070801e   1c                   ENDDO
025100070801      *
025200071024      * Operazioni finali
025300070801     c                   movel(p)  FIOR37ds      KPJBU
025400070801     c                   eval      *inLR   = *on
025500070801
025600070801      *---------------------------------------------------------------*
025700070801      *?Operazioni Iniziali                                          ?*
025800070801      *---------------------------------------------------------------*
025900070801     c     RoutInz       BEGSR
026000070801      *
026100070801      * Pulizia campi di output
026200070801     c                   clear                   O37cro
026300070801     c                   clear                   O37err
026400070801     c                   clear                   O37msg
026500071030     c                   eval      O37ret  = *off
026600070801      *
026700070801      * Controllo parametri ricevuti
026800070801sel 1c                   SELECT
026900070801      * - Controllo inserimento filiale gestione per Immissione/Variaz.
027000070801w   1c                   WHEN           I37opz  <> '1'
027100070801     c                             and  I37opz  <> '2'
027200070801     c                             and  I37opz  <> '3'
027300070801     c                             and  I37opz  <> '4'
027400070801     c                             and  I37opz  <> '5'
027500070801     c                             and  I37opz  <> '7'
027600070809     c                             and  I37opz  <> 'A'
027700070809     c                             and  I37opz  <> 'C'
027800071030     c                   eval      O37err  = 'E'
027900071030     c                   eval      O37msg  = $Msg(1)
028000071030     c                   eval      $Fine   = *on
028100070801     c                   leavesr
028200070807w   1c                   WHEN           I37opz  <> '1'
028300070801     c                             and  I37cro   = *zeros
028400071030     c                   eval      O37err  = 'E'
028500071030     c                   eval      O37msg  = $Msg(1)
028600071030     c                   eval      $Fine   = *on
028700070801     c                   leavesr
028800070802w   1c                   WHEN           I37opz  <> '5'
028900070802     c                             and  I37fgs   = *zeros
029000071030     c                   eval      O37err  = 'E'
029100071030     c                   eval      O37msg  = $Msg(1)
029200071030     c                   eval      $Fine   = *on
029300070801     c                   leavesr
029400070801e   1c                   ENDSL
029500070801      *
029600070801      * Reperimento dati job
029700070801     c                   exsr      DatiJob
029800070801      *
029900070801      * Impostazione nome programma a video
030000070801     c                   movel     SDSpgm        V1Tpgm
030100070801      *
030200070801      * Impostazione attributi di visualizzazione in testata
030300071030     c                   eval      DA01    = C_HI_RI
030400070801sel 1c                   select
030500071030w   1c                   when      I37opz  = '1'
030600071030     c                   eval      V1Topz  = $Fun(1)
030700071030w   1c                   when      I37opz  = '2'
030800071030     c                   eval      V1Topz  = $Fun(2)
030900071030w   1c                   when      I37opz  = '3'
031000071030     c                   eval      V1Topz  = $Fun(3)
031100071030w   1c                   when      I37opz  = '4'
031200071030     c                   eval      V1Topz  = $Fun(4)
031300071030w   1c                   when      I37opz  = '5'
031400071030     c                   eval      V1Topz  = $Fun(5)
031500071030w   1c                   when      I37opz  = '7'
031600071030     c                   eval      V1Topz  = $Fun(6)
031700071030w   1c                   when      I37opz  = 'A'
031800071030     c                   eval      V1Topz  = $Fun(1)
031900071030w   1c                   when      I37opz  = 'C'
032000071030     c                   eval      V1Topz  = $Fun(3)
032100070801x   1c                   other
032200071030     c                   eval      DA01    = C_ND
032300070801e   1c                   endsl
032400070801      *
032500070801      * Impostazione automatica della filiale di gestione
032600070801sel 1c                   select
032700070801      * - ricevuta
032800070801w   1c                   when           I37opz  <> *blanks
032900070801     c                             and  I37fgs  <> *zeros
033000070801      * - impostazione di default per filiali di 2° livello
033100070801w   1c                   when           DUTlpo   = '2'
033200070801     c                              or  DUTlpo   = *blanks
033300071030     c                   eval      I37fgs  = DUTpou
033400070801      * - impostazione di default per filiali di 1° livello
033500070801x   1c                   other
033600071030     c                   eval      I37fgs  = DUTtfp
033700070801e   1c                   endsl
033800121130
033900121130      /free
034000121130         //?Verifico se anagrafica ritiri fissi già richiamata
034100121130         //?in questo caso non abilito il tasto funzione F2=Ritiro Fissi
034200121130         //?per evitare la chiamata ricorsiva
034300121130         wGiaChiamato = *off;
034400121130         XRTVSTKR (pStack:pStartPoint:pEntries);
034500121130         dsStack = pStack;
034600121130         IF  %lookup('FIOR02R   ':skStack) > 0;
034700121130           wgiachiamato = *on;
034800121130         ENDIF;
034900130731         //?Se richiamato da nuova interrogazione anagrafica ritiri
035000130731         //?non abilito F2 in quanto non serve e se lo lascio abilitato
035100130731         //?cliente, in queso caso non emette niente perchè F2 va per FGS
035200130731         IF not wGiaChiamato;
035300130731           XRTVSTKR (pStack:pStartPoint:pEntries);
035400130731           dsStack = pStack;
035500130731           IF  %lookup('FIOR81R   ':skStack) > 0;
035600130731             wgiachiamato = *on;
035700130731           ENDIF;
035800130731         ENDIF;
035900121130      /end-free
036000070801      *
036100070801     c                   ENDSR
036200070801
036300070801      *---------------------------------------------------------------*
036400070801      *?Reperimento Dati del job (Utente/Operativi)                  ?*
036500070801      *---------------------------------------------------------------*
036600070801     c     DatiJob       BEGSR
036700070801      *
036800070801     c     *dtaara       define    §azute        azuteds
036900070801     c     *dtaara       define    §datiute      ddatiute
037000070801      *
037100070801     c                   in(E)     *dtaara
037200070801     c                   IF        %ERROR or RSUT = *blanks
037300070801     c                   clear                   Tibs34Ds
037400070801     c                   call      'TIBS34R'
037500070801     c                   parm                    Tibs34Ds
037600070801     c                   in        *dtaara
037700070801     c                   ENDIF
037800070801      *
037900070801     c                   ENDSR
038000070801
038100070801      *---------------------------------------------------------------*
038200070801      *?Gestione videata D01                                         ?*
038300070801      *---------------------------------------------------------------*
038400070801     c     GesD01        BEGSR
038500070801      *
038600070801      * Inizializzazione videata
038700070801if  1c                   if        $InzD01 = *on
038800070801     c                   exsr      InzD01
038900070801     c                   eval      $InzD01 = (*in90 = *on)
039000070801e   1c                   endif
039100070801      *
039200070801      * Scrittura di testata e di riga tasti funzionali abilitati
039300070801     c                   write     OR37T01
039400070801     c                   write     OR37Z01
039500070801      *
039600070801      * Emissione videata
039700070801if  1c                   if             I37opz <> '4'
039800070801     c                             and  I37opz <> '5'
039900070801     c                   exfmt     OR37D01
040000070801x   1c                   else
040100070801     c                   write     OR37D01
040200070801     c                   exfmt     PROTECT
040300070801e   1c                   endif
040400070801     c                   setoff                                       28  90
040500070801     c                   clear                   V1Dmsg
040600070801      *
040700070801sel 1c                   SELECT
040800070802      * F2=Anagrafica ritiri fissi
040900070802w   1c                   WHEN      *inKB
041000070802     c                   exsr      F02D01
041100070801      * F3=Fine
041200070801w   1c                   WHEN      *inKC
041300070801     c                   exsr      F03D01
041400070801      * F12=Ritorno
041500070801w   1c                   WHEN      *inKL
041600070801     c                   exsr      F12D01
041700070806      * Visualizzazione (nessun controllo, ma decodifiche)
041800070801w   1c                   WHEN      I37opz  = '5'
041900070802      * Inserimento/Modifica/Ripristino o Annullamento
042000070801x   1c                   OTHER
042100070802      * - Annullamento
042200070802if  2c                   if             I37opz  = '4'
042300070802     c                             or   *inKQ
042400070802     c                   exsr      Ctr_Annull
042500070802x   2c                   else
042600070802      * - Inserimento/Modifica/Ripristino
042700070801     c                   exsr      CtrD01
042800070802e   2c                   endif
042900070809if  2c                   if        NOT  *in90
043000070809      * - F5=Ripristino
043100070809     c                             and (*inKE
043200070802      * - F6=Conferma
043300070809     c                              or  *inKF
043400070802      * - F16=Annullamento
043500070802     c                              or  *inKQ)
043600070801     c                   exsr      F06D01
043700070801e   2c                   endif
043800070801e   1c                   ENDSL
043900070801      *
044000070801     c                   ENDSR
044100070801
044200070801      *---------------------------------------------------------------*
044300070801      *?Inizializzazione videata D01                                 ?*
044400070801      *---------------------------------------------------------------*
044500070801     c     InzD01        BEGSR
044600070801      *
044700070801     c                   clear                   OR37D01
044800070801     c                   clear                   FNACR000
044900070801     c                   clear                   FNACR100
045000070903     c                   clear                   SAVpocgi
045100070910     c                   clear                   SAVcgi
045200070910     c                   clear                   SAVcar
045300070910     c                   clear                   SAVlor
045400070910     c                   clear                   SAVprr
045500071030     c                   eval      SAVksc  = *hival
045600140113     c                   eval      *in41 = *off
045700140114
045800140114      /free
045900140114         //?Utente EDP
046000140114         *in39 = *off;
046100140114         IF  %subst(knmus:1:3) = 'EDP';
046200140114           *in39 = *on;
046300140114         ENDIF;
046400151012
046500151012         // -?Impostazione chiave d'accesso al file FNACRE1L per il?
046600160317         //  ?reperimento dei dati Alert Conferma Accettazione ORM?
046700151012         clear  keyFNACRE1;
046800151012         k_ACREcro = I37cro;
046900140114      /end-free
047000140114
047100070801      *
047200151012      * Reperimento dati
047300151012      *
047400070801sel 1c                   SELECT
047500070802      *
047600070801      * - Se richiesto inserimento:
047700070801      *   il controllo di effettiva inesistenza verrà eseguito nella
047800070801      *   subr. CTRD01.
047900070809w   1c                   WHEN           I37opz = '1'
048000070809     c                             or   I37opz = 'A'
048100071030     c                   eval      V2Ccd1  = I37fgs
048200130417     c                   eval      V2Ctcr  = 'R'
048300150909     c                   eval      V2Cpoa  = I37fgs
048400070802      *
048500070801      * - Se richiesta modifica/annullamento/ripristino:
048600070801      *   il record da aggiornare va allocato
048700070801w   1c                   WHEN           I37opz = '2'
048800070801     c                             or   I37opz = '4'
048900070801     c                             or   I37opz = '7'
049000070801     c     I37cro        chain     FNACR000
049100070801if  2c                   if        NOT  %found(FNACR01L)
049200070801     c                   seton                                        28  90
049300071030     c                   eval      V1Dmsg  = $Msg(2)
049400070801e   2c                   endif
049500071030     c     k02acr13      setll     FNACR100
049600070802      *
049700070801      * - Se richiesta visualizzazione/copia
049800070801      *   il record da visualizzare NON va allocato
049900070801w   1c                   WHEN           I37opz = '3'
050000070801     c                             or   I37opz = '5'
050100070809     c                             or   I37opz = 'C'
050200070801     c     I37cro        chain(n)  FNACR000
050300070801if  2c                   if        NOT  %found(FNACR01L)
050400070801     c                   seton                                        28  90
050500071030     c                   eval      V1Dmsg  = $Msg(2)
050600070801e   2c                   endif
050700071030     c     k02acr13      setll     FNACR100
050800070802      *
050900070801e   1c                   ENDSL
051000070801      *
051100070809      * Eventuali forzature
051200070802if  1c                   if             %found(FNACR01L)
051300071030     c                             and  I37opz  = '2'
051400070801     c                             and  I37fgs <> *zeros
051500150909     c                             and  I37fgs <>
051600150909     c                             %dec(%subst(%editc(ACRcro:'X'):1:3):3:0)
051700071030     c                   eval      I37opz  = '5'
051800070802e   1c                   endif
051900070801      *
052000070801      * Impostazione campi chiave a video
052100070809sel 1c                   select
052200070809w   1c                   when           I37opz  = '1'
052300071030     c                   eval      V2Ccd1    = I37fgs
052400071030     c                   eval      V2Cfcc    = 'S'
052500150909     c                   eval      V2Cpoa    = I37fgs
052600070809w   1c                   when           I37opz  = 'A'
052700070809     c                   movel     I37cro        ds_CRO
052800071030     c                   eval      V2Ccd1    = dsCRO1
052900071030     c                   eval      V2Ccd2    = dsCRO2
053000071030     c                   eval      V2Ccd3    = dsCRO3
053100071030     c                   eval      V2Cksc    = I37ksc
053200071030     c                   eval      V2Cfcc    = 'S'
053300150909     c                   eval      V2Cpoa    = V2Ccd1
053400070809x   1c                   other
053500150910     c                   movel     I37cro        ds_CRO
053600071030     c                   eval      V2Ccd1    = dsCRO1
053700071030     c                   eval      V2Ccd2    = dsCRO2
053800071030     c                   eval      V2Ccd3    = dsCRO3
053900070809e   1c                   endsl
054000070801      *
054100070801      * Impostazione altri campi a video
054200070802if  1c                   IF            (I37opz = '2'
054300070801     c                             or   I37opz = '3'
054400070801     c                             or   I37opz = '4'
054500070801     c                             or   I37opz = '5'
054600070809     c                             or   I37opz = '7'
054700070809     c                             or   I37opz = 'C')
054800070801     c                             and  %found(FNACR01L)
054900071030     c                   eval      V2Crsr    = ACRrsr
055000071030     c                   eval      V2Cinr    = ACRinr
055100071030     c                   eval      V2Ccar    = ACRcar
055200071030     c                   eval      V2Ccar    = ACRcar
055300071030     c                   eval      V2Clor    = ACRlor
055400071030     c                   eval      V2Cprr    = ACRprr
055500071030     c                   eval      V2Cnar    = ACRnar
055600071030if  2c                   IF        %equal(FNACR13L)
055700071030if  3c                   if             I37opz = '3'
055800071030     c                             or   I37opz = '5'
055900071030     c                             or   I37opz = 'C'
056000071030     c     k02acr13      reade(n)  FNACR100
056100071030x   3c                   else
056200071030     c     k02acr13      reade     FNACR100
056300071030e   3c                   endif
056400071030do  3c                   DoW       NOT  %eof(FNACR13L)
056500071030sel 4c                   select
056600071030w   4c                   when      ACR1atb  <> ACRatb
056700071030w   4c                   when      ACR1ain   = '='
056800071030     c                   eval      V2Cpocgi1 = ACR1pocgi
056900071030     c                   eval      V2Ccgi1   = ACR1cgi
057000071030w   4c                   when      ACR1ain   = '>'
057100071030     c                   eval      V2Cpocgi2 = ACR1pocgi
057200071030     c                   eval      V2Ccgi2   = ACR1cgi
057300071030w   4c                   when      ACR1ain   = '<'
057400071030     c                   eval      V2Cpocgi3 = ACR1pocgi
057500071030     c                   eval      V2Ccgi3   = ACR1cgi
057600071030e   4c                   endsl
057700071030if  4c                   if             I37opz = '3'
057800071030     c                             or   I37opz = '5'
057900071030     c                             or   I37opz = 'C'
058000071030     c     k02acr13      reade(n)  FNACR100
058100071030x   4c                   else
058200071030     c     k02acr13      reade     FNACR100
058300071030e   4c                   endif
058400071030e   3c                   EndDo
058500071030e   2c                   ENDIF
058600070801     c                   movel     ACRcco        ds_CRO
058700071030     c                   eval      V2Cco1    = dsCRO1
058800071030     c                   eval      V2Cco2    = dsCRO2
058900071030     c                   eval      V2Cco3    = dsCRO3
059000071030     c                   eval      V2Cksc    = ACRksc
059100091009if  2c                   if        ACRccc   = 999
059200091009     c                   clear                   v2cctr
059300091009     c                   else
059400091009     c                   move      ACRccc        V2Cctr
059500070801e   2c                   endif
059600071030     c                   eval      V2Ctcr    = ACRtcr
059700071030if  2c                   if        ACRfcc    = 'N'
059800071030     c                   eval      V2Cfcc    = ACRfcc
059900070802x   2c                   else
060000071030     c                   eval      V2Cfcc    = 'S'
060100070802e   2c                   endif
060200071030     c                   eval      V2Corr    = ACRorr
060300071030     c                   eval      V2Cper    = ACRper
060400071030     c                   eval      V2Ctel    = ACRtel
060500131113     c                   eval      dACR01    = ACRmai
060600140114     c                   eval      sav_§ACRstcr = dACR01.§ACRstcr
060700140114     c                   eval      sav_§ACRctcr = dACR01.§ACrctcr
060800131113      * - Valorizzazione degli orari solo SE effettivamente numerici
060900131113     c                   if        %check( c_Digits : %subst(
061000131113     c                                     ACRmai : 1
061100131113     c                                            : %len(dACR01.§ACRoa1) )
061200131113     c                                   ) = *zero
061300131113     c                   eval      V2ChmAmDa = %int( dACR01.§ACRoa1 )
061400131113     c                   endif
061500131113     c                   if        %check( c_Digits : %subst(
061600131113     c                                     ACRmai : %len(dACR01.§ACRoa1) + 1
061700131113     c                                            : %len(dACR01.§ACRoa2) )
061800131113     c                                   ) = *zero
061900131113     c                   eval      V2ChmAmA  = %int( dACR01.§ACRoa2 )
062000131113     c                   endif
062100131113     c                   if        %check( c_Digits : %subst(
062200131113     c                                     ACRmai : %len(dACR01.§ACRoa1) +
062300131113     c                                              %len(dACR01.§ACRoa2) + 1
062400131113     c                                            : %len(dACR01.§ACRoa3) )
062500131113     c                                   ) = *zero
062600131113     c                   eval      V2ChmPmDa = %int( dACR01.§ACRoa3 )
062700131113     c                   endif
062800131113     c                   if        %check( c_Digits : %subst(
062900131113     c                                     ACRmai : %len(dACR01.§ACRoa1) +
063000131113     c                                              %len(dACR01.§ACRoa2) +
063100131113     c                                              %len(dACR01.§ACRoa3) + 1
063200131113     c                                            : %len(dACR01.§ACRoa4) )
063300131113     c                                   ) = *zero
063400131113     c                   eval      V2ChmPmA  = %int( dACR01.§ACRoa4 )
063500131113     c                   endif
063600071030     c                   eval      V2Cfcl    = ACRfcl
063700071030     c                   eval      V2Cfbn    = ACRfbn
063800071030     c                   eval      V2Cfpk    = ACRfpk
063900071030     c                   eval      V2Cfmc    = ACRfmc
064000071030     c                   eval      V2Cfbl    = ACRfbl
064100071030     c                   eval      V2Cfmt    = ACRfmt
064200071030     c                   eval      V2Cfat    = ACRfat
064300071030     c                   eval      V2Cspi    = ACRspi
064400071030     c                   eval      V2Cntm    = ACRntm
064500071030     c                   eval      V2Cno1    = ACRno1
064600071030     c                   eval      V2Cno2    = ACRno2
064700140113      /free
064800140113       //?Se frequenza ritiro impostata in automatico da pgm contabili
064900140113       //?non posso toccarla quindi la proteggo
065000140113         IF  dACR01.§ACRctcr = 'C';
065100140113           *in40 = *on;
065200140113         ENDIF;
065300140113       //?Imposto la frequenza ritiro salvata
065400140113         V2Cstcr = dACR01.§ACRstcr;
065500150909       //?Filiale ritiro forzata
065600150909         V2Cpoa = ACRpoa;
065700160317       // -?Flag x Conferma Accettazione ORM?
065800151104         V2Cfca = dacr01.§ACRfmar;
065900160317       // -?E-Mail Conferma Accettazione ORM?
066000160317         k_ACREtrc = 'MC';
066100160317         chain(n)  %kds( keyFNACRE1 )  FNACRE00;
066200160317         IF  %found(FNACRE1L);
066300151012         V2Cmca = ACREdati;
066400160317         ENDIF;
066500160317       // -?E-Mail Conferma Accettazione ORM?
066600160317         k_ACREtrc = 'SC';
066700160317         chain(n)  %kds( keyFNACRE1 )  FNACRE00;
066800160317         IF  %found(FNACRE1L);
066900160317           V2Csca = ACREdati;
067000160317         ENDIF;
067100140113      /end-free
067200140113
067300070802e   1c                   ENDIF
067400070801      *
067500070801      * Abilitazione tasti funzionali:
067600070801      * - F2=Anagr.Fissi
067700070802     c                   eval      *in02   = *on
067800121130      /free
067900121130         //?Se anagrafica ritiri fissi già richiamata non abilito F2
068000121130           IF  wGiaChiamato;
068100121130             *in02 = *off;
068200121130           ENDIF;
068300121130      /end-free
068400070801      * - F3=Fine
068500070809     c                   eval      *in03   = (I37opz <> 'A'    and
068600070809     c                                        I37opz <> 'C')
068700070801      * - F5=Riattivazione
068800070809     c                   eval      *in05   = (I37opz  = '2'    and
068900070801     c                                        %found(FNACR01L) and
069000070801     c                                        ACRatb <> *blanks)
069100070801      * - F6=Conferma
069200070809     c                   eval      *in06   = (I37opz <> '5')
069300070828      * - F7=Interrogazione Giri di Ritiro
069400070828     c                   eval      *in07   = (I37opz <> '4'    and
069500070828     c                                        I37opz <> '5')
069600070801      * - F16=Annullamento
069700070809     c                   eval      *in16   = (I37opz  = '2'    and
069800070801     c                                        %found(FNACR01L) and
069900140723     c                                        ACRatb  = *blanks and
070000140807     c                                        (V2Ccd3 > 0 or
070100140807     c                                        (V2Ccd3 = 0 and
070200140807     c                                        (V2Ccd2 = 8888 or
070300140807     c                                         V2Ccd2 = 9999))))
070400070809      * - F12=Ritorno
070500070809     c                   eval      *in12   = (I37opz <> 'A'    and
070600070809     c                                        I37opz <> 'C')
070700070801      *
070800070801      * Protezione campo chiave a video
070900070802     c                   eval      *in41   = (I37opz <> '1'    and
071000070809     c                                        I37opz <> '3'    and
071100070809     c                                        I37opz <> 'A'    and
071200070809     c                                        I37opz <> 'C')
071300070802      * Protezione codice P.d.C.  e Tariffa
071400070801     c                   eval      *in42   = (I37ksc <> *zeros)
071500070802      * Evidenziazione record annullato
071600070801     c                   eval      *in43   = (ACRatb <> *blanks)
071700070802      *
071800070802      * Impostazione F5=Annullamento o F16=Ripristino
071900070802sel 1c                   select
072000070802w   1c                   when      *in05
072100070802     c                   eval      Z1Df16  = 'F5=Riattivazione'
072200070802w   1c                   when      *in16
072300070802     c                   eval      Z1Df16  = 'F16=Annullamento'
072400070802e   1c                   endsl
072500070911      *
072600070911      * Decodifiche
072700070911if  1c                   if             %found(FNACR01L)
072800070911     c                             or   I37opz  = 'A'
072900110121     c                   eval      wdecod='S'
073000070911     c                   exsr      CtrD01
073100110121     c                   eval      wdecod=' '
073200070911     c                   setoff                                       28  90
073300070911     c                   clear                   V1Dmsg
073400070911     c                   movea     *zeros        *in(50)
073500070911e   1c                   endif
073600070801      *
073700070801     c                   ENDSR
073800070801
073900070801      *---------------------------------------------------------------*
074000070801      *?Gestione tasto funzionale F2 da videata D01                  ?*
074100070801      *?F2=Gestione anagrafica ritiri fissi                          ?*
074200070801      *---------------------------------------------------------------*
074300070801     c     F02D01        BEGSR
074400070801      *
074500070801     c                   clear                   FIOR02ds
074600070801if  1c                   if        I37opz  = '5'
074700070801     c                   eval      I02ric  = '1'
074800070801x   1c                   else
074900070801     c                   eval      I02ric  = '2'
075000070801e   1c                   endif
075100070801     c                   eval      I02cro  = V2Ccd1 * 10000000
075200070801     c                                     + V2Ccd2 * 1000
075300070801     c                                     + V2Ccd3
075400070801     c                   eval      I02poa  = I37fgs
075500070801     c                   call      'FIOR02R'
075600070801     c                   parm                    KPJBA
075700070801     c                   parm                    FIOR02ds
075800070801      *
075900070828if  1c                   if        O02err <> *blanks
076000070801     c                   seton                                        28  90
076100070801     c                   eval      V1Dmsg  = O02msg
076200070801e   1c                   endif
076300070801      *
076400070801     c                   ENDSR
076500070801
076600070801      *---------------------------------------------------------------*
076700070801      *?Gestione tasto funzionale F3 da videata D01                  ?*
076800070801      *?F3=Fine                                                      ?*
076900070801      *---------------------------------------------------------------*
077000070801     c     F03D01        BEGSR
077100070801      *
077200070801      * Disallocazione del record
077300070801if  1c                   if             I37opz = '2'
077400070801     c                             or   I37opz = '4'
077500070801     c                             or   I37opz = '7'
077600070801     c                   UNLOCK    FNACR01L
077700071030     c                   UNLOCK    FNACR13L
077800070801e   1c                   endif
077900070802      *
078000070803      * Impostazione avviso di ritorno con F3
078100070803     c                   eval      O37ret  = *on
078200070801      *
078300070801      * Chiusura del programma
078400070801     c                   eval      $Fine   = *on
078500070801      *
078600070801     c                   ENDSR
078700070801
078800070801      *---------------------------------------------------------------*
078900070801      *?Gestione tasto funzionale F6 da videata D01                  ?*
079000070801      *?F6=Conferma                                                  ?*
079100070801      *---------------------------------------------------------------*
079200070801     c     F06D01        BEGSR
079300070801      *
079400070801      * Aggiornamento record
079500070801     c                   exsr      Upd_FNACR
079600070801      *
079700070801      * Chiusura del programma
079800070801if  1c                   if        NOT  *in90
079900070910if  2c                   if             I37opz = '1'
080000070910     c                             or   I37opz = '3'
080100070910     c                             or   I37opz = 'A'
080200070910     c                             or   I37opz = 'C'
080300070910     c                   eval      O37cro  = I37cro
080400070910e   2c                   endif
080500070801     c                   eval      $Fine   = *on
080600070801e   1c                   endif
080700070801      *
080800070801     c                   ENDSR
080900070801
081000070801      *---------------------------------------------------------------*
081100070801      *?Gestione tasto funzionale F12 da videata D01                 ?*
081200070801      *?F12=Ritorno                                                  ?*
081300070801      *---------------------------------------------------------------*
081400070801     c     F12D01        BEGSR
081500070801      *
081600070801      * Disallocazione del record
081700070801if  1c                   if             I37opz = '2'
081800070801     c                             or   I37opz = '4'
081900070801     c                             or   I37opz = '7'
082000070801     c                   UNLOCK    FNACR01L
082100071030     c                   UNLOCK    FNACR13L
082200070801e   1c                   endif
082300070803      *
082400070803      * Impostazione avviso di ritorno con F12
082500070803     c                   eval      O37ret  = '2'
082600070801      *
082700070801      * Chiusura del programma
082800070801     c                   eval      $Fine   = *on
082900070801      *
083000070801     c                   ENDSR
083100070911
083200070911      *---------------------------------------------------------------*
083300070911      *?Controllo e decodifica codice giro di ritiro                 ?*
083400070911      *---------------------------------------------------------------*
083500070911     c     Ctr_Giro      BEGSR
083600071030      *
083700071030      * - immessa solo la filiale (senza codice giro)
083800071030if  1c                   if             Wpocgi  <> *zeros
083900071030     c                             and  Wcgi     = *blanks
084000071105     c                   clear                   Wpocgi
084100071105     c                   clear                   WcgiD
084200071030e   1c                   endif
084300071030      *
084400071030      * - immesso solo il codice giro (senza filiale)
084500071030if  1c                   if             Wpocgi   = *zeros
084600071030     c                             and  Wcgi    <> *blanks
084700071105     c                   eval      Wpocgi   =  I37fgs
084800071030e   1c                   endif
084900071030      *
085000071030      * - controllo codice filiale del giro ritiro
085100071105      *   (filiale giro diversa dalla filiale gestione)
085200071105if  1c                   if             Wpocgi  <> I37fgs
085300071105     c                             and  Wcgi    <> *blanks
085400071105     c                   seton                                        286090
085500071030     c                   eval      V1Dmsg  = $Msg(18)
085600071030     c                   leavesr
085700071030e   1c                   endif
085800070911      *
085900071030      * - controllo giro CON data validità ("bypassabile")
086000071031if  1c                   IF             Wpocgi  <> D09ifgs
086100071031     c                             or   Wcgi    <> D09icgi
086200070911     c                   clear                   FIDG09ds
086300070911     c                   eval      D09iop0   = '001'
086400071030     c                   eval      D09ifgs   = Wpocgi
086500071030     c                   eval      D09icgi   = Wcgi
086600070911     c                   eval      D09idat   = *date
086700070911     c                   eval      D09itug   = 'R'
086800070911     c                   movel(p)  FIDG09ds      KPJBU
086900070911     c                   call      'FIDG09R'
087000070911     c                   parm                    KPJBA
087100070911     c                   movel     KPJBU         FIDG09ds
087200071105e   1c                   ENDIF
087300071105      *
087400071105sel 1c                   select
087500071105w   1c                   when      D09oerr   = *on
087600071105     c                   movel     D09odes       WcgiD
087700070911     c                   seton                                        286090
087800070911     c                   eval      V1Dmsg    = $Msg(29)
087900071112     c                   leavesr
088000071105w   1c                   when      D09ocgi  <> *blanks
088100071030     c                   movel     D09odes       WcgiD
088200071105e   1c                   endsl
088300070911      *
088400070911     c                   ENDSR
088500070801
088600070801      *---------------------------------------------------------------*
088700070801      *?Controllo videata D01                                        ?*
088800070801      *---------------------------------------------------------------*
088900070801     c     CtrD01        BEGSR
089000070801      *
089100070806     c                   movea     *zeros        *in(50)
089200070806      *
089300150909     c                   clear                   V2Dpoa
089400070802     c                   clear                   ds_CRO
089500070802     c                   eval      dsCRO1  = V2Ccd1
089600070802     c                   eval      dsCRO2  = V2Ccd2
089700070802     c                   eval      dsCRO3  = V2Ccd3
089800070802      *
089900070802      * Codice cliente ritiro
090000070802      * - Se IMMISSIONE (NON COPIA):
090100070802      *   i primi 7 caratteri del codice devono esistere in CNACO
090200070802if  1c                   if             I37opz  = '1'
090300070809     c                             or   I37opz  = 'A'
090400070802     c                   clear                   TIBS69ds
090500070802     c                   moveL     ds_CRO        I69kac
090600070809     c                   if            (I37opz  = '1'
090700070809     c                              or  I37opz  = 'A')
090800070809     c                             and  $InzD01 = *on
090900070809     c                   moveL     ds_CRO        I69kin
091000070809     c                   endif
091100070802     c                   exsr      Call_TIBS69
091200070809sel 2c                   select
091300070809w   2c                   when      *in90
091400070802     c                   eval      *in51   = *on
091500070802     c                   eval      V1Dmsg  = $Msg(7)
091600070802     c                   leavesr
091700070809x   2c                   when          (I37opz  = '1'
091800070809     c                              or  I37opz  = 'A')
091900070809     c                             and  $InzD01 = *on
092000070809     c                   movel     ACOrag        V2Crsr
092100070809     c                   movel     INDvia        V2Cinr
092200070809     c                   movel     INDcae        V2Ccar
092300070809     c                   movel     INDcit        V2Clor
092400070809     c                   movel     INDprv        V2Cprr
092500070809     c                   movel     INDsta        V2Cnar
092600080421     c                   movel     INDtel        V2Ctel
092700070809e   2c                   endsl
092800070802e   1c                   endif
092900070802      * - Se IMMISSIONE O COPIA:
093000070802if  1c                   if             I37opz  = '1'
093100070802     c                             or   I37opz  = '3'
093200070809     c                             or   I37opz  = 'A'
093300070809     c                             or   I37opz  = 'C'
093400070802      * - - controllo completo inserimento
093500070802if  2c                   IF        V2Ccd2  = *zeros
093600070802     c                   seton                                        285190
093700070802     c                   eval      V1Dmsg  = $Msg(8)
093800070802     c                   leavesr
093900070802      * - - controllo se già esistente
094000070802x   2C                   ELSE
094100070802     c                   move      ds_CRO        ACRcro
094200070802     c     ACRcro        chain(n)  FNACR000
094300070802if  3c                   if        %found(FNACR01L)
094400070802     c                   seton                                        285190
094500070802     c                   eval      V1Dmsg  = $Msg(9)
094600070802     c                   leavesr
094700070802e   3c                   endif
094800070802e   2c                   ENDIF
094900140723      /free
095000140723       //?Se NON esiste il luogo '000' devo per forza creare quello
095100140723         IF  dscro3 <> 000;
095200140723           clear ACRcro;
095300140806           ACRcro = (dscro1 * 10000000) + (dscro2 * 1000);
095400140723           chain ACRcro FNACR01L;
095500140723           IF  not %found(FNACR01L);
095600140723             *in28 = *on;
095700140723             *in51 = *on;
095800140723             *in90 = *on;
095900140723             V1Dmsg = $msg(32);
096000140723             leavesr;
096100140723           ENDIF;
096200140723         ENDIF;
096300140723      /end-free
096400070802e   1c                   endif
096500070802      *
096600070802      * Ragione sociale, Indirizzo, Località, CAP, Prov., Naz.
096700110121     c                   if        wdecod=' '
096800070802     c                   clear                   FNLV14ds
096900070802     c                   eval      I14dri  = *date
097000070802     c                   movel     V2Crsr        I14rsc
097100070802     c                   movel     V2Cinr        I14ind
097200070802     c                   movel     V2Clor        E14loc
097300070802     c                   movel     V2Cprr        E14prv
097400070802     c                   movel     V2Ccar        E14cap
097500070802     c                   movel     V2Cnar        E14nar
097600070802     c                   call      'FNLV14R'
097700070802     c                   parm                    KPJBA
097800070802     c                   parm                    FNLV14ds
097900070802     c                   movel     I14ind        V2Cinr
098000070802     c                   movel     E14loc        V2Clor
098100070802     c                   movel     E14prv        V2Cprr
098200070802     c                   movel     E14cap        V2Ccar
098300070802     c                   movel     E14nar        V2Cnar
098400070802if  1c                   if        O14msg <> *blanks
098500070802     c                   eval      *in53   = (O14err = '1')                     Ragione Sociale
098600070802     c                   eval      *in54   = (O14err = '2')                     Indirizzo
098700070802     c                   eval      *in55   = (O14err = '5')                     CAP
098800070802     c                   eval      *in56   = (O14err = '3')                     Località
098900070802     c                   eval      *in57   = (O14err = '4')                     Provincia
099000070802     c                   eval      *in58   = (O14err = '6')                     Nazione
099100070802     c                   seton                                        28  90
099200070802     c                   eval      V1Dmsg  = O14msg
099300070802     c                   leavesr
099400070802e   1c                   endif
099500070802      * - controllo specifico CAP
099600070802     c                   exsr      Ctr_CAP
099700070802if  1c                   if        *in90
099800070802     c                   leavesr
099900070802e   1c                   endif
100000110121     c                   endif
100100070801      *
100200070802      * Giro di ritiro
100300071105do  1c                   DO        C_MaxCGI      wNrCGI
100400071030      *
100500071031     c                   clear                   WcgiD
100600071030sel 2c                   select
100700071030w   2c                   when      wNrCGI  = 1
100800071030     c                   eval      Wpocgi  = V2Cpocgi1
100900071030     c                   eval      Wcgi    = V2Ccgi1
101000071030w   2c                   when      wNrCGI  = 2
101100071030     c                   eval      Wpocgi  = V2Cpocgi2
101200071030     c                   eval      Wcgi    = V2Ccgi2
101300071030w   2c                   when      wNrCGI  = 3
101400071030     c                   eval      Wpocgi  = V2Cpocgi3
101500071030     c                   eval      Wcgi    = V2Ccgi3
101600071030e   2c                   endsl
101700070911      *
101800071030      * - controllo codici giri ritiro
101900071031sel 2c                   Select
102000071105w   2c                   When      Wcgi    = *blanks
102100071105     c                   clear                   Wpocgi
102200071031      *   - interrogazione giri di ritiro
102300071031w   2c                   When      %scan('?' : %trimr(Wcgi)) > *zeros
102400071031     c                   exsr      Search_Giro
102500071030      *   - controllo codici giri ritiro
102600071031x   2c                   Other
102700070911     c                   exsr      Ctr_Giro
102800071031e   2c                   EndSl
102900071031      *
103000071031sel 2c                   Select
103100071031w   2c                   When      wNrCGI  = 1
103200071030     c                   eval      V2Cpocgi1 = Wpocgi
103300071031     c                   eval      V2Ccgi1   = Wcgi
103400071030     c                   eval      V2Dcgi1   = WcgiD
103500071031w   2c                   When      wNrCGI  = 2
103600071030     c                   eval      V2Cpocgi2 = Wpocgi
103700071031     c                   eval      V2Ccgi2   = Wcgi
103800071030     c                   eval      V2Dcgi2   = WcgiD
103900071105     c                   eval      *in76   = (*in59 = *on)
104000071030     c                   eval      *in77   = (*in60 = *on)
104100071030     c                   eval      *in59   = *off
104200071030     c                   eval      *in60   = *off
104300071031w   2c                   When      wNrCGI  = 3
104400071030     c                   eval      V2Cpocgi3 = Wpocgi
104500071031     c                   eval      V2Ccgi3   = Wcgi
104600071030     c                   eval      V2Dcgi3   = WcgiD
104700071105     c                   eval      *in78   = (*in59 = *on)
104800071030     c                   eval      *in79   = (*in60 = *on)
104900071030     c                   eval      *in59   = *off
105000071030     c                   eval      *in60   = *off
105100071031e   2c                   EndSl
105200071031if  2c                   if        *in90
105300070911     c                   leavesr
105400071031e   2c                   endif
105500071030      *
105600071030e   1c                   ENDDO
105700070802      *
105800070802      * Codice cliente Ordinante
105900070802     c                   clear                   V2Ddeo
106000070802if  1c                   IF             V2Cco1 <> *zeros
106100070802     c                             or   V2Cco2 <> *zeros
106200070802     c                             or   V2Cco3 <> *zeros
106300070802      * - Tipo anagrafica "R" = cliente Ritiro
106400070809     c                   eval      ACRcro  = V2Cco1 * 10000000
106500070809     c                                     + V2Cco2 * 1000
106600070809     c                                     + V2Cco3
106700070802     c     ACRcro        chain(n)  FNACR000
106800071030if  2c                   if             %found(FNACR01L)
106900070802     c                             and  ACRatb  = *blanks
107000070802     c                   movel     ACRrsr        V2Ddeo
107100071030x   2c                   else
107200070802     c                   seton                                        286190
107300070802     c                   eval      V1Dmsg  = $Msg(20)
107400070802     c                   leavesr
107500071030e   2c                   endif
107600071030if  2c                   if             V2Cco1  = V2Ccd1
107700070802     c                             and  V2Cco2  = V2Ccd2
107800070802     c                             and  V2Cco3  = V2Ccd3
107900070802     c                   seton                                        286190
108000070802     c                   eval      V1Dmsg  = $Msg(20)
108100070802     c                   leavesr
108200071030e   2c                   endif
108300070802      *
108400070802e   1c                   ENDIF
108500070802      *
108600070802      * Codice P.d.C.
108700070802     c                   clear                   V2Dksc
108800070802if  1c                   IF        V2Cksc <> *zeros
108900070802     c                   clear                   TIBS69ds
109000070802     c                   eval      I69kac  = V2Cksc
109100070802     c                   exsr      Call_TIBS69
109200070802if  2c                   if        *in90
109300070802     c                   seton                                        286290
109400070802     c                   eval      V1Dmsg  = $Msg(22)
109500070802     c                   leavesr
109600070802e   2c                   endif
109700070802     c                   clear                   Og143
109800070802     c                   moveL     V2Cksc        ORGfil
109900070802     c     ORGfil        chain     AZORG
110000070802if  2c                   if        %found(AZORG01L)
110100070802     c                   eval      Og143   = ORGde3
110200070802e   2c                   endif
110300070802if  2c                   if             §OGntw  = 'LOG'
110400070802     c                             or   §OGntw  = 'XXX'
110500070802     c                   seton                                        286290
110600070802     c                   eval      V1Dmsg  = $Msg(22)
110700070802     c                   leavesr
110800070802e   2c                   endif
110900070802     c                   movel     ACOrag        V2Dksc
111000070802e   1c                   ENDIF
111100070802      *
111200070802      * Tariffa
111300091009if  1c                   IF        V2Cctr <> *blanks
111400070802if  2c                   if        V2Cksc  = *zeros
111500070802     c                   seton                                        286390
111600070802     c                   eval      V1Dmsg  = $Msg(23)
111700070802     c                   leavesr
111800070802e   2c                   endif
111900070802      * - verifica esistenza tariffe per il cliente
112000070802     c                   eval      $Tam    = *off
112100070802     c                   eval      TAMksc  = V2Cksc
112200091009     c                   move      v2cctr        TAMctr
112300070802     c     k02tam01      setgt     TNTAM000
112400070802     c     k02tam01      readpe    TNTAM000
112500070802do  1c                   dow       NOT  %eof(TNTAM01L)
112600070802if  2c                   if        TAMatb  = *blanks
112700070802     c                   eval      $Tam    = *on
112800070802     c                   leave
112900070802e   2c                   endif
113000070802     c     k02tam01      readpe    TNTAM000
113100070802e   1c                   enddo
113200070802if  1c                   if        $Tam    = *off
113300070802     c                   seton                                        286390
113400070802     c                   eval      V1Dmsg  = $Msg(24)
113500070802     c                   leavesr
113600070802e   1c                   endif
113700070802e   1c                   ENDIF
113800070802      *
113900070802      * Frequenza Ritiro
114000070802      * - F = Fisso
114100070802      * - R = Ricorrente
114200151104      * - O = No Internet
114300070802      * - M = Mai
114400151104      /free
114500151104        IF  %scan('?':V2Ctcr) > 0;
114600151104          FineTcr = *off;
114700151104          clear savfreq;
114800151104          exsr RicTcr;
114900151104          V2Ctcr = savfreq;
115000151104        ENDIF;
115100151104      /end-free
115200070802if  1c                   if             V2Ctcr <> 'F'
115300070802     c                             and  V2Ctcr <> 'R'
115400070802     c                             and  V2Ctcr <> 'O'
115500070802     c                             and  V2Ctcr <> 'M'
115600070802     c                   seton                                        286490
115700070802     c                   eval      V1Dmsg  = $Msg(25)
115800070802     c                   leavesr
115900070802e   1c                   endif
116000150909
116100150909      /free
116200150909       //?Filiale ritiro forzata
116300150909         IF  V2Cpoa = 0;
116400150909           V1Dmsg = $msg(33);
116500150909           *in28 = *on;
116600150909           *in52 = *on;
116700150909           *in90 = *on;
116800150909           leavesr;
116900150909         ENDIF;
117000150909         chain (V2Cpoa) AZORG01L;
117100150909         IF  not %found(AZORG01L) or ORGfva <> *blanks;
117200150909           V1Dmsg = $msg(33);
117300150909           *in28 = *on;
117400150909           *in52 = *on;
117500150909           *in90 = *on;
117600150909           leavesr;
117700150909         ENDIF;
117800150909         V2Dpoa = ORGdes;
117900150909      /end-free
118000070802      *
118100070802      * Ora ritiro
118200070802if  1c                   if        V2Corr <> *zeros
118300070802     c                   movel     V2Corr        ds_Time
118400070802if  2c                   if             dsHH    < 1
118500070802     c                             or   dsHH    > 24
118600070802     c                             or   dsMM    > 59
118700070802     c                   seton                                        286590
118800070802     c                   eval      V1Dmsg  = $Msg(26)
118900070802     c                   leavesr
119000070802e   2c                   endif
119100070802e   1c                   endif
119200070802      *
119300070802      * Referente: numero di telefono
119400070802if  1c                   IF        V2Ctel <> *blanks
119500070802     c                   clear                   TRUL42ds
119600070802     c                   eval      D42fax  = V2Ctel
119700070802     c                   call      'TRUL42R'
119800070802     c                   parm                    TRUL42ds
119900070802if  2c                   if        D42err  = *on
120000070802     c                   seton                                        286790
120100070802     c                   eval      V1Dmsg  = D42msg
120200070802     c                   leavesr
120300070802e   2c                   endif
120400070802e   1c                   ENDIF
120500070802      *
120600131113      * Orari Apertura
120700131113     c                   exsr      CtrlOrariApert
120800131113     c                   if        *in28
120900131113     c                   leavesr
121000131113     c                   endif
121100070802      *
121200070802      * Flags immissione
121300070802     c                   select
121400070802      * - Colli
121500070802     c                   when           V2Cfcl <> *blanks
121600070802     c                             and  V2Cfcl <> 'F'
121700070802     c                             and  V2Cfcl <> 'O'
121800070802     c                   seton                                        286990
121900070802      * - Bancali
122000070802     c                   when           V2Cfbn <> *blanks
122100070802     c                             and  V2Cfbn <> 'F'
122200070802     c                             and  V2Cfbn <> 'O'
122300070802     c                   seton                                        287090
122400070802      * - Peso   (kg)
122500070802     c                   when           V2Cfpk <> *blanks
122600070802     c                             and  V2Cfpk <> 'F'
122700070802     c                             and  V2Cfpk <> 'O'
122800070802     c                   seton                                        287190
122900070802      * - Volume (m³)
123000070802     c                   when           V2Cfmc <> *blanks
123100070802     c                             and  V2Cfmc <> 'F'
123200070802     c                             and  V2Cfmc <> 'O'
123300070802     c                   seton                                        287290
123400070802      * - Bilico
123500070802     c                   when           V2Cfbl <> *blanks
123600070802     c                             and  V2Cfbl <> 'F'
123700070802     c                             and  V2Cfbl <> 'O'
123800070802     c                   seton                                        287390
123900070802      * - Motrice
124000070802     c                   when           V2Cfmt <> *blanks
124100070802     c                             and  V2Cfmt <> 'F'
124200070802     c                             and  V2Cfmt <> 'O'
124300070802     c                   seton                                        287490
124400070802      * - Autotreno
124500070802     c                   when           V2Cfat <> *blanks
124600070802     c                             and  V2Cfat <> 'F'
124700070802     c                             and  V2Cfat <> 'O'
124800070802     c                   seton                                        287590
124900070802     c                   endsl
125000070802     c                   if        *in28  and  *in90
125100070802     c                   eval      V1Dmsg  = $Msg(27)
125200070802     c                   leavesr
125300070802     c                   endif
125400070802      *
125500070802      * Se COPIA   &   codice piano dei conti non cambiato
125600070802      * => messaggio d'avvertimento
125700070809if  1c                   if            (I37opz  = '3'
125800070809     c                              or  I37opz  = 'C')
125900070910     c                             and (ACRksc <> *zeros
126000070910     c                             and  V2Cksc  = ACRksc
126100070910     c                             and  V2Cksc <> SAVksc)
126200070809     c                   eval      SAVksc  = V2Cksc
126300070802     c                   seton                                        286290
126400070802     c                   eval      V1Dmsg  = $Msg(28)
126500070802     c                   leavesr
126600070809e   1c                   endif
126700151012      *
126800151012      /free
126900151012
127000151012         // -?Controllo e-Mail Conferma Accettazione?
127100151012         If  V2Cmca <> *blank;
127200151012           clear dsEmail;
127300151012           §emlIndI = V2Cmca;
127400151012           xEmail( dsEmail );
127500151012           if §emlErrO = *on;
127600151012             *in66  = *on;
127700151012             *in28  = *on;
127800151012             *in90  = *on;
127900151012             V1Dmsg = §emlMsgO;
128000151012             leavesr;
128100151012           endif;
128200151012           V2Cmca = §emlIndO;
128300151012         EndIf;
128400160317       //?SMS
128500160317         IF  V2Csca <> *blanks;
128600160317           pInCell = %trim(V2Csca);
128700160317           clear pOutErr;
128800160317           clear pOutCell;
128900160317           IF  UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
129000160317             *in84 = *on;
129100160317             *in28 = *on;
129200160317             *in90 = *on;
129300160317             V1Dmsg = $msg(34);
129400160317             leavesr;
129500160317           ENDIF;
129600160317           IF  pOutCell <> *blanks;
129700160317             V2Csca = pOutCell;
129800160317           ENDIF;
129900160317         ENDIF;
130000151012
130100151012      /end-free
130200070801      *
130300070801     c                   ENDSR
130400131113
130500131113      /free
130600131113
130700131113       //--------------------------------------------------------------
130800131113       //?Controllo orari di apertura.                                 ?
130900131113       //--------------------------------------------------------------
131000131113       BEGSR  CtrlOrariApert;
131100131113
131200131113         // -?Richiamo *pgm per controllo orari di apertura del cliente:?
131300131113         //  ?si deve anche controllare che ci sia abbastanza spazio?
131400131113         //  ?nella Nota-1 e nella Nota-2 per poterli inserire in questi?
131500131113         //  ?campi per poi stamparli?
131600131113         If  (V2ChmAMda + V2ChmAMa + V2ChmPMda + V2ChmPMa) > *zero;
131700131113
131800131113           clear  TRUL03ds;
131900131113           TRUL03ds.i03hm1 = V2ChmAMda;
132000131113           TRUL03ds.i03hm2 = V2ChmAMa;
132100131113           TRUL03ds.i03hm3 = V2ChmPMda;
132200131113           TRUL03ds.i03hm4 = V2ChmPMa;
132300131113           TRUL03ds.i03nt1 = V2Cno1;
132400131113           TRUL03ds.i03nt2 = V2Cno2;
132500131113
132600131113           trul03r ( TRUL03ds );
132700131113
132800131113           If  TRUL03ds.o03err <> *off;
132900131113             V1Dmsg = TRUL03ds.o03msg;
133000131113             *in28 = *on;
133100131113             *in(79 + TRUL03ds.o03errpos) = *on;
133200131113           EndIf;
133300131113
133400131113         EndIf;
133500131113
133600131113       ENDSR;
133700151104
133800151104       //--------------------------------------------------------------
133900151104       //?Ricerca Frequenza Ritiro.                                    ?
134000151104       //--------------------------------------------------------------
134100151104       BEGSR RicTcr;
134200151104
134300151104       //?Inizializzazione videata
134400151104         exsr InzS02;
134500151104
134600151104       //?Visualizzazione del SFL
134700151104         *in30 = (S02nrr <= *zeros);
134800151104
134900151104         DOW not FineTcr;
135000151104
135100151104         //?Emissione videata
135200151104           exfmt  OR37C02;
135300151104           *in28 = *off;
135400151104
135500151104         //?- Enter Controllo ed esco
135600151104           exsr OpzS02;
135700151104           IF  *in28;
135800151104             iter;
135900151104           ENDIF;
136000151104           FineTcr = *on;
136100151104
136200151104         ENDDO;
136300151104
136400151104       ENDSR;
136500151104
136600151104       //--------------------------------------------------------------
136700151104       //?Inizializzazione Videata S02.
136800151104       //--------------------------------------------------------------
136900151104       BEGSR InzS02;
137000151104
137100151104       //?Pulizia subfile
137200151104         exsr PulS02;
137300151104
137400151104       //?Caricamento subfile
137500151104         exsr RieS02;
137600151104
137700151104         *in33 = *on;
137800151104
137900151104       //?Imposto il posizionamento al primo rcd
138000151104         IF  S02nrr > 0;
138100151104           W02rcd = 1;
138200151104         ELSE;
138300151104           clear W02rcd;
138400151104         ENDIF;
138500151104
138600151104         W02csr = W02rcd;
138700151104
138800151104       ENDSR;
138900151104
139000151104       //--------------------------------------------------------------
139100151104       //?Pulizia Subfile S02.
139200151104       //--------------------------------------------------------------
139300151104       BEGSR PulS02;
139400151104
139500151104       //?Pulizia subfile
139600151104         *in30 = *on;
139700151104         *in31 = *on;
139800151104         write OR37C02;
139900151104         *in31 = *off;
140000151104         *in33 = *off;
140100151104
140200151104         clear W02rcd;
140300151104         clear W02csr;
140400151104         clear S02nrr;
140500151104         clear W02msg;
140600151104
140700151104         *in28 = *off;
140800151104
140900151104       ENDSR;
141000151104
141100151104       //--------------------------------------------------------------
141200151104       //?Caricamento Subfile S02.
141300151104       //--------------------------------------------------------------
141400151104       BEGSR RieS02;
141500151104
141600151104         xx = 1;
141700151104         FOR xx by 1 to 4;
141800151104           clear  OR37S02;
141900151104           W02tcr = Frequenza(xx);
142000151104           W02des = Frequenza(xx) + DesFreq(xx);
142100151104           S02nrr += 1;
142200151104           write  OR37S02;
142300151104         ENDFOR;
142400151104
142500151104       ENDSR;
142600151104
142700151104       //--------------------------------------------------------------
142800151104       //?Gestione opzioni Subfile S02.
142900151104       //--------------------------------------------------------------
143000151104       BEGSR OpzS02;
143100151104
143200151104         xx = 1;
143300151104         FOR xx by 1 to 4;
143400151104           S02nrr = xx;
143500151104           chain S02nrr OR37S02;
143600151104           IF  not %found;
143700151104             leave;
143800151104           ENDIF;
143900151104
144000151104           SELECT;
144100151104         //?- Nessuna opzione
144200151104           WHEN  W02opz = *blank;
144300151104
144400151104         //?- 1 = Scelta
144500151104           WHEN  W02opz = '1';
144600151104             savfreq = W02tcr;
144700151104             leave;
144800151104
144900151104           OTHER;
145000151104             *in28 = *on;
145100151104             W02msg = 'Opzione errata';
145200151104             update OR37S02;
145300151104             leave;
145400151104           ENDSL;
145500151104
145600151104         ENDFOR;
145700151104
145800151104       ENDSR;
145900131113
146000131113      /end-free
146100071030
146200071030      *---------------------------------------------------------------*
146300071030      *?Interrogazione Giri di Ritiro                                ?*
146400071030      *---------------------------------------------------------------*
146500071031     c     Search_Giro   BEGSR
146600071030      *
146700071031     c                   clear                   Wpocgi
146800071031     c                   clear                   Wcgi
146900071030     c                   clear                   WcgiD
147000071030      *
147100071030     c                   clear                   FIDG09ds
147200071030     c                   eval      D09iop0   = 'P01'
147300071030     c                   eval      D09ifgs   = I37fgs
147400071030     c                   eval      D09idat   = *date
147500071030     c                   eval      D09itug   = 'R'
147600071030      *
147700071030     c                   movel(p)  FIDG09ds      KPJBU
147800071030     c                   call      'FIDG09R'
147900071030     c                   parm                    KPJBA
148000071030     c                   movel     KPJBU         FIDG09ds
148100071030      *
148200081211sel 1c                   select
148300081211w   1c                   when           D09Of03 <> *on
148400071030     c                             and  D09Of12 <> *on
148500071030     c                             and  D09Oerr <> *on
148600071030     c                   eval      Wpocgi    = D09ofgs
148700071030     c                   eval      Wcgi      = D09ocgi
148800071030     c                   movel     D09odes       WcgiD
148900081211w   1c                   when      D09Of03   = *on
149000081211     c                   eval      *in60     = *on
149100081211     c                   eval      *in90     = *on
149200081211w   1c                   when      D09Of12   = *on
149300081211e   1c                   endsl
149400071030      *
149500071030     c                   ENDSR
149600070802
149700070802      *---------------------------------------------------------------*
149800070802      *?Verifica se record annullabile                               ?*
149900070802      *---------------------------------------------------------------*
150000070802     c     Ctr_Annull    BEGSR
150100070802      *
150200071030     c                   eval      dsCRO1  = V2Ccd1
150300071030     c                   eval      dsCRO2  = V2Ccd2
150400071030     c                   eval      dsCRO3  = V2Ccd3
150500070802      *
150600070802      * Non deve esistere un Ritiro Fisso con questo codice
150700070802     c                   exsr      Ctr_RFissi
150800070802if  1c                   if        *in90   = *on
150900070802     c                   leavesr
151000070802e   1c                   endif
151100070802      *
151200070802      * Non deve esistere un O.R.M. con questo codice
151300070802     c                   exsr      Ctr_ORM
151400070802if  1c                   if        *in90
151500070802     c                   leavesr
151600070802e   1c                   endif
151700121220      *
151800121220      * Non deve esistere l'abilitazione OSR per questo codice
151900121220     c                   exsr      Ctr_OSR
152000121220if  1c                   if        *in90
152100121220     c                   leavesr
152200121220e   1c                   endif
152300070802      *
152400070802     c                   ENDSR
152500070802
152600070802      *---------------------------------------------------------------*
152700070802      *?Non deve esistere un Ritiro Fisso con il codice da annullare ?*
152800070802      *---------------------------------------------------------------*
152900070802     c     Ctr_RFissi    BEGSR
153000070802      *
153100070802      * Ricerca per codice ritiro
153200070802     c                   move      ds_CRO        ORScra
153300070802     c     ORScra        setll     FNORS002
153400070802     c     ORScra        reade     FNORS002
153500070802do  1c                   dow       NOT  %eof(FNORS02L)
153600071030if  2c                   if        ORSatb  = *blanks
153700070803     c                   seton                                        285190
153800071030     c                   eval      V1Dmsg  = $Msg(5)
153900070802     c                   leavesr
154000070802e   2c                   endif
154100070802     c     ORScra        reade     FNORS002
154200070802e   1c                   enddo
154300070802      *
154400070802      * Ricerca per codice ordinante
154500070802     c                   move      ds_CRO        ORScor
154600070802     c     ORScor        setll     FNORS003
154700070802     c     ORScor        reade     FNORS003
154800070802do  1c                   dow       NOT  %eof(FNORS03L)
154900071030if  2c                   if        ORSatb  = *blanks
155000070803     c                   seton                                        285190
155100071030     c                   eval      V1Dmsg  = $Msg(5)
155200070802     c                   leavesr
155300070802e   2c                   endif
155400070802     c     ORScor        reade     FNORS003
155500070802e   1c                   enddo
155600070802      *
155700070802     c                   ENDSR
155800070802
155900070802      *---------------------------------------------------------------*
156000070802      *?Non deve esistere un O.R.M. con il codice da annullare       ?*
156100070802      *---------------------------------------------------------------*
156200070802     c     Ctr_ORM       BEGSR
156300070802      *
156400070802      * Ricerca per codice ritiro
156500070802     c                   move      ds_CRO        ORMcra
156600070802     c     ORMcra        setll     FNORM018
156700070802     c     ORMcra        reade     FNORM018
156800070802do  1c                   dow       NOT  %eof(FNORM18L)
156900070807if  2c                   if             ORMatb  = *blanks
157000070807     C                             and  ORMfao  < 900
157100070803     c                   seton                                        285190
157200071030     c                   eval      V1Dmsg  = $Msg(6)
157300070802     c                   leavesr
157400070802e   2c                   endif
157500070807     c     ORScra        reade     FNORM018
157600070802e   1c                   enddo
157700070802      *
157800070802      * Ricerca per codice ordinante
157900070802     c                   move      ds_CRO        ORMcor
158000070802     c     ORMcor        setll     FNORM009
158100070802     c     ORMcor        reade     FNORM009
158200070802do  1c                   dow       NOT  %eof(FNORM09L)
158300070802if  2c                   if             ORMatb  = *blanks
158400070802     C                             and  ORMfao  < 900
158500070803     c                   seton                                        285190
158600071030     c                   eval      V1Dmsg  = $Msg(6)
158700070802     c                   leavesr
158800070802e   2c                   endif
158900070802     c     ORMcor        reade     FNORM009
159000070802e   1c                   enddo
159100070802      *
159200070802     c                   ENDSR
159300121220
159400121220      *---------------------------------------------------------------*
159500121220      *?Non deve esistere tab. OSR  con il codice da annullare       ?*
159600121220      *---------------------------------------------------------------*
159700121220     c     Ctr_OSR       BEGSR
159800121220
159900121220      * se non è aperto il file lo devo aprire
160000121220     c                   IF        not %open(TNTBE01L)
160100121220     c                   IF        %subst(knsif:7:1) = 'P'
160200121220     c                   eval      wLibTbe = 'GAITRAGRPS/TNTBE01L'
160300121220     c                   ELSE
160400121220     c                   eval      wLibTbe = 'GAITRAGRU/TNTBE01L'
160500121220     c                   ENDIF
160600121220     c                   open      TNTBE01L
160700121220     c                   ENDIF
160800121220
160900121220     c     'OSR'         setll     TNTBE01L
161000121220     c                   DO        *hival
161100121220     c     'OSR'         reade     TNTBE01L
161200121220     c                   IF        %eof(TNTBE01L)
161300121220     c                   leave
161400121220     c                   ENDIF
161500121220     c                   IF        TBEatb <> *blanks
161600121220     c                   iter
161700121220     c                   ENDIF
161800121220     c                   eval      dOSR = TBEuni
161900121220     c                   SELECT
162000121220     c                   WHEN      %subst(%editc(I37cro:'X'):8:3) = '333' or
162100121220     c                             %subst(%editc(I37cro:'X'):8:3) = '444'
162200121220     c                   IF        %subst(%editc(I37cro:'X'):1:7) =
162300121220     c                             %editc(d§OSRcli:'X') or
162400121220     c                             %subst(%editc(I37cro:'X'):1:7) =
162500121220     c                             %editc(d§OSRcl2:'X') or
162600121220     c                             %subst(%editc(I37cro:'X'):1:7) =
162700121220     c                             %editc(d§OSRcl3:'X') or
162800121220     c                             %subst(%editc(I37cro:'X'):1:7) =
162900121220     c                             %editc(d§OSRcl4:'X') or
163000121220     c                             %subst(%editc(I37cro:'X'):1:7) =
163100121220     c                             %editc(d§OSRcl5:'X') or
163200121220     c                             %subst(%editc(I37cro:'X'):1:7) =
163300121220     c                             %editc(d§OSRcl6:'X') or
163400121220     c                             %subst(%editc(I37cro:'X'):1:7) =
163500121220     c                             %editc(d§OSRcl7:'X') or
163600121220     c                             %subst(%editc(I37cro:'X'):1:7) =
163700121220     c                             %editc(d§OSRcl8:'X') or
163800121220     c                             %subst(%editc(I37cro:'X'):1:7) =
163900121220     c                             %editc(d§OSRcl9:'X') or
164000121220     c                             %subst(%editc(I37cro:'X'):1:7) =
164100121220     c                             %editc(d§OSRcl13:'X') or
164200121220     c                             %subst(%editc(I37cro:'X'):1:7) =
164300121220     c                             %editc(d§OSRcl11:'X') or
164400121220     c                             %subst(%editc(I37cro:'X'):1:7) =
164500121220     c                             %editc(d§OSRcl12:'X') or
164600121220     c                             %subst(%editc(I37cro:'X'):1:7) =
164700121220     c                             %editc(d§OSRcl13:'X') or
164800121220     c                             %subst(%editc(I37cro:'X'):1:7) =
164900121220     c                             %editc(d§OSRcl14:'X') or
165000121220     c                             %subst(%editc(I37cro:'X'):1:7) =
165100121220     c                             %editc(d§OSRcl15:'X') or
165200121220     c                             %subst(%editc(I37cro:'X'):1:7) =
165300121220     c                             %editc(d§OSRcl16:'X')
165400121220     c                   seton                                        285190
165500121220     c                   eval      V1Dmsg  = $Msg(31)
165600121220     c                   eval      V1Dmsg = %trim(V1Dmsg) + ' LNP ' +
165700121220     c                             %subst(TBEke1:1:3) + ' Serie ' +
165800121220     c                             %subst(TBEke1:4:2)
165900121220     c                   leavesr
166000121220     c                   ENDIF
166100121220     c                   WHEN      %subst(%editc(I37cro:'X'):8:3) = '333' and
166200121220     c                             %subst(%editc(I37cro:'X'):1:7) =
166300121220     c                             %editc(d§OSRcor:'X')
166400121220     c                   seton                                        285190
166500121220     c                   eval      V1Dmsg  = $Msg(31)
166600121220     c                   eval      V1Dmsg = %trim(V1Dmsg) + ' LNP ' +
166700121220     c                             %subst(TBEke1:1:3) + ' Serie ' +
166800121220     c                             %subst(TBEke1:4:2)
166900121220     c                   leavesr
167000121220     c                   WHEN      %subst(%editc(I37cro:'X'):8:3) = '444' and
167100121220     c                             %subst(%editc(I37cro:'X'):1:7) =
167200121220     c                             %editc(d§OSRcrc:'X')
167300121220     c                   seton                                        285190
167400121220     c                   eval      V1Dmsg  = $Msg(31)
167500121220     c                   eval      V1Dmsg = %trim(V1Dmsg) + ' LNP ' +
167600121220     c                             %subst(TBEke1:1:3) + ' Serie ' +
167700121220     c                             %subst(TBEke1:4:2)
167800121220     c                   leavesr
167900121220     c                   ENDSL
168000121220     c                   ENDDO
168100121220
168200121220     c                   ENDSR
168300070802
168400070802      *---------------------------------------------------------------*
168500070802      *?Controllo C.A.P.                                             ?*
168600070802      *---------------------------------------------------------------*
168700070802     c     Ctr_CAP       BEGSR
168800070802      *
168900070802     c                   clear                   TISI95ds
169000070802     c                   eval      I95tcn  = '7'
169100070802     c                   eval      I95nar  = V2Cnar
169200070802     c                   eval      I95cap  = V2Ccar
169300070802     c                   eval      I95loc  = V2Clor
169400070802     c                   eval      I95prv  = V2Cprr
169500070802     c                   eval      I95ind  = V2Cinr
169600070802     c                   clear                   FNLV13ds
169700070802     c                   eval      I13af0  = 'S'
169800070802     c                   eval      I13cnv  = 'S'
169900070802     c                   eval      I13af1  = 'S'
170000070802     c                   eval      I13sz2  = 'S'
170100070802     c                   eval      I13la3  = 'S'
170200070802     c                   eval      I13sz3  = 'S'
170300070802      *
170400070802      * Gestione errori forzabili:
170500070802      * se cambiato CAP/Località riazzero i valori di errori forzabili
170600070802      *
170700070802      * - Modifica CAP
170800070802if  1c                   if        V2Ccar <> SAVcar
170900070802     c                   eval      SAVcar  = V2Ccar
171000070802     c                   clear                   E13fzv
171100070802     c                   clear                   E13fz1
171200070802     c                   clear                   E13fz2
171300070802     c                   clear                   E13fz3
171400070802e   1c                   endif
171500070802      * - Modifica località
171600070802if  1c                   if        V2Clor <> SAVlor
171700070802     c                   eval      SAVlor  = V2Clor
171800070802     c                   clear                   E13fz1
171900070802     c                   clear                   E13fz2
172000070802     c                   clear                   E13fz3
172100070802e   1c                   endif
172200070802      * - Modifica PROVINCIA
172300070802if  1c                   if        V2Cprr <> SAVprr
172400070802     c                   eval      SAVprr  = V2Cprr
172500070802     c                   clear                   E13fz3
172600070802e   1c                   endif
172700070802      *
172800070802      * Richiamo programma
172900070802     c                   call      'FNLV13R'
173000070802     c                   parm                    KPJBA
173100070802     c                   parm                    FNLV13ds
173200070802     c                   parm                    TISI95ds
173300070802      *
173400070802      * Reimpostazione dei dati ricevuti in TISI95ds
173500070802if  1c                   if             O13ron  = 'S'
173600110121     c                   eval      V2Cnar  = O95nar
173700110121e   1c                   endif
173800110121     c                   if             O13roc  = 'S'
173900110121     c                   eval      V2Ccar  = O95cap
174000110121e   1c                   endif
174100110121     c                   if             O13rop  = 'S'
174200110121     c                   eval      V2Cprr  = O95prv
174300110121e   1c                   endif
174400110121     c                   if             O13rol  = 'S'
174500070802     c                   eval      V2Clor  = O95loc
174600070802e   1c                   endif
174700070802      *
174800070802      * Riscontrati errori o reimpostati dati
174900070802sel 1c                   select
175000070802      * - Emissione messaggio con l'errore rilevato
175100081022w   1c                   when      O13msg <> *blanks
175200070802     c                   eval      *in55   = (O13err = '5')                     CAP
175300070802     c                   eval      *in56   = (O13err = '3')                     Località
175400070802     c                   eval      *in57   = (O13err = '4')                     Provincia
175500070802     c                   eval      *in58   = (O13err = '6')                     Nazione
175600070802     c                   seton                                        28  90
175700070802     c                   eval      V1Dmsg  = O13msg
175800070802     c                   leavesr
175900070802      * - C.A.P. reimpostato
176000070802w   1c                   when      O13roc  = 'S'
176100070802     c                   seton                                        285590
176200070802     c                   eval      V1Dmsg  = $Msg(12)
176300070802     c                   leavesr
176400070802      * - Località reimpostata
176500070802w   1c                   when      O13rol  = 'S'
176600070802     c                   seton                                        285690
176700070802     c                   eval      V1Dmsg  = $Msg(13)
176800070802     c                   leavesr
176900070802      * - Provincia reimpostata
177000070802w   1c                   when      O13rop  = 'S'
177100070802     c                   seton                                        285790
177200070802     c                   eval      V1Dmsg  = $Msg(14)
177300070802     c                   leavesr
177400070802      * - Nazione reimpostata
177500070802w   1c                   when      O13ron  = 'S'
177600070802     c                   seton                                        285890
177700070802     c                   eval      V1Dmsg  = $Msg(15)
177800070802     c                   leavesr
177900070802e   1c                   endsl
178000070802      *
178100070802     c                   ENDSR
178200070801
178300070801      *---------------------------------------------------------------*
178400070801      *?Reperimento dati anagrafici                                  ?*
178500070801      *---------------------------------------------------------------*
178600070801     c     Call_TIBS69   BEGSR
178700070801      *
178800070801     c                   clear                   ds_CNACO
178900070801     c                   clear                   ds_CNIND
179000070801     c                   clear                   ds_CNCLP
179100070801     c                   clear                   ds_FNCLS
179200070801      *
179300070801     c                   call      'TIBS69R'
179400070801     c                   parm                    TIBS69ds
179500070801     c                   parm                    ds_CNACO
179600070801     c                   parm                    ds_CNIND
179700070801     c                   parm                    ds_CNCLP
179800070801     c                   parm                    ds_FNCLS
179900070801      *
180000071023     c                   eval      *in28   = (O69err = *on)
180100070801if  1c                   if        *in28
180200071023     c                   eval      *in90   = *on
180300071023     c                   eval      V1Dmsg  = O69msg
180400070801e   1c                   endif
180500070801      *
180600070801     c                   ENDSR
180700070801
180800070801      *---------------------------------------------------------------*
180900070801      *?Aggiornamento dati file FNACR00F                             ?*
181000070801      *---------------------------------------------------------------*
181100070801     c     Upd_FNACR     BEGSR
181200070801      *
181300071023     c                   eval      *in91   = *off
181400071023     c                   eval      I37cro  = V2Ccd1 * 10000000
181500071023     c                                     + V2Ccd2 * 1000
181600070807     c                                     + V2Ccd3
181700071030     c     I37cro        chain     FNACR000
181800070803      *
181900070801     c                   select
182000070801      *
182100070801      * Inserimento / Copia
182200070801w   1c                   when          (I37opz = '1'
182300070809     c                             or   I37opz = '3'
182400070809     c                             or   I37opz = 'A'
182500070809     c                             or   I37opz = 'C')
182600070801     c                             and  *inKF
182700070801     c                   exsr      RieRec
182800070803     c                   WRITE     FNACR000                             91
182900070801      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
183000070801      * Modifica
183100070801w   1c                   when           I37opz  = '2'
183200070801     c                             and  *inKF
183300070801     c                   exsr      RieRec
183400070801     c                   UPDATE    FNACR000
183500070801      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
183600070801      * Annullamento
183700070801w   1c                   when          (I37opz  = '4'
183800070801     c                             and  *inKF)
183900070801     c                             or   *inKQ
184000070801     c                   eval      ACRatb  = 'A'
184100070801     c                   UPDATE    FNACR000
184200070801      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
184300070801      * Ripristino
184400070801w   1c                   when          (I37opz  = '7'
184500070801     c                             and  *inKF)
184600070801     c                             or   *inKE
184700070802     c                   exsr      RieRec
184800070801     c                   UPDATE    FNACR000
184900070801      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
185000070801e   1c                   endsl
185100070801      *
185200070803if  1c                   if        *in91
185300070801     c                   seton                                        28  90
185400071030     c                   eval      V1Dmsg  = %trimr($Msg(4))
185500071030     c                                     + ' FNACR00F'
185600070801e   1c                   endif
185700070801      *
185800071030      * Aggiornamento ambito "standard" di intradamento codice giro
185900071030     c                   eval      ACR1ain = '='
186000071030     c                   eval      Wpocgi  = V2Cpocgi1
186100071030     c                   eval      Wcgi    = V2Ccgi1
186200070801     c                   exsr      Upd_FNACR1
186300071030      *
186400071030      * Aggiornamento ambito "oltre" di intradamento codice giro
186500071030     c                   eval      ACR1ain = '>'
186600071030     c                   eval      Wpocgi  = V2Cpocgi2
186700071030     c                   eval      Wcgi    = V2Ccgi2
186800071030     c                   exsr      Upd_FNACR1
186900071030      *
187000071030      * Aggiornamento ambito "sotto" di intradamento codice giro
187100071030     c                   eval      ACR1ain = '<'
187200071030     c                   eval      Wpocgi  = V2Cpocgi3
187300071030     c                   eval      Wcgi    = V2Ccgi3
187400071030     c                   exsr      Upd_FNACR1
187500151012      *
187600151012      /free
187700151012
187800151012         // -?Aggiornamento dati file FNACRE0F?
187900151012
188000151012         // -?Impostazione chiave d'accesso al file FNACRE1L per l'?
188100151012         //  ?aggionamento della "e-Mail Conferma Accettazione ORM"?
188200151012         clear  keyFNACRE1;
188300151012         k_ACREcro = I37cro;
188400151012         k_ACREtrc = 'MC';
188500151012         chain  %kds( keyFNACRE1 )  FNACRE00;
188600151012
188700151012         Select;
188800151012
188900151012           // -?Inserimento / Copia?
189000151012           When  Not %found(FNACRE1L)  and  V2Cmca <> *blank;
189100151012             clear  FNACRE00;
189200151012             ACREatb  = ACRatb;
189300151012             ACREcro  = k_ACREcro;
189400151012             ACREtrc  = k_ACREtrc;
189500151012             ACREdati = V2Cmca;
189600151012             Write  FNACRE00;
189700151012
189800151012           // -?Modifica / Annullamento / Ripristino?
189900151012           When  %found(FNACRE1L)  and  V2Cmca <> *blank;
190000151012             ACREatb  = ACRatb;
190100151012             ACREdati = V2Cmca;
190200151012             Update  FNACRE00;
190300151012
190400151012           // -?Cancellazione?
190500151012           When  %found(FNACRE1L)  and  V2Cmca = *blank;
190600151012             Delete  FNACRE00;
190700151012
190800151012         EndSl;
190900160317
191000160317         // -?Impostazione chiave d'accesso al file FNACRE1L per l'?
191100160317         //  ?aggionamento del "SMS Conferma Accettazione ORM"?
191200160317         clear  keyFNACRE1;
191300160317         k_ACREcro = I37cro;
191400160317         k_ACREtrc = 'SC';
191500160317         chain  %kds( keyFNACRE1 )  FNACRE00;
191600160317
191700160317         Select;
191800160317
191900160317           // -?Inserimento / Copia?
192000160317           When  Not %found(FNACRE1L)  and  V2Csca <> *blank;
192100160317             clear  FNACRE00;
192200160317             ACREatb  = ACRatb;
192300160317             ACREcro  = k_ACREcro;
192400160317             ACREtrc  = k_ACREtrc;
192500160317             ACREdati = V2Csca;
192600160317             Write  FNACRE00;
192700160317
192800160317           // -?Modifica / Annullamento / Ripristino?
192900160317           When  %found(FNACRE1L)  and  V2Csca <> *blank;
193000160317             ACREatb  = ACRatb;
193100160317             ACREdati = V2Csca;
193200160317             Update  FNACRE00;
193300160317
193400160317           // -?Cancellazione?
193500160317           When  %found(FNACRE1L)  and  V2Csca = *blank;
193600160317             Delete  FNACRE00;
193700160317
193800160317         EndSl;
193900151012
194000151012      /end-free
194100070801      *
194200070801     c                   ENDSR
194300070801
194400070801      *---------------------------------------------------------------*
194500070801      *?Aggiornamento dati file FNACR10F                             ?*
194600070801      *---------------------------------------------------------------*
194700070801     c     Upd_FNACR1    BEGSR
194800070801      *
194900071030     c                   eval      *in92   = *off
195000070807     c                   eval      I37cro  = V2Ccd1*10000000
195100070807     c                                     + V2Ccd2*1000
195200070807     c                                     + V2Ccd3
195300071024      *
195400071024sel 1c                   select
195500070801      *
195600070802      * Inserimento / Modifica / Copia
195700070803w   1c                   when          (I37opz  = '1'
195800070803     c                             or   I37opz  = '2'
195900070809     c                             or   I37opz  = '3'
196000070809     c                             or   I37opz  = 'A'
196100070809     c                             or   I37opz  = 'C')
196200070801     c                             and  *inKF
196300071030     c     k03acr13      chain     FNACR100
196400071105     c                   clear                   SAVpocgi
196500071105     c                   clear                   SAVcgi
196600071105if  2c                   if        %found(FNACR13L)
196700071105     c                   eval      SAVpocgi     = ACR1pocgi
196800071105     c                   eval      SAVcgi       = ACR1cgi
196900071105e   2c                   endif
197000071105      *
197100071030if  2c                   if        Wcgi   <> *blanks
197200070801     c                   exsr      RieRec1
197300071030e   2c                   endif
197400071105      *
197500070803sel 2c                   select
197600071030w   2c                   when      NOT  %found(FNACR13L)
197700071030     c                             and  Wcgi   <> *blanks
197800070801     c                   WRITE     FNACR100                             92
197900070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
198000071030w   2c                   when           %found(FNACR13L)
198100071030     c                             and  Wcgi   <> *blanks
198200071105     c                             and  Wcgi   <> SAVcgi
198300070801     c                   UPDATE    FNACR100
198400070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
198500071030w   2c                   when           %found(FNACR13L)
198600071030     c                             and  Wcgi    = *blanks
198700071024     c                   DELETE    FNACR100
198800071024      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
198900071030w   2c                   when           %found(FNACR13L)
199000071105     c                             and  Wcgi    = SAVcgi
199100071030     c                   UNLOCK    FNACR13L
199200071030      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
199300070803e   2c                   endsl
199400070801      *
199500070801      * Annullamento
199600070807w   1c                   when         ((I37opz  = '4'
199700070807     c                             and  *inKF)
199800070807     c                             or   *inKQ)
199900071024     c     I37cro        setll     FNACR100
200000071024     c     I37cro        reade     FNACR100
200100071030do  2c                   dow       NOT  %eof(FNACR13L)
200200080115     c                   DELETE    FNACR100
200300070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
200400071024     c     I37cro        reade     FNACR100
200500071024e   2c                   enddo
200600070801      * Ripristino
200700070807w   1c                   when         ((I37opz  = '7'
200800070801     c                             and  *inKF)
200900070807     c                             or   *inKE)
201000071024     c     I37cro        setll     FNACR100
201100071024     c     I37cro        reade     FNACR100
201200071030do  2c                   dow       NOT  %eof(FNACR13L)
201300071030if  3c                   if        I37fgs  = ACR1pocgi
201400070801     c                   exsr      RieRec1
201500071024x   3c                   else
201600071030     c                   eval      ACR1atb = *blanks
201700071030     c                   eval      ACR1dtv = *date
201800071030     c                   eval      ACR1ute = KNMUS
201900071024e   3c                   endif
202000070807     c                   UPDATE    FNACR100
202100070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
202200071024     c     I37cro        reade     FNACR100
202300071024e   2c                   enddo
202400070801e   1c                   endsl
202500070801      *
202600070801if  1c                   if        *in92
202700070801     c                   seton                                        28  90
202800071030     c                   eval      V1Dmsg  = %trimr($Msg(4))
202900071030     c                                     + ' FNACR10F'
203000070801e   1c                   endif
203100070801      *
203200070801     c                   ENDSR
203300070801
203400070801      *---------------------------------------------------------------*
203500070801      *?Impostazione valori nei campi del record FNACR00F            ?*
203600070801      *---------------------------------------------------------------*
203700070801     c     RieRec        BEGSR
203800070801      *
203900131113     c                   clear                   dACR01
204000070801     c                   clear                   FNACR000
204100070801      *
204200070809      * Lascia record annullato se lo era già (*in43)
204300070809      * e non ne è stato chiesto il ripristino
204400070807     c                   if             *in43
204500070807     c                             and  NOT *inKE
204600070807     c                             and  I37opz <> '7'
204700070807     c                   eval      ACRatb  = 'A'
204800070807     c                   endif
204900070809      *
205000070809      * Imposta tutti gli altri campi ("chiave" compresi)
205100071030     c                   eval      ACRtac  = 'R'
205200070801     c                   eval      ACRcro  = V2Ccd1*10000000
205300070801     c                                     + V2Ccd2*1000
205400070801     c                                     + V2Ccd3
205500070801     c                   eval      ACRrsr  = V2Crsr
205600070801     c                   eval      ACRinr  = V2Cinr
205700070801     c                   eval      ACRcar  = V2Ccar
205800070801     c                   eval      ACRlor  = V2Clor
205900070801     c                   eval      ACRprr  = V2Cprr
206000070801     c                   eval      ACRnar  = V2Cnar
206100070801     c                   eval      ACRksc  = V2Cksc
206200070801     c                   eval      ACRcco  = V2Cco1*10000000
206300070801     c                                     + V2Cco2*1000
206400070801     c                                     + V2Cco3
206500070801     c                   eval      ACRper  = V2Cper
206600070801     c                   eval      ACRtel  = V2Ctel
206700131113     c                   eval      dACR01.§ACRoa1 = %editc( V2ChmAmDa : 'X' )
206800131113     c                   eval      dACR01.§ACRoa2 = %editc( V2ChmAmA  : 'X' )
206900131113     c                   eval      dACR01.§ACRoa3 = %editc( V2ChmPmDa : 'X' )
207000131113     c                   eval      dACR01.§ACRoa4 = %editc( V2ChmPmA  : 'X' )
207100140114     c                   eval      dACR01.§ACRstcr = sav_§ACRstcr
207200140114     c                   eval      dACR01.§ACRctcr = sav_§ACrctcr
207300151104     c                   eval      dACR01.§ACRfmar = V2Cfca
207400131113     c                   eval      ACRmai  = dACR01
207500070801     c                   eval      ACRno1  = V2Cno1
207600070801     c                   eval      ACRno2  = V2Cno2
207700070801     c                   eval      ACRntm  = V2Cntm
207800150909     c                   eval      ACRpoa  = V2Cpoa
207900070801     c                   eval      ACRtcr  = V2Ctcr
208000070802if  1c                   if        V2Cfcc  = 'N'
208100070801     c                   eval      ACRfcc  = V2Cfcc
208200070802x   1c                   else
208300070802     c                   clear                   ACRfcc
208400070802e   1c                   endif
208500091009if  1c                   if        V2Cctr  = *blanks
208600070801     c                   eval      ACRccc  = 999
208700070801x   1c                   else
208800091009     c                   move      v2cctr        ACRccc
208900070801e   1c                   endif
209000070801     c                   eval      ACRfcl  = V2Cfcl
209100070802     c                   eval      ACRfpk  = V2Cfpk
209200070801     c                   eval      ACRfmc  = V2Cfmc
209300070801     c                   eval      ACRfbn  = V2Cfbn
209400070801     c                   eval      ACRfbl  = V2Cfbl
209500070801     c                   eval      ACRfmt  = V2Cfmt
209600070801     c                   eval      ACRfat  = V2Cfat
209700070801     c                   eval      ACRspi  = V2Cspi
209800070801     c                   eval      ACRorr  = V2Corr
209900070801     c                   eval      ACRdtr  = *date
210000070801      *
210100070801     c                   ENDSR
210200070801
210300070801      *---------------------------------------------------------------*
210400070801      *?Impostazione valori nei campi del record FNACR10F            ?*
210500070801      *---------------------------------------------------------------*
210600070801     c     RieRec1       BEGSR
210700070801      *
210800070913     c                   eval      ACR1atb   = ACRatb
210900070801     c                   eval      ACR1cro   = ACRcro
211000071030     c                   eval      ACR1pocgi = Wpocgi
211100071030     c                   eval      ACR1cgi   = Wcgi
211200071030if  1c                   if        NOT  %found(FNACR13L)
211300071030     c                             or  (SAVpocgi <> Wpocgi
211400071030     c                             or   SAVcgi   <> Wcgi)
211500070801     c                   eval      ACR1dtv   = *date
211600070802     c                   eval      ACR1ute   = KNMUS
211700070802e   1c                   endif
211800070801      *
211900070801     c                   ENDSR
212000070801
212100070801** - $Fun ------*
212200070801   INSERIMENTO      1
212300070801     MODIFICA       2
212400070801      COPIA         3
212500070801   ANNULLAMENTO     4
212600070801 VISUALIZZAZIONE    5
212700070801    RIPRISTINO      6
212800070801** - $Msg -------------------------------------------------------------------*
212900070801PARAMETRI ERRATI                                                                1
213000070801Richiesta funzione errata per record inesistente                                2
213100070801Record già esistente                                                            3
213200070801Errore in fase di scrittura record nel file                                     4
213300070802NON ANNULLABILE: reperite anagrafiche ritiri fissi con questo codice            5
213400070802NON ANNULLABILE: reperiti ritiri con questo codice                              6
213500070802Codice cliente ritiro non valido: NON esiste sul piano dei conti                7
213600070802Codice cliente ritiro obbligatorio                                              8
213700070802Codice cliente ritiro già esistente                                             9
213800070802Tipo anagrafica errato                                                         10
213900070802Frequenza ritiro errato                                                        11
214000070802ATTENZIONE: modificato C.A.P.                                                  12
214100070802ATTENZIONE: modificata località                                                13
214200070802ATTENZIONE: modificata provincia                                               14
214300070802ATTENZIONE: modificata nazione                                                 15
214400070808Immettere anche il giro di ritiro  o  togliere la filiale del giro             16
214500070808Immettere anche la filiale del giro  o  togliere il giro di ritiro             17
214600070802Filiale errata per codice giro di ritiro                                       18
214700070911Codice giro di ritiro inesistente o non valido                                 19
214800070802Codice cliente ordinante errato                                                20
214900070802Non è possibile inserire un codice ordinante per un cliente di tipo ordinante  21
215000070802Codice cliente piano dei conti errato                                          22
215100070802Inserire il codice tariffa solo se inserito anche il codice P.d.C.             23
215200091009Codice tariffa inesistente                                                     24
215300070802Frequenza ritiro errata                                                        25
215400070802Ora ritiro errata                                                              26
215500070802Valore flag immissione errato                                                  27
215600070802NON è stato modificato il codice Piano dei Conti  --  Enter per forzare        28
215700071112Codice giro di ritiro errato o NON in vigore                                   29
215800071030PRIMA posizionare il cursore sul campo per cui interrogare e/o selezionare     30
215900121220NON ANNULLABILE: Codice presente su tabella OSR                                31
216000140723Creare il luogo '000' per il codice cliente in gestione.                       32
216100150909Filiale Ritiro Forzata errata                                                  33
216200160317Immettere un numero di telefono valido                                         34
216300151104
216400151104** -- Frequenza / DesFreq ---------------------------------------------------*
216500151104F = Fisso
216600151104R = Ricorrente
216700151104O = No Internet
216800151104M = Mai
