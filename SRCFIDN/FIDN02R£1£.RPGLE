000100060314      *---------------------------------------------------------------*
000200060314      *? Stampa lettere:                                             ?*
000300060314      *? AL1 - Comunicaz. al Cliente per Pratica Assicurativa        ?*
000400080111      *?     - Ci saranno dal 2008 3 versioni diverse in base al tipo?*
000500080111      *?       anomalia Avaria Ammanco / mancanza / Avaria con reso  ?*
000600060314      *? AE1 - Comunicaz. al Cliente per Pratica Assicurativa ESTERO ?*
000700060314      *? AD1 - Comunicaz. al Cliente per Pratica Assicurativa DPD    ?*
000800060314      *? AM1 - Comunicaz. al Cliente per Pratica Assicurativa DPD    ?*
000900060314      *?                                 Mancanza                    ?*
001000060314      *? AL5 - Lettera Accompagnam. Reso Merce Danneggiata (Avaria)  ?*
001100060314      *---------------------------------------------------------------*
001200060314      *  Per i Franchi Import forzo la stampa del testo AE1           *
001300060314      *    indirizzato al destinatario                                *
001400060314      *---------------------------------------------------------------*
001500060314      *  Per i DPD forzo la lingua inglese e testo stampa AD1         *
001600060314      *  Per i DPD forzo la lingua inglese e testo stampa AM1 per     *
001700060314      *            MANCANZE                                           *
001800060314      *  Dal 27-03-2002 per DPD non forza più la lingua inglese,      *
001900060314      *    controlla byte 7 campo CLSFLO                              *
002000060314      *---------------------------------------------------------------*
002100980729
002200060315     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002300980729
002400060314     fFNDET01L  if   e           k disk
002500060314     fFNDCS01L  if   e           k disk
002600060314     fFNDFA01L  if   e           k disk
002700060314     fFNDKA01L  if   e           k disk
002800060314      *
002900060314     fFNARB01L  if   e           k disk
003000060314     fFNBLP01L  if   e           k disk    prefix(ARB:3)
003100060314      *
003200060314     fAZORG01L  if   e           k disk
003300060314      *
003400060314     fFNDCT01L  Uf   e           k disk
003500060314     fFNDCF01L  Uf A e           k disk
003600060314      *
003700060315     fFIDN02P   o    e             printer usropn
003800060315     f                                     OFlind(*in01)
003900060315     fFIDN02PM  o    e             printer usropn
004000060315     f                                     OFlind(*in02)
004100060314
004200060314      *---------------------------------------------------------------*
004300060314
004400060314      *
004500060314      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - - *?
004600060314      *
004700060314     d I_picpod        c                   const('UFF. INTERNAZIONALE')
004800060314      *
004900060314      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *?
005000060314      *
005100060314      * - Comandi x OVRPRTF
005200060315     d $Cmd            s             85    dim( 9) ctdata perrcd(1)
005300060315      *
005400060314      * - Codice testo da stampare
005500060315     d $CoTe           s              3    dim( 5) ctdata perrcd(1)             codici testo
005600060314      *
005700060315      * Testi da elaborare
005800060315     d $TeEl           s              3    dim( 2) inz                          testi da elaborare
005900060315      *
006000060314      * Descrizione Tipo Anomalia
006100060315     d $TAD            s             35    dim( 3) inz                          descr. tipo anomalia
006200060315      *
006300060315      * Note da stampare
006400060315     d $Note           s            105    dim( 5) inz                          $TAD x 5
006500060314      *
006600060314      *?  S T R U T T U R E   D A T I - - - - - - - - - - - - - - - - *?
006700060314      *
006800060314      * - Parametri ricevuti
006900060314     d KPJBA         e ds
007000060314      *
007100060314     d FIDN00ds      e ds
007200060314     d   Wnumca                6     19
007300081215      * - Ricerca del beneficiario
007400081215     d FIDNBEds      e ds
007500090325      * - Chiusura CA
007600090325     d FIDN22ds      e ds
007700060314      *
007800060314      * - Parametri x Controllo profilo utenti
007900060314     d TIBS34ds      e ds
008000060314      * - Ds di riferimento al file esterno AZUTE00F
008100060314     d AZUTEds       e ds                  extname(AZUTE00F)
008200060314      * - Ds per dati organigramma
008300060314     d dDatiUte      e ds
008400060314      *
008500060314      * - 143ª descrizione dell'organigramma
008600060314     d Og143         e ds                  inz
008700060314      *
008800060314      * - Flag operativi in FNDCT
008900060314     d dDCT01        e ds                  inz
009000060314      *
009100060314      * - Ricerca/Controllo tabelle
009200060314     d TIBS02ds      e ds                  inz
009300060314      * - tab. TAD = Tipi anomalia danni
009400060314     d dTAD          e ds                  inz
009500060314      * - tab. TLD = Testi lettera danni
009600060314     d dTLD          e ds                  inz
009700090325      * - tab. GPP = Gestione Pratiche perticolari per alcuni clienti
009800090325     d dGPP          e ds                  inz
009900060314      * - tab. TB  = Tibi Bolla
010000060314     d dsTB          e ds                  inz
010100060314      *
010200060314      * - Parametri x Ricerca/Controllo fasi C.A.
010300060314     d FIDN05ds      e ds                  inz
010400060314     d ds_FNDFA      e ds                  inz extname(FNDFA00F)
010500060314      *
010600060314      * - Parametri x Reperimento terminal di arrivo/partenza
010700060314     d FNLV55ds      e ds                  inz
010800060314     d   D55tla      e                     inz('L')
010900060314     d   D55tpt      e                     inz('P')
011000060314      *
011100060314      * - Parametri x Controllo validità numero fax
011200060314     d TRUL42ds      e ds                  inz
011300060314      *
011400060314      * - Parametri x Reperimento USRDFNDTA x comunicazione e-mail
011500060314     d TRUL44ds      e ds                  inz                                  X Invio e-mail
011600060314      * - Parametri x Ridefinizione dati utente estesi per mailing PDF
011700060314     d TRTCM1ds      e ds                  inz                                  X Invio e-mail
011800060315      *
011900060315      *
012000060315     d Status         sds
012100060315     d  SDSpgm           *proc
012200060314      *
012300060314      * - Parametri x Controllo/Inversione date
012400060314     d WLBDAT          ds                  inz
012500060314     d  G02dat                 1      8  0 inz
012600060314     d  G02inv                 9     16  0 inz
012700060315     d  G02err                17     17    inz('3')
012800060314     d  G02tgi                18     22  0 inz
012900060314      *
013000060314      * Ridefinizione sottoconto
013100060314     d wDS_ksc         ds             7    inz
013200060315     d  w0030                         3  0 inz
013300060315     d  w0040                         4  0 inz
013400060314      *
013500060314      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *?
013600060314      *
013700060314      * - Flag
013800060314     d $AE1            s              1    inz(*off)
013900060314     d $E_Mail         s              1    inz(*off)
014000060320     d $English        s              1    inz(*off)
014100060320     ***d wCLSflo7        s              1    inz
014200060314      * - Indici di schiera
014300060315     d XX              s              3  0 inz
014400060314     d YY              s              3  0 inz
014500060314     d JJ              s              3  0 inz
014600060314      * - Campi di comodo x invio e-mail
014700060314     d $Invio          s              1    inz
014800060314     d W_mail          s             60    inz
014900060314     d KSC_mail        s                   inz like(D44ksc)
015000060314      * - Parametri x TRULOUTQ
015100060314     d PrmFine         s              1    inz
015200060314     d PrmFil          s              3s 0 inz
015300060314     d PrmTpOutQ       s              1    inz
015400060314     d PrmOutQ         s             10    inz
015500060314     d PrmOutQLib      s             10    inz
015600060314     d PrmEsito        s              1    inz
015700060314      * - Campi x comandi di sistema
015800060314     d Qcmd            s            500    inz
015900060314     d Qlen            s             15  5 inz(500)
016000060314      * - Campi di comodo
016100060314     d Wtad            s                   like(DCTtad)   inz
016200081218     d Kcli            s                   like(dctksc)   inz
016300060314     d w§DCTlnpc       s                   like(DCTlnp)   inz
016400060314     d w§DCTport       s                   like(§DCTport) inz
016500060314     d w§TLDstp        s                   like(§TLDstp1) inz
016600060315      * - Campi per gestione date
016700060315     d wTime           s              6  0
016800060315     d wDate_AMG       s              8  0
016900060315     d wDate_EUR       s               d   datfmt(*eur)
017000060315     d wDta_EUR        s               d   datfmt(*eur)   inz(*loval)
017100060314
017200060314      *---------------------------------------------------------------*
017300060314      *?FASI C.A. FISSATE A PROGRAMMA                                ?*
017400060314      *---------------------------------------------------------------*
017500060314      *---------------------------------------------------------------*
017600060314
017700060314      *---------------------------------------------------------------*
017800060314      *?RIEPILOGO INDICATORI                                         ?*
017900060314      *---------------------------------------------------------------*
018000060316      * 01   - OverFlow in PrtF FIDN02P  (non gestito)                *
018100060316      * 02   - OverFlow in Prtf FIDN02PM (non gestibile x e-mail)     *
018200060314      * 30   - Comodo                                                 *
018300060315      * 41   - Stampa numero di FAX per allegato 1                    *
018400060314      * 42   - Stampa 2ª riga dettaglio anomalia                      *
018500060314      * 43   - Stampa quantità danneggiate                            *
018600060314      * 44   - Stampa 2a riga descrizione pezzi danneggiati/mancanti  *
018700060314      * 47   - Stampa riferimento mittente ALFAnumerico               *
018800060314      * 48   - Stampa numero evento                                   *
018900090325      * 49   - Evento fortuito                                        *
019000080114      * 50   - Tipo anomalia mancanza                                 *
019100080114      * 51   - Tipo anomalia avaria/ammanco senza reso                *
019200080114      * 52   - Tipo avaria con reso                                   *
019300060314      *---------------------------------------------------------------*
019400981203
019500060314     c     *Entry        plist
019600060314     c                   parm                    KPJBA
019700060314     c                   movel     KPJBU         FIDN00ds
019800060314
019900981204      * Aggancio anagrafico fasi per tipi testo
020000060314     c     I00fca        chain     FNDFA000
020100060314
020200060314      * Memorizzo in schiera i tipi testo da elaborare
020300060314     c                   eval      yy = 1
020400060314     c     DFAle1        lookup    $CoTe(yy)                              30
020500060314     c   30              movel     DFAle1        $TeEl(1)
020600060314     c                   eval      yy = 1
020700060314     c     DFAle2        lookup    $CoTe(yy)                              30
020800060314     c   30              movel     DFAle2        $TeEl(2)
020900060314
021000060314      * Se non esistono testi validi non elaboro
021100060314if  1c                   if             $TeEl(1) = *blanks
021200060314     c                             and  $TeEl(2) = *blanks
021300060314     c                   goto      FINE
021400060314e   1c                   endif
021500060314
021600980929      * Aggancio file bolle
021700060314     c                   if        I00tpb    = 'P'
021800060314     c     K04BLPARB     chain     FNBLP000
021900060314     c                   else
022000060314     c     K04BLPARB     chain     FNARB000
022100060314     c                   endif
022200060314
022300981203      *  Aggancio testata C.A.
022400060314     c     K03DCT01      chain     FNDCT000
022500060314     c                   movel     DCTflo        dDCT01
022600060314     c                   movel     §DCTlnpc      w§DCTlnpc
022700060314
022800060314      * Elaboro i tipi testo - - - - - - - - - - - - - - - - - - - - -*
022900060314do  1c     1             DO        2             jj
023000060314
023100060314if  2c                   if        $TeEl(jj)  = *blanks
023200060314     c                   iter
023300060314e   2c                   endif
023400060314
023500060314      * Pulizia dei campi di stampa
023600060314     c                   exsr      Inz_PrtF
023700060316
023800060316      * Se il testo da stampare è "AL1" e la spedizione è un PF Import
023900060316      *   EEX o EUP stampo "AE1"
024000060316if  2c                   if             $TeEl(jj)  =  'AL1'
024100060316     c                             and  §DCTport   =  'F'
024200060316     c                             and  §DCTtisp   =  'I'
024300060316     c                             and (§DCTeuro   =  'X'
024400060316     c                              or  §DCTeuro   =  'P')
024500060316     c                   eval      $TeEl(jj) = 'AE1'
024600060316     c                   eval      $AE1      = *on
024700060316e   2c                   endif
024800000222
024900060316      * Reperimento dati del destinatario della lettera:
025000060316      * - decodifica il cliente
025100060316      * - verifica se richiesto il testo in Inglese
025200060316      * - verifica se richiesta e-mail piuttosto che lettera
025300060314     c                   exsr      Decod_CLI
025400060316
025500060317      * Aggancio tabella TLD = Parametri di stampa Testo Lettera Danni
025600060316     c                   exsr      Tab_TLD
025700060315
025800060316      * Apertura file stampa & impostazione Tipo Modulo e Stampante
025900060315     c                   exsr      sr_OVERRIDE
026000060314
026100060314      * Stampa lettera
026200060314     c                   exsr      sr_Stampa
026300060314
026400060314e   1c                   ENDDO
026500060314
026600060314      * Aggiorno data base - - - - - - - - - - - - - - - - - - - - - -*
026700060315     c                   if        KNMUS    <> '1111111111'
026800060314     c                   exsr      Wrt_DCF                                      Dettaglio fase
026900060314     c                   exsr      Upd_DCT                                      Testata fase
027000060315     c                   endif
027100060314
027200060314      * Fine - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
027300060314     c                   clear                   TIBS02ds
027400060314     c                   movel     'C'           T02tla
027500981029     c                   call      'TIBS02R'
027600060314     c                   parm                    KPJBA
027700060314     c                   parm                    TIBS02ds
027800060314      *
027900060314      *
028000060314     c                   call      'TRULOUTQ'
028100060314     c                   parm      'L'           PrmFine
028200060314     c                   parm      *zeros        PrmFil
028300060314     c                   parm      *blanks       PrmTpOutQ
028400060314     c                   parm      *blanks       PrmOutQ
028500060314     c                   parm      *blanks       PrmOutQLib
028600060314     c                   parm      *blanks       PrmEsito
028700060314
028800060314     c     FINE          tag
028900060315     c                   eval      *inLR     = *on
029000060314
029100060314      *---------------------------------------------------------------*
029200060314      *?Routine iniziale                                             ?*
029300060314      *---------------------------------------------------------------*
029400060314     c     *InzSr        BEGSR
029500060314      *
029600060314     c                   exsr      DatiJob
029700060315      *
029800060315     c                   time                    wTime
029900060315     c                   movel     *date         wDate_EUR
030000060314      *
030100060315      * *Date in formato aaaa/mm/gg
030200060314     c                   clear                   WLBdat
030300060314     c                   eval      G02dat = *date
030400060314     c                   call      'XSRDA8'
030500060314     c                   parm                    WLBDAT
030600060314     c                   z-add     G02inv        wDate_AMG
030700060314      *
030800060314      * Definizione Key-List
030900060314     c     K03DCT01      klist
031000060314     c                   kfld                    I00aac
031100060314     c                   kfld                    I00fil
031200060314     c                   kfld                    I00nca
031300060314      *
031400060314     c     K02DET01      klist
031500060314     c                   kfld                    DCTaae
031600060314     c                   kfld                    DCTnev
031700060314      *
031800060314     c     K04DKA01      klist
031900060314     c                   kfld                    I00aac
032000060314     c                   kfld                    I00fil
032100060314     c                   kfld                    I00nca
032200060314     c                   kfld                    DKAtrc
032300060314      *
032400060314     c     K03DCS01      klist
032500060314     c                   kfld                    DCStpd
032600060314     c                   kfld                    DCSnkt
032700060314     c                   kfld                    DCSstd
032800060314     c     K07DCS01      klist
032900060314     c                   kfld                    DCStpd
033000060314     c                   kfld                    DCSnkt
033100060314     c                   kfld                    DCSstd
033200060314     c                   kfld                    DCSdim
033300060314     c                   kfld                    DCShim
033400060314     c                   kfld                    DCSnks
033500060314     c                   kfld                    DCStrc
033600060314      *
033700060314     c     K04BLPARB     klist
033800060314     c                   kfld                    I00aas
033900060314     c                   kfld                    I00lnp
034000060314     c                   kfld                    I00nrs
034100060314     c                   kfld                    I00nsp
034200060314      *
034300060314      * Inizializzazione campi chiave
034400060314     c                   eval      DCStpd = 'C'
034500060314      *
034600060314     c                   ENDSR
034700060314
034800060314      *---------------------------------------------------------------*
034900060314      *?Reperimento Dati del job (Utente/Operativi)                  ?*
035000060314      *---------------------------------------------------------------*
035100060314     c     DatiJob       BEGSR
035200060314      *
035300060314     c     *dtaara       define    §azute        AZUTEds
035400060314     c     *dtaara       define    §datiute      dDatiUte
035500060314      *
035600060314     c                   in(E)     *dtaara
035700060314     c                   IF        %ERROR or RSUT = *blanks
035800060314     c                   clear                   Tibs34Ds
035900060314     c                   call      'TIBS34R'
036000060314     c                   parm                    Tibs34Ds
036100060314     c                   in        *dtaara
036200060314     c                   ENDIF
036300060314      *
036400060314     c                   ENDSR
036500060314
036600060314      *---------------------------------------------------------------*
036700060314      *?Pulizia dei campi in stampa                                  ?*
036800060314      *---------------------------------------------------------------*
036900060314     c     Inz_PrtF      BEGSR
037000060314      *
037100060316     c                   clear                   DN02T01                        Intestazione+Oggetto
037200060316     c                   clear                   DN02N00                        Note
037300060320     c*** senza campi:   clear                   DN02AL51A                      Reso per Avaria
037400060320     c*** senza campi:   clear                   DN02AL51B                      Reso per Avaria
037500060320     c*** senza campi:   clear                   DN02AL11A                      Preavv.Danno NO even
037600060320     c*** senza campi:   clear                   DN02AL11B                      Preavv.Danno NO even
037700060320     c*** senza campi:   clear                   DN02AL13A                      Preavv.Danno SI even
037800060320     c*** senza campi:   clear                   DN02AL13B                      Preavv.Danno SI even
037900060320     c*** senza campi:   clear                   DN02AE11A                      Preavv.Danno Estero
038000060320     c*** senza campi:   clear                   DN02AE11B                      Preavv.Danno Estero
038100060316     c                   clear                   DN02P01                        Piede
038200060316     c                   clear                   DN02N01                        Note finali
038300060316      * > testi x estero
038400060316     c*** stessi campi:  clear                   DN02T02                        Intestazione+Oggetto
038500060320     c*** stessi campi:  clear                   DN02AL52A                      Reso per Avaria
038600060320     c*** stessi campi:  clear                   DN02AL52B                      Reso per Avaria
038700060320     c*** stessi campi:  clear                   DN02AL12A                      Preavv.Danno NO even
038800060320     c*** stessi campi:  clear                   DN02AL12B                      Preavv.Danno NO even
038900060320     c*** stessi campi:  clear                   DN02AL14A                      Preavv.Danno SI even
039000060320     c*** stessi campi:  clear                   DN02AL14B                      Preavv.Danno SI even
039100060320     c*** senza campi:   clear                   DN02AD12A                      Preavv.Danno Est.DPD
039200060320     c*** senza campi:   clear                   DN02AD12B                      Preavv.Danno Est.DPD
039300060320     c*** senza campi:   clear                   DN02AM12A                      Preavv.Danno Man.DPD
039400060320     c*** senza campi:   clear                   DN02AM12B                      Preavv.Danno Man.DPD
039500060316     c*** stessi campi:  clear                   DN02P02                        Piede
039600060316     c*** stessi campi:  clear                   DN02N02                        Note finali
039700060316      * > e-mail
039800060316     c*** stessi campi:  clear                   DN02T01M                       Intestazione+Oggetto
039900060316     c*** stessi campi:  clear                   DN02N00M                       Note
040000060316     c*** stessi campi:  clear                   DN02AL5M                       Reso per Avaria
040100060316     c*** stessi campi:  clear                   DN02AL1M                       Preavv.Danno NO even
040200060316     c*** stessi campi:  clear                   DN02AL1EM                      Preavv.Danno SI even
040300060316     c*** stessi campi:  clear                   DN02AE1M                       Preavv.Danno Estero
040400060316     c*** stessi campi:  clear                   DN02AD1M                       Preavv.Danno DPD
040500060316     c*** stessi campi:  clear                   DN02AM1M                       Preavv.Danno DPD Man
040600060316     c*** stessi campi:  clear                   DN02P01M                       Piede
040700060316     c*** stessi campi:  clear                   DN02N01M                       Note finali
040800060316      * > e-mail x estero
040900060316     c*** stessi campi:  clear                   DN02T02M                       Intestazione+Oggetto
041000060316     c*** stessi campi:  clear                   DN02AL5BM                      Reso per Avaria
041100060316     c*** stessi campi:  clear                   DN02AL1BM                      Preavv.Danno NO even
041200060316     c*** stessi campi:  clear                   DN02AL1EBM                     Preavv.Danno SI even
041300060316     c*** stessi campi:  clear                   DN02P02M                       Piede
041400060316     c*** stessi campi:  clear                   DN02N02M                       Note finali
041500060314      *
041600060314     c                   ENDSR
041700060314
041800060314      *---------------------------------------------------------------*
041900060316      *?Decodifica cliente, verifica se testo inglese e/o e-mail     ?*
042000060314      *---------------------------------------------------------------*
042100060314     c     Decod_CLI     BEGSR
042200060314      *
042300081217      * recupero beneficiario dal pgm esterno
042400081217     c                   clear                   fidnbeds
042500081217      *
042600081217     c                   movel     'B'           IBEmod
042700081217     c                   z-add     dctaac        IBEaac
042800081217     c                   z-add     dctfil        IBEfil
042900081217     c                   z-add     dctnca        IBEnca
043000081217     c                   z-add     dctaas        IBEaas
043100081217     c                   z-add     dctlnp        IBElnp
043200081217     c                   z-add     dctnrs        IBEnrs
043300081217     c                   z-add     dctnsp        IBEnsp
043400081217     c                   z-add     dctksc        IBEksc
043500090529if  3 * se lettera "AE1" indirizzo la lettera al destinatario
043600090529     c                   if        $TeEl(jj) = 'AE1'
043700090529     c                   eval      IBEptr = 'D'
043800090529     c                   else
043900081217     c                   movel     dctptr        IBEptr
044000090529     c                   endif
044100081217     c                   movel     dctflo        IBEflo
044200081217     c                   movel     I00tpb        IBEtpb
044300081218     c                   If        %subst(knsif:7:1) = 'P'
044400081217     c                   eval      IBEsif = 'P'
044500081218     c                   EndIf
044600081217      *
044700081217     c                   call      'FIDNBER'
044800081217     c                   parm                    fidnbeds
044900081217      *
045000081217     c                   If        obeerr = ' '
045100081217     c                   movel     obeksc        wds_ksc
045200081217     c                   endif
045300060314      *
045400081217      * Verifico se intestatario della lettera è cliente DPD
045500081217if  1c                   IF        wds_ksc > *zeros
045600060314     c                   exsr      Ctr_DPD
045700081217     c                   endif
045800081217
045900060314      *
046000081217     c                   movel     oberag        PICrag
046100081217     c                   movel     obevia        PICind
046200081217     c                   movel     obecap        PICcap
046300081217     c                   movel     obeloc        PICloc
046400081217     c                   movel     obeprv        PICpro
046500081217     c                   movel     obenaz        PICnaz
046600081217     c                   movel     obefax        PIXfax
046700081217     c                   eval      $Invio   = obeinv
046800081217     c                   eval      W_Mail   = obeima
046900081217     c                   eval      Ksc_Mail = obekma
047000060314      *
047100060314     c                   ENDSR
047200060314
047300060314      *---------------------------------------------------------------*
047400060314      *?Verifica se intestatario della lettera è cliente DPD         ?*
047500060314      *---------------------------------------------------------------*
047600060314     c     Ctr_DPD       BEGSR
047700060314      *
047800060314     c                   clear                   Og143
047900060314      *
048000060314     c     w0030         chain     AZORG
048100060314      *
048200060314if  1c                   if        NOT %found(AZORG01L)
048300060314     c                   leavesr
048400060314e   1c                   endif
048500060314      *
048600060314      * Valorizzo la 143ª descrizione
048700060314     c                   movel     ORGde3        Og143
048800060314      *
048900060314if  1c                   if        §OGntw = 'DPD'
049000060314      * Valorizzo il testo in AD1
049100060315     c                   movel     'AD1'         $TeEl(jj)
049200060314     c                   eval      $AE1   = *off
049300060314e   1c                   endif
049400060314      *
049500060314     c                   ENDSR
049600060314
049700060314      *---------------------------------------------------------------*
049800060317      *?Reperimento parametri di stampa per testo lettera danni      ?*
049900060314      *---------------------------------------------------------------*
050000060314     c     Tab_TLD       BEGSR
050100060314      *
050200060314     c                   clear                   dTLD
050300060314      *
050400060314     c                   clear                   TIBS02ds
050500060314     c                   eval      T02mod = 'C'
050600060314     c                   eval      T02sif = KNSIF
050700060314     c                   eval      T02cod = 'TLD'
050800060314     c                   if            $TeEl(jj) = 'AE1'
050900060314     c                             or  $TeEl(jj) = 'AD1'
051000060314     c                             or  $TeEl(jj) = 'AM1'
051100060314     c                   eval      T02ke1 = 'AL1'
051200060314     c                   else
051300060314     c                   eval      T02ke1 = $TeEl(jj)
051400060314     c                   endif
051500060314      *
051600060314     c                   call      'TIBS02R'
051700060314     c                   parm                    KPJBA
051800060314     c                   parm                    TIBS02ds
051900060314      *
052000060314     c                   movel     T02uni        dTLD
052100060314      *
052200060314     c                   ENDSR
052300060314
052400060314      *---------------------------------------------------------------*
052500060314      *?Reperimento tab. TAD = Tipo Anomalia                         ?*
052600060314      *---------------------------------------------------------------*
052700060314     c     Tab_TAD       BEGSR
052800060314      *
052900060314     c                   clear                   dTAD
053000060314      *
053100060314     c                   clear                   TIBS02ds
053200060314     c                   eval      T02mod = 'C'
053300060314     c                   eval      T02sif = KNSIF
053400060314     c                   eval      T02cod = 'TAD'
053500060315     c                   movel(p)  Wtad          T02ke1
053600060314      *
053700060314     c                   call      'TIBS02R'
053800060314     c                   parm                    KPJBA
053900060314     c                   parm                    TIBS02ds
054000060314      *
054100060314     c                   movel     T02uni        dTAD
054200060314      *
054300060314     c                   ENDSR
054400060314
054500060314      *---------------------------------------------------------------*
054600060314      *?Reperimento Nominativo dell'interlocutore                    ?*
054700060314      *---------------------------------------------------------------*
054800060314     c     Rep_PerCon    BEGSR
054900060314      *
055000060314     c*** già così:      eval      DCStpd = 'C'
055100060314     c                   eval      DCSnkt = Wnumca
055200060314     c                   eval      DCSstd = 'N'
055300060314      *
055400060315     c     K03DCS01      chain     FNDCS000
055500060314      *
055600060315     c                   if            %found(FNDCS01L)
055700060314     c                             and DCSnot <> *blanks
055800060314     c                   eval      PICpec =  'C.A. '  +  DCSnot
055900060314     c                   endif
056000060314      *
056100060316     c                   ENDSR
056200060314
056300060314      *---------------------------------------------------------------*
056400060314      *?Reperimento descrizione anomalia                             ?*
056500060314      *---------------------------------------------------------------*
056600060314     c     Rep_DesTAD    BEGSR
056700060314      *
056800060314     c                   clear                   YY
056900060315     c                   clear                   $TAD
057000060314      *
057100060314     c*** già così:      eval      DCStpd = 'C'
057200060314     c                   movel(P)  Wnumca        DCSnkt
057300060314     c                   eval      DCSstd = 'A'
057400060314     c*** non in klist:  clear                   DCSnks
057500060314     c*** non in klist:  movel     'D'           DCStrc
057600060314      *
057700060314     c     K03DCS01      setll     FNDCS000
057800060314     c     K03DCS01      reade     FNDCS000
057900060314      *
058000060314do  1c                   dow       NOT  %eof(FNDCS01L)
058100060404     c                             and  YY <  %elem($TAD)
058200060314     c                   add       1             yy
058300060315     c                   movel     DCSnot        $TAD(yy)
058400060314     c     K03DCS01      reade     FNDCS000
058500060314e   1c                   enddo
058600060314      *
058700060315     c                   eval      POCda1 = $TAD(1)
058800060315     c                   eval      POCda2 = $TAD(2) + $TAD(3)
058900060314      *
059000060314     c                   ENDSR
059100060314
059200060314      *---------------------------------------------------------------*
059300060314      *?Reperimento descrizione pezzi danneggiati/mancanti           ?*
059400060314      *---------------------------------------------------------------*
059500060314     c     Rep_DesPZD    BEGSR
059600060314      *
059700060314     c                   clear                   YY
059800060315     c                   clear                   $TAD
059900060314      *
060000060314     c*** già così:      eval      DCStpd = 'C'
060100060314     c                   movel(p)  Wnumca        DCSnkt
060200060314     c                   eval      DCSstd = 'P'
060300060314     c*** non in klist:  clear                   DCSnks
060400060314     c*** non in klist:  movel     'D'           DCStrc
060500060314      *
060600060314     c     K03DCS01      setll     FNDCS000
060700060314     c     K03DCS01      reade     FNDCS000
060800060314      *
060900060314do  1c                   dow       NOT  %eof(FNDCS01L)
061000060404     c                             and  YY <  %elem($TAD)
061100060314      *
061200060314     c                   add       1             yy
061300060315     c                   movel     DCSnot        $TAD(yy)
061400060314     c     K03DCS01      reade     FNDCS000
061500060314     c                   enddo
061600060314      *
061700060315     c                   eval      POCdpz = $TAD(1)
061800060315     c                   eval      POCdp1 = $TAD(2) + $TAD(3)
061900060314      *
062000060314     c                   ENDSR
062100060314
062200060314      *---------------------------------------------------------------*
062300060314      *?Impostazione dei campi in stampa   &   Stampa                ?*
062400060314      *---------------------------------------------------------------*
062500060315     c     sr_Stampa     BEGSR
062600060316      *
062700060316      * Determina tipo di invio:
062800060316sel 1c                   select
062900060316      * - M = e-mail
063000060316w   1c                   when      $E_Mail   = *on
063100060316     c                   eval      $Invio    = 'M'
063200060316      * - F = fax
063300060316w   1c                   when            $E_Mail   =  *off
063400060316     c                             and  (PIXfax    <> *blanks
063500060316     c                             and   $TeEl(jj) <> 'AL5')
063600060316     c                   eval      $Invio    = 'F'
063700060316      * - L = lettera
063800060316w   1c***                when            $E_Mail   =  *off
063900060316     c***                          and  (PIXfax    =  *blanks
064000060316     c***                           or   $TeEl(jj) =  'AL5')
064100060316x   1c                   other
064200060316     c                   eval      $Invio    = 'L'
064300060316e   1c                   endsl
064400060314      *
064500060314      * Impostazione dei campi in stampa
064600060314     c                   exsr      sr_INTES                                      intestazione
064700060314     c                   exsr      sr_OGGE                                       oggetto
064800060315     c                   exsr      sr_NOTE                                       note
064900060314     c                   exsr      sr_TESTO                                      testo
065000060314     c                   exsr      sr_FIRMA                                      firma
065100060315      *
065200060315      * Impostazione indicatori utilizzati nel PrtF
065300060316     c                   eval      *in41  = (PIXfax    <> *blanks  and
065400060316     c                                       $TeEl(jj) <> 'AL5')
065500060315     c                   eval      *in42  = (POCda2 <> *blanks)
065600060315     c                   eval      *in43  = (POCqtd <> *blanks)
065700080903     c                   eval      *in46  = (POCdp1 <> *blanks)
065800060315     c                   eval      *in47  = (ARBrma <> *blanks)
065900060315     c                   eval      *in48  = (DCTnev    >  *zeros   and
066000060315     c                                       $TeEl(jj) <> 'AL5')
066100060314      *
066200060314      * Stampa:
066300060314sel 1c                   SELECT
066400060314      *
066500060315      * Lettera in Italiano  - - - - - - - - - - - - - - - - - - - - -*
066600060314w   1c                   WHEN           $E_Mail   = *off
066700060314     c                             and  $English  = *off
066800060316      * - Intestazione + Oggetto
066900060314     c                   write     DN02T01
067000060314      * - Testo
067100060320     c                   exsr      sr_PrtTEXT
067200060320      * - Note (eventuali)
067300060320     c                   exsr      sr_PrtNOTE
067400060314      * - Piede
067500060320     c                   exsr      sr_PrtTEXT2
067600060314     c                   write     DN02P01
067700060320      * - Note finali
067800060320      *   ("Per ricevere le prossime comunicazioni via e-mail...")
067900060320      *   SE è un preavviso di danno
068000060320if  2c                   if        $TeEl(jj) <> 'AL5'
068100060320     c                   write     DN02N01
068200060320e   2c                   endif
068300060314      *
068400060315      * Lettera in Inglese - - - - - - - - - - - - - - - - - - - - - -*
068500060314w   1c                   WHEN           $E_Mail   = *off
068600060314     c                             and  $English  = *on
068700060316      * - Intestazione + Oggetto
068800060314     c                   write     DN02T02
068900060314      * - Testo
069000060320     c                   exsr      sr_PrtTEXT
069100060320      * - Note (eventuali)
069200060320     c                   exsr      sr_PrtNOTE
069300060314      * - Piede
069400060320     c                   exsr      sr_PrtTEXT2
069500060314     c                   write     DN02P02
069600060320      * - Note finali
069700060320      *   ("Per ricevere le prossime comunicazioni via e-mail...")
069800060320      *   SE è un preavviso di danno
069900060320if  2c                   if        $TeEl(jj) <> 'AL5'
070000060320     c                   write     DN02N02
070100060320e   2c                   endif
070200060314      *
070300060315      * e-mail in Italiano - - - - - - - - - - - - - - - - - - - - - -*
070400060314w   1c                   WHEN           $E_Mail   = *on
070500060314     c                             and  $English  = *off
070600060316      * - Intestazione + Oggetto
070700060315     c                   write     DN02T01M
070800060315      * - Testo
070900060320     c                   exsr      sr_PrtTEXT
071000060320      * - Note (eventuali)
071100060320     c                   exsr      sr_PrtNOTE
071200060315      * - Piede
071300060320     c                   exsr      sr_PrtTEXT2
071400060315     c                   write     DN02P01M
071500060314      *
071600060315      * e-mail in Inglese  - - - - - - - - - - - - - - - - - - - - - -*
071700060314w   1c                   WHEN           $E_Mail   = *on
071800060315     c                             and  $English  = *on
071900060316      * - Intestazione + Oggetto
072000060315     c                   write     DN02T02M
072100060315      * - Testo
072200060320     c                   exsr      sr_PrtTEXT
072300060320      * - Note (eventuali)
072400060320     c                   exsr      sr_PrtNOTE
072500060315      * - Piede
072600060320     c                   exsr      sr_PrtTEXT2
072700060315     c                   write     DN02P02M
072800060314      *
072900060314e   1c                   ENDSL
073000060314      *
073100060314     c                   ENDSR
073200060314
073300060315      *---------------------------------------------------------------*
073400060315      *?Impostazione del tipo-modulo e della stampante               ?*
073500060315      *---------------------------------------------------------------*
073600060315     c     sr_OVERRIDE   BEGSR
073700060320      *
073800060320      * Dati del P.O. (se reso per avaria utilizzo il P.O. di partenza)
073900060320if  1c                   if        $TeEl(jj) = 'AL5'
074000060320     c     w§DCTlnpc     chain     AZORG
074100060320x   1c                   else
074200060320     c     I00fgs        chain     AZORG
074300060320e   1c                   endif
074400060314      *
074500060314     c                   clear                   Qcmd
074600060314      *
074700060315if  1c                   IF             $TeEl(jj) <> 'AL5'
074800060315     c                             and  PIXfax    <> *blanks
074900060315      *
075000060315      * Controllo N° FAX
075100060315     c                   clear                   TRUL42ds
075200060315     c                   eval      D42fax  = PIXfax
075300060315     c                   call      'TRUL42R'
075400060315     c                   parm                    TRUL42ds
075500060315if  2c                   IF        D42err <> *blanks
075600060315      *
075700060315     c                   clear                   PIXfax
075800060315      *
075900060315x   2c                   ELSE
076000060315      *
076100060315     c                   movel     §TLDfaxi      PIXurg
076200060315      * - Cerco il nome della coda del fax
076300060315     c                   call      'TRULOUTQ'
076400060315     c                   parm                    PrmFine
076500060315     c                   parm      DUTpou        PrmFil
076600060315     c                   parm      '2'           PrmTpOutQ
076700060315     c                   parm      *blanks       PrmOutQ
076800060315     c                   parm      *blanks       PrmOutQLib
076900060315     c                   parm      *blanks       PrmEsito
077000060315if  3c                   if             PrmEsito = *off
077100060315     c                             and  PrmOutq <> *blanks
077200060315     c                   movel(p)  PrmOutq       w§TLDstp
077300060315x   3c                   else
077400060315     c                   movel(p)  '*SAME     '  w§TLDstp
077500060315e   3c                   endif
077600060315      *
077700060315e   2c                   ENDIF
077800060315      *
077900060315e   1c                   ENDIF
078000060314      *
078100060315      *
078200060315      * Imposto l'override da eseguire
078300060315sel 1c                   SELECT
078400060314      *
078500060315w   1c                   WHEN           $TeEl(jj) <> 'AL5'
078600060315     c                             and  $Invio     = 'M'
078700060315     c                             and  W_Mail    <> *blanks
078800060315     c                   clear                   TRUL44ds
078900060315     c                   eval      D44pgm    = SDSpgm
079000060315     c                   eval      D44po     = ORGfil
079100060315     c                   eval      D44Ksc    = Ksc_Mail
079200060315     c                   eval      D44Des    = W_Mail
079300060315     c                   eval      D44scope  = '*CALLLVL'
079400060315     c                   eval      D44prtf   = 'FIDN02P'
079500060320if  2c***                IF        wCLSflo7 <> *blanks
079600060320if  2c                   IF        $English  = *on
079700060315     c                   eval      D44obj    = '*OBJM*'
079800060315     c                                       + 'Damage comunication N. '
079900060315     c                                       + %char(DCTfil)
080000060315     c                                       + '/'
080100060315     c                                       + %trim(
080200060315     c                                           %editc( DCTnca:'Z' ))
080300060315     c                                       + ' year '
080400060315     c                                       + %char(DCTaac)
080500060315x   2c                   ELSE
080600060315     c                   eval      D44obj    = '*OBJM*'
080700060315     c                                       + 'Comunicazione di presunta +
080800060315     c                                          anomalia N.'
080900060315     c                                       + %char(DCTfil)
081000060315     c                                       + '/'
081100060315     c                                       + %trim(
081200060315     c                                           %editc( DCTnca:'Z' ))
081300060315     c                                       + ' anno '
081400060315     c                                       + %char(DCTaac)
081500060315e   2c                   ENDIF
081600060314     c                   call      'TRUL44R'
081700060315     c                   parm                    TRUL44ds
081800060315     c                   parm                    TRTCM1ds
081900060315      * - se c'è stato qualche problema faccio invio in altro modo
082000060315if  2c                   IF        D44err  <> *blanks
082100060315     c                   clear                   $Invio
082200060315x   2c                   ELSE
082300060315     c                   eval      $E_Mail  = *on
082400060315      * - - ovrprtf
082500060315     c                   eval      Qcmd     = $Cmd(9)
082600060315     c                   eval      %subst(Qcmd:23:10) = D44outq
082700060315     c                   eval      Qcmd     = %trim(Qcmd)
082800060315     c                                      + D44dta
082900060315     c                                      + ''')'
083000060315     c                   call      'QCMDEXC'                            30
083100060314     c                   parm                    Qcmd
083200060314     c                   parm                    Qlen
083300060315      * - - errore
083400060315if  3c                   if        *in30
083500060315     c                   clear                   $Invio
083600060315     c                   eval      $E_Mail  = *off
083700060315e   3c                   endif
083800060315e   2c                   ENDIF
083900060314      *
084000060315w   1c                   WHEN           $TeEl(jj) =  'AL5'
084100060315     c                             and  §TLDtmod  <> *blanks
084200060315     c                             and  w§TLDstp  <> *blanks
084300060315     c                   eval      Qcmd     = $Cmd(1)
084400060315     c                   eval      %subst(Qcmd:39:10) = w§TLDstp
084500060315     c                   eval      %subst(Qcmd:60:10) = §TLDtmod
084600060314      *
084700060315w   1c                   WHEN           $TeEl(jj) =  'AL5'
084800060315     c                             and  §TLDtmod  <> *blanks
084900060315     c                             and  w§TLDstp  =  *blanks
085000060315     c                   eval      Qcmd     = $Cmd(2)
085100060315     c                   eval      %subst(Qcmd:60:10) = §TLDtmod
085200060314      *
085300060315w   1c                   WHEN           $TeEl(jj) =  'AL5'
085400060315     c                             and  §TLDtmod  =  *blanks
085500060315     c                             and  w§TLDstp <> *blanks
085600060315     c                   eval      Qcmd     = $Cmd(3)
085700060315     c                   eval      %subst(Qcmd:39:10) = w§TLDstp
085800060314      *
085900060315w   1c                   WHEN           $TeEl(jj) <> 'AL5'
086000060315     c                             and  §TLDtmod  <> *blanks
086100060315     c                             and  w§TLDstp  <> *blanks
086200060315     c                             and  $Invio    =  *blanks
086300060315     c                   eval      Qcmd     = $Cmd(4)
086400060314     c                   eval      %subst(Qcmd:39:10) = w§TLDstp
086500060314     c                   eval      %subst(Qcmd:60:10) = §TLDtmod
086600060314      *
086700060315w   1c                   WHEN           $TeEl(jj) <> 'AL5'
086800060315     c                             and  §TLDtmod  <> *blanks
086900060315     c                             and  w§TLDstp  =  *blanks
087000060315     c                             and  $Invio    =  *blanks
087100060315     c                   eval      Qcmd     = $Cmd(5)
087200060315     c                   eval      %subst(Qcmd:60:10) = §TLDtmod
087300060314      *
087400060315w   1c                   WHEN           $TeEl(jj) <> 'AL5'
087500060315     c                             and  §TLDtmod  =  *blanks
087600060315     c                             and  w§TLDstp  <> *blanks
087700060315     c                             and  $Invio    =  *blanks
087800060315     c                   eval      Qcmd     = $Cmd(6)
087900060314     c                   eval      %subst(Qcmd:39:10) = w§TLDstp
088000060314      *
088100060315w   1c                   WHEN           $TeEl(jj) <> 'AL5'
088200060315     c                             and  $Invio    =  *blanks
088300060315     c                   eval      Qcmd     = $Cmd(7)
088400060314      *
088500060315x   1c                   OTHER
088600060315     c                   eval      Qcmd     = $Cmd(8)
088700060314      *
088800060315e   1c                   ENDSL
088900060314      *
089000060314      * Se ho impostato qualcosa eseguo l'OVRPRTF
089100060315if  1c                   if             Qcmd      <> *blanks
089200060315     c                             and  $Invio    =  *blanks
089300060314     c                   call      'QCMDEXC'
089400060314     c                   parm                    Qcmd
089500060314     c                   parm                    Qlen
089600060315e   1c                   endif
089700060314      *
089800060314      *  Apro file di stampa
089900060315if  1c                   if        $E_Mail = *off
090000060314     c                   open      FIDN02P
090100060315x   1c                   else
090200060314     c                   open      FIDN02PM
090300060315e   1c                   endif
090400060314      *
090500060316     c                   ENDSR
090600060314
090700060314      *---------------------------------------------------------------*
090800060314      *?Impostazione dati dell'intestazione                          ?*
090900060314      *---------------------------------------------------------------*
091000060314     c     sr_INTES      BEGSR
091100060314      *
091200060314      * Se P.O. che scrive è ESTERO forzo "Uff. Internazionale" come
091300060314      *   descrizione intestazione + dati da AZORG del terminal di
091400060314      *   partenza della linea di partenza della bolla.
091500060314if  1c                   if             ORGfl1 = 'E'
091600060314     c                             and  ORGfva = *blanks
091700060314      *
091800060315     c                   movel(p)  I_PICpod      PICpod
091900060314      *
092000060314     c                   reset                   FNLV55ds
092100060314     c                   eval      D55lin   = ORGfil
092200060314     c                   move      wDate_AMG     D55drf
092300060314     c                   call      'FNLV55R'
092400060314     c                   parm                    FNLV55ds
092500060314if  2c                   if        D55err   = *blanks
092600060314     c     D55tfp        chain     AZORG
092700060314     c                   movel     ORGind        PICpoi
092800060314     c                   movel     ORGcpf        PICpoc
092900060314     c                   movel     ORGloc        PICpol
093000060314     c                   movel     ORGpro        PICpop
093100060314     c                   movel     ORGtel        PICpot
093200060314     c                   movel     ORGfax        PICpof
093300060314e   2c                   endif
093400060314      *
093500060314x   1c                   ELSE
093600060314      *
093700060314     c                   movel     ORGdes        PICpod
093800060314     c                   movel     ORGind        PICpoi
093900060314     c                   movel     ORGcpf        PICpoc
094000060314     c                   movel     ORGloc        PICpol
094100060314     c                   movel     ORGpro        PICpop
094200060314     c                   movel     ORGtel        PICpot
094300060314     c                   movel     ORGfax        PICpof
094400060314      *
094500060314e   1c                   ENDIF
094600060314      *
094700060314      * Imposto stampante
094800060314     c                   movel     §TLDstp1      w§TLDstp
094900060314      *
095000060314      * Verifico se devo stampare in Inglese
095100060314     c                   reset                   $English
095200081217     c                   If        obeING = *on
095300060314     c                   eval      $English = *on
095400060314e   1c                   endif
095500060314      *
095600060314      * Recupero l'interlocutore non per la lettera reso avaria
095700060314     c                   exsr      Rep_PerCon
095800060314      *
095900060314      * Località + data
096000060314if  1c                   if             ORGfl1 = 'E'
096100060314     c                             and  ORGfva = *blanks
096200060314     c                   eval      PICdal   = 'MILANO, '
096300060314     c                                      + %editc(*date:'Y')
096400060314x   1c                   else
096500060314     c                   eval      PICdal   = %trim(ORGdes) + ', '
096600060314     c                                      + %editc(*date:'Y')
096700060314e   1c                   endif
096800060314      *
096900060314     c                   ENDSR
097000060314
097100060314      *---------------------------------------------------------------*
097200060314      *?Impostazione dati dell'oggetto                               ?*
097300060314      *---------------------------------------------------------------*
097400060314     c     sr_OGGE       BEGSR
097500060314      *
097600060314     c                   eval      POCnca  = %trim(
097700060314     c                                         %editw( I00fil:'0   ' ))
097800060314     c                                     + '/'
097900060314     c                                     + %trim(
098000060314     c                                         %editc( I00nca:'Z' ))
098100060314     c                   eval      POCaac  = I00aac
098200060314      *
098300060314     c                   z-add     I00lnp        POClnp
098400060314     c                   z-add     I00nrs        POCnrs
098500060314     c                   z-add     I00nsp        POCnsp
098600060426     c                   clear                   WLBdat
098700060426     c                   eval      G02inv  = (I00aas * 10000) + ARBmgs
098800060426     c                   eval      G02err  = '3'
098900060315     c                   call      'XSRDA8'
099000060315     c                   parm                    WLBDAT
099100060426     c                   z-add     G02dat        POCdat
099200060314     c                   movel     ARBrsm        POCrsm
099300060314     c                   z-add     ARBrmn        POCrmn
099400060314     c                   movel     ARBrma        POCrma
099500060314     c                   movel     ARBrsd        POCrsd
099600060314     c                   movel     ARBlod        POClod
099700060314     c                   if        ARBnzd <> *blanks
099800060314     c                   movel     ARBnzd        POCprn
099900060314     c                   else
100000060314     c                   movel     ARBprd        POCprn
100100060314     c                   endif
100200060314     c                   movel     ARBlod        POClod
100300060314     c                   z-add     ARBncl        POCncl
100400060314     c                   z-add     ARBpkf        POCpkf
100500060314      *
100600060314      * Evento
100700060314if  1c                   IF             DCTnev     > *zeros
100800060315     c                             and  $TeEl(jj) <> 'AL5'
100900060314     c                   eval      *in48   = *on
101000060314     c                   eval      POCkev  = %trim(
101100060314     c                                         %editw( DCTaae:'0    ' ))
101200060314     c                                     + '/'
101300060314     c                                     + %trim(
101400060314     c                                         %editc( DCTnev:'Z' ))
101500060314     c     K02DET01      chain     FNDET000
101600060314     c                   eval      Wtad    = DETtad
101700060314     c                   exsr      Tab_TAD
101800090325      * verifico se evento fortuito
101900090325     c                   eval      *in49 = (§tadevfo = 'S')
102000090325
102100060314sel 2c                   select
102200060320w   2c***                when           wCLSflo7 <> *blanks
102300060320w   2c                   when           $English =  *on
102400060314     c                             and  §TADdesi <> *blanks
102500060314     c                   movel     §TADdesi      POCtae
102600060314w   2c                   when      §TADdest <> *blanks
102700060314     c                   movel     §TADdest      POCtae
102800060314x   2c                   other
102900060314     c                   movel     §TADdesc      POCtae
103000060314e   2c                   endsl
103100060314e   1c                   ENDIF
103200060314      *
103300060314      * Decodifico tipo anomalia
103400060314     c                   eval      Wtad    = I00tad
103500060314     c                   exsr      Tab_TAD
103600080114      * verifico il tipo anomalia per scegliere il tipo di frase da stampare
103700080114     c                   eval      *in50 =  §tadragr = 'M'                      mancanza
103800080115     c                   eval      *in51 =  §tadragr = 'A' or                   ammanco
103900080115     c                                     (§tadragr = 'V' and dctdsd <>'R')    avaria no reso
104000080115     c                   eval      *in52 =  §tadragr = 'V' and dctdsd = 'R'     avaria con reso
104100080114      * se tipo anomalia mancanza imposto smarrimanto altrimenti avaria nel testo
104200080114     c                   clear                   prttad
104300080114     c                   if        §tadragr = 'M'
104400080114     c                   if        $English = *off
104500080114     c                   eval      prttad = 'smarrimento.'
104600080114     c                   else
104700080114     c                   eval      prttad = 'is missing.'
104800080114     c                   endif
104900080114     c                   else
105000080114     c                   if        $English = *off
105100080114     c                   eval      prttad = 'avaria.'
105200080114     c                   else
105300080114     c                   eval      prttad = 'was damaged.'
105400080114     c                   endif
105500080114     c                   endif
105600080114      *
105700060314sel 1c                   select
105800060320w   1c***                when           wCLSflo7 <> *blanks
105900060320w   1c                   when           $English =  *on
106000060314     c                             and  §TADdesi <> *blanks
106100060314     c                   movel     §TADdesi      POCtad
106200060314w   1c                   when      §TADdest <> *blanks
106300060314     c                   movel     §TADdest      POCtad
106400060314x   1c                   other
106500060314     c                   movel     §TADdesc      POCtad
106600060314e   1c                   endsl
106700060314      *
106800060314      * Descrizione anomalia
106900060314     c                   exsr      Rep_DesTAD
107000060314      *
107100060314      * Quantità danneggiate
107200060314      * - Peso
107300060314if  1c                   if        DCTpkd  > *zeros
107400060314     c                   eval      POCqtd  = 'Kg '
107500060314     c                                     + %trim(
107600060314     c                                         %editc( DCTpkd:'L' ))
107700060314e   1c                   endif
107800060314      * - Colli
107900060314if  1c                   if        DCTncn  > *zeros
108000060320if  2c***                if        wCLSflo7  =  *blanks
108100060320if  2c                   if        $English  =  *off
108200060314     c                   eval      POCqtd  = %trim(POCqtd)
108300060314     c                                     + ' Colli '
108400060314     c                                     + %trim(
108500060314     c                                         %editc( DCTncn:'Z' ))
108600060314x   2c                   else
108700060314     c                   eval      POCqtd  = %trim(POCqtd)
108800060314     c                                     + ' Parcels '
108900060314     c                                     + %trim(
109000060314     c                                         %editc( DCTncn:'Z' ))
109100060314e   2c                   endif
109200060314e   1c                   endif
109300060314      * - Pezzi
109400060314if  1c                   if        DCTnpz  > *zeros
109500060320if  2c***                if        wCLSflo7  =  *blanks
109600060320if  2c                   if        $English  =  *off
109700060314     c                   eval      POCqtd  = %trim(POCqtd)
109800060314     c                                     + ' Pezzi '
109900060314     c                                     + %trim(
110000060314     c                                         %editc( DCTnpz:'Z' ))
110100060314x   2c                   else
110200060314     c                   eval      POCqtd  = %trim(POCqtd)
110300060314     c                                     + ' Pieces '
110400060314     c                                     + %trim(
110500060314     c                                         %editc( DCTnpz:'Z' ))
110600060314e   2c                   endif
110700060314      *   - Descrizione pezzi danneggiati
110800060314     c                   exsr      Rep_DesPZD
110900060314e   1c                   endif
111000060314      *
111100060314     c                   ENDSR
111200060315
111300060315      *---------------------------------------------------------------*
111400060315      *?Impostazione note                                            ?*
111500060315      *---------------------------------------------------------------*
111600060315     c     sr_NOTE       BEGSR
111700060315      *
111800060315     c                   clear                   XX
111900060315     c                   clear                   YY
112000060315     c                   clear                   $TAD
112100060315      *
112200060315     c                   movel(p)  Wnumca        DCSnkt
112300060315     c                   eval      DCSstd = 'T'
112400060315     c                   eval      DCStrc = 'D'
112500060315     c                   clear                   DCShim
112600060315     c                   clear                   DCSdim
112700060315if  1c                   if             $TeEl(jj) = 'AE1'
112800060315     c                             or   $TeEl(jj) = 'AD1'
112900060315     c                             or   $TeEl(jj) = 'AM1'
113000060315     c                   movel(p)  'AL1'         DCSnks
113100060315x   1c                   else
113200060315     c                   movel(p)  $TeEl(jj)     DCSnks
113300060315e   1c                   endif
113400060315      *
113500060315     c     K07DCS01      setll     FNDCS000
113600060315     c     K07DCS01      reade     FNDCS000
113700060315      *
113800060315do  1c                   DOW       NOT %eof(FNDCS01L)
113900060315      *
114000060315     c                   add       1             yy
114100060315     c                   movel     DCSnot        $TAD(yy)
114200060315      *
114300060315      * Sposto in $Note ogni 3 records letti
114400060315if  2c                   if        yy     = %elem($TAD)
114500060315     c                   add       1             xx
114600060315     c                   movea     $TAD          $Note(xx)
114700060315     c                   clear                   yy
114800060315     c                   clear                   $TAD
114900060315if  3c                   if        xx    >= %elem($Note)
115000060315     c                   leavesr
115100060315e   3c                   endif
115200060315e   2c                   endif
115300060315      *
115400060315     c     K07DCS01      reade     FNDCS000
115500060315      *
115600060315e   1c                   ENDDO
115700060315      *
115800060315      * Memorizzo in $Note le note residue
115900060315if  1c                   if        yy     > *zeros
116000060315     c                   add       1             xx
116100060315     c                   movea     $TAD          $Note(xx)
116200060315e   1c                   endif
116300060315      *
116400060315     c                   ENDSR
116500060314
116600060314      *---------------------------------------------------------------*
116700060314      *?Impostazione testo                                           ?*
116800060314      *---------------------------------------------------------------*
116900060315     c     sr_TESTO      BEGSR
117000060315      *
117100060315      *ATTENZIONE?
117200060315      *?  SE TESTO LETTERA AD1 E TIPO ANOMALIA MANCANZA               ?
117300060315      *?  => TRASFORMO TESTO LETTERA IN AM1                           ?
117400060315      *
117500060315     c                   if             $TeEl(jj) = 'AD1'
117600060315     c                             and  §TADragr  = 'M'
117700060315      * valorizzo il testo in AM1
117800060315     c                   eval      $TeEl(jj) = 'AM1'
117900060315     c                   endif
118000060314      *
118100060315     c                   ENDSR
118200060315
118300060315      *---------------------------------------------------------------*
118400060315      *?Impostazione firma                                           ?*
118500060315      *---------------------------------------------------------------*
118600060315     c     sr_FIRMA      BEGSR
118700060315      *
118800060315      * Se P.O. che scrive è ESTERO forzo "Uff. Internazionale"
118900060315     c                   movel(p)  PICpod        PFCpod
119000060315      *
119100060315      * Se non ho inviato via e-mail
119200060315      *  e se è un preavviso di danno
119300060315      * => reperisco l'indirizzo mail per le note finali
119400060315      *    ("Per ricevere le prossime comunicazioni via e-mail...")
119500060315if  1c                   IF             $E_Mail    = *off
119600060315     c                             and  $TeEl(jj) <> 'AL5'
119700090331      * già valorizzato !!
119800090331     c***                move      Kcli          wDS_ksc
119900060315      * - - Recupero l'indirizzo del mittente
120000060315     c                   clear                   TRUL44ds
120100060315     c                   eval      D44pgm = SDSpgm
120200060315     c                   eval      D44po  = w0030
120300060315     c                   call      'TRUL44R'
120400060315     c                   parm                    TRUL44ds
120500060315     c                   parm                    TRTCm1ds
120600060315if  2c                   if             w0040 <> 8888
120700060315     c                             and  w0040 <> 9999
120800060315     c                   movel     D44mit        PFCem1
120900090331     c                   move      wds_ksc       PFCcod
121000060315e   2c                   endif
121100060315e   1c                   ENDIF
121200060314      *
121300060315     c                   ENDSR
121400060316
121500060316      *---------------------------------------------------------------*
121600060316      *?Stampa delle note (eventuali)                                ?*
121700060316      *---------------------------------------------------------------*
121800060316     c     sr_PrtNOTE    BEGSR
121900060316      *
122000060316     c                   clear                   yy
122100060316      *
122200060316do  1c                   DOW       yy        <  %elem($Note)
122300060316      *
122400060316     c                   add       1             yy
122500060316if  2c                   if        $Note(yy) =  *blanks
122600060316     c                   leave
122700060316e   2c                   endif
122800060316      * - lascia una riga (vuota) prima delle note
122900060316if  2c                   if        yy        =  1
123000060316     c                   clear                   PDCnot
123100060316if  3c                   if        $E_Mail   = *off
123200060316     c                   write     DN02N00
123300060316x   3c                   else
123400060316     c                   write     DN02N00M
123500060316e   3c                   endif
123600060316e   2c                   endif
123700060316      *
123800060316     c                   movel     $Note(yy)     PDCnot
123900060316if  2c                   if        $E_Mail   = *off
124000060316     c                   write     DN02N00
124100060316x   2c                   else
124200060316     c                   write     DN02N00M
124300060316e   2c                   endif
124400060316      *
124500060316e   1c                   ENDDO
124600060316      *
124700060316     c                   ENDSR
124800060316
124900060316      *---------------------------------------------------------------*
125000060316      *?Stampa testo lettera                                         ?*
125100060316      *---------------------------------------------------------------*
125200060320     c     sr_PrtTEXT    BEGSR
125300060316      *
125400060316sel 1c                   SELECT
125500060316      *
125600060316      * RESO PER AVARIA  - - - - - - - - - - - - - - - - - - - - - - -*
125700060316w   1c                   WHEN           $TeEl(jj) = 'AL5'
125800060316sel 2c                   select
125900060316      * - lettera in lingua italiana
126000060316w   2c                   when           $English  = *off
126100060316     c                             and  $E_Mail   = *off
126200060320     c                   write     DN02AL51A
126300060316      * - e-mail in lingua italiana
126400060316w   2c                   when           $English  = *off
126500060316     c                             and  $E_Mail   = *on
126600060320     c                   write     DN02AL51AM
126700060316      * - lettera in lingua inglese
126800060316w   2c                   when           $English  = *on
126900060316     c                             and  $E_Mail   = *off
127000060320     c                   write     DN02AL52A
127100060316      * - e-mail in lingua inglese
127200060316w   2c                   when           $English  = *on
127300060316     c                             and  $E_Mail   = *on
127400060320     c                   write     DN02AL52AM
127500060316e   2c                   endsl
127600060316      *
127700080114      * PREAVVISO DANNO CON O SENZA EVENTO - - - - - - - - - - - - - - - - -*
127800060316w   1c                   WHEN           $TeEl(jj) = 'AL1'
127900060316sel 2c                   select
128000060316      * - lettera in lingua italiana
128100060316w   2c                   when           $English  = *off
128200060316     c                             and  $E_Mail   = *off
128300060320     c                   write     DN02AL11A
128400080114      * In base al tipo anomalia stampo una frase diversa
128500080114     c   50              write     DN02ML11A                                    mancanza
128600080114     c   51              write     DN02CL11A                                    no mancanza no reso
128700080114     c   52              write     DN02RL11A                                    avaria con reso
128800060316      * - e-mail in lingua italiana
128900060316w   2c                   when           $English  = *off
129000060316     c                             and  $E_Mail   = *on
129100060320     c                   write     DN02AL11AM
129200080114      * In base al tipo anomalia stampo una frase diversa
129300080114     c   50              write     DN02ML11AM                                   mancanza
129400080114     c   51              write     DN02CL11AM                                   no mancanza no reso
129500080114     c   52              write     DN02RL11AM                                   avaria con reso
129600060316      * - lettera in lingua inglese
129700060316w   2c                   when           $English  = *on
129800060316     c                             and  $E_Mail   = *off
129900060320     c                   write     DN02AL12A
130000080114      * In base al tipo anomalia stampo una frase diversa
130100080114     c   50              write     DN02ML12A                                    mancanza
130200080114     c   51              write     DN02CL12A                                    no mancanza no reso
130300080114     c   52              write     DN02RL12A                                    avaria con reso
130400060316      * - e-mail in lingua inglese
130500060316w   2c                   when           $English  = *on
130600060316     c                             and  $E_Mail   = *on
130700060320     c                   write     DN02AL12AM
130800080114      * In base al tipo anomalia stampo una frase diversa
130900080114     c   50              write     DN02ML12AM                                   mancanza
131000080114     c   51              write     DN02CL12AM                                   no mancanza no reso
131100080114     c   52              write     DN02RL12AM                                   avaria con reso
131200060316e   2c                   endsl
131300060316      *
131400060316      * PREAVVISO DANNO ESTERO - EEX - - - - - - - - - - - - - - - - -*
131500060316w   1c                   WHEN           $TeEl(jj) = 'AE1'
131600060316sel 2c                   select
131700060316      * - lettera (in lingua italiana)
131800060316w   2c                   when           $E_Mail   = *off
131900060320     c                   write     DN02AE11A
132000060316      * - e-mail  (in lingua italiana)
132100060316w   2c                   when           $E_Mail   = *on
132200060320     c                   write     DN02AE11AM
132300060316e   2c                   endsl
132400060316      *
132500060316      * PREAVVISO DANNO ESTERO - DPD - - - - - - - - - - - - - - - - -*
132600060316w   1c                   WHEN           $TeEl(jj) = 'AD1'
132700060316sel 2c                   select
132800060316      * - lettera (in lingua inglese)
132900060316w   2c                   when           $E_Mail   = *off
133000060320     c                   write     DN02AD12A
133100060316      * - e-mail  (in lingua inglese)
133200060316w   2c                   when           $E_Mail   = *on
133300060320     c                   write     DN02AD12AM
133400060316e   2c                   endsl
133500060316      *
133600060316      * PREAVVISO DANNO ESTERO - DPD - MANCANZA  - - - - - - - - - - -*
133700060316w   1c                   WHEN           $TeEl(jj) = 'AM1'
133800060316sel 2c                   select
133900060316      * - lettera (in lingua inglese)
134000060316w   2c                   when           $E_Mail   = *off
134100060320     c                   write     DN02AM12A
134200060316      * - e-mail  (in lingua inglese)
134300060316w   2c                   when           $E_Mail   = *on
134400060320     c                   write     DN02AM12AM
134500060316e   2c                   endsl
134600060316      *
134700060316e   1c                   ENDSL
134800060316      *
134900060316     c                   ENDSR
135000060320
135100060320      *---------------------------------------------------------------*
135200060320      *?Stampa parte finale del testo lettera                        ?*
135300060320      *---------------------------------------------------------------*
135400060320     c     sr_PrtTEXT2   BEGSR
135500060320      *
135600060320sel 1c                   SELECT
135700060320      *
135800060320      * RESO PER AVARIA  - - - - - - - - - - - - - - - - - - - - - - -*
135900060320w   1c                   WHEN           $TeEl(jj) = 'AL5'
136000060320sel 2c                   select
136100060320      * - lettera in lingua italiana
136200060320w   2c                   when           $English  = *off
136300060320     c                             and  $E_Mail   = *off
136400060320     c                   write     DN02AL51B
136500060320      * - e-mail in lingua italiana
136600060320w   2c                   when           $English  = *off
136700060320     c                             and  $E_Mail   = *on
136800060320     c                   write     DN02AL51BM
136900060320      * - lettera in lingua inglese
137000060320w   2c                   when           $English  = *on
137100060320     c                             and  $E_Mail   = *off
137200060320     c                   write     DN02AL52B
137300060320      * - e-mail in lingua inglese
137400060320w   2c                   when           $English  = *on
137500060320     c                             and  $E_Mail   = *on
137600060320     c                   write     DN02AL52BM
137700060320e   2c                   endsl
137800060320      *
137900080115      * PREAVVISO DANNO  - - - - - - - - - - - - - - - - -*
138000060320w   1c                   WHEN           $TeEl(jj) = 'AL1'
138100060320sel 2c                   select
138200060320      * - lettera in lingua italiana
138300060320w   2c                   when           $English  = *off
138400060320     c                             and  $E_Mail   = *off
138500060320     c                   write     DN02AL11B
138600060320      * - e-mail in lingua italiana
138700060320w   2c                   when           $English  = *off
138800060320     c                             and  $E_Mail   = *on
138900060320     c                   write     DN02AL11BM
139000060320      * - lettera in lingua inglese
139100060320w   2c                   when           $English  = *on
139200060320     c                             and  $E_Mail   = *off
139300060320     c                   write     DN02AL12B
139400060320      * - e-mail in lingua inglese
139500060320w   2c                   when           $English  = *on
139600060320     c                             and  $E_Mail   = *on
139700060320     c                   write     DN02AL12BM
139800060320e   2c                   endsl
139900060320      *
140000060320      *
140100060320      * PREAVVISO DANNO ESTERO - EEX - - - - - - - - - - - - - - - - -*
140200060320w   1c                   WHEN           $TeEl(jj) = 'AE1'
140300060320sel 2c                   select
140400060320      * - lettera (in lingua italiana)
140500060320w   2c                   when           $E_Mail   = *off
140600060320     c                   write     DN02AE11B
140700060320      * - e-mail  (in lingua italiana)
140800060320w   2c                   when           $E_Mail   = *on
140900060320     c                   write     DN02AE11BM
141000060320e   2c                   endsl
141100060320      *
141200060320      * PREAVVISO DANNO ESTERO - DPD - - - - - - - - - - - - - - - - -*
141300060320w   1c                   WHEN           $TeEl(jj) = 'AD1'
141400060320sel 2c                   select
141500060320      * - lettera (in lingua inglese)
141600060320w   2c                   when           $E_Mail   = *off
141700060320     c                   write     DN02AD12B
141800060320      * - e-mail  (in lingua inglese)
141900060320w   2c                   when           $E_Mail   = *on
142000060320     c                   write     DN02AD12BM
142100060320e   2c                   endsl
142200060320      *
142300060320      * PREAVVISO DANNO ESTERO - DPD - MANCANZA  - - - - - - - - - - -*
142400060320w   1c                   WHEN           $TeEl(jj) = 'AM1'
142500060320sel 2c                   select
142600060320      * - lettera (in lingua inglese)
142700060320w   2c                   when           $E_Mail   = *off
142800060320     c                   write     DN02AM12B
142900060320      * - e-mail  (in lingua inglese)
143000060320w   2c                   when           $E_Mail   = *on
143100060320     c                   write     DN02AM12BM
143200060320e   2c                   endsl
143300060320      *
143400060320e   1c                   ENDSL
143500060320      *
143600060320     c                   ENDSR
143700060315
143800060315      *---------------------------------------------------------------*
143900060315      *?Scrittura STORICO FASI                                       ?*
144000060315      *---------------------------------------------------------------*
144100060315     c     Wrt_DCF       BEGSR
144200980804      *
144300060315     c                   clear                   FNDCF000
144400980804      *
144500060315     c                   eval      DCFaac  = I00aac
144600060315     c                   eval      DCFfil  = I00fil
144700060315     c                   eval      DCFnca  = I00nca
144800060315if  1c                   if        $AE1    = *on
144900060315     c                   eval      DCFptr  = 'D'
145000060315x   1c                   else
145100060315     c                   eval      DCFptr  = DCTptr
145200060315e   1c                   endif
145300060315     c                   eval      DCFfca  = I00fca
145400060314     c     *ISO          movel     wDate_EUR     DCFdfc
145500060315     c                   movel     wTime         DCFhfc
145600060315     c                   eval      DCFfev  = I00fgs
145700060315     c                   eval      DCFpru  = KNMUS
145800041109      * memorizzo il tipo invio
145900060314     c                   move      $Invio        DCFlet
146000060315     c***                clear                   DCFftr
146100060315     c***                clear                   DCFdtr
146200060315      *                  __________________
146300060315     c                   WRITE     FNDCF000
146400060315      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
146500060315     c                   ENDSR
146600060315
146700060315      *---------------------------------------------------------------*
146800060315      *?Aggiornamento TESTATA C.A.                                   ?*
146900060315      *---------------------------------------------------------------*
147000060315     c     Upd_DCT       BEGSR
147100980804      *
147200060315      * Richiamo pgm per determinare il P.O. che gestirà la prossima
147300060315      *   fase
147400060315     c                   clear                   FIDN05ds
147500060315     c                   eval      I05mod  = 'F'
147600060315     c                   eval      I05fca  = I00fca
147700060315     c                   eval      I05fpr  = I00fpr
147800060315     c                   eval      I05tpc  = DCTptr
147900060315     c                   eval      I05ffs  = 'O'
148000060315if  1c                   if        DCTnev  > *zeros
148100060315     c                   eval      I05fpe  = 'E'
148200060315x   1c                   else
148300060315     c                   eval      I05fpe  = 'N'
148400060315e   1c                   endif
148500060315if  1c                   if        DCTnde  > *zeros
148600060315     c                   eval      I05fde  = 'D'
148700060315x   1c                   else
148800060315     c                   eval      I05fde  = 'N'
148900060315e   1c                   endif
149000060315     c                   eval      I05tad  = I00tad
149100060314     c     *iso          movel     wDate_EUR     I05dta
149200981125      * valorizzo numero c.a.
149300060315     c                   eval      I05aac  = DCTaac
149400060315     c                   eval      I05fil  = DCTfil
149500060315     c                   eval      I05nca  = DCTnca
149600060315      *
149700060315     c                   call      'FIDN05R'
149800060315     c                   parm                    KPJBA
149900060315     c                   parm                    FIDN05ds
150000060315      *
150100060315if  1c                   IF        O05err  = *blanks
150200060315     c                   movel     O05rec        ds_FNDFA
150300060315sel 2c                   SELECT
150400060315w   2c                   WHEN      DFAgfs  = 'P'
150500060315     c                   movel     §DCTlnpc      DCTgfc
150600060315w   2c                   WHEN      DFAgfs  = 'A'
150700060315     c                   eval      DCTgfc  = DCTlna
150800060315w   2c                   WHEN      DFAgfs  = 'S'
150900060315     c                   eval      DCTgfc  = 46
151000060315x   2c                   OTHER
151100060315     c                   eval      DCTgfc  = DCTfil
151200060316e   2c                   ENDSL
151300060315e   1c                   ENDIF
151400060315      *
151500060315     c                   z-add     I00fca        DCTfca
151600060315     c                   clear                   DCTft1
151700060315     c                   clear                   DCTft2
151800060315      *                  __________________
151900060315     c                   EXCEPT    AggDCT
152000060315      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
152100090325      * se si tratta di preavviso di danno per un cliente
152200090325      * che gestisce in modo particolare le proprie pratiche
152300090325      * richiamo la routine di chiusura
152400090325
152500090325     c                   If        i00fca = 400
152600090325      *
152700090325     c                   clear                   TIBS02ds
152800090325     c                   eval      T02mod = 'C'
152900090325     c                   eval      T02sif = KNSIF
153000090325     c                   eval      T02cod = 'GPP'
153100090331     c                   move (p)  WDS_KSC       T02ke1
153200090325      *
153300090325     c                   call      'TIBS02R'
153400090325     c                   parm                    KPJBA
153500090325     c                   parm                    TIBS02ds
153600090325      * non trovato in tabella non faccio nulla
153700090325     c                   IF        T02err <> *blank
153800090325     c                   leavesr
153900090325     c                   endif
154000090325     c                   movel     T02uni        dGpp
154100090325      * verifico se c'è evento se è c'è fortuito  e sono esclusi dalla chiusura CA
154200090325     c                   if        §gppevf = 'S' and *in49
154300090325     c                   leavesr
154400090325     c                   endif
154500090325      * verifico il raggruppamento anomalia della CA
154600090325     c                   If        §tadragr = §gpprg1 or §tadragr = §gpprg2 or
154700090325     c                             §tadragr = §gpprg3
154800090325
154900090325      * eseguo la chiusura
155000090325     c                   exsr      SR_CHIUSCA
155100090325     c                   endif
155200090325     c                   endif
155300090325      *
155400090325     c
155500060315     c                   ENDSR
155600060314
155700090325      *---------------------------------------------------------------*
155800090325      * Chiusura CA clienti particolare
155900090325      *---------------------------------------------------------------*
156000090325     c     sr_CHIUSCA    BEGSR
156100090325     c* richiamo il PGM di chiusura
156200090325     c                   clear                   fidn22ds
156300090325     c                   eval      i22tri = 'R'                                 * richiamato
156400090325     c                   eval      i22mod = 'C'                                 * chiusura
156500090325     c                   eval      i22aac = dctaac                              * anno CA
156600090325     c                   eval      i22fil = dctfil                              * P.O. apertura CA
156700090325     c                   eval      i22nca = dctnca                              * Numero CA
156800090325     c                   eval      i22cch = §gppcch
156900090325     c                   eval      i22fgs = dctGFC
157000090325     c                   movel     fidn22ds      kpjbu
157100090325     c                   call      'FIDN22R'
157200090325     c                   parm                    kpjba
157300090325      *
157400090325     c                   endsr
157500060314      *---------------------------------------------------------------*
157600060314
157700060314     oFNDCT000  e            AGGDCT
157800060314      *
157900060314     o                       DCTgfc
158000060314     o                       DCTfca
158100060314     o                       DCTft1
158200060314     o                       DCTft2
158300060314
158400060314      *---------------------------------------------------------------*
158500060314
158600060314      *
158700060314      *?  S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E?
158800060314      *
158900060315** - $Cmd --------------------------------------------------------------------------*
159000060315OVRPRTF FIDN02P                  OUTQ(xxxxxxxxxx) FORMTYPE(xxxxxxxxxx) SAVE(*YES)     1
159100060315OVRPRTF FIDN02P                                   FORMTYPE(xxxxxxxxxx) SAVE(*YES)     2
159200060315OVRPRTF FIDN02P                  OUTQ(xxxxxxxxxx)                      SAVE(*YES)     3
159300060315OVRPRTF FIDN02P SPLFNAME(LETCOM) OUTQ(xxxxxxxxxx) FORMTYPE(xxxxxxxxxx)                4
159400060315OVRPRTF FIDN02P SPLFNAME(LETCOM)                  FORMTYPE(xxxxxxxxxx) SAVE(*YES)     5
159500060315OVRPRTF FIDN02P SPLFNAME(LETCOM) OUTQ(xxxxxxxxxx)                                     6
159600060315OVRPRTF FIDN02P SPLFNAME(LETCOM)                                       SAVE(*YES)     7
159700060315OVRPRTF FIDN02P                                                        SAVE(*YES)     8
159800060315OVRPRTF FIDN02PM OUTQ(          ) USRDFNDTA('                                         9
159900060314** - $CoTe
160000080111AL1  > Comunicaz. al Cliente per Pratica Assicurativa Avaria/Ammanco senza reso
160100060315AL5  > Lettera Accompagnam. Reso Merce Danneggiata (Avaria)
160200060315AE1  > Comunicaz. al Cliente per Pratica Assicurativa ESTERO
160300060315AD1  > Comunicaz. al Cliente per Pratica Assicurativa DPD
160400060315AM1  > Comunicaz. al Cliente per Pratica Assicurativa DPD Mancanza
