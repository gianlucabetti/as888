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
002800171120     fFIPND01L  if   e           k disk
002900060314      *
003000060314     fFNARB01L  if   e           k disk
003100060314     fFNBLP01L  if   e           k disk    prefix(ARB:3)
003200060314      *
003300060314     fAZORG01L  if   e           k disk
003400060314      *
003500060314     fFNDCT01L  Uf   e           k disk
003600060314     fFNDCF01L  Uf A e           k disk
003700060314      *
003800060315     fFIDN02P   o    e             printer usropn
003900060315     f                                     OFlind(*in01)
004000060315     fFIDN02PM  o    e             printer usropn
004100060315     f                                     OFlind(*in02)
004200060314
004300060314      *---------------------------------------------------------------*
004400060314
004500060314      *
004600060314      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - - *?
004700060314      *
004800060314     d I_picpod        c                   const('UFF. INTERNAZIONALE')
004900060314      *
005000060314      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *?
005100060314      *
005200060314      * - Comandi x OVRPRTF
005300060315     d $Cmd            s             85    dim( 9) ctdata perrcd(1)
005400060315      *
005500060314      * - Codice testo da stampare
005600060315     d $CoTe           s              3    dim( 5) ctdata perrcd(1)             codici testo
005700060314      *
005800060315      * Testi da elaborare
005900060315     d $TeEl           s              3    dim( 2) inz                          testi da elaborare
006000060315      *
006100060314      * Descrizione Tipo Anomalia
006200060315     d $TAD            s             35    dim( 3) inz                          descr. tipo anomalia
006300060315      *
006400060315      * Note da stampare
006500060315     d $Note           s            105    dim( 5) inz                          $TAD x 5
006600060314      *
006700060314      *?  S T R U T T U R E   D A T I - - - - - - - - - - - - - - - - *?
006800060314      *
006900060314      * - Parametri ricevuti
007000060314     d KPJBA         e ds
007100060314      *
007200060314     d FIDN00ds      e ds
007300060314     d   Wnumca                6     19
007400081215      * - Ricerca del beneficiario
007500081215     d FIDNBEds      e ds
007600171120     d FIDNBE2ds     e ds                  inz   qualified
007700090325      * - Chiusura CA
007800090325     d FIDN22ds      e ds
007900060314      *
008000060314      * - Parametri x Controllo profilo utenti
008100060314     d TIBS34ds      e ds
008200060314      * - Ds di riferimento al file esterno AZUTE00F
008300060314     d AZUTEds       e ds                  extname(AZUTE00F)
008400060314      * - Ds per dati organigramma
008500060314     d dDatiUte      e ds
008600060314      *
008700060314      * - 143ª descrizione dell'organigramma
008800060314     d Og143         e ds                  inz
008900060314      *
009000060314      * - Flag operativi in FNDCT
009100060314     d dDCT01        e ds                  inz
009200171120      *
009300171120      * - Flag etichetta DPD
009400171120     d dPNDdp5       e ds                  inz   qualified
009500060314      *
009600060314      * - Ricerca/Controllo tabelle
009700060314     d TIBS02ds      e ds                  inz
009800060314      * - tab. TAD = Tipi anomalia danni
009900060314     d dTAD          e ds                  inz
010000060314      * - tab. TLD = Testi lettera danni
010100060314     d dTLD          e ds                  inz
010200090325      * - tab. GPP = Gestione Pratiche perticolari per alcuni clienti
010300090325     d dGPP          e ds                  inz
010400060314      * - tab. TB  = Tibi Bolla
010500060314     d dsTB          e ds                  inz
010600060314      *
010700060314      * - Parametri x Ricerca/Controllo fasi C.A.
010800060314     d FIDN05ds      e ds                  inz
010900060314     d ds_FNDFA      e ds                  inz extname(FNDFA00F)
011000060314      *
011100060314      * - Parametri x Reperimento terminal di arrivo/partenza
011200060314     d FNLV55ds      e ds                  inz
011300060314     d   D55tla      e                     inz('L')
011400060314     d   D55tpt      e                     inz('P')
011500060314      *
011600060314      * - Parametri x Controllo validità numero fax
011700060314     d TRUL42ds      e ds                  inz
011800060314      *
011900060314      * - Parametri x Reperimento USRDFNDTA x comunicazione e-mail
012000060314     d TRUL44ds      e ds                  inz                                  X Invio e-mail
012100060314      * - Parametri x Ridefinizione dati utente estesi per mailing PDF
012200060314     d TRTCM1ds      e ds                  inz                                  X Invio e-mail
012300060315      *
012400060315      *
012500060315     d Status         sds
012600060315     d  SDSpgm           *proc
012700060314      *
012800060314      * - Parametri x Controllo/Inversione date
012900060314     d WLBDAT          ds                  inz
013000060314     d  G02dat                 1      8  0 inz
013100060314     d  G02inv                 9     16  0 inz
013200060315     d  G02err                17     17    inz('3')
013300060314     d  G02tgi                18     22  0 inz
013400060314      *
013500060314      * Ridefinizione sottoconto
013600060314     d wDS_ksc         ds             7    inz
013700060315     d  w0030                         3  0 inz
013800060315     d  w0040                         4  0 inz
013900060314      *
014000060314      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *?
014100060314      *
014200060314      * - Flag
014300060314     d $AE1            s              1    inz(*off)
014400060314     d $E_Mail         s              1    inz(*off)
014500060320     d $English        s              1    inz(*off)
014600060320     ***d wCLSflo7        s              1    inz
014700060314      * - Indici di schiera
014800060315     d XX              s              3  0 inz
014900060314     d YY              s              3  0 inz
015000060314     d JJ              s              3  0 inz
015100060314      * - Campi di comodo x invio e-mail
015200060314     d $Invio          s              1    inz
015300060314     d W_mail          s             60    inz
015400060314     d KSC_mail        s                   inz like(D44ksc)
015500060314      * - Parametri x TRULOUTQ
015600060314     d PrmFine         s              1    inz
015700060314     d PrmFil          s              3s 0 inz
015800060314     d PrmTpOutQ       s              1    inz
015900060314     d PrmOutQ         s             10    inz
016000060314     d PrmOutQLib      s             10    inz
016100060314     d PrmEsito        s              1    inz
016200060314      * - Campi x comandi di sistema
016300060314     d Qcmd            s            500    inz
016400060314     d Qlen            s             15  5 inz(500)
016500060314      * - Campi di comodo
016600060314     d Wtad            s                   like(DCTtad)   inz
016700081218     d Kcli            s                   like(dctksc)   inz
016800060314     d w§DCTlnpc       s                   like(DCTlnp)   inz
016900060314     d w§DCTport       s                   like(§DCTport) inz
017000060314     d w§TLDstp        s                   like(§TLDstp1) inz
017100060315      * - Campi per gestione date
017200060315     d wTime           s              6  0
017300060315     d wDate_AMG       s              8  0
017400060315     d wDate_EUR       s               d   datfmt(*eur)
017500060315     d wDta_EUR        s               d   datfmt(*eur)   inz(*loval)
017600060314
017700060314      *---------------------------------------------------------------*
017800060314      *?FASI C.A. FISSATE A PROGRAMMA                                ?*
017900060314      *---------------------------------------------------------------*
018000060314      *---------------------------------------------------------------*
018100060314
018200060314      *---------------------------------------------------------------*
018300060314      *?RIEPILOGO INDICATORI                                         ?*
018400060314      *---------------------------------------------------------------*
018500060316      * 01   - OverFlow in PrtF FIDN02P  (non gestito)                *
018600060316      * 02   - OverFlow in Prtf FIDN02PM (non gestibile x e-mail)     *
018700060314      * 30   - Comodo                                                 *
018800060315      * 41   - Stampa numero di FAX per allegato 1                    *
018900060314      * 42   - Stampa 2ª riga dettaglio anomalia                      *
019000060314      * 43   - Stampa quantità danneggiate                            *
019100060314      * 44   - Stampa 2a riga descrizione pezzi danneggiati/mancanti  *
019200060314      * 47   - Stampa riferimento mittente ALFAnumerico               *
019300060314      * 48   - Stampa numero evento                                   *
019400090325      * 49   - Evento fortuito                                        *
019500080114      * 50   - Tipo anomalia mancanza                                 *
019600080114      * 51   - Tipo anomalia avaria/ammanco senza reso                *
019700080114      * 52   - Tipo avaria con reso                                   *
019800060314      *---------------------------------------------------------------*
019900981203
020000060314     c     *Entry        plist
020100060314     c                   parm                    KPJBA
020200060314     c                   movel     KPJBU         FIDN00ds
020300060314
020400981204      * Aggancio anagrafico fasi per tipi testo
020500060314     c     I00fca        chain     FNDFA000
020600060314
020700060314      * Memorizzo in schiera i tipi testo da elaborare
020800060314     c                   eval      yy = 1
020900060314     c     DFAle1        lookup    $CoTe(yy)                              30
021000060314     c   30              movel     DFAle1        $TeEl(1)
021100060314     c                   eval      yy = 1
021200060314     c     DFAle2        lookup    $CoTe(yy)                              30
021300060314     c   30              movel     DFAle2        $TeEl(2)
021400060314
021500060314      * Se non esistono testi validi non elaboro
021600060314if  1c                   if             $TeEl(1) = *blanks
021700060314     c                             and  $TeEl(2) = *blanks
021800060314     c                   goto      FINE
021900060314e   1c                   endif
022000060314
022100980929      * Aggancio file bolle
022200060314     c                   if        I00tpb    = 'P'
022300060314     c     K04BLPARB     chain     FNBLP000
022400060314     c                   else
022500060314     c     K04BLPARB     chain     FNARB000
022600060314     c                   endif
022700060314
022800981203      *  Aggancio testata C.A.
022900060314     c     K03DCT01      chain     FNDCT000
023000060314     c                   movel     DCTflo        dDCT01
023100060314     c                   movel     §DCTlnpc      w§DCTlnpc
023200060314
023300060314      * Elaboro i tipi testo - - - - - - - - - - - - - - - - - - - - -*
023400060314do  1c     1             DO        2             jj
023500060314
023600060314if  2c                   if        $TeEl(jj)  = *blanks
023700060314     c                   iter
023800060314e   2c                   endif
023900060314
024000060314      * Pulizia dei campi di stampa
024100060314     c                   exsr      Inz_PrtF
024200060316
024300060316      * Se il testo da stampare è "AL1" e la spedizione è un PF Import
024400060316      *   EEX o EUP stampo "AE1"
024500060316if  2c                   if             $TeEl(jj)  =  'AL1'
024600060316     c                             and  §DCTport   =  'F'
024700060316     c                             and  §DCTtisp   =  'I'
024800060316     c                             and (§DCTeuro   =  'X'
024900060316     c                              or  §DCTeuro   =  'P')
025000060316     c                   eval      $TeEl(jj) = 'AE1'
025100060316     c                   eval      $AE1      = *on
025200060316e   2c                   endif
025300000222
025400060316      * Reperimento dati del destinatario della lettera:
025500060316      * - decodifica il cliente
025600060316      * - verifica se richiesto il testo in Inglese
025700060316      * - verifica se richiesta e-mail piuttosto che lettera
025800060314     c                   exsr      Decod_CLI
025900060316
026000060317      * Aggancio tabella TLD = Parametri di stampa Testo Lettera Danni
026100060316     c                   exsr      Tab_TLD
026200060315
026300060316      * Apertura file stampa & impostazione Tipo Modulo e Stampante
026400060315     c                   exsr      sr_OVERRIDE
026500060314
026600060314      * Stampa lettera
026700060314     c                   exsr      sr_Stampa
026800060314
026900060314e   1c                   ENDDO
027000060314
027100060314      * Aggiorno data base - - - - - - - - - - - - - - - - - - - - - -*
027200060315     c                   if        KNMUS    <> '1111111111'
027300060314     c                   exsr      Wrt_DCF                                      Dettaglio fase
027400060314     c                   exsr      Upd_DCT                                      Testata fase
027500060315     c                   endif
027600060314
027700060314      * Fine - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
027800060314     c                   clear                   TIBS02ds
027900060314     c                   movel     'C'           T02tla
028000981029     c                   call      'TIBS02R'
028100060314     c                   parm                    KPJBA
028200060314     c                   parm                    TIBS02ds
028300060314      *
028400060314      *
028500060314     c                   call      'TRULOUTQ'
028600060314     c                   parm      'L'           PrmFine
028700060314     c                   parm      *zeros        PrmFil
028800060314     c                   parm      *blanks       PrmTpOutQ
028900060314     c                   parm      *blanks       PrmOutQ
029000060314     c                   parm      *blanks       PrmOutQLib
029100060314     c                   parm      *blanks       PrmEsito
029200060314
029300060314     c     FINE          tag
029400060315     c                   eval      *inLR     = *on
029500060314
029600060314      *---------------------------------------------------------------*
029700060314      *?Routine iniziale                                             ?*
029800060314      *---------------------------------------------------------------*
029900060314     c     *InzSr        BEGSR
030000060314      *
030100060314     c                   exsr      DatiJob
030200060315      *
030300060315     c                   time                    wTime
030400060315     c                   movel     *date         wDate_EUR
030500060314      *
030600060315      * *Date in formato aaaa/mm/gg
030700060314     c                   clear                   WLBdat
030800060314     c                   eval      G02dat = *date
030900060314     c                   call      'XSRDA8'
031000060314     c                   parm                    WLBDAT
031100060314     c                   z-add     G02inv        wDate_AMG
031200060314      *
031300060314      * Definizione Key-List
031400060314     c     K03DCT01      klist
031500060314     c                   kfld                    I00aac
031600060314     c                   kfld                    I00fil
031700060314     c                   kfld                    I00nca
031800060314      *
031900060314     c     K02DET01      klist
032000060314     c                   kfld                    DCTaae
032100060314     c                   kfld                    DCTnev
032200060314      *
032300060314     c     K04DKA01      klist
032400060314     c                   kfld                    I00aac
032500060314     c                   kfld                    I00fil
032600060314     c                   kfld                    I00nca
032700060314     c                   kfld                    DKAtrc
032800060314      *
032900060314     c     K03DCS01      klist
033000060314     c                   kfld                    DCStpd
033100060314     c                   kfld                    DCSnkt
033200060314     c                   kfld                    DCSstd
033300060314     c     K07DCS01      klist
033400060314     c                   kfld                    DCStpd
033500060314     c                   kfld                    DCSnkt
033600060314     c                   kfld                    DCSstd
033700060314     c                   kfld                    DCSdim
033800060314     c                   kfld                    DCShim
033900060314     c                   kfld                    DCSnks
034000060314     c                   kfld                    DCStrc
034100060314      *
034200060314     c     K04BLPARB     klist
034300060314     c                   kfld                    I00aas
034400060314     c                   kfld                    I00lnp
034500060314     c                   kfld                    I00nrs
034600060314     c                   kfld                    I00nsp
034700060314      *
034800060314      * Inizializzazione campi chiave
034900060314     c                   eval      DCStpd = 'C'
035000060314      *
035100060314     c                   ENDSR
035200060314
035300060314      *---------------------------------------------------------------*
035400060314      *?Reperimento Dati del job (Utente/Operativi)                  ?*
035500060314      *---------------------------------------------------------------*
035600060314     c     DatiJob       BEGSR
035700060314      *
035800060314     c     *dtaara       define    §azute        AZUTEds
035900060314     c     *dtaara       define    §datiute      dDatiUte
036000060314      *
036100060314     c                   in(E)     *dtaara
036200060314     c                   IF        %ERROR or RSUT = *blanks
036300060314     c                   clear                   Tibs34Ds
036400060314     c                   call      'TIBS34R'
036500060314     c                   parm                    Tibs34Ds
036600060314     c                   in        *dtaara
036700060314     c                   ENDIF
036800060314      *
036900060314     c                   ENDSR
037000060314
037100060314      *---------------------------------------------------------------*
037200060314      *?Pulizia dei campi in stampa                                  ?*
037300060314      *---------------------------------------------------------------*
037400060314     c     Inz_PrtF      BEGSR
037500060314      *
037600060316     c                   clear                   DN02T01                        Intestazione+Oggetto
037700060316     c                   clear                   DN02N00                        Note
037800060320     c*** senza campi:   clear                   DN02AL51A                      Reso per Avaria
037900060320     c*** senza campi:   clear                   DN02AL51B                      Reso per Avaria
038000060320     c*** senza campi:   clear                   DN02AL11A                      Preavv.Danno NO even
038100060320     c*** senza campi:   clear                   DN02AL11B                      Preavv.Danno NO even
038200060320     c*** senza campi:   clear                   DN02AL13A                      Preavv.Danno SI even
038300060320     c*** senza campi:   clear                   DN02AL13B                      Preavv.Danno SI even
038400060320     c*** senza campi:   clear                   DN02AE11A                      Preavv.Danno Estero
038500060320     c*** senza campi:   clear                   DN02AE11B                      Preavv.Danno Estero
038600060316     c                   clear                   DN02P01                        Piede
038700060316     c                   clear                   DN02N01                        Note finali
038800060316      * > testi x estero
038900060316     c*** stessi campi:  clear                   DN02T02                        Intestazione+Oggetto
039000060320     c*** stessi campi:  clear                   DN02AL52A                      Reso per Avaria
039100060320     c*** stessi campi:  clear                   DN02AL52B                      Reso per Avaria
039200060320     c*** stessi campi:  clear                   DN02AL12A                      Preavv.Danno NO even
039300060320     c*** stessi campi:  clear                   DN02AL12B                      Preavv.Danno NO even
039400060320     c*** stessi campi:  clear                   DN02AL14A                      Preavv.Danno SI even
039500060320     c*** stessi campi:  clear                   DN02AL14B                      Preavv.Danno SI even
039600060320     c*** senza campi:   clear                   DN02AD12A                      Preavv.Danno Est.DPD
039700060320     c*** senza campi:   clear                   DN02AD12B                      Preavv.Danno Est.DPD
039800060320     c*** senza campi:   clear                   DN02AM12A                      Preavv.Danno Man.DPD
039900060320     c*** senza campi:   clear                   DN02AM12B                      Preavv.Danno Man.DPD
040000060316     c*** stessi campi:  clear                   DN02P02                        Piede
040100060316     c*** stessi campi:  clear                   DN02N02                        Note finali
040200060316      * > e-mail
040300060316     c*** stessi campi:  clear                   DN02T01M                       Intestazione+Oggetto
040400060316     c*** stessi campi:  clear                   DN02N00M                       Note
040500060316     c*** stessi campi:  clear                   DN02AL5M                       Reso per Avaria
040600060316     c*** stessi campi:  clear                   DN02AL1M                       Preavv.Danno NO even
040700060316     c*** stessi campi:  clear                   DN02AL1EM                      Preavv.Danno SI even
040800060316     c*** stessi campi:  clear                   DN02AE1M                       Preavv.Danno Estero
040900060316     c*** stessi campi:  clear                   DN02AD1M                       Preavv.Danno DPD
041000060316     c*** stessi campi:  clear                   DN02AM1M                       Preavv.Danno DPD Man
041100060316     c*** stessi campi:  clear                   DN02P01M                       Piede
041200060316     c*** stessi campi:  clear                   DN02N01M                       Note finali
041300060316      * > e-mail x estero
041400060316     c*** stessi campi:  clear                   DN02T02M                       Intestazione+Oggetto
041500060316     c*** stessi campi:  clear                   DN02AL5BM                      Reso per Avaria
041600060316     c*** stessi campi:  clear                   DN02AL1BM                      Preavv.Danno NO even
041700060316     c*** stessi campi:  clear                   DN02AL1EBM                     Preavv.Danno SI even
041800060316     c*** stessi campi:  clear                   DN02P02M                       Piede
041900060316     c*** stessi campi:  clear                   DN02N02M                       Note finali
042000060314      *
042100060314     c                   ENDSR
042200060314
042300060314      *---------------------------------------------------------------*
042400060316      *?Decodifica cliente, verifica se testo inglese e/o e-mail     ?*
042500060314      *---------------------------------------------------------------*
042600060314     c     Decod_CLI     BEGSR
042700060314      *
042800081217      * recupero beneficiario dal pgm esterno
042900081217     c                   clear                   fidnbeds
043000081217      *
043100081217     c                   movel     'B'           IBEmod
043200081217     c                   z-add     dctaac        IBEaac
043300081217     c                   z-add     dctfil        IBEfil
043400081217     c                   z-add     dctnca        IBEnca
043500081217     c                   z-add     dctaas        IBEaas
043600081217     c                   z-add     dctlnp        IBElnp
043700081217     c                   z-add     dctnrs        IBEnrs
043800081217     c                   z-add     dctnsp        IBEnsp
043900081217     c                   z-add     dctksc        IBEksc
044000090529if  3 * se lettera "AE1" indirizzo la lettera al destinatario
044100090529     c                   if        $TeEl(jj) = 'AE1'
044200090529     c                   eval      IBEptr = 'D'
044300090529     c                   else
044400081217     c                   movel     dctptr        IBEptr
044500090529     c                   endif
044600081217     c                   movel     dctflo        IBEflo
044700081217     c                   movel     I00tpb        IBEtpb
044800081218     c                   If        %subst(knsif:7:1) = 'P'
044900081217     c                   eval      IBEsif = 'P'
045000081218     c                   EndIf
045100171120      *
045200171121if  1c                   if        §DCTeuro = 'D'
045300171120     c                   eval      FIDNBE2ds.iBEtisp = §DCTtisp
045400171121     c                   call      'FIDNBER'
045500171121     c                   parm                    fidnbeds
045600171121     c                   parm                    fidnBE2ds
045700171121x   1c                   else
045800081217     c                   call      'FIDNBER'
045900081217     c                   parm                    fidnbeds
046000171121e   1c                   endif
046100081217      *
046200171122if  1c                   If        obeerr = ' '
046300171122if  2c                   if        §DCTeuro = 'D'
046400171122     c                   movel     obeKMA        wds_ksc
046500171122x   2c                   else
046600171122     c                   movel     obeksc        wds_ksc
046700171122e   2c                   endif
046800171122e   1c                   EndIf
046900060314      *
047000081217      * Verifico se intestatario della lettera è cliente DPD
047100081217if  1c                   IF        wds_ksc > *zeros
047200060314     c                   exsr      Ctr_DPD
047300171122e   1c                   endif
047400060314      *
047500081217     c                   movel     oberag        PICrag
047600081217     c                   movel     obevia        PICind
047700081217     c                   movel     obecap        PICcap
047800081217     c                   movel     obeloc        PICloc
047900081217     c                   movel     obeprv        PICpro
048000081217     c                   movel     obenaz        PICnaz
048100081217     c                   movel     obefax        PIXfax
048200081217     c                   eval      $Invio   = obeinv
048300081217     c                   eval      W_Mail   = obeima
048400081217     c                   eval      Ksc_Mail = obekma
048500060314      *
048600060314     c                   ENDSR
048700060314
048800060314      *---------------------------------------------------------------*
048900060314      *?Verifica se intestatario della lettera è cliente DPD         ?*
049000060314      *---------------------------------------------------------------*
049100060314     c     Ctr_DPD       BEGSR
049200060314      *
049300060314     c                   clear                   Og143
049400060314      *
049500060314     c     w0030         chain     AZORG
049600060314      *
049700060314if  1c                   if        NOT %found(AZORG01L)
049800060314     c                   leavesr
049900060314e   1c                   endif
050000060314      *
050100060314      * Valorizzo la 143ª descrizione
050200060314     c                   movel     ORGde3        Og143
050300060314      *
050400060314if  1c                   if        §OGntw = 'DPD'
050500060314      * Valorizzo il testo in AD1
050600060315     c                   movel     'AD1'         $TeEl(jj)
050700060314     c                   eval      $AE1   = *off
050800060314e   1c                   endif
050900060314      *
051000060314     c                   ENDSR
051100060314
051200060314      *---------------------------------------------------------------*
051300060317      *?Reperimento parametri di stampa per testo lettera danni      ?*
051400060314      *---------------------------------------------------------------*
051500060314     c     Tab_TLD       BEGSR
051600060314      *
051700060314     c                   clear                   dTLD
051800060314      *
051900060314     c                   clear                   TIBS02ds
052000060314     c                   eval      T02mod = 'C'
052100060314     c                   eval      T02sif = KNSIF
052200060314     c                   eval      T02cod = 'TLD'
052300060314     c                   if            $TeEl(jj) = 'AE1'
052400060314     c                             or  $TeEl(jj) = 'AD1'
052500060314     c                             or  $TeEl(jj) = 'AM1'
052600060314     c                   eval      T02ke1 = 'AL1'
052700060314     c                   else
052800060314     c                   eval      T02ke1 = $TeEl(jj)
052900060314     c                   endif
053000060314      *
053100060314     c                   call      'TIBS02R'
053200060314     c                   parm                    KPJBA
053300060314     c                   parm                    TIBS02ds
053400060314      *
053500060314     c                   movel     T02uni        dTLD
053600060314      *
053700060314     c                   ENDSR
053800060314
053900060314      *---------------------------------------------------------------*
054000060314      *?Reperimento tab. TAD = Tipo Anomalia                         ?*
054100060314      *---------------------------------------------------------------*
054200060314     c     Tab_TAD       BEGSR
054300060314      *
054400060314     c                   clear                   dTAD
054500060314      *
054600060314     c                   clear                   TIBS02ds
054700060314     c                   eval      T02mod = 'C'
054800060314     c                   eval      T02sif = KNSIF
054900060314     c                   eval      T02cod = 'TAD'
055000060315     c                   movel(p)  Wtad          T02ke1
055100060314      *
055200060314     c                   call      'TIBS02R'
055300060314     c                   parm                    KPJBA
055400060314     c                   parm                    TIBS02ds
055500060314      *
055600060314     c                   movel     T02uni        dTAD
055700060314      *
055800060314     c                   ENDSR
055900060314
056000060314      *---------------------------------------------------------------*
056100060314      *?Reperimento Nominativo dell'interlocutore                    ?*
056200060314      *---------------------------------------------------------------*
056300060314     c     Rep_PerCon    BEGSR
056400060314      *
056500060314     c*** già così:      eval      DCStpd = 'C'
056600060314     c                   eval      DCSnkt = Wnumca
056700060314     c                   eval      DCSstd = 'N'
056800060314      *
056900060315     c     K03DCS01      chain     FNDCS000
057000060314      *
057100060315     c                   if            %found(FNDCS01L)
057200060314     c                             and DCSnot <> *blanks
057300060314     c                   eval      PICpec =  'C.A. '  +  DCSnot
057400060314     c                   endif
057500060314      *
057600060316     c                   ENDSR
057700060314
057800060314      *---------------------------------------------------------------*
057900060314      *?Reperimento descrizione anomalia                             ?*
058000060314      *---------------------------------------------------------------*
058100060314     c     Rep_DesTAD    BEGSR
058200060314      *
058300060314     c                   clear                   YY
058400060315     c                   clear                   $TAD
058500060314      *
058600060314     c*** già così:      eval      DCStpd = 'C'
058700060314     c                   movel(P)  Wnumca        DCSnkt
058800060314     c                   eval      DCSstd = 'A'
058900060314     c*** non in klist:  clear                   DCSnks
059000060314     c*** non in klist:  movel     'D'           DCStrc
059100060314      *
059200060314     c     K03DCS01      setll     FNDCS000
059300060314     c     K03DCS01      reade     FNDCS000
059400060314      *
059500060314do  1c                   dow       NOT  %eof(FNDCS01L)
059600060404     c                             and  YY <  %elem($TAD)
059700060314     c                   add       1             yy
059800060315     c                   movel     DCSnot        $TAD(yy)
059900060314     c     K03DCS01      reade     FNDCS000
060000060314e   1c                   enddo
060100060314      *
060200060315     c                   eval      POCda1 = $TAD(1)
060300060315     c                   eval      POCda2 = $TAD(2) + $TAD(3)
060400060314      *
060500060314     c                   ENDSR
060600060314
060700060314      *---------------------------------------------------------------*
060800060314      *?Reperimento descrizione pezzi danneggiati/mancanti           ?*
060900060314      *---------------------------------------------------------------*
061000060314     c     Rep_DesPZD    BEGSR
061100060314      *
061200060314     c                   clear                   YY
061300060315     c                   clear                   $TAD
061400060314      *
061500060314     c*** già così:      eval      DCStpd = 'C'
061600060314     c                   movel(p)  Wnumca        DCSnkt
061700060314     c                   eval      DCSstd = 'P'
061800060314     c*** non in klist:  clear                   DCSnks
061900060314     c*** non in klist:  movel     'D'           DCStrc
062000060314      *
062100060314     c     K03DCS01      setll     FNDCS000
062200060314     c     K03DCS01      reade     FNDCS000
062300060314      *
062400060314do  1c                   dow       NOT  %eof(FNDCS01L)
062500060404     c                             and  YY <  %elem($TAD)
062600060314      *
062700060314     c                   add       1             yy
062800060315     c                   movel     DCSnot        $TAD(yy)
062900060314     c     K03DCS01      reade     FNDCS000
063000060314     c                   enddo
063100060314      *
063200060315     c                   eval      POCdpz = $TAD(1)
063300060315     c                   eval      POCdp1 = $TAD(2) + $TAD(3)
063400060314      *
063500060314     c                   ENDSR
063600060314
063700060314      *---------------------------------------------------------------*
063800060314      *?Impostazione dei campi in stampa   &   Stampa                ?*
063900060314      *---------------------------------------------------------------*
064000060315     c     sr_Stampa     BEGSR
064100060316      *
064200060316      * Determina tipo di invio:
064300060316sel 1c                   select
064400060316      * - M = e-mail
064500060316w   1c                   when      $E_Mail   = *on
064600060316     c                   eval      $Invio    = 'M'
064700060316      * - F = fax
064800060316w   1c                   when            $E_Mail   =  *off
064900060316     c                             and  (PIXfax    <> *blanks
065000060316     c                             and   $TeEl(jj) <> 'AL5')
065100060316     c                   eval      $Invio    = 'F'
065200060316      * - L = lettera
065300060316w   1c***                when            $E_Mail   =  *off
065400060316     c***                          and  (PIXfax    =  *blanks
065500060316     c***                           or   $TeEl(jj) =  'AL5')
065600060316x   1c                   other
065700060316     c                   eval      $Invio    = 'L'
065800060316e   1c                   endsl
065900060314      *
066000060314      * Impostazione dei campi in stampa
066100060314     c                   exsr      sr_INTES                                      intestazione
066200060314     c                   exsr      sr_OGGE                                       oggetto
066300060315     c                   exsr      sr_NOTE                                       note
066400060314     c                   exsr      sr_TESTO                                      testo
066500060314     c                   exsr      sr_FIRMA                                      firma
066600060315      *
066700060315      * Impostazione indicatori utilizzati nel PrtF
066800060316     c                   eval      *in41  = (PIXfax    <> *blanks  and
066900060316     c                                       $TeEl(jj) <> 'AL5')
067000060315     c                   eval      *in42  = (POCda2 <> *blanks)
067100060315     c                   eval      *in43  = (POCqtd <> *blanks)
067200080903     c                   eval      *in46  = (POCdp1 <> *blanks)
067300060315     c                   eval      *in47  = (ARBrma <> *blanks)
067400060315     c                   eval      *in48  = (DCTnev    >  *zeros   and
067500060315     c                                       $TeEl(jj) <> 'AL5')
067600060314      *
067700060314      * Stampa:
067800060314sel 1c                   SELECT
067900060314      *
068000060315      * Lettera in Italiano  - - - - - - - - - - - - - - - - - - - - -*
068100060314w   1c                   WHEN           $E_Mail   = *off
068200060314     c                             and  $English  = *off
068300060316      * - Intestazione + Oggetto
068400060314     c                   write     DN02T01
068500060314      * - Testo
068600060320     c                   exsr      sr_PrtTEXT
068700060320      * - Note (eventuali)
068800060320     c                   exsr      sr_PrtNOTE
068900060314      * - Piede
069000060320     c                   exsr      sr_PrtTEXT2
069100060314     c                   write     DN02P01
069200060320      * - Note finali
069300060320      *   ("Per ricevere le prossime comunicazioni via e-mail...")
069400060320      *   SE è un preavviso di danno
069500060320if  2c                   if        $TeEl(jj) <> 'AL5'
069600060320     c                   write     DN02N01
069700060320e   2c                   endif
069800060314      *
069900060315      * Lettera in Inglese - - - - - - - - - - - - - - - - - - - - - -*
070000060314w   1c                   WHEN           $E_Mail   = *off
070100060314     c                             and  $English  = *on
070200060316      * - Intestazione + Oggetto
070300060314     c                   write     DN02T02
070400060314      * - Testo
070500060320     c                   exsr      sr_PrtTEXT
070600060320      * - Note (eventuali)
070700060320     c                   exsr      sr_PrtNOTE
070800060314      * - Piede
070900060320     c                   exsr      sr_PrtTEXT2
071000060314     c                   write     DN02P02
071100060320      * - Note finali
071200060320      *   ("Per ricevere le prossime comunicazioni via e-mail...")
071300060320      *   SE è un preavviso di danno
071400060320if  2c                   if        $TeEl(jj) <> 'AL5'
071500060320     c                   write     DN02N02
071600060320e   2c                   endif
071700060314      *
071800060315      * e-mail in Italiano - - - - - - - - - - - - - - - - - - - - - -*
071900060314w   1c                   WHEN           $E_Mail   = *on
072000060314     c                             and  $English  = *off
072100060316      * - Intestazione + Oggetto
072200060315     c                   write     DN02T01M
072300060315      * - Testo
072400060320     c                   exsr      sr_PrtTEXT
072500060320      * - Note (eventuali)
072600060320     c                   exsr      sr_PrtNOTE
072700060315      * - Piede
072800060320     c                   exsr      sr_PrtTEXT2
072900060315     c                   write     DN02P01M
073000060314      *
073100060315      * e-mail in Inglese  - - - - - - - - - - - - - - - - - - - - - -*
073200060314w   1c                   WHEN           $E_Mail   = *on
073300060315     c                             and  $English  = *on
073400060316      * - Intestazione + Oggetto
073500060315     c                   write     DN02T02M
073600060315      * - Testo
073700060320     c                   exsr      sr_PrtTEXT
073800060320      * - Note (eventuali)
073900060320     c                   exsr      sr_PrtNOTE
074000060315      * - Piede
074100060320     c                   exsr      sr_PrtTEXT2
074200060315     c                   write     DN02P02M
074300060314      *
074400060314e   1c                   ENDSL
074500060314      *
074600060314     c                   ENDSR
074700060314
074800060315      *---------------------------------------------------------------*
074900060315      *?Impostazione del tipo-modulo e della stampante               ?*
075000060315      *---------------------------------------------------------------*
075100060315     c     sr_OVERRIDE   BEGSR
075200060320      *
075300060320      * Dati del P.O. (se reso per avaria utilizzo il P.O. di partenza)
075400060320if  1c                   if        $TeEl(jj) = 'AL5'
075500060320     c     w§DCTlnpc     chain     AZORG
075600060320x   1c                   else
075700060320     c     I00fgs        chain     AZORG
075800060320e   1c                   endif
075900060314      *
076000060314     c                   clear                   Qcmd
076100060314      *
076200060315if  1c                   IF             $TeEl(jj) <> 'AL5'
076300060315     c                             and  PIXfax    <> *blanks
076400060315      *
076500060315      * Controllo N° FAX
076600060315     c                   clear                   TRUL42ds
076700060315     c                   eval      D42fax  = PIXfax
076800060315     c                   call      'TRUL42R'
076900060315     c                   parm                    TRUL42ds
077000060315if  2c                   IF        D42err <> *blanks
077100060315      *
077200060315     c                   clear                   PIXfax
077300060315      *
077400060315x   2c                   ELSE
077500060315      *
077600060315     c                   movel     §TLDfaxi      PIXurg
077700060315      * - Cerco il nome della coda del fax
077800060315     c                   call      'TRULOUTQ'
077900060315     c                   parm                    PrmFine
078000060315     c                   parm      DUTpou        PrmFil
078100060315     c                   parm      '2'           PrmTpOutQ
078200060315     c                   parm      *blanks       PrmOutQ
078300060315     c                   parm      *blanks       PrmOutQLib
078400060315     c                   parm      *blanks       PrmEsito
078500060315if  3c                   if             PrmEsito = *off
078600060315     c                             and  PrmOutq <> *blanks
078700060315     c                   movel(p)  PrmOutq       w§TLDstp
078800060315x   3c                   else
078900060315     c                   movel(p)  '*SAME     '  w§TLDstp
079000060315e   3c                   endif
079100060315      *
079200060315e   2c                   ENDIF
079300060315      *
079400060315e   1c                   ENDIF
079500060314      *
079600060315      *
079700060315      * Imposto l'override da eseguire
079800060315sel 1c                   SELECT
079900060314      *
080000060315w   1c                   WHEN           $TeEl(jj) <> 'AL5'
080100060315     c                             and  $Invio     = 'M'
080200060315     c                             and  W_Mail    <> *blanks
080300060315     c                   clear                   TRUL44ds
080400060315     c                   eval      D44pgm    = SDSpgm
080500060315     c                   eval      D44po     = ORGfil
080600060315     c                   eval      D44Ksc    = Ksc_Mail
080700060315     c                   eval      D44Des    = W_Mail
080800060315     c                   eval      D44scope  = '*CALLLVL'
080900060315     c                   eval      D44prtf   = 'FIDN02P'
081000060320if  2c***                IF        wCLSflo7 <> *blanks
081100060320if  2c                   IF        $English  = *on
081200060315     c                   eval      D44obj    = '*OBJM*'
081300060315     c                                       + 'Damage comunication N. '
081400060315     c                                       + %char(DCTfil)
081500060315     c                                       + '/'
081600060315     c                                       + %trim(
081700060315     c                                           %editc( DCTnca:'Z' ))
081800060315     c                                       + ' year '
081900060315     c                                       + %char(DCTaac)
082000060315x   2c                   ELSE
082100060315     c                   eval      D44obj    = '*OBJM*'
082200060315     c                                       + 'Comunicazione di presunta +
082300060315     c                                          anomalia N.'
082400060315     c                                       + %char(DCTfil)
082500060315     c                                       + '/'
082600060315     c                                       + %trim(
082700060315     c                                           %editc( DCTnca:'Z' ))
082800060315     c                                       + ' anno '
082900060315     c                                       + %char(DCTaac)
083000060315e   2c                   ENDIF
083100060314     c                   call      'TRUL44R'
083200060315     c                   parm                    TRUL44ds
083300060315     c                   parm                    TRTCM1ds
083400060315      * - se c'è stato qualche problema faccio invio in altro modo
083500060315if  2c                   IF        D44err  <> *blanks
083600060315     c                   clear                   $Invio
083700060315x   2c                   ELSE
083800060315     c                   eval      $E_Mail  = *on
083900060315      * - - ovrprtf
084000060315     c                   eval      Qcmd     = $Cmd(9)
084100060315     c                   eval      %subst(Qcmd:23:10) = D44outq
084200060315     c                   eval      Qcmd     = %trim(Qcmd)
084300060315     c                                      + D44dta
084400060315     c                                      + ''')'
084500060315     c                   call      'QCMDEXC'                            30
084600060314     c                   parm                    Qcmd
084700060314     c                   parm                    Qlen
084800060315      * - - errore
084900060315if  3c                   if        *in30
085000060315     c                   clear                   $Invio
085100060315     c                   eval      $E_Mail  = *off
085200060315e   3c                   endif
085300060315e   2c                   ENDIF
085400060314      *
085500060315w   1c                   WHEN           $TeEl(jj) =  'AL5'
085600060315     c                             and  §TLDtmod  <> *blanks
085700060315     c                             and  w§TLDstp  <> *blanks
085800060315     c                   eval      Qcmd     = $Cmd(1)
085900060315     c                   eval      %subst(Qcmd:39:10) = w§TLDstp
086000060315     c                   eval      %subst(Qcmd:60:10) = §TLDtmod
086100060314      *
086200060315w   1c                   WHEN           $TeEl(jj) =  'AL5'
086300060315     c                             and  §TLDtmod  <> *blanks
086400060315     c                             and  w§TLDstp  =  *blanks
086500060315     c                   eval      Qcmd     = $Cmd(2)
086600060315     c                   eval      %subst(Qcmd:60:10) = §TLDtmod
086700060314      *
086800060315w   1c                   WHEN           $TeEl(jj) =  'AL5'
086900060315     c                             and  §TLDtmod  =  *blanks
087000060315     c                             and  w§TLDstp <> *blanks
087100060315     c                   eval      Qcmd     = $Cmd(3)
087200060315     c                   eval      %subst(Qcmd:39:10) = w§TLDstp
087300060314      *
087400060315w   1c                   WHEN           $TeEl(jj) <> 'AL5'
087500060315     c                             and  §TLDtmod  <> *blanks
087600060315     c                             and  w§TLDstp  <> *blanks
087700060315     c                             and  $Invio    =  *blanks
087800060315     c                   eval      Qcmd     = $Cmd(4)
087900060314     c                   eval      %subst(Qcmd:39:10) = w§TLDstp
088000060314     c                   eval      %subst(Qcmd:60:10) = §TLDtmod
088100060314      *
088200060315w   1c                   WHEN           $TeEl(jj) <> 'AL5'
088300060315     c                             and  §TLDtmod  <> *blanks
088400060315     c                             and  w§TLDstp  =  *blanks
088500060315     c                             and  $Invio    =  *blanks
088600060315     c                   eval      Qcmd     = $Cmd(5)
088700060315     c                   eval      %subst(Qcmd:60:10) = §TLDtmod
088800060314      *
088900060315w   1c                   WHEN           $TeEl(jj) <> 'AL5'
089000060315     c                             and  §TLDtmod  =  *blanks
089100060315     c                             and  w§TLDstp  <> *blanks
089200060315     c                             and  $Invio    =  *blanks
089300060315     c                   eval      Qcmd     = $Cmd(6)
089400060314     c                   eval      %subst(Qcmd:39:10) = w§TLDstp
089500060314      *
089600060315w   1c                   WHEN           $TeEl(jj) <> 'AL5'
089700060315     c                             and  $Invio    =  *blanks
089800060315     c                   eval      Qcmd     = $Cmd(7)
089900060314      *
090000060315x   1c                   OTHER
090100060315     c                   eval      Qcmd     = $Cmd(8)
090200060314      *
090300060315e   1c                   ENDSL
090400060314      *
090500060314      * Se ho impostato qualcosa eseguo l'OVRPRTF
090600060315if  1c                   if             Qcmd      <> *blanks
090700060315     c                             and  $Invio    =  *blanks
090800060314     c                   call      'QCMDEXC'
090900060314     c                   parm                    Qcmd
091000060314     c                   parm                    Qlen
091100060315e   1c                   endif
091200060314      *
091300060314      *  Apro file di stampa
091400060315if  1c                   if        $E_Mail = *off
091500060314     c                   open      FIDN02P
091600060315x   1c                   else
091700060314     c                   open      FIDN02PM
091800060315e   1c                   endif
091900060314      *
092000060316     c                   ENDSR
092100060314
092200060314      *---------------------------------------------------------------*
092300060314      *?Impostazione dati dell'intestazione                          ?*
092400060314      *---------------------------------------------------------------*
092500060314     c     sr_INTES      BEGSR
092600060314      *
092700060314      * Se P.O. che scrive è ESTERO forzo "Uff. Internazionale" come
092800060314      *   descrizione intestazione + dati da AZORG del terminal di
092900060314      *   partenza della linea di partenza della bolla.
093000060314if  1c                   if             ORGfl1 = 'E'
093100060314     c                             and  ORGfva = *blanks
093200060314      *
093300060315     c                   movel(p)  I_PICpod      PICpod
093400060314      *
093500060314     c                   reset                   FNLV55ds
093600060314     c                   eval      D55lin   = ORGfil
093700060314     c                   move      wDate_AMG     D55drf
093800060314     c                   call      'FNLV55R'
093900060314     c                   parm                    FNLV55ds
094000060314if  2c                   if        D55err   = *blanks
094100060314     c     D55tfp        chain     AZORG
094200060314     c                   movel     ORGind        PICpoi
094300060314     c                   movel     ORGcpf        PICpoc
094400060314     c                   movel     ORGloc        PICpol
094500060314     c                   movel     ORGpro        PICpop
094600060314     c                   movel     ORGtel        PICpot
094700060314     c                   movel     ORGfax        PICpof
094800060314e   2c                   endif
094900060314      *
095000060314x   1c                   ELSE
095100060314      *
095200060314     c                   movel     ORGdes        PICpod
095300060314     c                   movel     ORGind        PICpoi
095400060314     c                   movel     ORGcpf        PICpoc
095500060314     c                   movel     ORGloc        PICpol
095600060314     c                   movel     ORGpro        PICpop
095700060314     c                   movel     ORGtel        PICpot
095800060314     c                   movel     ORGfax        PICpof
095900060314      *
096000060314e   1c                   ENDIF
096100060314      *
096200060314      * Imposto stampante
096300060314     c                   movel     §TLDstp1      w§TLDstp
096400060314      *
096500060314      * Verifico se devo stampare in Inglese
096600060314     c                   reset                   $English
096700081217     c                   If        obeING = *on
096800060314     c                   eval      $English = *on
096900060314e   1c                   endif
097000060314      *
097100060314      * Recupero l'interlocutore non per la lettera reso avaria
097200060314     c                   exsr      Rep_PerCon
097300060314      *
097400060314      * Località + data
097500060314if  1c                   if             ORGfl1 = 'E'
097600060314     c                             and  ORGfva = *blanks
097700060314     c                   eval      PICdal   = 'MILANO, '
097800060314     c                                      + %editc(*date:'Y')
097900060314x   1c                   else
098000060314     c                   eval      PICdal   = %trim(ORGdes) + ', '
098100060314     c                                      + %editc(*date:'Y')
098200060314e   1c                   endif
098300060314      *
098400060314     c                   ENDSR
098500060314
098600060314      *---------------------------------------------------------------*
098700060314      *?Impostazione dati dell'oggetto                               ?*
098800060314      *---------------------------------------------------------------*
098900060314     c     sr_OGGE       BEGSR
099000060314      *
099100060314     c                   eval      POCnca  = %trim(
099200060314     c                                         %editw( I00fil:'0   ' ))
099300060314     c                                     + '/'
099400060314     c                                     + %trim(
099500060314     c                                         %editc( I00nca:'Z' ))
099600060314     c                   eval      POCaac  = I00aac
099700060314      *
099800060314     c                   z-add     I00lnp        POClnp
099900060314     c                   z-add     I00nrs        POCnrs
100000060314     c                   z-add     I00nsp        POCnsp
100100060426     c                   clear                   WLBdat
100200060426     c                   eval      G02inv  = (I00aas * 10000) + ARBmgs
100300060426     c                   eval      G02err  = '3'
100400060315     c                   call      'XSRDA8'
100500060315     c                   parm                    WLBDAT
100600060426     c                   z-add     G02dat        POCdat
100700060314     c                   movel     ARBrsm        POCrsm
100800060314     c                   z-add     ARBrmn        POCrmn
100900060314     c                   movel     ARBrma        POCrma
101000060314     c                   movel     ARBrsd        POCrsd
101100060314     c                   movel     ARBlod        POClod
101200060314     c                   if        ARBnzd <> *blanks
101300060314     c                   movel     ARBnzd        POCprn
101400060314     c                   else
101500060314     c                   movel     ARBprd        POCprn
101600060314     c                   endif
101700060314     c                   movel     ARBlod        POClod
101800060314     c                   z-add     ARBncl        POCncl
101900060314     c                   z-add     ARBpkf        POCpkf
102000060314      *
102100060314      * Evento
102200060314if  1c                   IF             DCTnev     > *zeros
102300060315     c                             and  $TeEl(jj) <> 'AL5'
102400060314     c                   eval      *in48   = *on
102500060314     c                   eval      POCkev  = %trim(
102600060314     c                                         %editw( DCTaae:'0    ' ))
102700060314     c                                     + '/'
102800060314     c                                     + %trim(
102900060314     c                                         %editc( DCTnev:'Z' ))
103000060314     c     K02DET01      chain     FNDET000
103100060314     c                   eval      Wtad    = DETtad
103200060314     c                   exsr      Tab_TAD
103300090325      * verifico se evento fortuito
103400090325     c                   eval      *in49 = (§tadevfo = 'S')
103500090325
103600060314sel 2c                   select
103700060320w   2c***                when           wCLSflo7 <> *blanks
103800060320w   2c                   when           $English =  *on
103900060314     c                             and  §TADdesi <> *blanks
104000060314     c                   movel     §TADdesi      POCtae
104100060314w   2c                   when      §TADdest <> *blanks
104200060314     c                   movel     §TADdest      POCtae
104300060314x   2c                   other
104400060314     c                   movel     §TADdesc      POCtae
104500060314e   2c                   endsl
104600060314e   1c                   ENDIF
104700060314      *
104800060314      * Decodifico tipo anomalia
104900060314     c                   eval      Wtad    = I00tad
105000060314     c                   exsr      Tab_TAD
105100080114      * verifico il tipo anomalia per scegliere il tipo di frase da stampare
105200080114     c                   eval      *in50 =  §tadragr = 'M'                      mancanza
105300080115     c                   eval      *in51 =  §tadragr = 'A' or                   ammanco
105400080115     c                                     (§tadragr = 'V' and dctdsd <>'R')    avaria no reso
105500080115     c                   eval      *in52 =  §tadragr = 'V' and dctdsd = 'R'     avaria con reso
105600080114      * se tipo anomalia mancanza imposto smarrimanto altrimenti avaria nel testo
105700080114     c                   clear                   prttad
105800080114     c                   if        §tadragr = 'M'
105900080114     c                   if        $English = *off
106000080114     c                   eval      prttad = 'smarrimento.'
106100080114     c                   else
106200080114     c                   eval      prttad = 'is missing.'
106300080114     c                   endif
106400080114     c                   else
106500080114     c                   if        $English = *off
106600080114     c                   eval      prttad = 'avaria.'
106700080114     c                   else
106800080114     c                   eval      prttad = 'was damaged.'
106900080114     c                   endif
107000080114     c                   endif
107100080114      *
107200060314sel 1c                   select
107300060320w   1c***                when           wCLSflo7 <> *blanks
107400060320w   1c                   when           $English =  *on
107500060314     c                             and  §TADdesi <> *blanks
107600060314     c                   movel     §TADdesi      POCtad
107700060314w   1c                   when      §TADdest <> *blanks
107800060314     c                   movel     §TADdest      POCtad
107900060314x   1c                   other
108000060314     c                   movel     §TADdesc      POCtad
108100060314e   1c                   endsl
108200060314      *
108300060314      * Descrizione anomalia
108400060314     c                   exsr      Rep_DesTAD
108500060314      *
108600060314      * Quantità danneggiate
108700060314      * - Peso
108800060314if  1c                   if        DCTpkd  > *zeros
108900060314     c                   eval      POCqtd  = 'Kg '
109000060314     c                                     + %trim(
109100060314     c                                         %editc( DCTpkd:'L' ))
109200060314e   1c                   endif
109300060314      * - Colli
109400060314if  1c                   if        DCTncn  > *zeros
109500060320if  2c***                if        wCLSflo7  =  *blanks
109600060320if  2c                   if        $English  =  *off
109700060314     c                   eval      POCqtd  = %trim(POCqtd)
109800060314     c                                     + ' Colli '
109900060314     c                                     + %trim(
110000060314     c                                         %editc( DCTncn:'Z' ))
110100060314x   2c                   else
110200060314     c                   eval      POCqtd  = %trim(POCqtd)
110300060314     c                                     + ' Parcels '
110400060314     c                                     + %trim(
110500060314     c                                         %editc( DCTncn:'Z' ))
110600060314e   2c                   endif
110700060314e   1c                   endif
110800060314      * - Pezzi
110900060314if  1c                   if        DCTnpz  > *zeros
111000060320if  2c***                if        wCLSflo7  =  *blanks
111100060320if  2c                   if        $English  =  *off
111200060314     c                   eval      POCqtd  = %trim(POCqtd)
111300060314     c                                     + ' Pezzi '
111400060314     c                                     + %trim(
111500060314     c                                         %editc( DCTnpz:'Z' ))
111600060314x   2c                   else
111700060314     c                   eval      POCqtd  = %trim(POCqtd)
111800060314     c                                     + ' Pieces '
111900060314     c                                     + %trim(
112000060314     c                                         %editc( DCTnpz:'Z' ))
112100060314e   2c                   endif
112200060314      *   - Descrizione pezzi danneggiati
112300060314     c                   exsr      Rep_DesPZD
112400060314e   1c                   endif
112500060314      *
112600060314     c                   ENDSR
112700060315
112800060315      *---------------------------------------------------------------*
112900060315      *?Impostazione note                                            ?*
113000060315      *---------------------------------------------------------------*
113100060315     c     sr_NOTE       BEGSR
113200060315      *
113300060315     c                   clear                   XX
113400060315     c                   clear                   YY
113500060315     c                   clear                   $TAD
113600060315      *
113700060315     c                   movel(p)  Wnumca        DCSnkt
113800060315     c                   eval      DCSstd = 'T'
113900060315     c                   eval      DCStrc = 'D'
114000060315     c                   clear                   DCShim
114100060315     c                   clear                   DCSdim
114200060315if  1c                   if             $TeEl(jj) = 'AE1'
114300060315     c                             or   $TeEl(jj) = 'AD1'
114400060315     c                             or   $TeEl(jj) = 'AM1'
114500060315     c                   movel(p)  'AL1'         DCSnks
114600060315x   1c                   else
114700060315     c                   movel(p)  $TeEl(jj)     DCSnks
114800060315e   1c                   endif
114900060315      *
115000060315     c     K07DCS01      setll     FNDCS000
115100060315     c     K07DCS01      reade     FNDCS000
115200060315      *
115300060315do  1c                   DOW       NOT %eof(FNDCS01L)
115400060315      *
115500060315     c                   add       1             yy
115600060315     c                   movel     DCSnot        $TAD(yy)
115700060315      *
115800060315      * Sposto in $Note ogni 3 records letti
115900060315if  2c                   if        yy     = %elem($TAD)
116000060315     c                   add       1             xx
116100060315     c                   movea     $TAD          $Note(xx)
116200060315     c                   clear                   yy
116300060315     c                   clear                   $TAD
116400060315if  3c                   if        xx    >= %elem($Note)
116500060315     c                   leavesr
116600060315e   3c                   endif
116700060315e   2c                   endif
116800060315      *
116900060315     c     K07DCS01      reade     FNDCS000
117000060315      *
117100060315e   1c                   ENDDO
117200060315      *
117300060315      * Memorizzo in $Note le note residue
117400060315if  1c                   if        yy     > *zeros
117500060315     c                   add       1             xx
117600060315     c                   movea     $TAD          $Note(xx)
117700060315e   1c                   endif
117800060315      *
117900060315     c                   ENDSR
118000060314
118100060314      *---------------------------------------------------------------*
118200060314      *?Impostazione testo                                           ?*
118300060314      *---------------------------------------------------------------*
118400060315     c     sr_TESTO      BEGSR
118500060315      *
118600060315      *ATTENZIONE?
118700060315      *?  SE TESTO LETTERA AD1 E TIPO ANOMALIA MANCANZA               ?
118800060315      *?  => TRASFORMO TESTO LETTERA IN AM1                           ?
118900060315      *
119000060315     c                   if             $TeEl(jj) = 'AD1'
119100060315     c                             and  §TADragr  = 'M'
119200060315      * valorizzo il testo in AM1
119300060315     c                   eval      $TeEl(jj) = 'AM1'
119400060315     c                   endif
119500060314      *
119600060315     c                   ENDSR
119700060315
119800060315      *---------------------------------------------------------------*
119900060315      *?Impostazione firma                                           ?*
120000060315      *---------------------------------------------------------------*
120100060315     c     sr_FIRMA      BEGSR
120200060315      *
120300060315      * Se P.O. che scrive è ESTERO forzo "Uff. Internazionale"
120400060315     c                   movel(p)  PICpod        PFCpod
120500060315      *
120600060315      * Se non ho inviato via e-mail
120700060315      *  e se è un preavviso di danno
120800060315      * => reperisco l'indirizzo mail per le note finali
120900060315      *    ("Per ricevere le prossime comunicazioni via e-mail...")
121000060315if  1c                   IF             $E_Mail    = *off
121100060315     c                             and  $TeEl(jj) <> 'AL5'
121200090331      * già valorizzato !!
121300090331     c***                move      Kcli          wDS_ksc
121400060315      * - - Recupero l'indirizzo del mittente
121500060315     c                   clear                   TRUL44ds
121600060315     c                   eval      D44pgm = SDSpgm
121700060315     c                   eval      D44po  = w0030
121800060315     c                   call      'TRUL44R'
121900060315     c                   parm                    TRUL44ds
122000060315     c                   parm                    TRTCm1ds
122100060315if  2c                   if             w0040 <> 8888
122200060315     c                             and  w0040 <> 9999
122300060315     c                   movel     D44mit        PFCem1
122400090331     c                   move      wds_ksc       PFCcod
122500060315e   2c                   endif
122600060315e   1c                   ENDIF
122700060314      *
122800060315     c                   ENDSR
122900060316
123000060316      *---------------------------------------------------------------*
123100060316      *?Stampa delle note (eventuali)                                ?*
123200060316      *---------------------------------------------------------------*
123300060316     c     sr_PrtNOTE    BEGSR
123400060316      *
123500060316     c                   clear                   yy
123600060316      *
123700060316do  1c                   DOW       yy        <  %elem($Note)
123800060316      *
123900060316     c                   add       1             yy
124000060316if  2c                   if        $Note(yy) =  *blanks
124100060316     c                   leave
124200060316e   2c                   endif
124300060316      * - lascia una riga (vuota) prima delle note
124400060316if  2c                   if        yy        =  1
124500060316     c                   clear                   PDCnot
124600060316if  3c                   if        $E_Mail   = *off
124700060316     c                   write     DN02N00
124800060316x   3c                   else
124900060316     c                   write     DN02N00M
125000060316e   3c                   endif
125100060316e   2c                   endif
125200060316      *
125300060316     c                   movel     $Note(yy)     PDCnot
125400060316if  2c                   if        $E_Mail   = *off
125500060316     c                   write     DN02N00
125600060316x   2c                   else
125700060316     c                   write     DN02N00M
125800060316e   2c                   endif
125900060316      *
126000060316e   1c                   ENDDO
126100060316      *
126200060316     c                   ENDSR
126300060316
126400060316      *---------------------------------------------------------------*
126500060316      *?Stampa testo lettera                                         ?*
126600060316      *---------------------------------------------------------------*
126700060320     c     sr_PrtTEXT    BEGSR
126800060316      *
126900060316sel 1c                   SELECT
127000060316      *
127100060316      * RESO PER AVARIA  - - - - - - - - - - - - - - - - - - - - - - -*
127200060316w   1c                   WHEN           $TeEl(jj) = 'AL5'
127300060316sel 2c                   select
127400060316      * - lettera in lingua italiana
127500060316w   2c                   when           $English  = *off
127600060316     c                             and  $E_Mail   = *off
127700060320     c                   write     DN02AL51A
127800060316      * - e-mail in lingua italiana
127900060316w   2c                   when           $English  = *off
128000060316     c                             and  $E_Mail   = *on
128100060320     c                   write     DN02AL51AM
128200060316      * - lettera in lingua inglese
128300060316w   2c                   when           $English  = *on
128400060316     c                             and  $E_Mail   = *off
128500060320     c                   write     DN02AL52A
128600060316      * - e-mail in lingua inglese
128700060316w   2c                   when           $English  = *on
128800060316     c                             and  $E_Mail   = *on
128900060320     c                   write     DN02AL52AM
129000060316e   2c                   endsl
129100060316      *
129200080114      * PREAVVISO DANNO CON O SENZA EVENTO - - - - - - - - - - - - - - - - -*
129300060316w   1c                   WHEN           $TeEl(jj) = 'AL1'
129400060316sel 2c                   select
129500060316      * - lettera in lingua italiana
129600060316w   2c                   when           $English  = *off
129700060316     c                             and  $E_Mail   = *off
129800060320     c                   write     DN02AL11A
129900080114      * In base al tipo anomalia stampo una frase diversa
130000080114     c   50              write     DN02ML11A                                    mancanza
130100080114     c   51              write     DN02CL11A                                    no mancanza no reso
130200080114     c   52              write     DN02RL11A                                    avaria con reso
130300060316      * - e-mail in lingua italiana
130400060316w   2c                   when           $English  = *off
130500060316     c                             and  $E_Mail   = *on
130600060320     c                   write     DN02AL11AM
130700080114      * In base al tipo anomalia stampo una frase diversa
130800080114     c   50              write     DN02ML11AM                                   mancanza
130900080114     c   51              write     DN02CL11AM                                   no mancanza no reso
131000080114     c   52              write     DN02RL11AM                                   avaria con reso
131100060316      * - lettera in lingua inglese
131200060316w   2c                   when           $English  = *on
131300060316     c                             and  $E_Mail   = *off
131400060320     c                   write     DN02AL12A
131500080114      * In base al tipo anomalia stampo una frase diversa
131600080114     c   50              write     DN02ML12A                                    mancanza
131700080114     c   51              write     DN02CL12A                                    no mancanza no reso
131800080114     c   52              write     DN02RL12A                                    avaria con reso
131900060316      * - e-mail in lingua inglese
132000060316w   2c                   when           $English  = *on
132100060316     c                             and  $E_Mail   = *on
132200060320     c                   write     DN02AL12AM
132300080114      * In base al tipo anomalia stampo una frase diversa
132400080114     c   50              write     DN02ML12AM                                   mancanza
132500080114     c   51              write     DN02CL12AM                                   no mancanza no reso
132600080114     c   52              write     DN02RL12AM                                   avaria con reso
132700060316e   2c                   endsl
132800060316      *
132900060316      * PREAVVISO DANNO ESTERO - EEX - - - - - - - - - - - - - - - - -*
133000060316w   1c                   WHEN           $TeEl(jj) = 'AE1'
133100060316sel 2c                   select
133200060316      * - lettera (in lingua italiana)
133300060316w   2c                   when           $E_Mail   = *off
133400060320     c                   write     DN02AE11A
133500060316      * - e-mail  (in lingua italiana)
133600060316w   2c                   when           $E_Mail   = *on
133700060320     c                   write     DN02AE11AM
133800060316e   2c                   endsl
133900060316      *
134000060316      * PREAVVISO DANNO ESTERO - DPD - - - - - - - - - - - - - - - - -*
134100060316w   1c                   WHEN           $TeEl(jj) = 'AD1'
134200060316sel 2c                   select
134300060316      * - lettera (in lingua inglese)
134400060316w   2c                   when           $E_Mail   = *off
134500060320     c                   write     DN02AD12A
134600060316      * - e-mail  (in lingua inglese)
134700060316w   2c                   when           $E_Mail   = *on
134800060320     c                   write     DN02AD12AM
134900060316e   2c                   endsl
135000060316      *
135100060316      * PREAVVISO DANNO ESTERO - DPD - MANCANZA  - - - - - - - - - - -*
135200060316w   1c                   WHEN           $TeEl(jj) = 'AM1'
135300060316sel 2c                   select
135400060316      * - lettera (in lingua inglese)
135500060316w   2c                   when           $E_Mail   = *off
135600060320     c                   write     DN02AM12A
135700060316      * - e-mail  (in lingua inglese)
135800060316w   2c                   when           $E_Mail   = *on
135900060320     c                   write     DN02AM12AM
136000060316e   2c                   endsl
136100060316      *
136200060316e   1c                   ENDSL
136300060316      *
136400060316     c                   ENDSR
136500060320
136600060320      *---------------------------------------------------------------*
136700060320      *?Stampa parte finale del testo lettera                        ?*
136800060320      *---------------------------------------------------------------*
136900060320     c     sr_PrtTEXT2   BEGSR
137000060320      *
137100060320sel 1c                   SELECT
137200060320      *
137300060320      * RESO PER AVARIA  - - - - - - - - - - - - - - - - - - - - - - -*
137400060320w   1c                   WHEN           $TeEl(jj) = 'AL5'
137500060320sel 2c                   select
137600060320      * - lettera in lingua italiana
137700060320w   2c                   when           $English  = *off
137800060320     c                             and  $E_Mail   = *off
137900060320     c                   write     DN02AL51B
138000060320      * - e-mail in lingua italiana
138100060320w   2c                   when           $English  = *off
138200060320     c                             and  $E_Mail   = *on
138300060320     c                   write     DN02AL51BM
138400060320      * - lettera in lingua inglese
138500060320w   2c                   when           $English  = *on
138600060320     c                             and  $E_Mail   = *off
138700060320     c                   write     DN02AL52B
138800060320      * - e-mail in lingua inglese
138900060320w   2c                   when           $English  = *on
139000060320     c                             and  $E_Mail   = *on
139100060320     c                   write     DN02AL52BM
139200060320e   2c                   endsl
139300060320      *
139400080115      * PREAVVISO DANNO  - - - - - - - - - - - - - - - - -*
139500060320w   1c                   WHEN           $TeEl(jj) = 'AL1'
139600060320sel 2c                   select
139700060320      * - lettera in lingua italiana
139800060320w   2c                   when           $English  = *off
139900060320     c                             and  $E_Mail   = *off
140000060320     c                   write     DN02AL11B
140100060320      * - e-mail in lingua italiana
140200060320w   2c                   when           $English  = *off
140300060320     c                             and  $E_Mail   = *on
140400060320     c                   write     DN02AL11BM
140500060320      * - lettera in lingua inglese
140600060320w   2c                   when           $English  = *on
140700060320     c                             and  $E_Mail   = *off
140800060320     c                   write     DN02AL12B
140900060320      * - e-mail in lingua inglese
141000060320w   2c                   when           $English  = *on
141100060320     c                             and  $E_Mail   = *on
141200060320     c                   write     DN02AL12BM
141300060320e   2c                   endsl
141400060320      *
141500060320      *
141600060320      * PREAVVISO DANNO ESTERO - EEX - - - - - - - - - - - - - - - - -*
141700060320w   1c                   WHEN           $TeEl(jj) = 'AE1'
141800060320sel 2c                   select
141900060320      * - lettera (in lingua italiana)
142000060320w   2c                   when           $E_Mail   = *off
142100060320     c                   write     DN02AE11B
142200060320      * - e-mail  (in lingua italiana)
142300060320w   2c                   when           $E_Mail   = *on
142400060320     c                   write     DN02AE11BM
142500060320e   2c                   endsl
142600060320      *
142700060320      * PREAVVISO DANNO ESTERO - DPD - - - - - - - - - - - - - - - - -*
142800060320w   1c                   WHEN           $TeEl(jj) = 'AD1'
142900060320sel 2c                   select
143000060320      * - lettera (in lingua inglese)
143100060320w   2c                   when           $E_Mail   = *off
143200060320     c                   write     DN02AD12B
143300060320      * - e-mail  (in lingua inglese)
143400060320w   2c                   when           $E_Mail   = *on
143500060320     c                   write     DN02AD12BM
143600060320e   2c                   endsl
143700060320      *
143800060320      * PREAVVISO DANNO ESTERO - DPD - MANCANZA  - - - - - - - - - - -*
143900060320w   1c                   WHEN           $TeEl(jj) = 'AM1'
144000060320sel 2c                   select
144100060320      * - lettera (in lingua inglese)
144200060320w   2c                   when           $E_Mail   = *off
144300060320     c                   write     DN02AM12B
144400060320      * - e-mail  (in lingua inglese)
144500060320w   2c                   when           $E_Mail   = *on
144600060320     c                   write     DN02AM12BM
144700060320e   2c                   endsl
144800060320      *
144900060320e   1c                   ENDSL
145000060320      *
145100060320     c                   ENDSR
145200060315
145300060315      *---------------------------------------------------------------*
145400060315      *?Scrittura STORICO FASI                                       ?*
145500060315      *---------------------------------------------------------------*
145600060315     c     Wrt_DCF       BEGSR
145700980804      *
145800060315     c                   clear                   FNDCF000
145900980804      *
146000060315     c                   eval      DCFaac  = I00aac
146100060315     c                   eval      DCFfil  = I00fil
146200060315     c                   eval      DCFnca  = I00nca
146300060315if  1c                   if        $AE1    = *on
146400060315     c                   eval      DCFptr  = 'D'
146500060315x   1c                   else
146600060315     c                   eval      DCFptr  = DCTptr
146700060315e   1c                   endif
146800060315     c                   eval      DCFfca  = I00fca
146900060314     c     *ISO          movel     wDate_EUR     DCFdfc
147000060315     c                   movel     wTime         DCFhfc
147100060315     c                   eval      DCFfev  = I00fgs
147200060315     c                   eval      DCFpru  = KNMUS
147300041109      * memorizzo il tipo invio
147400060314     c                   move      $Invio        DCFlet
147500060315     c***                clear                   DCFftr
147600060315     c***                clear                   DCFdtr
147700060315      *                  __________________
147800060315     c                   WRITE     FNDCF000
147900060315      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
148000060315     c                   ENDSR
148100060315
148200060315      *---------------------------------------------------------------*
148300060315      *?Aggiornamento TESTATA C.A.                                   ?*
148400060315      *---------------------------------------------------------------*
148500060315     c     Upd_DCT       BEGSR
148600980804      *
148700060315      * Richiamo pgm per determinare il P.O. che gestirà la prossima
148800060315      *   fase
148900060315     c                   clear                   FIDN05ds
149000060315     c                   eval      I05mod  = 'F'
149100060315     c                   eval      I05fca  = I00fca
149200060315     c                   eval      I05fpr  = I00fpr
149300060315     c                   eval      I05tpc  = DCTptr
149400060315     c                   eval      I05ffs  = 'O'
149500060315if  1c                   if        DCTnev  > *zeros
149600060315     c                   eval      I05fpe  = 'E'
149700060315x   1c                   else
149800060315     c                   eval      I05fpe  = 'N'
149900060315e   1c                   endif
150000060315if  1c                   if        DCTnde  > *zeros
150100060315     c                   eval      I05fde  = 'D'
150200060315x   1c                   else
150300060315     c                   eval      I05fde  = 'N'
150400060315e   1c                   endif
150500060315     c                   eval      I05tad  = I00tad
150600060314     c     *iso          movel     wDate_EUR     I05dta
150700981125      * valorizzo numero c.a.
150800060315     c                   eval      I05aac  = DCTaac
150900060315     c                   eval      I05fil  = DCTfil
151000060315     c                   eval      I05nca  = DCTnca
151100060315      *
151200060315     c                   call      'FIDN05R'
151300060315     c                   parm                    KPJBA
151400060315     c                   parm                    FIDN05ds
151500060315      *
151600060315if  1c                   IF        O05err  = *blanks
151700060315     c                   movel     O05rec        ds_FNDFA
151800060315sel 2c                   SELECT
151900060315w   2c                   WHEN      DFAgfs  = 'P'
152000060315     c                   movel     §DCTlnpc      DCTgfc
152100060315w   2c                   WHEN      DFAgfs  = 'A'
152200060315     c                   eval      DCTgfc  = DCTlna
152300060315w   2c                   WHEN      DFAgfs  = 'S'
152400060315     c                   eval      DCTgfc  = 46
152500060315x   2c                   OTHER
152600060315     c                   eval      DCTgfc  = DCTfil
152700060316e   2c                   ENDSL
152800060315e   1c                   ENDIF
152900060315      *
153000060315     c                   z-add     I00fca        DCTfca
153100060315     c                   clear                   DCTft1
153200060315     c                   clear                   DCTft2
153300060315      *                  __________________
153400060315     c                   EXCEPT    AggDCT
153500060315      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
153600090325      * se si tratta di preavviso di danno per un cliente
153700090325      * che gestisce in modo particolare le proprie pratiche
153800090325      * richiamo la routine di chiusura
153900090325
154000090325     c                   If        i00fca = 400
154100090325      *
154200090325     c                   clear                   TIBS02ds
154300090325     c                   eval      T02mod = 'C'
154400090325     c                   eval      T02sif = KNSIF
154500090325     c                   eval      T02cod = 'GPP'
154600090331     c                   move (p)  WDS_KSC       T02ke1
154700090325      *
154800090325     c                   call      'TIBS02R'
154900090325     c                   parm                    KPJBA
155000090325     c                   parm                    TIBS02ds
155100090325      * non trovato in tabella non faccio nulla
155200090325     c                   IF        T02err <> *blank
155300090325     c                   leavesr
155400090325     c                   endif
155500090325     c                   movel     T02uni        dGpp
155600090325      * verifico se c'è evento se è c'è fortuito  e sono esclusi dalla chiusura CA
155700090325     c                   if        §gppevf = 'S' and *in49
155800090325     c                   leavesr
155900090325     c                   endif
156000090325      * verifico il raggruppamento anomalia della CA
156100090325     c                   If        §tadragr = §gpprg1 or §tadragr = §gpprg2 or
156200090325     c                             §tadragr = §gpprg3
156300090325
156400090325      * eseguo la chiusura
156500090325     c                   exsr      SR_CHIUSCA
156600090325     c                   endif
156700090325     c                   endif
156800090325      *
156900090325     c
157000060315     c                   ENDSR
157100060314
157200090325      *---------------------------------------------------------------*
157300090325      * Chiusura CA clienti particolare
157400090325      *---------------------------------------------------------------*
157500090325     c     sr_CHIUSCA    BEGSR
157600090325     c* richiamo il PGM di chiusura
157700090325     c                   clear                   fidn22ds
157800090325     c                   eval      i22tri = 'R'                                 * richiamato
157900090325     c                   eval      i22mod = 'C'                                 * chiusura
158000090325     c                   eval      i22aac = dctaac                              * anno CA
158100090325     c                   eval      i22fil = dctfil                              * P.O. apertura CA
158200090325     c                   eval      i22nca = dctnca                              * Numero CA
158300090325     c                   eval      i22cch = §gppcch
158400090325     c                   eval      i22fgs = dctGFC
158500090325     c                   movel     fidn22ds      kpjbu
158600090325     c                   call      'FIDN22R'
158700090325     c                   parm                    kpjba
158800090325      *
158900090325     c                   endsr
159000060314      *---------------------------------------------------------------*
159100060314
159200060314     oFNDCT000  e            AGGDCT
159300060314      *
159400060314     o                       DCTgfc
159500060314     o                       DCTfca
159600060314     o                       DCTft1
159700060314     o                       DCTft2
159800060314
159900060314      *---------------------------------------------------------------*
160000060314
160100060314      *
160200060314      *?  S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E?
160300060314      *
160400060315** - $Cmd --------------------------------------------------------------------------*
160500060315OVRPRTF FIDN02P                  OUTQ(xxxxxxxxxx) FORMTYPE(xxxxxxxxxx) SAVE(*YES)     1
160600060315OVRPRTF FIDN02P                                   FORMTYPE(xxxxxxxxxx) SAVE(*YES)     2
160700060315OVRPRTF FIDN02P                  OUTQ(xxxxxxxxxx)                      SAVE(*YES)     3
160800060315OVRPRTF FIDN02P SPLFNAME(LETCOM) OUTQ(xxxxxxxxxx) FORMTYPE(xxxxxxxxxx)                4
160900060315OVRPRTF FIDN02P SPLFNAME(LETCOM)                  FORMTYPE(xxxxxxxxxx) SAVE(*YES)     5
161000060315OVRPRTF FIDN02P SPLFNAME(LETCOM) OUTQ(xxxxxxxxxx)                                     6
161100060315OVRPRTF FIDN02P SPLFNAME(LETCOM)                                       SAVE(*YES)     7
161200060315OVRPRTF FIDN02P                                                        SAVE(*YES)     8
161300060315OVRPRTF FIDN02PM OUTQ(          ) USRDFNDTA('                                         9
161400060314** - $CoTe
161500080111AL1  > Comunicaz. al Cliente per Pratica Assicurativa Avaria/Ammanco senza reso
161600060315AL5  > Lettera Accompagnam. Reso Merce Danneggiata (Avaria)
161700060315AE1  > Comunicaz. al Cliente per Pratica Assicurativa ESTERO
161800060315AD1  > Comunicaz. al Cliente per Pratica Assicurativa DPD
161900060315AM1  > Comunicaz. al Cliente per Pratica Assicurativa DPD Mancanza
