000100050727      *PARMS OPTION(*NOXREF) TGTRLS(*CURRENT)
000200051026      *PARMS DFTACTGRP(*NO)  ACTGRP(*CALLER)
000300050727      *===============================================================*
000400051026      *?FIDN88R * Stampa richiesta recupero C.A.                     ?*
000500050727      *===============================================================*
000600050727
000700050727     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000800050727
000900050727      *---------------------------------------------------------------*
001000050727
001100050727     fFIDN86D   cf   e             workstn
001200050728     f                                     include(FI86S02)
001300050728     f                                     include(FI86C02)
001400050727     f                                     sfile(FI86S02:S02nrr)
001500050728     f                                     infds(dsFMT)
001600050727      *
001700050728     fFNDET01L  Uf   e           k disk
001800051026     f                                     usropn  commit
001900050728     fFNDRA01L  Uf a e           k disk
002000051026     f                                     usropn  commit
002100050727      *
002200050727     fFNDCT01L  if   e           k disk
002300050727      *
002400050727     fTITAS30C  if   e           k disk
002500050727     fFNDKA01L  if   e           k disk
002600050727     fFNSPE01L  if   e           k disk
002700050727     fTNTMD01L  if   e           k disk
002800050727      *
002900050727     fAZORG01L  if   e           k disk
003000050727     fTABEL00F  if   e           k disk
003100050727      *
003200050728     fFIDN88P   o    e             printer oflind(*in01)
003300050727     f                                     infds(PrtfInf)
003400050727
003500050727      *---------------------------------------------------------------*
003600050727
003700050728      *
003800050728      * Costanti - - - - - - - - - - - - - - - - - - - - - - - - - - -*
003900050728      *
004000051026     d C_StrCmtCtl     c                   const('StrCmtCtl +
004100051026     d                                            lcklvl(*CHG) +
004200051026     d                                            cmtscope(*ACTGRP)')
004300051026     d C_EndCmtCtl     c                   const('EndCmtCtl')
004400050727      *
004500050727      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
004600050727      *
004700050727      *
004800050727      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
004900050727      *
005000050727     d KPJBA         e ds
005100050727      *
005200050727      * - Parametri x Controllo profilo utenti
005300050727     d TIBS34ds      e ds                  inz
005400050727      * - Ds di riferimento al file esterno AZUTE00F
005500050727     d AZUTEds       e ds                  extname(AZUTE00F)
005600050727      * - Ds per dati organigramma
005700050727     d DDatiUte      e ds
005800050727      *
005900050727      * - Parametri x Interrogazione tabelle
006000050728     d TIBS02ds      e ds                  inz
006100050727     d   T02mod      e                     inz('C')
006200050727     d   T02cod      e                     inz('TAD')
006300050728      * - Tab. "GED/DN"
006400050728     d dGEDdn        e ds                  inz
006500050727      * - Tab. "STD/4"
006600050727     d dSTD4         e ds                  inz
006700050727      * - Tab. "TAD"
006800050727     d dTAD          e ds                  inz
006900050727      * - Tab. "TB"
007000050727     d dsTB          e ds                  inz
007100050727      *
007200050727     d TIBS69ds      e ds                  inz
007300050727     d ds_CNACO      e ds                  inz extname(CNACO00F)
007400050727     d ds_CNIND      e ds                  inz extname(CNIND00F)
007500050727     d ds_CNCLP      e ds                  inz extname(CNCLP00F)
007600050727     d ds_FNCLS      e ds                  inz extname(FNCLS00F)
007700050727      *
007800050727     d ds_TNTMD      e ds                  inz extname(TNTMD00F)
007900050727      *
008000050727     d dDCT01        e ds                  inz
008100050727      *
008200050727     d Status         sds           333
008300050727     d   SDSpgm          *proc
008400050727      *
008500050728     d dsFMT           ds           512
008600050727     d  £Tasto               369    369
008700050727     d  £Nrg                 370    370
008800050727     d  £Ncl                 371    371
008900050727     d  £SFLnrr              378    379B 0
009000050727      *
009100050727     d PrtfInf         ds
009200050727     d  Cur_Line             367    368I 0
009300050728      *
009400050728     d ds_Evento       ds                  inz
009500050728     d  H2Caae                             inz
009600050728     d  H2Cnev                             inz
009700050727      *
009800050727     d ds_NumCA        ds                  inz
009900050728     d  H2Caac                             inz
010000050728     d  S2Cfil                             inz
010100050728     d  S2Cnca                             inz
010200050727      *
010300050727      * - Controllo/Inversione date
010400050727     d WLBdat          ds                  inz
010500050727     d   G08dat                       8  0 inz
010600050727     d   G08inv                       8  0 inz
010700050727     d   G08err                       1    inz('3')
010800050727     d   G08tgi                       5  0 inz
010900050727      *
011000050727      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
011100050727      *
011200050727      * - flag
011300051026     d $StrCmtCtl      s              1    inz(*off)
011400050727     d $First          s              1    inz(*on)
011500050727     d $Rivalsa        s              1    inz(*off)
011600050728     d $TIBS69R        s              1    inz(*off)
011700050728      * - indici di schiera
011800050727      * - Variabili riferite al data base o al display file
011900050728     d S02nrr          s                   like(C02rcd)   inz
012000050728     d w§DCTport       s                   like(§DCTport) inz
012100050728     d wCLI            s                   like(TASksc)   inz
012200050728     d wTAD            s                   like(DETtad)   inz
012300050728      * - Variabili definite internamente
012400050728     d wDate           s               d   inz  datfmt(*iso)
012500051026     d Qcmd            s             80    inz
012600051026     d Qlen            s             15  5 inz(%len(Qcmd))
012700050728     d w0040a          s              4  0 inz
012800050728     d w0040b          s              4  0 inz
012900050727      *
013000050727      * Key-List - - - - - - - - - - - - - - - - - - - - - - - - - - -*
013100050727      *
013200050727      * - FNDET01L
013300050727     c     K02DET01      klist
013400050727     c                   kfld                    H2Caae
013500050727     c                   kfld                    H2Cnev
013600050727      * - FNDRA01L / FNDCL01L
013700050727     c     K03DRA01      klist
013800050727     c                   kfld                    H2Caac
013900050727     c                   kfld                    S2Cfil
014000050727     c                   kfld                    S2Cnca
014100050727      * - TITAS30C / TNTMD01L
014200050727     c     K04TAS30      klist
014300050727     c                   kfld                    H2Caas
014400050727     c                   kfld                    H2Clnp
014500050727     c                   kfld                    H2Cnrs
014600050727     c                   kfld                    H2Cnsp
014700050727      * - FNDKA01L
014800050727     c     K04DKA01      klist
014900050727     c                   kfld                    H2Caac
015000050727     c                   kfld                    S2Cfil
015100050727     c                   kfld                    S2Cnca
015200050727     c                   kfld                    DKAtrc
015300050727      * - FNSPE01L
015400050727     c     K03SPE01      klist
015500050727     c                   kfld                    SPEfls
015600050727     c                   kfld                    SPEcli
015700050727     c                   kfld                    SPEcod
015800050727      * - TABEL00F
015900050727     c     K03TABEL      klist
016000050727     c                   kfld                    TBLkut
016100050728     c                   kfld                    TBLcod
016200050727     c                   kfld                    TBLkey
016300050727
016400050727      *---------------------------------------------------------------*
016500050727      *  RIEPILOGO INDICATORI                                         *
016600050727      *---------------------------------------------------------------*
016700050727      * 04    - Alta intensità riga di testo                          *
016800050727      * 10    - Comodo                                                *
016900050728      * 35    - Ripulisce il subfile S02                              *
017000050728      * 36    - NON emette il subfile S02                             *
017100050728      * 37    - Record modificato nel subfile S02 (sflnxtchg)         *
017200050728      * 38    - Fine dati nel subfile S02         (sflend)            *
017300051025      * 41    - Flag Esclusione Evento da Richiesta Recupero in (RI)  *
017400050727      *---------------------------------------------------------------*
017500050727
017600050727     c     *Entry        plist
017700050727     c                   parm                    KPJBA
017800050727      *
017900050727      * Operazioni iniziali
018000050727     c                   exsr      OpeIni
018100050727      *
018200050727      * Stampa
018300050727     c                   exsr      Stampa
018400050727      *
018500050727      * Fine
018600051026      * - Confermo le modifiche se è stata stampata almeno una c.a.
018700051026     c                   if        $First     =  *off
018800051026     c                   COMMIT
018900051026     c***                else
019000051026     c*** inutile:       ROLBK
019100051026     c                   endif
019200051026     c                   close     FNDET01L
019300051026     c                   close     FNDRA01L
019400051026      * - Chiudo il commit control se è stato avviato da questo pgm.
019500051026     c                   if        $StrCmtCtl =  *on
019600051026     c                   call      'QCMDEXC'
019700051026     c                   parm      C_EndCmtCtl   Qcmd
019800051026     c                   parm                    Qlen
019900051026     c                   eval      $StrCmtCtl =  *off
020000051026     c                   endif
020100051026      *
020200050727     c                   clear                   TIBS02ds
020300050728     c                   eval      T02tla   = 'C'
020400050727     c                   call      'TIBS02R'
020500050727     c                   parm                    KPJBA
020600050727     c                   parm                    TIBS02ds
020700050728     c                   if        $TIBS69R = *on
020800050728     c                   clear                   TIBS69ds
020900050728     c                   eval      I69tla   = 'C'
021000050728     c                   call      'TIBS69R'
021100050728     c                   parm                    TIBS69ds
021200050728     c                   endif
021300050727      *
021400050727     c                   movel     *on           *inLR
021500050727      *
021600050727      *---------------------------------------------------------------*
021700050727      * Operazioni Iniziali                                           *
021800050727      *---------------------------------------------------------------*
021900050727     c     OpeIni        BEGSR
022000051026      *
022100051026      * Avvio del controllo di sintonia (se NON già avviato)
022200051026     c*** già così:      eval      $StrCmtCtl =  *off
022300051026     c                   call(e)   'QCMDEXC'
022400051026     c                   parm      C_StrCmtCtl   Qcmd
022500051026     c                   parm                    Qlen
022600051026     c                   if        NOT %error
022700051026     c                   eval      $StrCmtCtl =  *on
022800051026     c                   endif
022900051026     c                   open      FNDET01L
023000051026     c                   open      FNDRA01L
023100050727      *
023200050727      * Reperimento dati del lavoro
023300050727     c                   exsr      DatiJob
023400050728      *
023500050728     c                   move      *date         wDate
023600050728      *
023700050728      * Impostazione campi fissi
023800050728     c                   eval      TBLkut = 1
023900050728     c                   eval      SPEfls = 'L'
024000050728     c                   eval      SPEcod = '006'
024100050728      *
024200050728      * Aggancio tabella GED (Gestionale: Divise)
024300050728     c                   clear                   dGEDdn
024400050728     c                   clear                   TIBS02ds
024500050728     c                   eval      T02mod = 'C'
024600050728     c                   eval      T02sif = KNSIF
024700050728     c                   eval      T02cod = 'GED'
024800050728     c                   movel(p)  'DANNI'       T02ke1
024900050728     c                   call      'TIBS02R'
025000050728     c                   parm                    KPJBA
025100050728     c                   parm                    TIBS02ds
025200050728     c                   if        T02err = *blanks
025300050728     c                   movel     T02uni        dGEDdn
025400050728     c                   endif
025500050727      *
025600050921      * Reperimento tab. "STD/4" = Dati A.I.G.
025700050727     c                   clear                   dSTD4
025800050727     c                   clear                   TIBS02ds
025900050728     c                   eval      T02mod = 'C'
026000050728     c                   eval      T02sif = KNSIF
026100050728     c                   eval      T02cod = 'STD'
026200050727     c                   movel(p)  '4'           T02ke1
026300050727     c                   call      'TIBS02R'
026400050727     c                   parm                    KPJBA
026500050727     c                   parm                    TIBS02ds
026600050727if  1c                   if        T02err = *blanks
026700050727     c                   movel     T02uni        dSTD4
026800050727e   1c                   endif
026900050727      *
027000050727     c                   ENDSR
027100050727      *
027200050727      *---------------------------------------------------------------*
027300050727      * Reperimento Dati del job (Utente/Operativi)                   *
027400050727      *---------------------------------------------------------------*
027500050727     c     DatiJob       BEGSR
027600050727      *
027700050727     c     *dtaara       define    §azute        azuteds
027800050727     c     *dtaara       define    §datiute      ddatiute
027900050727      *
028000050727     c                   in(E)     *dtaara
028100050727     c                   IF        %ERROR or RSUT = *blanks
028200050727     c                   clear                   Tibs34Ds
028300050727     c                   call      'TIBS34R'
028400050727     c                   parm                    Tibs34Ds
028500050727     c                   in        *dtaara
028600050727     c                   ENDIF
028700050727      *
028800050727     c                   ENDSR
028900050727      *
029000050727      *---------------------------------------------------------------*
029100050727      * Stampa elenco C.A.                                            *
029200050727      *---------------------------------------------------------------*
029300050727     c     Stampa        BEGSR
029400050727      *
029500050727      * Elaborazione sfl FIDN86D
029600050728     c                   eval      *in28  = *off
029700050728     c                   readc     FI86S02
029800050727      *
029900050727do  1c                   DOW       NOT %eof(FIDN86D)
030000050727      *
030100050728     c                   eval      *in37  = *off
030200050728     c                   movea     *zeros        *in(50)
030300050728     c                   z-add     S02nrr        C02rcd
030400050727      *
030500051025      * R = Richiesta recupero ad A.I.G.
030600050728if  2c                   if            S2Copz = 'R'
030700050728     c                             and S2Cesc = *blanks
030800050727     c                   exsr      StampaCA
030900050727e   2c                   endif
031000050727      *
031100050727      * Reimpostazione indicatori per DSPATR
031200050728     c     S2Cesc        comp      *blanks                            4141
031300050727      *
031400050727      * Aggiornamento sfl
031500050727if  2c                   if        *in28
031600050728     c                   eval      *in37  = *on
031700050728     c                   z-add     C02rcd        C02csr
031800050727x   2c                   else
031900050728     c                   clear                   S2Copz
032000050728     c                   eval      S2Cdrr = *date
032100050727e   2c                   endif
032200050728     c                   UPDATE    FI86S02
032300050727if  2c                   if        *in28
032400050727     c                   leave
032500050727e   2c                   endif
032600050727      *
032700050728     c                   readc     FI86S02
032800050727      *
032900050727e   1c                   ENDDO
033000050727      *
033100050727      * Stampa piede lettera
033200050727if  1c                   if        NOT *in28
033300050802     c                   exsr      StampaFin
033400050727e   1c                   endif
033500050727      *
033600050727     c                   ENDSR
033700050727      *
033800050727      *---------------------------------------------------------------*
033900051025      * Stampa intestazione per richiesta recupero ad A.I.G.          *
034000050727      *---------------------------------------------------------------*
034100050802     c     StampaIni     BEGSR
034200050728      *
034300050802     c                   clear                   DN88pI
034400050802     c                   clear                   DN88pO
034500050802     c*** solo testo:    clear                   DN88pT
034600050727      *
034700050727      * Reperimento dati evento
034800050727     c     K02DET01      chain     FNDET000
034900050727     c                   if        NOT %found(FNDET01L)
035000050914     c                             or  DETatb <> *blanks
035100050727      * ...impossibile !!!
035200050727     c                   clear                   FNDET000
035300050727     c                   endif
035400050727      *
035500050727      * Decodifica Tipo Anomalia
035600050727     c                   eval      wTAD = DETtad
035700050727     c                   exsr      DecodTAD
035800050727      *
035900050727      * Decodifica P.O. che ha Comunicato l'Evento
036000050727     c                   exsr      DecodFCE
036100050727      *
036200050727      * Salto ad inizio pagina
036300050802     c                   write     DN88pP
036400050727      *
036500050727      * Stampa intestazione lettera
036600050727     c                   eval      PICraa = §STD4rsc
036700050727     c                   eval      PICina = §STD4ind
036800050727     c                   eval      PICcaa = §STD4cap
036900050727     c                   eval      PICloa = §STD4loc
037000050727     c                   eval      PICpra = §STD4prv
037100050802     c                   eval      PICdal = 'Bologna, '
037200050727     c                                    + %editc(*date:'Y')
037300050802     c                   write     DN88pI
037400050801      *
037500050801     c                   do        2
037600050802     c                   write     DN88p0
037700050801     c                   enddo
037800050727      *
037900050727      * Stampa oggetto lettera
038000050914     c                   reset                   WLBdat
038100050914     c                   eval      G08inv = (H2Caae * 10000) + H2Cmge
038200050914     c                   call      'XSRDA8'
038300050914     c                   parm                    WLBdat
038400050727     c                   eval      POCogg = 'Evento nr. '
038500050727     c                                    + %trim(%editc(H2Cnev:'Z'))
038600050914     c                                    + ' del '
038700050914     c                                    + %trim(%editc(G08dat:'Y'))
038800050914     c                                    + '         '
038900050727     c                                    + 'Tipo anomalia: '
039000050923     c                                    + DETtad
039100050727     c                                    + '  '
039200050727     c                                    + %trim(§TADdest)
039300050802     c                   write     DN88pO
039400050801      *
039500050801     c                   eval      POCogg = 'P.O. che ha segnalato +
039600050801     c                                       l''evento:'
039700050801     c                                    + %editw(DETfce:'0   ')
039800050801     c                                    + '  '
039900050801     c                                    + %trim(ORGdes)
040000050802     c                   write     DN88pO
040100050802      *
040200050802      * Stampa inizio lettera
040300050802     c                   write     DN88pT
040400050727      *
040500050727     c                   ENDSR
040600050727      *
040700050727      *---------------------------------------------------------------*
040800051025      * Stampa elenco C.A. per richiesta recupero ad A.I.G.           *
040900050727      *---------------------------------------------------------------*
041000050727     c     StampaCA      BEGSR
041100050727      *
041200050727      * Stampa intestazione lettera
041300050727if  1c                   if        $First = *on
041400050802     c                   exsr      StampaIni
041500050727e   1c                   endif
041600050728      *
041700050802     c                   clear                   DN88pD
041800050727      *
041900051025      * Reperimento dati richiesta recupero
042000050727     c     K03DRA01      chain     FNDRA000
042100050727if  1c                   if        NOT %found(FNDRA01L)
042200050914     c                             or  DRAatb <> *blanks
042300050727     c                   clear                   FNDRA000
042400050727     c                   eval      DRAaac = H2Caac
042500050727     c                   eval      DRAfil = S2Cfil
042600050727     c                   eval      DRAnca = S2Cnca
042700050727e   1c                   endif
042800050727      *
042900050727      * Reperimento dati C.A.
043000050727     c     K03DRA01      chain     FNDCT000
043100050727if  1c                   if        NOT %found(FNDCT01L)
043200050914     c                             or  DCTatb <> *blanks
043300050727     c                   clear                   DCTmgc
043400050804     c                   clear                   DCTprn
043500050804     c                   clear                   DCTpra
043600050727     c                   clear                   DCTflo
043700050727e   1c                   endif
043800050727     c                   movel     DCTflo        dDCT01
043900050727      *
044000050727      * Reperimento dati LdV
044100050727     c     K04TAS30      chain     TITAS30C
044200050914if  1c*** impossibile:   if        NOT %found(TITAS30C)
044300050727     c***                clear                   TAS...
044400050727e   1c***                endif
044500050727      * - se tipo porto "recupero": rileggo
044600050727     c                   clear                   dsTB
044700050727     c                   movel     'TB'          TBLcod
044800050727     c                   movel(P)  TAStbl        TBLkey
044900050727     c     K03TABEL      chain     TABEL
045000050914if  1c                   if            %found(TABEL00F)
045100050914     c                             and TBLflg =  *blanks
045200050727     c                   movel     TBLuni        dsTB
045300050727e   1c                   endif
045400050727if  1c                   if        §TBrbl = 'R'
045500050727     c     K04TAS30      reade     TITAS30C
045600050727e   1c                   endif
045700050727      *
045800050727      * Decodifica beneficiario
045900050727     c                   exsr      sr_Benef
046000050727      *
046100050727      * Stampa elenco C.A.
046200050727      * - impostazione dati singola C.A.
046300050727      *   - Numero e data C.A.
046400050728     c                   eval      POCnca = %trim(%editw(DRAfil:'0   '))
046500050728     c                                    + '/'
046600050728     c                                    + %trim(%editc(DRAnca:'Z'))
046700050728     c                   reset                   WLBdat
046800050728     c                   eval      G08inv = (DCTaac * 10000) + DCTmgc
046900050728     c                   call      'XSRDA8'
047000050728     c                   parm                    WLBdat
047100050728if  1c                   if        G08err = *off
047200050728     c                   eval      POCdca = G08dat
047300050728e   1c                   endif
047400050804      *   - Numero e data Pratica Assicurativa
047500050804if  1c                   if        DCTprn > *zeros
047600050804     c                   eval      POCprn = 'Pratica assicurativa nr. '
047700050804     c                                    + %trim(%editc(DCTprn:'Z'))
047800050804     c                                    + ' Anno '
047900050804     c                                    + %trim(%editc(DCTpra:'Z'))
048000050804e   1c                   endif
048100050804      *
048200050728      *   - Importo liquidato
048300050728     c                   eval      POCipl = %trim(%editc(S2Cipl:'1'))
048400050728     c                                    + ' '
048500050728     c                                    + §GEDdba
048600050727      *
048700050727      * - test OF
048800050727if  1c                   if        *in01  = *on
048900050802     c                   write     DN88pP
049000050727do  2c                   do        2
049100050802     c                   write     DN88p0
049200050727e   2c                   enddo
049300050727     c                   eval      *in01  = *off
049400050727e   1c                   endif
049500050727      * - stampa dati singola C.A.
049600050802     c                   write     DN88pD
049700050727      *
049800050727      * Aggiornamento flag DETFTR in FNDET00F (per evento)
049900050727     c                   if        $First = *on
050000050727     c                   eval      DETftr = 'S'
050100050727     c                   UPDATE    FNDET000
050200050727     c                   eval      $First = *off
050300050727     c                   endif
050400050727      *
050500050727      * Aggiornamento flag DRAESC in FNDRA00F (per singola C.A.)
050600050728     c                   move      wDate         DRAdrr
050700050728if  1c                   if        NOT %found(FNDRA01L)
050800050728     c                   WRITE     FNDRA000
050900050728x   1c                   else
051000050727     c                   UPDATE    FNDRA000
051100050728e   1c                   endif
051200050727      *
051300050727     c                   ENDSR
051400050728      *
051500050728      *---------------------------------------------------------------*
051600050802      * Stampa piede lettera                                          *
051700050728      *---------------------------------------------------------------*
051800050802     c     StampaFin     BEGSR
051900050728      *
052000050802      * Stampa fine lettera
052100051026     c                   write     DN88pF
052200050728      *
052300050728     c                   ENDSR
052400050727      *
052500050727      *---------------------------------------------------------------*
052600050727      * Decodifica / Controllo TIPO ANOMALIA                          *
052700050727      *---------------------------------------------------------------*
052800050727     c     DecodTAD      BEGSR
052900050727      *
053000050727     c                   clear                   dTAD
053100050727     c                   reset                   TIBS02ds
053200050727     c                   movel     KNSIF         T02sif
053300050727     c*** già impostato: movel     'TAD'         T02cod
053400050727     c                   movel(p)  wTAD          T02ke1
053500050727     c                   call      'TIBS02R'
053600050727     c                   parm                    KPJBA
053700050727     c                   parm                    TIBS02ds
053800050727     c                   if        T02err   = *blanks
053900050727     c                   movel     T02uni        dTAD
054000050727     c                   endif
054100050727      *
054200050727     c                   if        §TADdest = *blanks
054300050727     c                   movel(p)  §TADdesc      §TADdest
054400050727     c                   endif
054500050727      *
054600050727     c                   ENDSR
054700050727      *
054800050727      *---------------------------------------------------------------*
054900050727      * Decodifica P.O. che ha Comunicato l'Evento                    *
055000050727      *---------------------------------------------------------------*
055100050727     c     DecodFCE      BEGSR
055200050727      *
055300050727     c     DETfce        chain     AZORG
055400050727     c                   if        NOT %found(AZORG01L)
055500050727     c                             or  ORGfva <> *blanks
055600050727     c                   clear                   ORGdes
055700050727     c                   endif
055800050727      *
055900050727     c                   ENDSR
056000050727      *
056100050727      *---------------------------------------------------------------*
056200050727      * Reperimento dati cliente beneficiario                         *
056300050727      *---------------------------------------------------------------*
056400050727     c     sr_Benef      BEGSR
056500050727      *
056600050727     c                   clear                   wCli
056700050727     c                   reset                   $Rivalsa
056800050727      *
056900050727      * Dalla spedizione determino il codice beneficiario
057000050727      *
057100050727      * Per recuperare Mitt/Dest utilizzo il porto sped. che coincide con
057200050727      *  quello C.A. tranne se la sped. è "Mamma di cambio porto" e mi trovo
057300050727      *  in filiale, in questo caso inverto il porto C.A.
057400050727sel 1c                   select
057500050727w   1c                   when      TAScca    <> '9'
057600050727     c                   eval      w§DCTport =  §DCTport
057700050727w   1c                   when      §DCTport  =  'A'
057800050727     c                   eval      w§DCTport =  'F'
057900050727w   1c                   when      §DCTport  =  'F'
058000050727     c                   eval      w§DCTport =  'A'
058100050727e   1c                   endsl
058200050727      *
058300050727sel 1c                   select
058400050727      * - destinatario / pf
058500050727w   1c                   when          DCTptr    = 'D'
058600050727     c                             and w§DCTport = 'F'
058700050727      * - mittente     / pa
058800050727w   1c                   when          DCTptr    = 'M'
058900050727     c                             and w§DCTport = 'A'
059000050727     c                   eval      wCli      =  TASccm
059100050727x   1c                   other
059200050727     c                   eval      wCli      =  TASksc
059300050727e   1c                   endsl
059400050727      *
059500050727      * Scorporo i codici cliente
059600050727     c                   move      wCli          w0040a
059700050727     c                   move      DCTksc        w0040b
059800050727      *
059900050727      * Controllo se ho rivalsa
060000050727     c                   eval      DKAtrc    =  'R'
060100050728     c     K04DKA01      chain     FNDKA000
060200050914if  1c                   if            %found(FNDKA01L)
060300050914     c                             and DKAatb =  *blanks
060400050727     c                   eval      $Rivalsa  =  *on
060500050727e   1c                   endif
060600050727      *
060700050727      *
060800050727sel 1c                   SELECT
060900050727      * Rivalsa
061000050727      * -------
061100050727w   1c                   WHEN      $Rivalsa  =  *on
061200050727     c                   exsr      Benef_DKA
061300050727     *** *   se beneficiario alternativo codificato memorizzo il codice
061400050727     *** *   per ricavare la società
061500050727     ***c                   if             w0040b >  *zeros
061600050727     ***c                             and  w0040b <> 8888
061700050727     ***c                   eval      wCli      =  DCTksc
061800050727     ***c                   endif
061900050727      *
062000050727      * Beneficiario alternativo generico
062100050727      * ---------------------------------
062200050727w   1c                   WHEN      w0040b    =  8888
062300050727     c                   eval      DKAtrc    =  'C'
062400050727     c     K04DKA01      chain     FNDKA000
062500050914if  2c                   if            %found(FNDKA01L)
062600050914     c                             and DKAatb =  *blanks
062700050727     c                   exsr      Benef_DKA
062800050728e   2c                   endif
062900050727      *
063000050727      * Beneficiario alternativo codificato
063100050727      * -----------------------------------
063200050727w   1c                   WHEN      DCTksc    >  *zeros
063300050727     c                   eval      wCli      =  DCTksc
063400050727     c                   clear                   TIBS69ds
063500050727     c                   eval      I69kac    =  wCli
063600050727     c                   eval      I69kin    =  wCli
063700050727     c                   eval      I69kcs    =  wCli
063800050728     c                   exsr      Rep_Anagra
063900050727     c                   movel     ACOrag        POCrsb
064000050727     c                   movel     INDvia        POCinb
064100050727     c                   movel     INDcae        POCcab
064200050727     c                   movel     INDcit        POClob
064300050727if  2c                   if        INDsta    =  *blanks
064400050727     c                   movel     INDprv        POCprb
064500050727x   2c                   else
064600050727     c                   movel     INDsta        POCnab
064700050727e   2c                   endif
064800050727      * - controllo se esiste luogo 6
064900050727     c     K03SPE01      chain     FNSPE000
065000050914if  2c                   if            %found(FNSPE01L)
065100050914     c                             and SPEflg  = *blanks
065200050727     c                   movel     SPErag        POCrsb
065300050727     c                   movel     SPEind        POCinb
065400050727     c                   movel     SPEcap        POCcab
065500050727     c                   movel     SPEloc        POClob
065600050727if  3c                   if        SPEnaz    =  *blanks
065700050727     c                   movel     SPEpro        POCprb
065800050727x   3c                   else
065900050727     c                   movel     SPEnaz        POCnab
066000050727e   3c                   endif
066100050727e   2c                   endif
066200050727      *
066300050727      * Cliente in spedizione
066400050727      * ---------------------
066500050727x   1c                   OTHER
066600050727     c                   clear                   TIBS69ds
066700050727     c                   eval      I69kcs    =  wCli
066800050728     c                   exsr      Rep_Anagra
066900050727     c                   eval      *in10 = *off
067000050727if  2c                   if             w0040a >  *zeros
067100050727     c                             and  w0040a <> 8888
067200050727     c                             and  w0040a <> 9999
067300050727      * - indirizzo da luogo 6
067400050727     c     K03SPE01      chain     FNSPE000                           10
067500050727x   2c                   else
067600050727     c                   eval      *in10     =  *on
067700050727e   2c                   endif
067800050727if  2c                   IF        not *in10
067900050727     c                   movel     SPErag        POCrsb
068000050727     c                   movel     SPEind        POCinb
068100050727     c                   movel     SPEcap        POCcab
068200050727     c                   movel     SPEloc        POClob
068300050727if  3c                   if        SPEnaz    =  *blanks
068400050727     c                   movel     SPEpro        POCprb
068500050727x   3c                   else
068600050727     c                   movel     SPEnaz        POCnab
068700050727w   3c                   endif
068800050727x   2c                   ELSE
068900050727      * - Aggancio file mittenti/destinatari
069000050727     c     K04TAS30      chain     TNTMD000
069100050727if  3c                   if        NOT %found(TNTMD01L)
069200050727     c                   clear                   ds_TNTMD
069300050727e   3c                   endif
069400050727      *   - indirizzo da destinatario spedizione
069500050727if  3c                   if        DCTptr    =  'D'
069600050727     c                   movel     TMDrsd        POCrsb
069700050727     c                   movel     TMDind        POCinb
069800050727     c                   movel     TMDcad        POCcab
069900050727     c                   movel     TMDlod        POClob
070000050727if  4c                   if        TMDnzd    =  *blanks
070100050727     c                   movel     TMDprd        POCprb
070200050727x   4c                   else
070300050727     c                   movel     TMDnzd        POCnab
070400050727e   4c                   endif
070500050727x   3c                   else
070600050727      *   - indirizzo da mittente spedizione
070700050727     c                   movel     TMDrsm        POCrsb
070800050727     c                   movel     TMDinm        POCinb
070900050727     c                   movel     TMDcam        POCcab
071000050727     c                   movel     TMDlom        POClob
071100050727if  4c                   if        TMDnzm    =  *blanks
071200050727     c                   movel     TMDprm        POCprb
071300050727x   4c                   else
071400050727     c                   movel     TMDnzm        POCnab
071500050727e   4c                   endif
071600050727e   3c                   endif
071700050727      *
071800050727e   2c                   ENDIF
071900050727      *
072000050727e   1c                   ENDSL
072100050727      *
072200050727     c                   ENDSR
072300050727      *
072400050727      *---------------------------------------------------------------*
072500050727      * Decodifica beneficiario da FNDKA                              *
072600050727      *---------------------------------------------------------------*
072700050727     c     Benef_DKA     BEGSR
072800050727      *
072900050728     c                   movel     DKArag        POCrsb
073000050728     c                   movel     DKAvia        POCinb
073100050728     c                   movel     DKAcap        POCcab
073200050728     c                   movel     DKAloc        POClob
073300050727     c                   if        DKAnaz = *blanks
073400050728     c                   movel     DKAprv        POCprb
073500050727     c                   else
073600050728     c                   movel     DKAnaz        POCnab
073700050727     c                   endif
073800050727      *
073900050727     c                   ENDSR
074000050728      *
074100050728      *---------------------------------------------------------------*
074200050728      * Reperimento dati anagrafici                                   *
074300050728      *---------------------------------------------------------------*
074400050728     c     Rep_Anagra    BEGSR
074500050728      *
074600050728     c                   clear                   ds_CNACO
074700050728     c                   clear                   ds_CNIND
074800050728     c                   clear                   ds_CNCLP
074900050728     c                   clear                   ds_FNCLS
075000050728      *
075100050728     c                   call      'TIBS69R'
075200050728     c                   parm                    TIBS69ds
075300050728     c                   parm                    ds_CNACO
075400050728     c                   parm                    ds_CNIND
075500050728     c                   parm                    ds_CNCLP
075600050728     c                   parm                    ds_FNCLS
075700050728      *
075800050728     c                   eval      *in35 = (O69err = *on)
075900050728      *
076000050728     c                   eval      $TIBS69R = *on
076100050728      *
076200050728     c                   ENDSR
