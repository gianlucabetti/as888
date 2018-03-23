000100050617     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200050617      *---------------------------------------------------------------*
000300050708      *?D O C U M E N T O   D I   R I T I R O                        ?*
000400050617      *---------------------------------------------------------------*
000500050617      *ATTENZIONE:?                                                  *
000600050617      * Questo programma forza la stampa con il nuovo modulo          *
000700050617      * per stampanti ad aghi.                                        *
000800050617      *---------------------------------------------------------------*
000900050624
001000050617     fFNLS11D   cf   e             workstn
001100050617     fAZORG01L  if   e           k disk
001200050617     fTABEL00F  if   e           k disk
001300050624     fFLNUF01L  if   e           k disk
001400050617
001500050617      *---------------------------------------------------------------*
001600050617
001700050617      *
001800050617      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - - *?
001900050617      *
002000050617     d C_PDC           c                   const('su Piano dei Conti')
002100050617      *
002200050617      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *?
002300050617      *
002400050617      * - Giorni validi
002500050617     d $GG             s              1    dim(8)  ctdata perrcd(8)
002600050617      * - Messaggi di errore
002700050708     d $Msg            s             78    dim(45) ctdata perrcd(1)
002800050617      *
002900050617      *?  D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *?
003000050617      *
003100050617     d Status         sds
003200050624     d  SDSpgm           *proc
003300050617      *
003400050617      * - Parametri
003500050617     d KPJBA         e ds
003600050617      *
003700050617      * - Parametri x Controllo profilo utenti
003800050617     d TIBS34ds      e ds
003900050617      * - Ds di riferimento al file esterno AZUTE00F
004000050617     d AZUTEds       e ds                  extname(AZUTE00F)
004100050617      * - Ds per dati organigramma
004200050617     d dDATIUTE      e ds
004300050617      *
004400050617      * - Parametri per pgm. TIBS69R (decodifica cliente)
004500050617     d TIBS69ds      e ds                  inz
004600050617     d CNACOds       e ds                  extname(CNACO00F) inz
004700050617     d CNINDds       e ds                  extname(CNIND00F) inz
004800050617     d CNCLPds       e ds                  extname(CNCLP00F) inz
004900050617     d FNCLSds       e ds                  extname(FNCLS00F) inz
005000050617      * - Descrizione 143 dell'ORGANIGRAMMA
005100050617     d og143         e ds                  inz
005200050624      *
005300050617      * - Parametri per TIBS02R - Gestione TNTBE00F
005400050617     d TIBS02ds      e ds
005500050617      * - Tab. GED (ds gestionale: divise)
005600050617     d dGED          e ds                  inz
005700050617      *
005800050617     d dsCV          e ds
005900050617     d ds1A          e ds
006000050617     d ds3A          e ds
006100050617     d ds5E          e ds
006200050617      *
006300050617      * - Controllo C/Assegno
006400050617     d TRUL21ds      e ds
006500050617      * - Controllo Valore da Assicurare
006600050617     d TRUL22ds      e ds
006700050617      *
006800051011      * - Parametri per stampa Documento di Ritiro
006900050620     d FNLSB5ds      e ds                  inz
007000050617      * - Parametri per controllo di un CAP
007100050617     d FNLV13ds      e ds                  inz
007200050620     d TISI95ds      e ds                  inz
007300050617      * - Parametri per controllo di un indirizzo completo senza CAP
007400050617     d FNLV14ds      e ds                  inz
007500050617      *
007600060705     d TRULTICDS     e ds                  inz
007700060705      *
007800050617      * - Parametri per FNLV06R (creazione numeratori per P.O.)
007900050617     d FNLV06ds        ds                  inz
008000050617      *   Anno
008100050617     d  D06aas                 1      2  0 inz
008200050617      *   P.O.
008300050617     d  D06fil                 3      5  0 inz
008400050617      *   Codice P.O. elaboratore di PARFIL
008500050620     d  D06fel                 6      8  0 inz
008600050617      *   Codice Numeratore
008700050620     d  D06cnu                 9     11    inz
008800050617      *   Tipo P.O.
008900050620     d  D06ftf                12     12    inz
009000050627      *
009100050627      * Codice Cliente
009200050627     d                 ds                  inz
009300050627     d   wFLC                         3s 0 inz
009400050627     d   wCCM                         4s 0 inz
009500050627     d ds_KSC                  1      7s 0 inz
009600050617      *
009700050630      * Parametri per controllo data e trasform. in giorni - XSRDA8 -
009800050617     d WLBdat          ds                  inz
009900050617     d  G02dat                 1      8  0 inz
010000050617     d  G02inv                 9     16  0 inz
010100050617     d  G02err                17     17    inz
010200050617     d  G02tgi                18     22  0 inz
010300050630      *
010400050630      * Ora consegna richiesta
010500050630     d W1Cocr          ds             4    inz
010600050630     d  W1Chcr                 1      2  0 inz
010700050630     d  W1Cmcr                 3      4  0 inz
010800050617      *
010900050617      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *?
011000050617      *
011100050624      * - Flags
011200050624     d $Fine           s              1    inz(*off)
011300050624     d $InzD01         s              1    inz(*on)
011400050624     d $InzD02         s              1    inz(*on)
011500050627     d WinKG           s              1    inz(*off)
011600050627     d Win90           s              1    inz(*off)
011700050624     d ForzaCAS        s              1    inz(*off)
011800050624     d ForzaIAS        s              1    inz(*off)
011900050624      *
012000050624     d $Video          s              1    inz('1')
012100050624      *
012200050617     d Win4080         s             41    inz(*zeros)
012300050624      *
012400050624     d w0133           s             13  3 inz
012500050624     d w0033           s              3  3 inz
012600050624     d w0020           s              2  0 inz
012700050624     d w0010           s              1  0 inz
012800050624     d w0140           s             14  0 inz
012900050624     d wDtGio          s              8  0 inz
013000050624     d DateU           s              8  0 inz
013100050624     d W1Cdsp          s              8  0 inz
013200050624     d W2Cdsp          s              8  0 inz
013300050624     d W1Cdcr          s                   like(DB0dcr)  inz
013400050630     d W15eft          s                   like(O14eft)  inz
013500050624     d sav_V1Ccas      s                   like(V1Ccas)  inz
013600050624     d sav_V1Cias      s                   like(V1Cias)  inz
013700050624     d SAVcmo          s                   like(V1Ccad)  inz
013800050627     d SAVfzv1         s                   like(E13fzv)  inz
013900050627     d SAVfzvM         s                   like(E13fzv)  inz
014000050627     d SAVfz1M         s                   like(E13fz1)  inz
014100050627     d SAVfz2M         s                   like(E13fz2)  inz
014200050627     d SAVfzvD         s                   like(E13fzv)  inz
014300050627     d SAVfz1D         s                   like(E13fz1)  inz
014400050627     d SAVfz2D         s                   like(E13fz2)  inz
014500050627     d SAVccm          s                   like(V1Cccm)  inz
014600050627     d VARcad          s                   like(V1Ccad)  inz
014700050627     d VARcam          s                   like(V1Ccad)  inz
014800050627     d VARlod          s                   like(V1Clod)  inz
014900050627     d VARlom          s                   like(V1Clom)  inz
015000050620     d wVLT            s                   like(V1Cvca)  inz
015100050620     d wGC1            s                   like(V1Cgc1)  inz
015200050620     d wGC2            s                   like(V1Cgc2)  inz
015300050620     d wRMO            s                   like(V1Crmo)  inz
015400050620     d wCMO            s                   like(V1Ccmo)  inz
015500050620     d wNAR            s                   like(V1Cnmo)  inz
015600050627     d wCLI            s              1    inz
015700050624     d WdesIE          s              1    inz
015800050624     d wGCx            s              1    inz
015900050627     d wERR            s              1    inz
016000050617      *
016100050617      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - - *?
016200050617      *
016300050617      * - TABEL
016400050620     c     KTABEL        klist
016500050617     c                   kfld                    TBLkut
016600050617     c                   kfld                    TBLcod
016700050617     c                   kfld                    TBLkey
016800050617      * - FLNUF01L
016900050617     c     K04FLNUF      klist
017000050617     c                   kfld                    NUFaaa
017100050617     c                   kfld                    NUFcnu
017200050617     c                   kfld                    NUFfil
017300050617     c                   kfld                    NUFfl2
017400011211
017500050617      *===============================================================*
017600050617      *?RIEPILOGO INDICATORI                                         ?*
017700050617      *---------------------------------------------------------------*
017800050617      * 01    - Abilita la Ragione Sociale per la ricerca alfabetica  *
017900050617      * 02    - Abilita tutto il Codice Mittente per l'inserimento    *
018000050617      * 28    - Emissione campo messaggio                             *
018100050617      * 40/80 - Posizionamento cursore sul campo errato               *
018200050617      * 90    - Riemissione videata                                   *
018300050617      *===============================================================*
018400050617
018500050617     c     *Entry        plist
018600050617     c                   parm                    KPJBA
018700050624      *
018800050624     c                   exsr      OpeIni
018900050624      *
019000050624      * Gestione Video
019100050624do  1c                   DOW       $Fine = *off
019200050624      * - Dati in stampa
019300050624cas 2c     $Video        caseq     '1'           GesD01
019400050624      * - Richiesta numero copie
019500050624cas 2c     $Video        caseq     '2'           GesD02
019600050624e   2c                   endcs
019700050624e   1c                   ENDDO
019800050624      *
019900050624      * Fine
020000050624     c                   clear                   FNLV13ds
020100050624     c                   eval      I13tla = 'C'
020200050624     c                   call      'FNLV13R'
020300050624     c                   parm                    KPJBA
020400050624     c                   parm                    FNLV13ds
020500050624     c                   parm                    TISI95ds
020600050624      *
020700050624     c                   clear                   FNLV14ds
020800050624     c                   eval      I14tla = 'C'
020900050624     c                   call      'FNLV14R'
021000050624     c                   parm                    KPJBA
021100050624     c                   parm                    FNLV14ds
021200050624      *
021300050624     c                   clear                   TIBS69ds
021400050624     c                   eval      I69TLA  = 'C'
021500050624     c                   call      'TIBS69R'
021600050624     c                   parm                    TIBS69ds
021700050624     c                   parm                    CNACOds
021800050624     c                   parm                    CNINDds
021900050624     c                   parm                    CNCLPds
022000050624     c                   parm                    FNCLSds
022100050624      *
022200060705     c
022300060705     C                   CLEAR                   trulTICds
022400060705     C                   MOVEL     'C'           IticTLA
022500060705     C                   CALL      'TRULTICR'
022600060705     C                   PARM                    TRULTICDS
022700060705     C                   PARM                    DS1A
022800060705     C*
022900060705     C*
023000050624     c                   eval      *inLR = *on
023100050624      *
023200050624      *---------------------------------------------------------------*
023300050624      * OPERAZIONI INIZIALI                                           *
023400050624      *---------------------------------------------------------------*
023500050624     c     OPEINI        BEGSR
023600050624      *
023700050624      * Imposto i campi chiave "fissi"
023800050624     c                   z-add     1             TBLkut
023900050624     c                   clear                   NUFaaa
024000050624     c                   z-add     20            NUFcnu
024100050624     c                   clear                   NUFfl2
024200050624      *
024300050624      * Reperisco dati aziendali
024400050624     c     *dtaara       define    §azute        AZUTEds
024500050624     c     *dtaara       define    §datiute      dDATIUTE
024600050624     c                   in(E)     *dtaara
024700050624     c                   if        %ERROR or RSUT = *blanks
024800050624     c                   clear                   Tibs34Ds
024900050624     c                   call      'TIBS34R'
025000050624     c                   parm                    Tibs34Ds
025100050624     c                   in        *dtaara
025200050624     c                   endif
025300050624      *
025400050624      * Reperisco la data corrente
025500050630     c                   time                    w0140
025600050624      * UDATE in gg/mm/aaaa
025700050624     c                   move      w0140         wDtGio
025800050624      * UDATE in aaaa/mm/gg
025900050624     c                   clear                   WLBdat
026000050624     c                   z-add     wDtGio        G02dat
026100050624     c                   call      'XSRDA8'
026200050624     c                   parm                    WLBdat
026300050624     c                   movel     G02inv        DateU
026400050624      *
026500050624      * Aggancio tab."GED" per divisa della moneta di conto
026600050624     c                   clear                   TIBS02ds
026700050624     c                   movel     'C'           T02mod
026800050624     c                   movel     KNSIF         T02sif
026900050624     c                   movel     'GED'         T02cod
027000050624     c                   movel     '1'           T02ke1
027100050624     c                   call      'TIBS02R'
027200050704     c                   parm                    KPJBA
027300050624     c                   parm                    TIBS02ds
027400050624     c                   if        T02err = *blanks
027500050624     c                   movel     T02uni        dGED
027600050624     c                   endif
027700050624      *
027800050624      * Tabella CV della moneta di conto
027900050624     c                   movel     'CV'          TBLcod
028000050624     c                   movel(p)  §GEDcn        TBLkey
028100050624     c                   exsr      CTABEL
028200050624     c                   if        NOT *in30
028300050624     c                   movel     TBLuni        dsCV
028400050624     c                   endif
028500050624      *
028600050624      * Se NON c'è il numeratore: lo creo
028700050624     c     K04FLNUF      setll     FLNUF
028800050624if  1c                   if        NOT %equal(FLNUF01L)
028900050624     c                   clear                   FNLV06ds
029000050624     c                   move      DUTpou        D06fil
029100050624     c                   move      DUTtfp        D06fel
029200050624     c                   movel     '020'         D06cnu
029300050624     c                   movel     'P'           D06ftf
029400050624     c                   movel(P)  FNLV06ds      KPJBU
029500050624     c                   call      'FNLV06R'
029600050624     c                   parm                    KPJBA
029700050624e   1c                   endif
029800050624      *
029900050624     c                   movel     SDSpgm        V1Cpgm
030000050624      *
030100050624     c                   ENDSR
030200050624      *
030300050624      *---------------------------------------------------------------*
030400050624      * Gestione videata D01                                          *
030500050624      *---------------------------------------------------------------*
030600050624     c     GesD01        BEGSR
030700050624      *
030800050624      * Inizializzo la videata
030900050624if  1c                   if        $InzD01  = *on
031000050624     c                   exsr      InzD01
031100050624     c                   movel     *off          $InzD01
031200050624e   1c                   endif
031300050624      *
031400050624      * Emetto la testata
031500050624     c                   write     LS11T01
031600050624      * Emetto la videata
031700050624     c                   exfmt     LS11D01
031800050624     c                   setoff                                       28  90
031900050624     c                   movea     Win4080       *in(40)
032000050624     c                   clear                   V1Cmsg
032100050624      *
032200050624      * F3=Fine
032300050624if  1c                   if        *inKC
032400050624     c                   eval      $Fine = *on
032500050624     c                   goto      EndGesD01
032600050624e   1c                   endif
032700050624      *
032800050624      * F7=Ricerca Mittente
032900050624if  1c                   if        *inKG
033000050624     c                   eval      *in01 = *on
033100050701     c                   eval      *in63 = *on
033200050627     c                   eval      WinKG = *on
033300050624     c                   goto      EndGesD01
033400050624e   1c                   endif
033500050624      *
033600050624      * Controllo dati immessi a video
033700050624     c                   exsr      CtrD01
033800050624     c     *in90         cabeq     *on           EndGesD01
033900050624      *
034000050624      * F6=Conferma
034100050624if  1c                   if        *inKF
034200050624     c                   eval      $Video = '2'
034300050624     c                   reset                   $InzD02
034400050624e   1c                   endif
034500050624      *
034600050624     c     EndGesD01     ENDSR
034700050624      *
034800050624      *---------------------------------------------------------------*
034900050624      * Inizializzazione videata D01                                  *
035000050624      *---------------------------------------------------------------*
035100050624     c     InzD01        BEGSR
035200050624      *
035300050624     c                   clear                   LS11D01
035400050624      *
035500050624      * Imposto i dati di default
035600050624      * - P.O. cliente
035700050624     c                   move      DUTtfp        V1Cflc
035800050624      * Propongo la divisa di conto gestionale nei campi divisa
035900050624     c                   eval      V1Cvca = §GEDcn
036000050624     c                   eval      V1Cvas = §GEDcn
036100050624      *
036200050624     c                   ENDSR
036300050624      *
036400050624      *---------------------------------------------------------------*
036500050624      * Controllo dati immessi in videata D01                         *
036600050624      *---------------------------------------------------------------*
036700050624     c     CtrD01        BEGSR
036800050624      *
036900050624     c                   clear                   W1Cdsp
037000050624     c                   clear                   W2Cdsp
037100050624     c                   clear                   W1Cdcr
037200050630      *
037300050630      * DATA DOCUMENTO (Data Spedizione)
037400050630if  1c                   IF        V1Cdsp >  *zeros
037500050630     c                   clear                   WLBdat
037600050630     c                   eval      G02dat =  V1Cdsp
037700050630     c                   call      'XSRDA8'
037800050630     c                   parm                    WLBdat
037900050630if  2c                   if        G02err =  *on
038000050630     c                   seton                                        409028
038100050630     c                   movel     $Msg(1)       V1Cmsg
038200050630     c                   leavesr
038300050630x   2c                   else
038400050630     c                   eval      V1Cdsp =  G02dat
038500050630     c                   eval      W1Cdsp =  G02inv
038600050630     c                   eval      W2Cdsp =  G02inv
038700050630e   2c                   endif
038800050630x   1c                   ELSE
038900060131     c***                eval      W1Cdsp =  DateU
039000060131     c                   eval      W2Cdsp =  DateU
039100050630e   1c                   ENDIF
039200050630      *
039300050630      * CODICE BOLLA
039400050630     c                   clear                   ds3A
039500050630if  1c                   IF        V1Ccbo <> *blanks
039600050630     c                   eval      TBLcod =  '3A'
039700050630      * - Ricerca
039800050630     c     '?'           scan      V1Ccbo                                 30
039900050630if  2c                   if        *in30
040000050630     c                   exsr      CX§TAB
040100050630     c                   movel     TBLkey        V1Ccbo
040200050630     c                   seton                                        41  90
040300050630     c                   leavesr
040400050630e   2c                   endif
040500050630      * - Controllo
040600050630     c                   movel(p)  V1Ccbo        TBLkey
040700050630     c                   exsr      CTABEL
040800050630if  2c                   if        *in30
040900050630     c                   movel     $Msg(2)       V1Cmsg
041000050630     c                   seton                                        412890
041100050630     c                   leavesr
041200050704e   2c                   endif
041300050630     c                   movel     TBLuni        ds3A
041400050630      *   - vedo se causale utilizzabile
041500050630if  2c                   if        §3Aucb =  'N'
041600050630     c                   movel     $Msg(2)       V1Cmsg
041700050630     c                   seton                                        412890
041800050630     c                   leavesr
041900050630e   2c                   endif
042000050630e   1c                   ENDIF
042100050630      *
042200050630      * TIPO SERVIZIO
042300050630     c                   clear                   ds5E
042400050630if  1c                   IF        V1Ctsp <> *blanks
042500050630     c                   eval      TBLcod =  '5E'
042600050630      * - Ricerca
042700050630     c     '?'           scan      V1Ctsp                                 30
042800050630if  2c                   if        *in30
042900050630     c                   exsr      CX§TAB
043000050630     c                   movel     TBLkey        V1Ctsp
043100090604     c**!!!              movel     §Des          V1Dtsp
043200050630     c                   seton                                        42  90
043300090604     c**!!!              leavesr
043400050630e   2c                   endif
043500050630      * - Controllo
043600050630     c                   movel(p)  V1Ctsp        TBLkey
043700050630     c                   exsr      CTABEL
043800050630if  2c                   if        *in30
043900050630     c                   movel     $Msg(3)       V1Cmsg
044000050630     c                   seton                                        422890
044100050630     c                   leavesr
044200050630e   2c                   endif
044300050630     c                   movel     TBLuni        ds5E
044400090604     c**!!!              movel     §5Edes        V1Dtsp
044500090604     c                   movel     §5Ed08        V1Dtsp
044600050630      * - - vedo se tipo servizio utilizzabile in bollettazione
044700050630if  2c                   if        §5Efub <> 'S'
044800050630     c                   movel     $Msg(3)       V1Cmsg
044900050630     c                   seton                                        422890
045000050630     c                   leavesr
045100050630e   2c                   endif
045200050630e   1c                   ENDIF
045300050624      *
045400050624      * MITTENTE
045500050624      *
045600050624      * - Ricerca alfabetica se:
045700050624      *   · è stato premuto F7
045800050627if  1c                   IF            WinKG  =  *on
045900050624      *   · il codice cliente è vuoto
046000050624     c                             and V1Cccm =  *zeros
046100050624      *   · la ragione sociale è piena
046200050624     c                             and V1Crsm <> *blanks
046300050624     c                   clear                   PAXdit
046400050624if  2c                   if        V1Cflc > *zeros
046500050624     c     V1Cflc        chain     AZORG
046600050624if  3c                   if        %found(AZORG01L)
046700050624     c                   movel     ORGdit        PAXdit
046800050624e   3c                   endif
046900050624e   2c                   endif
047000050627      *   codice da ricercare
047100050627      *   (se immessa la LNP: cerco solo al suo interno
047200050627     c                   movel(p)  V1Cflc        PAXflr
047300050624      *
047400050624     c                   call      'XALFA3BR'
047500050624     c                   parm      RSUT          PAXdut           30
047600050624     c                   parm                    TBLkut
047700050624     c                   parm      V1Crsm        PAXdmt           48
047800050624     c                   parm      DUTkci        PAXccc            4 0
047900050624     c                   parm      9             PAXsta            1 0
048000050627     c                   parm                    PAXflr           90
048100050624     c                   parm                    PAXdit            3
048200050624     c                   parm      1             PAXnum            2 0
048300050624     c                   parm                    PAXksc           80
048400050624     c                   parm                    PAXksm          140
048500050624     c                   parm                    PAXkdm           60
048600050624      *
048700050624      *   · Imposto il codice selezionato a video
048800050624      *     (se selezionato e non premuto F12)
048900050624if  2c                   if        PAXsta >= *zeros
049000050627     c                   movel     PAXksm        ACOksc
049100050624     c                   move      ACOksc        V1Cccm
049200050624     c                   movel     ACOksc        V1Cflc
049300050624e   2c                   endif
049400050624      *
049500050624     c                   seton                                        61  90
049600050624e   1c                   ENDIF
049700050624      *
049800050624      * - Decodifica Mittente
049900050624if  1c                   if        not *in01  and  not *in02
050000050624     c                   clear                   V1Crsm
050100050624     c                   clear                   V1Cinm
050200050624     c                   clear                   V1Ccam
050300050624     c                   clear                   V1Clom
050400050624     c                   clear                   V1Cprm
050500050624     c                   clear                   V1Cnzm
050600050624e   1c                   endif
050700050624      *
050800050704     c                   clear                   ds_KSC
050900050624if  1c                   if        V1Cccm <> *zeros
051000050624     c                   eval      wFLC   =  V1Cflc
051100050624     c                   eval      wCCM   =  V1Cccm
051200050624     c                   exsr      CtrCCM
051300050627if  2c                   if        *in90
051400050627     c                   leavesr
051500050627e   2c                   endif
051600050624x   1c                   else
051700050624     c                   eval      *in02  =  *off
051800050624e   1c                   endif
051900050624      *
052000050624      *   - *in02 ON => controllo il mittente per esteso
052100050624if  1c                   IF        *in02  =  *on
052200050624     c                   clear                   FNLV14ds
052300050624     c                   eval      I14rsc =  V1Crsm
052400050624     c                   eval      I14ind =  V1Cinm
052500050624     c                   eval      E14loc =  V1Clom
052600050624     c                   eval      E14prv =  V1Cprm
052700050624     c                   eval      E14cap =  V1Ccam
052800050624     c                   eval      E14nar =  V1Cnzm
052900050624     c                   eval      Wcli   =  'M'
053000050624     c                   exsr      CtrIND
053100050624     c                   eval      V1Clom =  E14loc
053200050624     c                   eval      V1Cprm =  E14prv
053300050624     c                   eval      V1Ccam =  E14cap
053400050624     c                   eval      V1Cnzm =  E14nar
053500050624if  2c                   if        *in28   or   *in90
053600050624     c                   leavesr
053700050624e   2c                   endif
053800050624      *
053900050624e   1c                   ENDIF
054000050630      *
054100050630      * MITTENTE ORIGINALE
054200050630     c                   eval      wRMO   =  V1Crmo
054300050630     c                   eval      wCMO   =  V1Ccmo
054400050630     c                   eval      wNAR   =  V1Cnmo
054500050630     c                   exsr      CtrMO
054600050630     c                   movel     wNAR          V1Cnmo
054700050630     c                   if        *in90
054800050630     c                   leavesr
054900050630     c                   endif
055000050630      *
055100050630      * DATA COSEGNA RICHIESTA
055200050630if  1c                   IF        V1Cdcr <> *zeros
055300050630     c                   clear                   WLBdat
055400050630     c                   eval      G02dat =  V1Cdcr
055500050630     c                   call      'XSRDA8'
055600050630     c                   parm                    WLBdat
055700050630if  2c                   if        G02err =  *on
055800050708     c                   movel     $Msg(4)       V1Cmsg
055900050630     c                   seton                                        442890
056000050630     c                   leavesr
056100050630e   2c                   endif
056200050630     c                   eval      W1Cdcr =  G02inv
056300050630     c                   eval      V1Cdcr =  G02dat
056400050630e   1c                   ENDIF
056500050630      *
056600050630      * ORA  COSEGNA RICHIESTA
056700050630     c                   move      V1Cocr        W1Cocr
056800050630if  1c                   if            W1Chcr <  *zeros
056900050630     c                             or  W1Chcr >  24
057000050708     c                   movel     $Msg(5)       V1Cmsg
057100050630     c                   seton                                        452890
057200050630     c                   leavesr
057300050630e   1c                   endif
057400050630if  1c                   if            W1Cmcr <  *zeros
057500050630     c                             or  W1Cmcr >  59
057600050708     c                   movel     $Msg(6)       V1Cmsg
057700050630     c                   seton                                        452890
057800050630     c                   leavesr
057900050630e   1c                   endif
058000050630      *
058100050630      * Se immesso TIPO devo immettere anche DATA e/o ORA
058200050630if  1c                   if            V1Ctcr <> *blanks
058300050630     c                             and V1Cdcr =  *zeros
058400050630     c                             and V1Cocr =  *zeros
058500050708     c                   movel     $Msg(7)       V1Cmsg
058600050630     c                   seton                                        442890
058700050630     c                   leavesr
058800050630e   1c                   endif
058900050624      *
059000050624      * DESTINATARIO (se immesso)
059100050624if  1c                   if            V1Crsd <> *blanks
059200050624     c                             or  V1Cind <> *blanks
059300050624     c                             or  V1Ccad <> *blanks
059400050624     c                             or  V1Clod <> *blanks
059500050624     c                             or  V1Cprd <> *blanks
059600050624     c                             or  V1Cnzd <> *blanks
059700050624     c                   clear                   FNLV14ds
059800050624     c                   movel     V1Crsd        I14rsc
059900050704     c                   move      V1Crsd        I14rs2
060000050624     c                   eval      I14ind =  V1Cind
060100050624     c                   eval      E14loc =  V1Clod
060200050624     c                   eval      E14prv =  V1Cprd
060300050624     c                   eval      E14cap =  V1Ccad
060400050624     c                   eval      E14nar =  V1Cnzd
060500050624     c                   eval      Wcli   =  'D'
060600050624     c                   exsr      CtrIND
060700050624     c                   eval      V1Clod =  E14loc
060800050624     c                   eval      V1Cprd =  E14prv
060900050624     c                   eval      V1Ccad =  E14cap
061000050624     c                   eval      V1Cnzd =  E14nar
061100050624if  2c                   if        *in28   or   *in90
061200050624     c                   leavesr
061300050624e   2c                   endif
061400050624      *
061500050624e   1c                   ENDIF
061600050624      *
061700050624      * Immettere il cod.mittente e/o la ragione sociale destinatario
061800050624if  1c                   if            V1Crsm =  *blanks
061900050624     c                             and V1Crsd =  *blanks
062000050624     c                   seton                                        612890
062100050708     c                   movel     $Msg(25)      V1Cmsg
062200050627     c                   leavesr
062300050624e   1c                   endif
062400050630      *
062500050630      * GIORNI DI CHIUSURA
062600050630     c                   eval      wGC1   =  V1Cgc1
062700050630     c                   eval      wGC2   =  V1Cgc2
062800050630     c                   exsr      CtrGCH
062900050630     c                   if        *in90
063000050630     c                   leavesr
063100050630     c                   endif
063200050624      *
063300050624      * CONTRASSEGNO
063400050630      * - Controllo se previsto dal codice bolla
063500050708if  1c                   IF            V1Ccbo <> *blanks
063600050624     c                             and §3Afca =  *zeros
063700050624if  2c                   if        V1Ctic <> *blanks
063800050708     c                   movel     $Msg(8)       V1Cmsg
063900050624     c                   seton                                        512890
064000050624     c                   leavesr
064100050624e   2c                   endif
064200050624if  2c                   if        V1Ccas <> *zeros
064300050708     c                   movel     $Msg(9)       V1Cmsg
064400050624     c                   seton                                        522890
064500050624     c                   leavesr
064600050624e   2c                   endif
064700050624e   1c                   ENDIF
064800050624      *
064900060705      * TIPO INCASSO C/ASSEGNO e divisa li controllo con trulticr
065000060705     c                   clear                   trulticds
065100060705     c                   clear                   ds1a
065200060705     c                   eval      itictbo='P'
065300060705     c                   eval      iticnzd=v1cnzd
065400090707     c                   eval      iticprd=v1cprd
065500090707     c                   eval      iticcad=v1ccad
065600090707     c                   eval      iticlod=v1clod
065700090707     c                   eval      iticind=v1cind
065800090707     c                   eval      iticrsd=v1crsd
065900060705     c                   eval      iticdsp=w2cdsp
066000060705     C                   MOVEL     ds_ksc        iticccm
066100060705     c                   eval      itictic=v1ctic
066200060705     c                   eval      iticdiv=v1cvca
066300060705     c                   eval      iticcas=v1ccas
066400060705     c                   eval      iticpgm='FNLS11R'
066500060705     c                   call      'TRULTICR'
066600060705     c                   parm                    TRULTICDS
066700060705     c                   parm                    ds1a
066800060705     c
066900060705     c                   if        oticric='T'
067000060705     c                   movel     otictic       v1ctic
067100060705     C                   SETON                                        5190
067200060705     c                   leavesr
067300060705     c                   endif
067400060705     c                   if        oticric='D'
067500060705     c                   movel     oticdiv       v1cvca
067600060705     C                   SETON                                        5390
067700060705     c                   leavesr
067800060705     c                   endif
067900060705     c* Errore tipo incasso
068000091211     c                   if        oticerr='T' or oticerr='C'
068100060705     C                   MOVEL     oticmsg       V1cMSG
068200060705     C                   SETON                                        519028
068300060705     c                   leavesr
068400060705    2C                   ENDIF
068500060705     c* Errore divisa
068600060705     c                   if        oticerr='D'
068700060705     C                   MOVEL     oticmsg       V1cmSG
068800060705     C                   SETON                                        539028
068900060705     c                   leavesr
069000060705    2C                   ENDIF
069100060705     c
069200050624      *
069300050624      * - Controllo importo
069400050624      *   - pulisco il flag x la forzatura c/assegno forzabile
069500050624if  1c                   if        V1Ccas <> sav_V1Ccas
069600050624     c                   reset                   ForzaCAS
069700050624e   1c                   endif
069800050624      *   - controllo il c/assegno tramite il TRUL21R
069900050624  1b c                   IF        V1Ccas <> *zeros
070000050624      *
070100050624     c                   eval      sav_V1Ccas = V1Ccas
070200050624     c                   clear                   TRUL21ds
070300050624     c                   eval      I21tla =  'L'
070400050624     c                   eval      I21cbo =  V1Ccbo
070500050624     c                   eval      I21tsp =  V1Ctsp
070600050624     c                   eval      I21lnp =  DUTpou
070700050624     c                   eval      I21nzm =  V1Cnzm
070800050624     c                   eval      I21nzd =  V1Cnzd
070900050624     c                   movel     V1Cflc        I21ksc
071000050624     c                   move      V1Cccm        I21ksc
071100050624     c                   movel     V1Cncp        I21ctr
071200050624     c                   eval      I21tic =  V1Ctic
071300050624     c                   eval      I21imp =  V1Ccas
071400050624     c                   eval      I21div =  V1Cvca
071500050624     c                   eval      I21ute =  KNMUS
071600050624     c                   eval      I21pgm =  SDSpgm
071700050624     c                   call      'TRUL21R'
071800050624     c                   parm                    TRUL21ds
071900050624      *   - controllo che non sia minore del limite previsto
072000050624  2b c                   if        O21fmn <> *blanks
072100050708     c                   eval      V1Cmsg =  $Msg(29)
072200050624     c                   eval      %subst(V1Cmsg : 31 : 14)
072300050624     c                             = (%editc(O21lmn : '4'))
072400050624     c                   seton                                        522890
072500050624     c                   leavesr
072600050624  2e c                   endif
072700050624      *   - controllo che non sia maggiore del limite previsto
072800050624      *     non forzabile
072900050624  2b c                   if        O21fx2 <> *blanks
073000050708     c                   eval      V1Cmsg =  $Msg(28)
073100050624     c                   eval      %subst(V1Cmsg : 47 : 14)
073200050624     c                             = (%editc(O21lx2 : '4'))
073300050624     c                   seton                                        522890
073400050624     c                   leavesr
073500050624  2e c                   endif
073600050624      *   - controllo che non sia maggiore del limite per bancari
073700050624      *     Bartolini non abilitati da sede
073800050624      *     non forzabile
073900050624  3b c                   if        O21fb  <> *blanks
074000050708     c                   eval      V1Cmsg =  $Msg(34)
074100050624     c                   eval      %subst(V1Cmsg : 26 : 14)
074200050624     c                             = (%editc(O21lb  : '4'))
074300050624     c                   seton                                        522890
074400050624     c                   leavesr
074500050624  3e c                   endif
074600050624      *   - controllo che non sia maggiore del limite previsto
074700050624      *     forzabile
074800050624  2b c                   if            O21fx1   <> *blanks
074900050624     c                             and ForzaCAS =  *off
075000050624     c                   eval      ForzaCAS = *on
075100050708     c                   eval      V1Cmsg   =  $Msg(30)
075200050624     c                   eval      %subst(V1Cmsg : 24 : 14)
075300050624     c                             = (%editc(O21lx1 : '4'))
075400050624     c                   seton                                        522890
075500050624     c                   leavesr
075600050624  2e c                   endif
075700050624      *
075800050624  1e c                   ENDIF
075900050624      *
076000050624      * IMPORTO DA ASSICURARE
076100050624      * - se immesso importo da assicurare è obbligatorio immettere
076200050624      *   anche la divisa
076300050624if  1c                   if            V1Cias >  *zeros
076400050624     c                             and V1Cvas =  *blanks
076500050708     c                   movel     $Msg(13)      V1Cmsg
076600050624     c                   seton                                        762890
076700050624     c                   leavesr
076800050624e   1c                   endif
076900050624      * - Controllo divisa
077000050624     c                   eval      wVLT   =  V1Cvas
077100050701     c                   eval      w0133  =  V1Cias
077200050624     c                   exsr      CtrVLT
077300050624     c                   eval      V1Cvas =  wVLT
077400050624if  1c                   if        *in90  =  *on
077500050624     c                   eval      *in76  =  *on
077600050624     c                   leavesr
077700050624e   1c                   endif
077800050624      * - controllo scadenza divisa
077900050624if  1c                   if            W2Cdsp >  §CVdli
078000050624     c                             and V1Cias <> *zeros
078100050708     c                   movel     $Msg(14)      V1Cmsg
078200050624     c                   seton                                        762890
078300050624     c                   leavesr
078400050624e   1c                   endif
078500050624      *
078600050624      * - Controllo importo
078700050624      *   - pulisco il flag x la forzatura importo da assicurare forzabile
078800050624if  1c                   if        V1Cias <> sav_V1Cias
078900050624     c                   reset                   ForzaIAS
079000050624e   1c                   endif
079100050624      *   - controllo l'importo da assicurare tramite il TRUL21R
079200050624  1b c                   IF        V1Cias <> *zeros
079300050624      *
079400050624     c                   eval      sav_V1Cias = V1Cias
079500050624     c                   clear                   TRUL22ds
079600050624     c                   eval      I22tla =  'L'
079700050624     c                   eval      I22cbo =  V1Ccbo
079800050624     c                   eval      I22tsp =  V1Ctsp
079900050624     c                   eval      I22lnp =  DUTpou
080000050624     c                   eval      I22nzm =  V1Cnzm
080100050624     c                   eval      I22nzd =  V1Cnzd
080200050624     c                   movel     V1Cflc        I22ksc
080300050624     c                   move      V1Cccm        I22ksc
080400050624     c                   movel     V1Cncp        I22ctr
080500050624     c                   eval      I22imp =  V1Cias
080600050624     c                   eval      I22div =  V1Cvas
080700050624     c                   eval      I22ute =  KNMUS
080800050624     c                   eval      I22pgm =  SDSpgm
080900050624     c                   call      'TRUL22R'
081000050624     c                   parm                    TRUL22ds
081100050624      *   - se ho tutti i flag impostati non posso immettere l'importo
081200050624      *     da assicurare
081300050624  2b c                   if            O22fmn <> *blanks
081400050624     c                             and O22fx1 <> *blanks
081500050624     c                             and O22fx2 <> *blanks
081600050708     c                   eval      V1Cmsg = $Msg(31)
081700050624     c                   seton                                        752890
081800050624     c                   leavesr
081900050624  2e c                   endif
082000050624      *   - controllo che non sia minore del limite previsto
082100050624  2b c                   if        O22fmn <> *blanks
082200050708     c                   eval      V1Cmsg = $Msg(32)
082300050624     c                   seton                                        752890
082400050624     c                   leavesr
082500050624  2e c                   endif
082600050624      *   - controllo che non sia maggiore del limite previsto
082700050624      *     non forzabile
082800050624  2b c                   if        O22fx2 <> *blanks
082900050708     c                   eval      V1Cmsg = $Msg(26)
083000050624     c                   eval      %subst(V1Cmsg : 38 : 14)
083100050624     c                             = (%editc(o22lx2 : '4'))
083200050624     c                   seton                                        752890
083300050624     c                   leavesr
083400050624  2e c                   endif
083500050628      *   - controllo che non sia maggiore del limite previsto
083600050628      *     forzabile
083700050624  2b c                   if            O22fx1   <> *blanks
083800050624     c                             and ForzaIAS =  *blanks
083900050624     c                   eval      Forzaias = *on
084000050708     c                   eval      V1Cmsg   = $Msg(33)
084100050624     c                   eval      %subst(V1Cmsg : 28 : 14)
084200050624     c                             = (%editc(O22lx1 : '4'))
084300050624     c                   seton                                        752890
084400050624     c                   leavesr
084500050624  2e c                   endif
084600050624      *
084700050624  1e c                   ENDIF
084800050624      *
084900050624     c                   ENDSR
085000050624      *
085100050624      *---------------------------------------------------------------*
085200050624      * Gestione videata D02                                          *
085300050624      *---------------------------------------------------------------*
085400050624     c     GesD02        BEGSR
085500050624      *
085600050624      * Inizializzo la videata
085700050624if  1c                   if        $InzD02  = *on
085800050624     c                   exsr      InzD02
085900050624     c                   movel     *off          $InzD02
086000050624e   1c                   endif
086100050624      * Emetto la window
086200050624     c                   exfmt     LS11D02
086300050624     c                   setoff                                       28  90
086400050624     c                   movea     Win4080       *in(40)
086500050624     c                   clear                   V1Cmsg
086600050624      *
086700050624      * F12=Ritorno
086800050624if  1c                   if        *inKL
086900050624     c                   reset                   $Video
087000050624     c                   goto      EndGesD02
087100050624e   1c                   endif
087200050624      *
087300050624      * Controllo dati immessi a video
087400050627     c***                exsr      CtrD02
087500050627     c***  *in90         cabeq     *on           EndGesD02
087600050624      *
087700050624      * Esecuzione stampa
087800050624     c                   exsr      StampaRM
087900050624      *
088000050624      * Ritorno alla videata precedente (D01)
088100050624     c                   reset                   $Video
088200050624     c                   reset                   $InzD01
088300050624      *
088400050624     c     EndGesD02     ENDSR
088500050624      *
088600050624      *---------------------------------------------------------------*
088700050624      * Inizializzazione videata D02                                  *
088800050624      *---------------------------------------------------------------*
088900050624     c     InzD02        BEGSR
089000050624      *
089100050624     c                   clear                   LS11D02
089200050624      *
089300050624      * Imposto i dati di default
089400050624     c                   eval      V1Cncp = 1
089500050624     c                   eval      V1Cstm = 'S'
089600050624      *
089700050624     c                   ENDSR
089800050617      *
089900050617      *---------------------------------------------------------------*
090000050617      * CALL AL PGM DI RICERCA TABELLE X§TABER                        *
090100050617      *---------------------------------------------------------------*
090200050617     c     CX§TAB        BEGSR
090300050617      *
090400050617     c                   call      'X§TABER'
090500050617     c                   parm                    TBLkut
090600050617     c                   parm                    TBLcod
090700050627     c                   parm      *blanks       TBLkey
090800050627     c                   parm      *blanks       §DES             25
090900050617      *
091000050617     c                   ENDSR
091100050617      *
091200050617      *---------------------------------------------------------------*
091300050617      * CHAIN TABEL00F                                                *
091400050617      *---------------------------------------------------------------*
091500050617     c     CTABEL        BEGSR
091600050617      *
091700050621     c                   eval      *in30 = *off
091800050620     c     KTABEL        chain     TABEL
091900050617     c                   if        NOT %found(TABEL00F)
092000050617     c                             or   TBLflg <> *blanks
092100050617     c                   eval      *in30 = *on
092200050617     c                   endif
092300050617      *
092400050617     c                   ENDSR
092500050617      *
092600050617      *---------------------------------------------------------------*
092700050617      * CONTROLLO DIVISA                                              *
092800050617      *---------------------------------------------------------------*
092900050624     c     CtrVLT        BEGSR
093000050617      *
093100050624     c                   eval      TBLcod =  'CV'
093200050624      * Ricerca
093300050624     c     '?'           scan      wVLT                                   90
093400050624if  1c                   if        *in90
093500050624     c                   exsr      CX§TAB
093600050624     c                   movel     TBLkey        wVLT
093700050624     c                   leavesr
093800050624e   1c                   endif
093900050624      *
094000050624      * Controllo
094100050624     c                   movel(P)  wVLT          TBLkey
094200050624     c                   exsr      CTABEL
094300050624      * - errore
094400050624if  1c                   if        *in30
094500050708     c                   movel     $Msg(10)      V1Cmsg
094600050624     c                   seton                                        28  90
094700050624     c                   leavesr
094800050624e   1c                   endif
094900050624      *
095000050624     c                   movel     TBLuni        dsCV
095100050624      *
095200050624      * Vedo se ammessi importi decimali
095300050624if  1c                   if        §CVfdc <> 'S'
095400050701     c                   z-add     w0133         w0033
095500050701if  2c                   if        w0033  <> *zeros
095600050708     c                   movel     $Msg(11)      V1Cmsg
095700050624e   2c                   endif
095800050624x   1c                   else
095900050624      * Verifico se i decimali inseriti sono quelli ammessi
096000050624sel 2c                   select
096100050624w   2c                   when      §CVdec =  3
096200050624w   2c                   when      §CVdec =  2
096300050701     c                   move      w0133         w0010
096400050701     c                   if        w0010  <> *zeros
096500050708     c                   movel     $Msg(12)      V1Cmsg
096600050624     c                   endif
096700050624w   2c                   when      §CVdec =  1
096800050701     c                   move      w0133         w0020
096900050701     c                   if        w0020  <> *zeros
097000050708     c                   movel     $Msg(12)      V1Cmsg
097100050624     c                   endif
097200050624w   2c                   when      §CVdec =  0
097300050701     c                   z-add     w0133         w0033
097400050701     c                   if        w0033  <> *zeros
097500050708     c                   movel     $Msg(12)      V1Cmsg
097600050624     c                   endif
097700050624e   2c                   endsl
097800050624e   1c                   ENDIF
097900050624      *
098000050624     c                   if        V1Cmsg <> *blanks
098100050624     c                   seton                                        28  90
098200050624     c                   leavesr
098300050624     c                   endif
098400050624      *
098500050624     c                   ENDSR
098600050624      *
098700050624      *---------------------------------------------------------------*
098800050624      * CONTROLLO GIORNI DI CHIUSURA                                  *
098900050624      *---------------------------------------------------------------*
099000050624     c     CtrGCH        BEGSR
099100050624      *
099200050624      * 1° GIORNO CHIUSURA
099300050624if  1c                   IF        wGC1 <> *blanks
099400050624      * - giorno settimanale
099500050624     c                   movel     wGC1          wGCx
099600050624     c     wGCx          lookup    $GG                                    30
099700050624if  2c                   if        NOT *in30
099800050708     c                   movel     $Msg(18)      V1Cmsg
099900050624     c                   seton                                        562890
100000050624     c                   leavesr
100100050624e   2c                   endif
100200050624      * - mattino/pomeriggio
100300050627     c                   move      wGC1          wGCx
100400050627if  2c                   if            wGCx <> *blanks
100500050627     c                             and wGCx <> 'M'
100600050627     c                             and wGCx <> 'P'
100700050708     c                   movel     $Msg(18)      V1Cmsg
100800050624     c                   seton                                        562890
100900050624     c                   leavesr
101000050624e   2c                   endif
101100050624e   1c                   ENDIF
101200050624      *
101300050624      * 2° GIORNO CHIUSURA
101400050624if  1c                   IF        wGC2 <> *blanks
101500050624      * - giorno settimanale
101600050624     c                   movel     wGC2          wGCx
101700050624     c     wGCx          lookup    $GG                                    30
101800050624if  2c                   if        NOT *in30
101900050708     c                   movel     $Msg(18)      V1Cmsg
102000050624     c                   seton                                        572890
102100050624     c                   leavesr
102200050624e   2c                   endif
102300050624      * - mattino/pomeriggio
102400050630     c                   move      wGC2          wGCx
102500050627if  2c                   if            wGCx <> *blanks
102600050627     c                             and wGCx <> 'M'
102700050627     c                             and wGCx <> 'P'
102800050708     c                   movel     $Msg(18)      V1Cmsg
102900050624     c                   seton                                        572890
103000050624     c                   leavesr
103100050624e   2c                   endif
103200050624      * I giorni di chiusura NON possono essere uguali
103300050624if  2c                   if        wGC1 = wGC2
103400050708     c                   movel     $Msg(18)      V1Cmsg
103500050624     c                   seton                                        572890
103600050624     c                   leavesr
103700050624e   2c                   endif
103800050624e   1c                   ENDIF
103900050624      *
104000050624      * NON possono essere entrambi ' P' o ' M'
104100050624if  1c                   if            (wGC1 = ' M'
104200050624     c                             or   wGC1 = ' P')
104300050624     c                             AND (wGC2 = ' M'
104400050624     c                             or   wGC2 = ' P')
104500050708     c                   movel     $Msg(18)      V1Cmsg
104600050624     c                   seton                                        572890
104700050624     c                   leavesr
104800050624e   1c                   endif
104900050624      *
105000050624     c                   ENDSR
105100050624      *
105200050624      *---------------------------------------------------------------*
105300050624      * CONTROLLO IL MITTENTE ORIGINALE                               *
105400050624      *---------------------------------------------------------------*
105500050624     c     CtrMO         BEGSR
105600050624      *
105700050624      * Ragione sociale obbligatoria se immesso il comune
105800050624if  1c                   if             wCMO <> *blanks
105900050624     c                             and  wRMO =  *blanks
106000050708     c                   movel     $Msg(19)      V1Cmsg
106100050624     c                   seton                                        582890
106200050624     c                   leavesr
106300050624e   1c                   endif
106400050624      *
106500050624      * Ragione sociale obbligatoria e comune obbligatori
106600050624      *   se immessa la nazione
106700050624if  1c                   if             wNAR <> *blanks
106800050624     c                             and (wCMO =  *blanks
106900050624     c                              or  wRMO =  *blanks)
107000050708     c                   movel     $Msg(19)      V1Cmsg
107100050624     c                   seton                                        582890
107200050624     c                   leavesr
107300050624e   1c                   endif
107400050624      *
107500050624      * Controllo nazione mittente originale
107600050624if  1c                   if        wNAR <> *blanks
107700050624     c                   exsr      CtrNAR
107800050624if  2c                   if        *in90
107900050624     c                   leavesr
108000050624e   2c                   endif
108100050624e   1c                   endif
108200050624      *
108300050624      * Controllo CAP mittente originale
108400050624if  1c                   IF        wCMO <> *blanks
108500050624      *
108600050624      * CAP con viario => errore forzabile
108700050624if  2c                   if        SAVcmo <> wCMO
108800050627     c                   clear                   SAVfzv1
108900050624     c                   eval      SAVcmo =  wCMO
109000050624e   2c                   endif
109100050624      *
109200050624     c                   clear                   TISI95ds
109300050624     c                   clear                   FNLV13ds
109400050624     c                   movel     wNAR          I95nar
109500050624     c                   movel     wCMO          I95cap
109600050704     c                   movel     '3'           I95tcn
109700050624      * NON posso dare errore per affidabilità = 0 perchè non ho la
109800050624      *  provincia
109900050624     c                   movel     'S'           I13cnv
110000050624     c                   movel     'S'           I13szv
110100050627     c                   movel     SAVfzv1       E13fzv
110200050624     c                   call      'FNLV13R'
110300050624     c                   parm                    KPJBA
110400050624     c                   parm                    FNLV13ds
110500050624     c                   parm                    TISI95ds
110600050624      * Flag forzatura
110700050627     c                   movel     E13fzv        SAVfzv1
110800050624      *
110900050624      * SE CAP NON TROVATO  ERRORE
111000050624if  2c                   if        O13err <> *blanks
111100050624     c                   movel     O13msg        V1Cmsg
111200050624     c                   seton                                        592890
111300050624     c                   leavesr
111400050624e   2c                   endif
111500050624      *
111600050624e   1c                   ENDIF
111700050624      *
111800050624     c                   ENDSR
111900050624      *
112000050624      *---------------------------------------------------------------*
112100050624      * CONTROLLO NAZIONE E P.IVA                                     *
112200050624      *---------------------------------------------------------------*
112300050624     c     CtrNAR        BEGSR
112400050624      *
112500050624     c                   eval      TBLcod = '15'
112600050624      * Ricerca
112700050624     c     '?'           scan      wNAR                                   30
112800050624if  1c                   if        *in30
112900050624     c                   exsr      CX§TAB
113000050624     c                   movel     TBLkey        wNAR
113100050624     c                   seton                                        60  90
113200050624     c                   leavesr
113300050624e   1c                   endif
113400050624      * Controllo
113500050624     c                   movel(p)  wNAR          TBLkey
113600050624     c                   exsr      CTABEL
113700050624if  1c                   if        *in30
113800050708     c                   movel     $Msg(20)      V1Cmsg
113900050630     c                   seton                                        602890
114000050624     c                   leavesr
114100050624e   1c                   endif
114200050624      *
114300050624     c                   ENDSR
114400050617      *
114500050617      *---------------------------------------------------------------*
114600050617      * CONTROLLO CODICE CLIENTE                                      *
114700050617      *---------------------------------------------------------------*
114800050617     c     CtrCCM        BEGSR
114900050617      *
115000050617      * SE E' ACCESO IL 90 ME LO SALVO
115100050627     c                   movel     *in90         Win90
115200050627if  1c                   if        *in90 = *on
115300050627     c                   setoff                                       61  90
115400050627e   1c                   endif
115500050617      *
115600050617      * Decodifica cliente
115700050627if  1c                   if        ds_KSC <> INDksc
115800050617     c                   clear                   TIBS69ds
115900050617     c                   clear                   CNACOds
116000050617     c                   clear                   CNINDds
116100050617     c                   clear                   CNCLPds
116200050617     c                   clear                   FNCLSds
116300050627     c                   eval      I69kac =  ds_KSC
116400050620if  2c                   if            wCCM <> 8888
116500050620     c                             and wCCM <> 9999
116600050627     c                   eval      I69kin =  ds_KSC
116700050620e   2c                   endif
116800050617     c                   call      'TIBS69R'
116900050617     c                   parm                    TIBS69ds
117000050617     c                   parm                    CNACOds
117100050617     c                   parm                    CNINDds
117200050617     c                   parm                    CNCLPds
117300050617     c                   parm                    FNCLSds
117400050617if  2c                   if        O69err = *on
117500050708     c                   movel     $Msg(21)      V1Cmsg
117600050617     c                   seton                                        612890
117700050617     c                   goto      EndCCM
117800050617e   2c                   endif
117900050617e   1c                   endif
118000050627      *
118100050627      * Flag = 8 => Non Spedisce - No Bolle
118200050627if  1c                   if        ACOabl = '8'   or ACOabl = '*'
118300050708     c                   movel     $Msg(22)      V1Cmsg
118400050627     c                   seton                                        612890
118500050627     c                   goto      EndCCM
118600050627e   1c                   endif
118700050627      *
118800050627      * Non accetto clienti con ntw LOG o XXX
118900050627     c     wFLC          chain     AZORG
119000050701if  1c                   if        %found(AZORG01L)
119100050627     c                   movel     ORGde3        og143
119200050620x   1c                   else
119300050627     c                   movel     'XXX'         §OGntw
119400050620e   1c                   endif
119500050627if  1c                   if        §OGntw = 'XXX' or §OGntw = 'LOG'
119600050708     c                   movel     $Msg(23)      V1Cmsg
119700050627     c                   seton                                        622890
119800050627     c                   goto      EndCCM
119900050620e   1c                   endif
120000050627      *
120100050627     c     *in90         cabeq     *on           EndCCM
120200050627      *
120300050701      * Se è 8888 o 9999:
120400050701if  1c                   IF            wCCM   =  8888
120500050701     c                             or  wCCM   =  9999
120600050701      *
120700050701      * - accendo *in02
120800050701if  2c                   if        *in02  = *off
120900050628     c                   eval      *in02  = *on
121000050620     c                   seton                                        63  90
121100050701e   2c                   endif
121200050701      * - se prima era codificato e ora no:
121300050701      *   pulisco i campi anagrafici del mittente
121400050701if  2c                   if            wCCM   <> SAVccm
121500050701if  3c                   if           (SAVccm <> 8888
121600050701     c                             or  SAVccm <> 9999)
121700050701     c                   clear                   V1Crsm
121800050701     c                   clear                   V1Cinm
121900050701     c                   clear                   V1Clom
122000050701     c                   clear                   V1Ccam
122100050701     c                   clear                   V1Cprm
122200050701     c                   clear                   V1Cnzm
122300050701e   3c                   endif
122400050701     c                   eval      SAVccm = wCCM
122500050701e   2c                   endif
122600050627      *
122700050701      * ...altrimenti: eseguo i controlli sull'indirizzo
122800050701x   1c                   ELSE
122900050630      *
123000050630     c                   movel     ACOrag        V1Crsm
123100050701      * - controllo se esiste in anagrafica la provincia
123200090825if  2c****               if        INDprv = *blanks
123300090825     c****               movel     $Msg(24)      V1Cmsg
123400090825     c****               seton                                        612890
123500090825e   2c****               endif
123600050701      * - CAP obbligatorio: controllo congruenza con provincia
123700050627     c                   clear                   TISI95ds
123800050627     c                   clear                   FNLV13ds
123900050627     c                   eval      I95tcn = '3'
124000050627     c                   eval      I95cap = INDcae
124100050627     c                   eval      I95nar = INDsta
124200050627     c                   eval      I95prv = INDprv
124300050627     c                   eval      I95loc = INDcit
124400050627     c                   eval      I95dat = DateU
124500050701      * - Errore affidabilità = 0
124600050627     c                   eval      I13af0 = 'S'
124700050701      * - Controllo congruenza CAP/Prpvincia
124800050627     c                   eval      I13cng = 'S'
124900050701      * - Controllo NON forzabile Città con viario
125000050627     c                   eval      I13cnv = 'S'
125100050627     c                   call      'FNLV13R'
125200050627     c                   parm                    KPJBA
125300050627     c                   parm                    FNLV13ds
125400050627     c                   parm                    TISI95ds
125500050701      * - Se errore: imposto il messaggio aggiungendovi la scritta
125600050617      *   "SUL PIANO DEI CONTI"
125700050630if  2c                   if        O13err <> *blanks
125800050630if  3c                   if        %len(%trim(O13msg)) + 2 < 61
125900050617     c                   eval      V1Cmsg = %trim(O13msg)
126000050617     c                                    + '  '
126100050617     c                                    + C_PDC
126200050630x   3c                   else
126300050617     c                   movel     O95msg        V1Cmsg
126400050617     c                   move      C_PDC         V1Cmsg
126500050630e   3c                   endif
126600050617     c                   seton                                        612890
126700050617     c                   goto      EndCCM
126800050630e   2c                   endif
126900050701      * - Visto che è un codificato, prendo la divisa che può servire
127000050701      *   per l'importo da assicurare o per il valore merce dichiarato
127100050627     c                   movel     '15'          TBLcod
127200050701     c                   movel(p)  INDsta        TBLkey
127300050627     c                   exsr      CTABEL
127400050701      * - - Nazione errata
127500050630if  2c                   if        *in30 = *on
127600050708     c                   movel     $Msg(20)      V1Cmsg
127700050627     c                   seton                                        612890
127800050630x   2c                   else
127900050627     c                   eval      *in02 = *off
128000050630e   2c                   endif
128100050701     c     *in90         cabeq     *on           EndCCM
128200050701      * - Se tutto OK (e non è 8888 o 9999 o cliente di altro P.O.)
128300050701      *   => proteggo il mittente e memorizzo i campi
128400050701     c                   eval      V1Crsm = ACOrag
128500050701     c                   eval      V1Cinm = INDvia
128600050701     c                   eval      V1Clom = INDcit
128700050701     c                   eval      V1Ccam = INDcae
128800050701     c                   eval      V1Cprm = INDprv
128900050701     c                   eval      V1Cnzm = INDsta
129000050627      *
129100050630e   1c                   ENDIF
129200050627      *
129300050627if  1c                   if        NOT *in90 and Win90 = *on
129400050628     c                   eval      *in90 = *on
129500050627e   1c                   endif
129600050627      *
129700050627     c     EndCCM        ENDSR
129800050627      *
129900050627      *---------------------------------------------------------------*
130000050627      * CONTROLLO INDIRIZZO COMPLETO MITTENTE/DESTINATARIO            *
130100050627      *---------------------------------------------------------------*
130200050627     c     CtrIND        BEGSR
130300050627      *
130400050627     c                   clear                   FNLV13ds
130500050627     c                   clear                   TISI95ds
130600050627     c                   clear                   I14iso
130700050627     c                   eval      I14dri  = DateU
130800050627     c                   call      'FNLV14R'
130900050627     c                   parm                    KPJBA
131000050627     c                   parm                    FNLV14ds
131100050627      *
131200050627     c                   eval      wERR    = O14err
131300050627     c                   eval      V1Cmsg  = O14msg
131400050627     c                   exsr      CtrERR
131500050627     c     *in90         cabeq     *on           EndIND
131600050627      *
131700050627      * Controllo CAP
131800050627     c                   eval      I95tcn  = '7'
131900050627     c                   eval      I95cap  = E14cap
132000050627     c                   eval      I95loc  = E14loc
132100050627     c                   eval      I95prv  = E14prv
132200050627     c                   eval      I95nar  = E14nar
132300050627     c                   eval      I95dat  = DateU
132400050627     c                   eval      I13af0  = 'S'
132500050627     c                   eval      I13af1  = 'S'
132600050627     c                   eval      I13sz2  = 'S'
132700050627     c                   eval      I13cnv  = 'S'
132800050627     c                   eval      I13szv  = 'S'
132900050627      *
133000050627      * MITTENTE
133100050627if  1c                   IF        wCLI    = 'M'
133200050627      * - Se cambiato CAP: controllo se ha viario (errore forzabile)
133300050627if  2c                   if        E14cap <> VARcam
133400050627     c                   clear                   SAVfzvM
133500050627     c                   clear                   SAVfz1M
133600050627     c                   clear                   SAVfz2M
133700050627     c                   eval      VARcam  = E14cap
133800050627e   2c                   endif
133900050627      * - Cambiata località
134000050627if  2c                   if        E14loc <> VARlom
134100050627     c                   clear                   SAVfz1M
134200050627     c                   clear                   SAVfz2M
134300050627     c                   eval      VARlom  = E14loc
134400050627e   2c                   endif
134500050627      *
134600050627     c                   eval      E13fzv  = SAVfzvM
134700050627     c                   eval      E13fz1  = SAVfz1M
134800050627     c                   eval      E13fz2  = SAVfz2M
134900050627      *
135000050627x   1c                   ELSE
135100050627      *
135200050627      * DESTINATARIO
135300050627      * - Se cambiato CAP: controllo se ha viario
135400050627if  2c                   if        E14cap <> VARcad
135500050627     c                   clear                   SAVfzvD
135600050627     c                   clear                   SAVfz1D
135700050627     c                   clear                   SAVfz2D
135800050627     c                   eval      VARcad  = E14cap
135900050627e   2c                   endif
136000050627      * - Cambiata località
136100050627if  2c                   if        E14loc <> VARlod
136200050627     c                   clear                   SAVfz1D
136300050627     c                   clear                   SAVfz2D
136400050627     c                   eval      VARlod  = E14loc
136500050627e   2c                   endif
136600050627      *
136700050627     c                   eval      E13fzv  = SAVfzvD
136800050627     c                   eval      E13fz1  = SAVfz1D
136900050627     c                   eval      E13fz2  = SAVfz2D
137000050627      *
137100050627e   1c                   ENDIF
137200050627      * ---
137300050627     c                   call      'FNLV13R'
137400050627     c                   parm                    KPJBA
137500050627     c                   parm                    FNLV13ds
137600050627     c                   parm                    TISI95ds
137700050627      *
137800050627     c                   eval      wERR    = O13err
137900050627     c                   eval      V1Cmsg  = O13msg
138000050627     c                   exsr      CtrERR
138100050627      *
138200050627      * Reimpostazione delle forzature
138300050627if  1c                   if        wCLI    = 'M'
138400050704     c                   eval      SAVfzvM = E13fzv
138500050627     c                   eval      SAVfz1M = E13fz1
138600050627     c                   eval      SAVfz2M = E13fz2
138700050627x   1c                   else
138800050704     c                   eval      SAVfzvD = E13fzv
138900050627     c                   eval      SAVfz1D = E13fz1
139000050627     c                   eval      SAVfz2D = E13fz2
139100050627e   1c                   endif
139200050627      *
139300050627     c     *in90         cabeq     *on           EndIND
139400050627      *
139500050627if  1c                   if        wCLI   <> 'M'
139600050627     c                   eval      WdesIE  = O14ie
139700050627     c                   eval      W15eft  = O14eft
139800050627e   1c                   endif
139900050627      *
140000050627     c     EndIND        ENDSR
140100050627      *
140200050627      *---------------------------------------------------------------*
140300050627      * CONTROLLO SE C'E' ERRORE                                      *
140400050627      *---------------------------------------------------------------*
140500050627     c     CtrERR        BEGSR
140600050627      *
140700050627      * Imposto i campi passati
140800050627     c                   if        O13ron = 'S'
140900050627     c                   eval      E14nar = O95nar
141000050627     c                   endif
141100050627     c                   if        O13roc = 'S'
141200050627     c                   eval      E14cap = O95cap
141300050627     c                   endif
141400050627     c                   if        O13rop = 'S'
141500050627     c                   eval      E14prv = O95prv
141600050627     c                   endif
141700050627     c                   if        O13rol = 'S'
141800050627     c                   eval      E14loc = O95loc
141900050627     c                   endif
142000050627      *
142100050627      * Errore o ricerca campo
142200050627if  1c                   IF        wERR   <> *blanks
142300050627      *
142400050627if  2c                   if        V1Cmsg <> *blanks
142500050627     c                   eval      *in28  =  *on
142600050627e   2c                   endif
142700050627      * Posizionamento corsore
142800050627sel 2c                   SELECT
142900050708      * - ragione sociale
143000050627w   2c                   WHEN      wERR   =  '1'  or  wERR  =  '2'
143100050627if  3c                   if        wCLI   =  'M'
143200050627     c                   eval      *in63  =  *on
143300050627     c                   else
143400050627     c                   eval      *in69  =  *on
143500050627e   3c                   endif
143600050708      * - località
143700050627w   2c                   WHEN      wERR   =  '3'
143800050627if  3c                   if        wCLI   =  'M'
143900050627     c                   eval      *in65  =  *on
144000050627     c                   else
144100050627     c                   eval      *in71  =  *on
144200050627e   3c                   endif
144300050708      * - provincia
144400050627w   2c                   WHEN      wERR   =  '4'
144500050627if  3c                   if        wCLI   =  'M'
144600050627     c                   eval      *in66  =  *on
144700050627     c                   else
144800050627     c                   eval      *in72  =  *on
144900050627e   3c                   endif
145000050708      * - cap
145100050627w   2c                   WHEN      wERR   =  '5'
145200050627if  3c                   if        wCLI   =  'M'
145300050627     c                   eval      *in64  =  *on
145400050627     c                   else
145500050627     c                   eval      *in70  =  *on
145600050627e   3c                   endif
145700050708      * - nazione
145800050627w   2c                   WHEN      wERR   =  '6'
145900050627if  3c                   if        wCLI   =  'M'
146000050627     c                   eval      *in67  =  *on
146100050627     c                   else
146200050627     c                   eval      *in73  =  *on
146300050627e   3c                   endif
146400050708      * - partita iva
146500050708w   2c***                WHEN      wERR   =  '7'
146600050708      * - partita iva estera
146700050708w   2c***                WHEN      wERR   =  '8'
146800050627e   2c                   ENDSL
146900050627      *
147000050627     c                   eval      *in90  =  *on
147100050627      *
147200050627e   1c                   ENDIF
147300050627      *
147400050627     c                   ENDSR
147500050620      *
147600050620      *---------------------------------------------------------------*
147700050708      * STAMPA IL DOCUMENTO DI RITIRO                                 *
147800050620      *---------------------------------------------------------------*
147900050627     c     StampaRM      BEGSR
148000050620      *
148100050620     c                   clear                   FNLSB5ds
148200050627      *
148300050627      * Tipo Bolla     = Partenza
148400050627     c                   eval      DB0tbo = 'P'
148500050627      * Flag gestione  = "B" (dati passati)
148600050627     c                   eval      DB0ges = 'B'
148700050627      * Flag ristampa  = "M" (Rimesso Mittente)
148800050627     c                   eval      DB0ris = 'M'
148900050627      * P.O. partenza  = P.O. richiedente
149000050627     c                   eval      DB0lnp = DUTpou
149100050627      * Codice tariffa = Numero Copie
149200050627     c                   eval      DB0ctr = V1Cncp
149300050627      * Codice bolla
149400050627if  1c                   if        V1Ccbo <> *blanks
149500050627     c                   movel     V1Ccbo        DB0cbo
149600050704     c                   movel     §3Atb1        DB0tb1
149700050704     c                   movel     §3Atb2        DB0tb2
149800050704     c                   movel     §3Ades        DB0dcb
149900050704e   1c                   endif
150000050627      * Tipo servizio bolle
150100050627if  1c                   if        V1Ctsp <> *blanks
150200050627     c                   movel     V1Ctsp        DB0tsp
150300050704     c                   movel     §5Edes        DB0dts
150400050627     c                   movel     §5Efst        DB0fst
150500050708e   1c                   endif
150600050627      * Data spedizione
150700050627     c                   movel     W1Cdsp        DB0aas
150800050627     c                   z-add     W1Cdsp        DB0mgs
150900050627      * Importo da assicurare
151000050627     c                   movel     V1Cias        DB0ias
151100050627     c                   if        DB0ias <> *zeros
151200050627     c                   movel     V1Cvas        DB0vas
151300050627     c                   endif
151400050627      * Tipo incasso c/assegno
151500050627if  1c                   IF        V1Ctic <> *blanks
151600050627     c                   movel     V1Ctic        DB0tic
151700050627     c                   movel     §1Aucm        DB0ucm
151800050627     c                   movel     §1Aste        DB0ste
151900050627     c                   movel     §1Ades        DB0dti
152000050627      *
152100050627      * - la descrizione del tipo incasso la imposto solo:
152200050627      *   · se s'è l'importo oppure
152300050627      *   · se §1ADEO = *blanks, cioè ce n'è solo una
152400050627      *   vedo se impostare la descrizione oltre limite
152500050627if  2c                   if            §1Adeo <> *blanks
152600050627     c                             and V1Ccas >  *zeros
152700050627      *   (il TRUL di controllo è già stato richiamato nella routine
152800050627      *    dei controlli)
152900050627     c                             and O21fca <> *blanks
153000050627     c                   movel     §1Adeo        DB0dti
153100050627e   2c                   endif
153200050627      * - se c'è la seconda descrizione e non ho l'importo del C/Ass.
153300050627      *   non posso decodificare e quindi abblenko la decodifica
153400050627if  2c                   if            §1Adeo <> *blanks
153500050627     c                             and V1Ccas =  *zeros
153600050627     c                   clear                   DB0dti
153700050627e   2c                   endif
153800050627e   1c                   ENDIF
153900050627      * Importo c/assegno
154000050627     c                   z-add     V1Ccas        DB0cas
154100050627     c                   movel     V1Cvca        DB0vca
154200050627      * Codice cliente mittente
154300050627     c                   move      ds_KSC        DB0ccm
154400050627      * La decodifica del mittente la stampo solo se richiesta
154500050627if  1c                   IF        v1cSTM =  'S'
154600050627     c                   movel     V1Crsm        DB0rsm
154700050627     c                   movel     V1Cinm        DB0inm
154800050627     c                   movel     V1Clom        DB0lom
154900050627     c                   movel     V1Cprm        DB0prm
155000050627if  2c                   if        V1Ccmo <> *blanks
155100050627     c                   movel     V1Ccmo        DB0cam
155200050627     c                   movel     V1Cnmo        DB0nzm
155300050627x   2c                   else
155400050627     c                   movel     V1Ccam        DB0cam
155500050627     c                   movel     V1Cnzm        DB0nzm
155600050627e   2c                   endif
155700050627e   1c                   ENDIF
155800050627      * Decodifica del destinatario
155900050627     c                   movel     V1Crsd        DB0rsd
156000050627     c                   move      V1Crsd        DB0rd2
156100050627     c                   movel     V1Cind        DB0ind
156200050627     c                   movel     V1Ccad        DB0cad
156300050627     c                   movel     V1Clod        DB0lod
156400050627     c                   movel     V1Cprd        DB0prd
156500050627     c                   movel     V1Cnzd        DB0nzd
156600050627      * Mittente originale
156700050627     c                   movel     V1Crmo        DB0rmo
156800050627      * Giorni chiusura
156900050627     c                   movel     V1Cgc1        DB0gc1
157000050627     c                   movel     V1Cgc2        DB0gc2
157100050627      * Natura merce
157200050627     c                   movel     V1Cnas        DB0nas
157300050627      * Numero colli
157400050627     c                   z-add     V1Cncl        DB0ncl
157500050627      * Peso
157600050627     c                   z-add     V1Cpkg        DB0pkf
157700050627      * Volume
157800050627     c                   if        V1Cvol > *zeros
157900050627     c                   z-add     V1Cvol        DB0vlf
158000050627     c                   movel     'R'           DB0fvf
158100050627     c                   endif
158200050627      * Note
158300050627     c                   movel     V1Cnt1        DB0nt1
158400050627      * Flag fermo deposito
158500050627     c                   movel     V1Cffd        DB0ffd
158600050627      * Data / Ora / Tipo consegna richiesta
158700050627     c                   z-add     W1Cdcr        DB0dcr
158800050630     c                   move      V1Cocr        DB0hcr
158900050627     c                   movel     V1Ctcr        DB0tcr
159000030620      *
159100050616     c                   call      'FNLSB4R'
159200050620     c                   parm                    FNLSB5ds
159300050620      *
159400050620     c                   ENDSR
159500050620      *
159600050617** > $GG ----------------------------------------------------------------------*
159700050617 1234567
159800050617** > $Msg ---------------------------------------------------------------------*
159900050630Data Documento Errata                                                          1
160000050628Codice Bolla errato o non utilizzabile                                         2
160100050630Tipo servizio errato o non utilizzabile                                        3
160200050708Data consegna richiesta errata                                                 4
160300050708Ora consegna richiesta errata                                                  5
160400050708Minuti consegna richiesta errata                                               6
160500050708Se immesso tipo immettere anche la data o l'ora Consegna Richiesta             7
160600050708Tipo Incasso C/Assegno inesistente o non e' da indicare                        8
160700050708Importo C/Assegno Errato                                                       9
160800050708Divisa errata                                                                 10
160900050708Non ammessi importi con decimali per la divisa indicata                       11
161000050708Numero di decimali errato per la divisa immessa                               12
161100050708Divisa obbligatoria                                                           13
161200050708Divisa scaduta                                                                14
161300050708Tipo incasso C/Assegno inesistente o non e' da indicare                       15
161400050708Tipo incasso non utilizzabile se C/Assegno in divisa estera                   16
161500050708Tipo Incasso non ammesso per il cliente indicato                              17
161600050708Giorno Chiusura Errato                                                        18
161700050708Se immesso CAP o Nazione Mit.Originale immettere tutti 3 i campi:CAP NAZ e MIT19
161800050708Nazione Errata                                                                20
161900050708Cliente Errato o Annullato                                                    21
162000050708Codice inutilizzabile: cliente che non spedisce                               22
162100050708Codice cliente o linea cliente non utilizzabile                               23
162200050708Manca provincia del Mittente in anagrafica: INSERIRLA!!!!                     24
162300050708Immettere il Codice Mittente e/o la Ragione Sociale Destinatario              25
162400050708Importo da Ass. > del limite imposto xxxxxxxxxx,xxx EUR. TEL.IN SEDE X AUTOR. 26
162500050708Tipo incasso non utilizzabile per la divisa del contrassegno                  27
162600050708Importo C/Assegno SUPERIORE al limite imposto xxxxxxxxxx,xxx EUR              28
162700050708Importo C/Assegno INFERIORE a xxxxxxxxxx,xxx EUR                              29
162800050708Importo C/Assegno > di xxxxxxxxxx,xxx EUR: VERIFICARE. ENTER X FORZARE        30
162900050708Importo da Assicurare non ammesso                                             31
163000060220Importo da Assicurare INFERIORE a quanto previsto dalla legge                 32
163100050708Importo da Assicurare > di xxxxxxxxxx,xxx EUR: VERIFICARE. ENTER PER FORZARE  33
163200110512C/A  SUPERIORE al limite xxxxxxxxxx,xxx EUR non ammesso per bancari BRT       34
