000100060808      *===============================================================*
000200060808      *?TISIDWR * Estrazione nostro cappario per DPD (filtro)        ?*
000300060808      *===============================================================*
000400060808
000500060808     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600060808
000700060808      *---------------------------------------------------------------*
000800060808
000900060808     fAZCPR02L  if   e           k disk
001000060808      *
001100060808     fTISIDWD   cf   e             workstn
001200060808
001300060808      *---------------------------------------------------------------*
001400060808
001500060808      *
001600060808      * Costanti - - - - - - - - - - - - - - - - - - - - - - - - - - -*
001700060808      *
001800060808     d C_WrkF          c                   const('WFCAPXDPD')
001900060808      *
002000060808      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002100060808      *
002200060808     d $Msg            s             78    dim( 4)  ctdata  perrcd(1)
002300060808      *
002400060808      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002500060808      *
002600060808     d KPJBA         e ds
002700060808      *
002800060808      * - Parametri x Controllo profilo utenti
002900060808     d TIBS34ds      e ds
003000060808      * - Ds di riferimento al file esterno AZUTE00F
003100060808     d AZUTEds       e ds                  extname(AZUTE00F)
003200060808      * - Ds per dati organigramma
003300060808     d DDatiUte      e ds
003400060808      *
003500060808      * DS di procedura
003600060808     d TISI00ds      e ds                  inz
003700060808     d   D00op0      e                     inz('R01')
003800060808     d   D00f03      e                     inz(*off)
003900060808     d   D00f12      e                     inz(*off)
004000060808     d   D00err      e                     inz(*off)
004100060808      *
004200060808     d Status         sds           333
004300060808     d   SDSpgm          *proc
004400060808      *
004500060808      * Parametri per controllo data e transform in giorni - XSRDA8 -
004600060808     d WLBdat          ds                  inz
004700060808     d  G08dat                        8  0 inz
004800060808     d  G08inv                        8  0 inz
004900060808     d  G08err                        1    inz('3')
005000060808     d  G08tgi                        5  0 inz
005100060808      *
005200060808      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
005300060808      *
005400060808      * - Flags
005500060808     d $Fine           s              1    inz(*off)
005600060808     d $InzD01         s              1    inz(*on)
005700060808      *
005800060808      * Parametri x pgm. QCMDEXC
005900060808     d Qcmd            s            100    inz
006000060808     d Qlen            s             15  5 inz(100)
006100060808      *
006200060808      * - Altri campi
006300060808     d wDate           s              8  0 inz
006400060808     d W1Cdri          s                   inz  like(CPRdde)
006500060808      *
006600060808      * Packing - List - - - - - - - - - - - - - - - - - - - - - - - -*
006700060808      *
006800060808     c     K02CPR02      klist
006900060808     c                   kfld                    D00dde
007000060808     c                   kfld                    D00ver
007100060808
007200060808      *---------------------------------------------------------------*
007300060808      * I N D I C A T O R I                                           *
007400060808      *---------------------------------------------------------------*
007500060808      *  10     - Comodo                                              *
007600060808      *  28     - Emissione messaggio di errore a video               *
007700060808      *  51     - Data riferimento errata                             *
007800060808      *  90     - Generico di errore                                  *
007900060808      *---------------------------------------------------------------*
008000060808
008100060808     c     *Entry        plist
008200060808     c                   parm                    KPJBA
008300060808      *
008400060808      * Operazioni Iniziali
008500060808     c                   exsr      RoutInz
008600060808      *
008700060808      * Gestione Video
008800060808do  1c                   dow       $Fine   = *off
008900060808     c                   exsr      GesD01
009000060808e   1c                   enddo
009100060808      *
009200060808      * Fine
009300060808     c                   movel     *on           *inLR
009400060808      *
009500060808      *---------------------------------------------------------------*
009600060808      * Operazioni Iniziali                                           *
009700060808      *---------------------------------------------------------------*
009800060808     c     RoutInz       BEGSR
009900060808      *
010000060808      * Reperisco dati job
010100060808     c                   exsr      DatiJob
010200060808      *
010300060808     c                   movel     SDSpgm        V1Tpgm
010400060808     c                   clear                   WLBdat
010500060808     c                   eval      G08dat  = *date
010600060808     c                   call      'XSRDA8'
010700060808     c                   parm                    WLBdat
010800060808     c                   movel     G08inv        wDate
010900060808      *
011000060808     c                   ENDSR
011100060808      *
011200060808      *---------------------------------------------------------------*
011300060808      * Reperimento Dati del job (Utente/Operativi)                   *
011400060808      *---------------------------------------------------------------*
011500060808     c     DatiJob       BEGSR
011600060808      *
011700060808     c     *dtaara       define    §azute        azuteds
011800060808     c     *dtaara       define    §datiute      ddatiute
011900060808      *
012000060808     c                   in(E)     *dtaara
012100060808     c                   IF        %ERROR or RSUT = *blanks
012200060808     c                   clear                   Tibs34Ds
012300060808     c                   call      'TIBS34R'
012400060808     c                   parm                    Tibs34Ds
012500060808     c                   in        *dtaara
012600060808     c                   ENDIF
012700060808      *
012800060808     c                   ENDSR
012900060808      *
013000060808      *---------------------------------------------------------------*
013100060808      * Gestione videata D01                                          *
013200060808      *---------------------------------------------------------------*
013300060808     c     GesD01        BEGSR
013400060808      *
013500060808      * Inizializzazione videata
013600060808if  1c                   if        $InzD01 = *on
013700060808     c                   exsr      InzD01
013800060808     c                   movel     *off          $InzD01
013900060808e   1c                   endif
014000060808      *
014100060808      * Emissione Testata
014200060808     c                   write     SIDWT01
014300060808      * Emissione videata (con Piede)
014400060808     c                   exfmt     SIDWD01
014500060808     c                   setoff                                       28  90
014600060808     c                   clear                   V1Dmsg
014700060808      *
014800060808sel 1c                   SELECT
014900060808      * F3=Fine
015000060808w   1c                   WHEN      *inKC
015100060808     c                   exsr      F03D01
015200060808      * F9=Visualizzazione file WFCAPXDPD
015300060808w   1c                   WHEN           *inKI
015400060808     c                             and  V1Clib <> *all'? '
015500060808     c                   exsr      F09D01
015600060808      * F10=Ricerca Versioni
015700060808w   1c                   WHEN      *inKJ
015800060808     c                   exsr      F10D01
015900060808      *
016000060808x   1c                   OTHER
016100060808     c                   exsr      CtrD01
016200060808if  2c                   if        *in90
016300060808     c                   leavesr
016400060808e   2c                   endif
016500060808      * F6=Conferma
016600060808if  2c                   if        *inKF
016700060808     c                   exsr      sr_SbmJob
016800060808e   2c                   endif
016900060808      *
017000060808e   1c                   ENDSL
017100060808      *
017200060808     c                   ENDSR
017300060808      *
017400060808      *---------------------------------------------------------------*
017500060808      * Inizializzazione videata D01                                  *
017600060808      *---------------------------------------------------------------*
017700060808     c     InzD01        BEGSR
017800060808      *
017900060808     c                   clear                   SIDWD01
018000060808      *
018100060808      * Data di riferimento
018200060809      * (reperisce la data decorrenza dell'ultima versione)
018300060809     c                   call      'TISI96R'
018400060809     c                   parm      Wdate         P96dri            8 0
018500060809     c                   parm      *zeros        P96ver            5 0
018600060809     c                   reset                   WLBdat
018700060809     c                   eval      G08inv  = P96dri
018800060809     c                   call      'XSRDA8'
018900060809     c                   parm                    WLBdat
019000060809     c                   eval      V1Cdri  = G08dat
019100060809     c                   eval      W1Cdri  = P96dri
019200060809     c***                eval      V1Cver  = P96ver
019300060808      *
019400060808      * Libreria in cui è sito l'archivio - che DEVE essere in *LIBL -
019500060809      * (la segnalazione dell'errore - se NON trovato l'archivio -
019600060809      * verrà eseguita nella subr. CtrD01)
019700060808     c                   call      'TRUL29C'
019800060808     c                   parm      C_WrkF        V1Cfil
019900060808     c                   parm      '*FILE   '    UL29typ           8
020000060808     c                   parm      *blanks       V1Clib
020100060808      *
020200060808     c                   if        V1Clib  = *blanks
020300060808     c                   eval      V1Clib  = *all'? '
020400060808     c                   endif
020500060808      *
020600060808     c                   ENDSR
020700060808      *
020800060808      *---------------------------------------------------------------*
020900060808      * Gestione tasto funzionale F3 da videata D01                   *
021000060808      *---------------------------------------------------------------*
021100060808     c     F03D01        BEGSR
021200060808      *
021300060808      * Chiusura del programma
021400060808     c                   eval      $Fine   = *on
021500060808      *
021600060808     c                   ENDSR
021700060808      *
021800060808      *---------------------------------------------------------------*
021900060808      * Gestione tasto funzionale F9 da videata D01                   *
022000060808      *---------------------------------------------------------------*
022100060808     c     F09D01        BEGSR
022200060808      *
022300060808      * Visualizzazione file WFCAPXDPD
022400060808     c                   eval      Qcmd    = 'DSPDBF FILE'
022500060808     c                                     + '(*LIBL/'
022600060808     c                                     + C_WrkF + ') '
022700060808     c                                     + 'MBR(*FIRST) '
022800060808     c                                     + 'RCDSLT(*NO) '
022900060808     c                                     + 'OUTPUT(*) '
023000060808     c                   call      'QCMDEXC'
023100060808     c                   parm                    Qcmd
023200060808     c                   parm                    Qlen
023300060808      *
023400060808     c                   ENDSR
023500060808      *
023600060808      *---------------------------------------------------------------*
023700060808      * Gestione tasto funzionale F10 da videata D01                  *
023800060808      *---------------------------------------------------------------*
023900060808     c     F10D01        BEGSR
024000060808      *
024100060808     c                   clear                   V1Kver
024200060808     c                   clear                   V1Cver
024300060808     c                   clear                   V1Dver
024400060808      *
024500060808      * Ricerca versioni
024600060808     c                   reset                   TISI00ds
024700060808     c                   call      'TISI46R'
024800060808     c                   parm                    KPJBA
024900060808     c                   parm                    TISI00ds
025000060808      *
025100060808sel 1c                   SELECT
025200060808      * - Ritorno in errore
025300060808w   1c                   WHEN      D00err  = *on
025400060808     c                   seton                                        285190
025500060808     c                   eval      V1Dmsg  = $Msg(4)
025600060808      * - Ritorno in F3 o F12
025700060808w   1c                   WHEN           D00f03  = *on
025800060808     c                             or   D00f12  = *on
025900060808      * - Selezione effettuata
026000060808x   1c                   OTHER
026100060808     c                   reset                   WLBdat
026200060808     c                   eval      G08inv  = D00dde
026300060808     c                   call      'XSRDA8'
026400060808     c                   parm                    WLBdat
026500060808     c                   eval      V1Cdri  = G08dat
026600060808     c                   eval      V1Kver  = 'Versione cappario +
026700060808     c                                        ..........:'
026800060808     c                   eval      V1Cver  = D00ver
026900060808     c     K02CPR02      chain     AZCPR000
027000060808if  2c                   if             %found(AZCPR02L)
027100060808     c                             and  CPRatb  = *blanks
027200060808     c                   eval      V1Dver  = CPRdes
027300060808e   2c                   endif
027400060808e   1c                   ENDSL
027500060808      *
027600060808     c                   ENDSR
027700060808      *
027800060808      *---------------------------------------------------------------*
027900060808      * Controllo dati immessi in videata D01                         *
028000060808      *---------------------------------------------------------------*
028100060808     c     CtrD01        BEGSR
028200060808      *
028300060808     c                   movea     *zeros        *in(51)
028400060808     c                   clear                   V1Kver
028500060808     c                   clear                   V1Cver
028600060808     c                   clear                   V1Dver
028700060808      *
028800060808      * Controllo esistenza archivio WFCAPXDPD in lista di librerie
028900060808      *   (in pratica già eseguita nel rilevare la libreria)
029000060808     c                   if        V1Clib  = *all'? '
029100060808     c                   seton                                        28  90
029200060808     c                   eval      V1Dmsg  = $Msg(1)
029300060808     c                   eval      V1Dmsg  = %replace(C_Wrkf  :
029400060808     c                                                $Msg(1) :
029500060808     c                                       %scan('&WRKF    ':$Msg(1)))
029600060808     c                   leavesr
029700060808     c                   endif
029800060808      *
029900060808      * Controllo formale data
030000060808     c                   clear                   WLBdat
030100060808if  1c                   if        V1Cdri  > *zeros
030200060808     c                   eval      G08dat  = V1Cdri
030300060808     c                   call      'XSRDA8'
030400060808     c                   parm                    WLBdat
030500060808x   1c                   else
030600060808     c                   eval      G08err  = *on
030700060808e   1c                   endif
030800060808if  1c                   if        G08err  = *on
030900060808     c                   seton                                        285190
031000060808     c                   eval      V1Dmsg  = $Msg(2)
031100060808     c                   leavesr
031200060808e   1c                   endif
031300060808     c                   eval      V1Cdri  = G08dat
031400060808     c                   eval      W1Cdri  = G08inv
031500060808      *
031600060808      * Reperimento della versione
031700060808     c     W1Cdri        setll     AZCPR000
031800060808     c                   read      AZCPR000
031900060808do  1c                   DOW       NOT %eof(AZCPR02L)
032000060808if  2c                   if             CPRatb  = *blanks
032100060808     c                             and  W1Cdri >= CPRdde
032200060808     c                             and  W1Cdri <= CPRdsc
032300060808     c                   eval      V1Kver  = 'Versione cappario +
032400060808     c                                        ..........:'
032500060808     c                   eval      V1Cver  = CPRver
032600060808     c                   eval      V1Dver  = CPRdes
032700060808     c                   leave
032800060808e   2c                   endif
032900060808     c                   read      AZCPR000
033000060808e   1c                   ENDDO
033100060808      *
033200060808if  1c                   if        V1Cver  = *zeros
033300060808     c                   seton                                        285190
033400060808     c                   eval      V1Dmsg  = $Msg(3)
033500060808     c                   leavesr
033600060808e   1c                   endif
033700060808      *
033800060808     c                   ENDSR
033900060808      *
034000060808      *---------------------------------------------------------------*
034100060808      * Gestione tasto funzionale F6 da videata D01                   *
034200060808      *---------------------------------------------------------------*
034300060808     c     sr_SbmJob     BEGSR
034400060808      *
034500060808      * Impostazione parametri
034600060808     c                   clear                   TISI00ds
034700060808     c                   eval      D00dri  = W1Cdri
034800060808     c                   eval      D00ver  = V1Cver
034900060808     c*** già così:      eval      D00nar  = *blanks
035000060808     c                   eval      D00dde  = W1Cdri
035100060808      *
035200060808      * Sottomissione del lavoro batch
035300060808     c                   eval      KCoAz   = 'SIDX'
035400060808     c                   movel(p)  TISI00ds      KPJBU
035500060808if  1c                   if        KNmUs   = *all'1'
035600060808     c                   call      'TISIDXC'
035700060808     c                   parm                    KPJBA
035800060808x   1c                   else
035900060808     c                   call      'BCH10'
036000060808     c                   parm                    KPJBA
036100060808e   1c                   endif
036200060808      *
036300060808      * Chiusura del programma
036400060808     c                   eval      $Fine   = *on
036500060808      *
036600060808     c                   ENDSR
036700060808
036800060808      *---------------------------------------------------------------*
036900060808
037000060808**  -  $MSG -----------------------------------------------------------------*
037100060808Non trovato archivio &WRKF     : sistema informativo errato                    1
037200060808Data errata                                                                    2
037300060808Non esistono dati nel periodo richiesto                                        3
037400060808Errori nella funzione di richiesta versioni. Avvertire il CED                  4
