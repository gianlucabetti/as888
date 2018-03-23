000100071010      * FIOR76R1 *----------------------------------------------------*
000200071010      *?STAMPA DETTAGLIO ORM DA ASSEGNARE IN ASSEGNAZIONE DISTINTE   ?*
000300071010      *---------------------------------------------------------------*
000400071010
000500071010     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600071010
000700071010      *---------------------------------------------------------------*
000800071010
000900071010     fAZORG01L  if   e           k disk
001000071010      *
001100071010     fFIOR76P1  o    e             printer oflind(*in25)
001200071010
001300071010      *---------------------------------------------------------------*
001400071010
001500071010      *
001600071010      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - -?*
001700071010      *
001800071010      *
001900071010      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - -?*
002000071010      *
002100071010      *
002200071010      *?  S T R U T T U R E   D A T I   - - - - - - - - - - - - - - -?*
002300071010      *
002400071010      * - Parametri
002500071010     d KPJBA         e ds
002600071010     d FIOR76ds      e ds
002700071010      * - (dati da elaboorare)
002800071010     d £FNORMds        s                   like(FNORMds)
002900071010     d £FNORGds        s                   like(FNORGds)
003000071010      *
003100071010      * - Reperimento dati utente
003200071010     d TIBS34ds      e ds
003300071010     d AZUTEds       e ds                  extname(AZUTE00F)
003400071010     d dDatiUte      e ds
003500071010      *
003600071010      * - Dati del singolo ORM da stampare
003700071010     d FNORMds       e ds                  inz  extname(FNORM00F)
003800071010     d FNORGds       e ds                  inz  extname(FNORG00F)
003900071010      *
004000071010     d dORM01        e ds                  inz
004100071010      *
004200071010     d Status         sds           333
004300071010     d   SDSpgm          *proc
004400071010     d   SDSprm          *parms
004500071010      *
004600071010      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
004700071010      *
004800071010      * - Flags
004900071010     d $First          s              1    inz(*on)
005000071010     d $RecOK          s              1    inz(*off)
005100071010      * - Indici di schiera
005200071010      * - Comodo
005300071010     d wData_Eur       s               d   datfmt(*eur)  inz(*loval)
005400071010     d wDate8          s              8  0 inz
005500071010      *
005600071010      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
005700071010      *
005800071010
005900071010      *---------------------------------------------------------------*
006000071010      *?RIEPILOGO INDICATORI                                         ?*
006100071010      *---------------------------------------------------------------*
006200071010      *  25    - OverFlow                                             *
006300071010      *---------------------------------------------------------------*
006400071010
006500071010     c     *Entry        plist
006600071010     c                   parm                    KPJBA
006700071010     c                   parm                    £FNORMds
006800071010     c                   parm                    £FNORGds
006900071010      *
007000071010      * Operazioni iniziali
007100071010     c                   exsr      sr_Start
007200071010      *
007300071011     c                   IF        SDSprm  > 1
007400071010      * Selezione del record
007500071010     c                   exsr      sr_Selez
007600071010      * Stampa dettaglio del singolo O.R.M.
007700071011     c                   if        $RecOK  = *on
007800071010     c                   exsr      sr_PrtORM
007900071010     c                   endif
008000071011     c                   ENDIF
008100071010      *
008200071010      * Fine
008300071010     c                   exsr      sr_End
008400071010
008500071010      *---------------------------------------------------------------*
008600071010      *?Subroutine Iniziale                                          ?*
008700071010      *---------------------------------------------------------------*
008800071010     c     *InzSR        BEGSR
008900071010      *
009000071010      * Reperimento dati job
009100071010     c                   exsr      DatiJob
009200071010      *
009300071010      * Pulizia iniziale campi in testata
009400071010     c                   clear                   PT1dat
009500071010     c                   clear                   PT1fil
009600071010     c                   clear                   PT1fild
009700071010      *
009800071010     c                   ENDSR
009900071010
010000071010      *---------------------------------------------------------------*
010100071010      *?Reperimento Dati del job (Utente/Operativi)                  ?*
010200071010      *---------------------------------------------------------------*
010300071010     c     DatiJob       BEGSR
010400071010      *
010500071010     c     *dtaara       define    §azute        azuteds
010600071010     c     *dtaara       define    §datiute      ddatiute
010700071010      *
010800071010     c                   in(E)     *dtaara
010900071010     c                   IF        %ERROR or RSUT = *blanks
011000071010     c                   clear                   Tibs34Ds
011100071010     c                   call      'TIBS34R'
011200071010     c                   parm                    Tibs34Ds
011300071010     c                   in        *dtaara
011400071010     c                   ENDIF
011500071010      *
011600071010     c                   ENDSR
011700071010
011800071010      *---------------------------------------------------------------*
011900071010      *?Operazioni Iniziali                                          ?*
012000071010      *---------------------------------------------------------------*
012100071010     c     sr_Start      BEGSR
012200071010      *
012300071010      * Impostazione parametri ricevuti
012400071010     c                   clear                   FNORMds
012500071010     c                   clear                   FNORGds
012600071010sel 1c                   select
012700071010w   1c                   when      SDSprm  > 2
012800071010     c                   movel     £FNORMds      FNORMds
012900071010     c                   movel     £FNORGds      FNORGds
013000071010w   1c                   when      SDSprm  > 1
013100071010     c                   movel     £FNORMds      FNORMds
013200071010e   1c                   endsl
013300071010     c                   eval      dORM01  = ORMflo
013400071010      *
013500071010      * SOLO AL 1° RICHIAMO:
013600071011if  1c                   IF        $First  = *on
013700071011      * - Impostazione parametri ricevuti
013800071011if  2c                   if        KPJBU  <> *blanks
013900071011     c                   movel     KPJBU         FIOR76ds
014000071011x   2c                   else
014100071011     c                   clear                   FIOR76ds
014200071011e   2c                   endif
014300071010      * - Impostazione campi in testata
014400071011if  2c                   if        OR76dat > *zeros
014500071010     c     *iso          move      OR76dat       wData_Eur
014600071010     c                   move      wData_Eur     PT1dat
014700071011e   2c                   endif
014800071011if  2c                   if        OR76fil > *zeros
014900071010     c                   eval      PT1fil  = OR76fil
015000071010     c     OR76fil       chain     AZORG
015100071011if  3c                   if        %found(AZORG01L)
015200071010     c                   eval      PT1fild = ORGdes
015300071010     c***                eval      Og150   = ORGdf0
015400071011e   3c                   endif
015500071011e   2c                   endif
015600071010      * - Stampa 1ª testata
015700071010     c                   write     OR76T1
015800071010     c                   eval      *in25   = *off
015900071010     c                   eval      $First  = *off
016000071010e   1c                   ENDIF
016100071010      *
016200071010     c                   ENDSR
016300071010
016400071010      *---------------------------------------------------------------*
016500071010      *?Operazioni Finali                                            ?*
016600071010      *---------------------------------------------------------------*
016700071010     c     sr_End        BEGSR
016800071010      *
016900071010if  1c                   if             SDSprm <= 1
017000071010     c                             or  (ORMpoe  = *zeros
017100071010     c                              and ORMnsr  = *zeros
017200071010     c                              and ORMnor  = *zeros
017300071010     c                              and ORMnrv  = *zeros)
017400071010     c                   write     OR76E1
017500071010     c***                close     FIOR76P1
017600071010     c                   eval      *inLR   = *on
017700071010e   1c                   endif
017800071010      *
017900071010     c                   return
018000071010      *
018100071010     c                   ENDSR
018200071010
018300071010      *---------------------------------------------------------------*
018400071010      *?Selezione dati del singolo ORM (ricevuto come parametro)     ?*
018500071010      *---------------------------------------------------------------*
018600071010     c     sr_Selez      BEGSR
018700071010      *
018800071010     c                   eval      $RecOK  = *off
018900071010      *
019000071010sel 1c                   select
019100071010      * eclusione ORM con allegati
019200071010w   1c                   when      OR76allega  =  'E'
019300071010     c                             and (ORMtor =  'P'
019400071010     c                             or   §ORsrm =  'S')
019500071010     c                   leavesr
019600071010      * inclusione ORM con allegati
019700071010w   1c                   when      OR76allega  =  'I'
019800071010     c                             and (ORMtor <> 'P'
019900071010     c                             and  §ORsrm <> 'S')
020000071010     c                   leavesr
020100071010e   1c                   endsl
020200071010      *
020300071010     c                   eval      $RecOK      = *on
020400071010      *
020500071010     c                   ENDSR
020600071010
020700071010      *---------------------------------------------------------------*
020800071010      *?Stampa dati del singolo ORM (ricevuto come parametro)        ?*
020900071010      *---------------------------------------------------------------*
021000071010     c     sr_PrtORM     BEGSR
021100071010      *
021200071010      * Gestione testata (se overflow)
021300071010if  1c                   if        *in25
021400071010     c                   write     OR76T1
021500071010     c                   eval      *in25   = *off
021600071010e   1c                   endif
021700071010      *
021800071010      * Impostazione campi in stampa
021900071010      * - data ritiro richiesta
022000071010     c                   clear                   PD1dar
022100071010if  1c                   if        ORMdar  > *zeros
022200071010     c     *iso          move      ORMdar        wData_Eur
022300071010     c                   move      wData_Eur     wDate8
022400071010     c                   movel     wDate8        PD1dar
022500071010e   1c                   endif
022600071010      * - Commissionato
022700071010     c                   clear                   PD1com
022800071010if  1c                   if        §ORcom  = 'S'
022900071010     c                   eval      PD1com  = §ORcom
023000071010e   1c                   endif
023100071010      * - Sponda Idraulica
023200071010     c                   clear                   PD1spi
023300071010if  1c                   if        ORMspi  = 'S'
023400071010     c                   eval      PD1com  = 'Sp'
023500071010e   1c                   endif
023600071010      * - Automezzo
023700071010     c                   clear                   PD1aut
023800071010sel 1c                   select
023900071010w   1c                   when      ORMblc <> *zeros
024000071010     c                   eval      PD1aut  = 'BIL'
024100071010w   1c                   when      ORMatt <> *zeros
024200071010     c                   eval      PD1aut  = 'AUT'
024300071010w   1c                   when      ORMmtc <> *zeros
024400071010     c                   eval      PD1aut  = 'MOT'
024500071010x   1c***                other
024600071010     c***                clear                   PD1aut
024700071010e   1c                   endsl
024800071010      * - Allegati / Prepagato
024900071010     c                   clear                   PD1all
025000071010sel 1c                   select
025100071010      *   - Allegati
025200071010w   1c                   when      §ORsrm  = 'S'
025300071010     c                   eval      PD1all  = 'RiO'
025400071010      *   - Prepagato
025500071010w   1c                   when      ORMtor  = 'P'
025600071010     c                   eval      PD1all  = 'Pp '
025700071010x   1c***                other
025800071010     c***                clear                   PD1all
025900071010e   1c                   endsl
026000071010      *
026100071010      * Stampa dettaglio
026200071010     c                   write     OR76D1
026300071010      *
026400071010     c                   ENDSR
