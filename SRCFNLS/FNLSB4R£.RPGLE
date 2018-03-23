000100050616     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200050616      *---------------------------------------------------------------*
000300051011      *    STAMPA  DOCUMENTO  DI  RITIRO  SU  STAMPANTE  AD  AGHI     *
000400050616      *---------------------------------------------------------------*
000500050621      *                                                               *
000600050621      *?NOTA BENE:?TUTTI i dati da stampare sono ricevuti in input.   *
000700050621      *            NON è richiesta alcuna decodifica.                 *
000800050621      *                                                               *
000900050621      *---------------------------------------------------------------*
001000050620
001100050623     fTABEL00F  if   e           k disk
001200050621     fFLNUF01L  Uf   e           k disk
001300050621      *
001400050620     fFNLSB4P   o    e             printer usropn
001500050620
001600050620      *---------------------------------------------------------------*
001700050620
001800050620      *
001900050620      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - - *?
002000050620      *
002100050620      *
002200050620      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *?
002300050620      *
002400050620      * - Giorni della settimana e relativa decodifica
002500050620     d $GgS            s              1    dim(9)  ctdata  perrcd(1)
002600050624     d $Gio            s              5    dim(9)  alt($GgS)
002700050620      *
002800050620      *?  D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *?
002900050620      *
003000050620     d KPJBA         e ds
003100050620      * Dati LDV da stampare
003200050620     d FNLSB5ds      e ds
003300050620      * - Parametri x Controllo profilo utenti
003400050620     d TIBS34ds      e ds
003500050620      * - Ds di riferimento al file esterno AZUTE00F
003600050620     d AZUTEds       e ds                  extname(AZUTE00F)
003700050620      * - Ds per dati organigramma
003800050628     d dDatiUte      e ds
003900050628     *** *
004000050628     ***d TIBS02ds      e ds                  inz
004100050628     ***d dGED          e ds
004200050620      *
004300050620     d Status         sds
004400050620     d  SDSpgm           *proc
004500050620     d  SDSusr               254    263
004600050623      *
004700050623     d KEYpar          ds             8    inz
004800050623     d  WTPpar                 1      1    inz
004900050623     d  WCDpar                 2      3    inz
005000050623     d  Wprogr                 4      8    inz('S0001')
005100050620      *
005200050620      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *?
005300050620      *
005400050621     d Data_EUR        s               d   datfmt(*eur)   inz(*loval)
005500050630     d Data_DMY        s               d   datfmt(*dmy)   inz(*loval)
005600050621     d XX              s              3  0 inz
005700050621     d w0080           s              8  0 inz
005800050621     d w001a           s              1    inz
005900050623     d w035a           s             35    inz
006000050621      *
006100050621      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - - *?
006200050621      *
006300050623      * - TABEL00F
006400050623     c     K03TABEL      klist
006500050623     c                   kfld                    TBLkut
006600050623     c                   kfld                    TBLcod
006700050623     c                   kfld                    TBLkey
006800050621      * - FLNUF01L
006900050621     c     K04NUF01      klist
007000050621     c                   kfld                    NUFaaa
007100050621     c                   kfld                    NUFcnu
007200050621     c                   kfld                    NUFfil
007300050621     c                   kfld                    NUFfl2
007400050620
007500050620      *===============================================================*
007600050620      *?RIEPILOGO INDICATORI                                         ?*
007700050620      *---------------------------------------------------------------*
007800050621      *?31   ?- di comodo                                             *
007900050620      *===============================================================*
008000050620
008100050620     c     *Entry        plist
008200050620     c                   parm                    FNLSB5ds
008300050620      *
008400050620      * Tipo lancio, DB0tla "C" -           Chiudo con LR
008500050620      * Tipo lancio, DB0tla "L" - Elaboro e chiudo con LR
008600050620      * Tipo lancio, DB0tla " " - Elaboro e chiudo in RETRN
008700050620      *
008800050620if  1c                   If        DB0tla = 'C'
008900050620     c                   goto      Fine
009000050620e   1c                   EndIf
009100050620      *
009200050620if  1c                   if        DB0fl3 = '1'
009300050620     c                   close     FNLSB4P
009400050620     c                   open      FNLSB4P
009500050620     c                   clear                   Page
009600050620e   1c                   endif
009700050620      *
009800050620      * Pulisco il PRTF
009900050620     c                   exsr      ClrPRTF
010000050621      *
010100050620      * Imposto i campi di stampa con quelli della DS
010200050620     c                   exsr      PrepPRT
010300050620      *
010400051011      * Stampo il DOCUMENTO DI RITIRO
010500050621      * - stampo il numero di copie richiesto
010600050621      *   (devo stampare il riferimento numerico aggiornato
010700050621      *   di volta in volta su FLNUF)
010800050621if  1c                   IF        DB0ris = 'M'
010900050621     c                   clear                   NUFaaa
011000050621     c                   eval      NUFcnu = 20
011100050621     c                   eval      NUFfil = DB0lnp
011200050621     c                   clear                   NUFfl2
011300050621do  2c                   DO        DB0ctr
011400050621     c     K04NUF01      chain     FLNUF01L
011500050621if  3c                   if        %found(FLNUF01L)
011600050621     c                   add       1             NUFnum
011700050621     c                   UPDATE    FLNUF
011800050621     c                   eval      Pndc = NUFnum
011900050621     c                   write     LSB4P1
012000050621e   3c                   endif
012100050621e   2c                   ENDDO
012200050621e   1c                   ENDIF
012300050620      *
012400050620      * The End
012500050620     c     Fine          tag
012600050621      * - Chiusura pgm
012700050620if  1c                   if        DB0tla = *blanks
012800050620     c                   return
012900050620     c                   else
013000050628     ***c                   reset                   TIBS02ds
013100050628     ***c                   movel     'C'           T02tla
013200050628     ***c                   call      'TIBS02R'
013300050628     ***c                   parm                    KPJBA
013400050628     ***c                   parm                    TIBS02ds
013500050620     c                   eval      *inLR  = *on
013600050620e   1c                   endif
013700050620      *
013800050620      *---------------------------------------------------------------*
013900050620      *?OPERAZIONI INIZIALI                                          ?*
014000050620      *---------------------------------------------------------------*
014100050620     c     *InzSR        BEGSR
014200050620      *
014300050620      * Reperimento Dati del job (Utente/Operativi)
014400050620     c     *dtaara       define    §azute        AZUTEds
014500050620     c     *dtaara       define    §datiute      dDATIUTE
014600050620     c                   in(E)     *dtaara
014700050620     c                   if        %ERROR or RSUT = *blanks
014800050620     c                   clear                   Tibs34Ds
014900050620     c                   call      'TIBS34R'
015000050620     c                   parm                    Tibs34Ds
015100050620     c                   in        *dtaara
015200050620     c                   endif
015300050620      * Impostazione dati in KPJBA
015400050624     c                   eval      KNMUS  = SDSusr
015500050624      * Impostazione campi chiave fissi
015600050624     c                   eval      TBLkut = 1
015700050628     *** *
015800050628     *** * Reperisco la moneta di conto
015900050628     ***c                   clear                   dGED
016000050628     ***c                   clear                   TIBS02ds
016100050628     ***c                   eval      T02mod = 'C'
016200050628     ***c                   eval      T02cod = 'GED'
016300050628     ***c                   eval      T02ke1 = '1         '
016400050628     ***c                   call      'TIBS02R'
016500050628     ***c                   parm                    KPJBA
016600050628     ***c                   parm                    TIBS02ds
016700050628if  1***c                   if        T02err = *blanks
016800050628     ***c                   movel     T02uni        dGED
016900050628e   1***c                   endif
017000050620      *
017100050620      * Apro il file di stampa
017200050620     c                   open      FNLSB4P
017300050620      *
017400050620     c                   ENDSR
017500050620      *
017600050620      *---------------------------------------------------------------*
017700050620      *?Pulisco i campi di stampa                                    ?*
017800050620      *---------------------------------------------------------------*
017900050620     c     ClrPRTF       BEGSR
018000050620      *
018100050620     c                   clear                   LSB4P1
018200050620      *
018300050620     c                   movea     *zeros        *in(01)
018400050620      *
018500050620     c                   ENDSR
018600050620      *
018700050620      *---------------------------------------------------------------*
018800050620      *?Imposta i campi di stampa con quelli della DS                ?*
018900050620      *---------------------------------------------------------------*
019000050620     c     PrepPRT       BEGSR
019100050620      *
019200050621      * Mittente
019300050620     c                   if        DB0ccm <> *zeros
019400050620     c                   move      DB0ccm        Pccm
019500050620     c                   endif
019600050620     c                   eval      Prsm   =  DB0rsm
019700050620     c                   eval      Pinm   =  DB0inm
019800050620     c                   eval      Pcam   =  DB0cam
019900050620     c                   eval      Plom   =  DB0lom
020000050704     c                   if        DB0nzm =  *blanks
020100050704     c                   eval      Ppnm   =  DB0prm
020200050704     c                   else
020300050704     c                   eval      Ppnm   =  DB0nzm
020400050704     c                   endif
020500050707     *** * Partita IVA del mittente
020600050707     *** * (se: · in partenza
020700050707     *** *      · porto franco
020800050707     *** *      · codice cliente mittente xxx8888)
020900050707if  1***c                   if        DB0cpi <> *blanks
021000050707     ***c                   eval      Pkpi   =  'Partita IVA    '
021100050707     ***c                   eval      Ppim   =  DB0cpi
021200050707e   1***c                   endif
021300050621      * Mittente originale
021400050620if  1c                   if        DB0rmo <> *blanks
021500050623     c                   eval      Pkmo   =  'Mitt.Originale '
021600050620     c                   eval      Prmo   =  DB0rmo
021700050620e   1c                   endif
021800050620      *
021900050621      * Destinatario
022000050620if  1c                   if        DB0ccd <> *zeros
022100050620     c                   move      DB0ccd        Pccd
022200050620e   1c                   endif
022300050620     c                   eval      Prsd   =  DB0rsd
022400050620     c                   eval      Prse   =  DB0rd2
022500050620     c                   eval      Pind   =  DB0ind
022600050620     c                   eval      Pcad   =  DB0cad
022700050620     c                   eval      Plod   =  DB0lod
022800050704     c                   if        DB0nzd =  *blanks
022900050704     c                   eval      Ppnd   =  DB0prd
023000050704     c                   else
023100050704     c                   eval      Ppnd   =  DB0nzd
023200050704     c                   endif
023300050621      *
023400050621      * Numero documento
023500050621      * (reperito ed aggiornata in fase di stampa)
023600050621      *
023700050621      * Data   documento
023800050621if  1c                   if            DB0aas <> *zeros
023900050621     c                             and DB0mgs <> *zeros
024000050621     c                   eval      w0080 = (DB0aas * 10000) + DB0mgs
024100050622     c     *iso          movel     w0080         Data_EUR
024200050622     c     *eur          movel     Data_EUR      Pddc
024300060131x   1***c                   else
024400060131     ***c                   move      *date         Pddc
024500050621e   1c                   endif
024600050621      *
024700050621      * Resa: Franco / Assegnato
024800050704sel  c                   select
024900050704      * - (non indicato)
025000050704w   1c                   when      DB0cbo =  *blanks
025100050704      * - porto assegnato
025200050704w   1c                   when      %subst(DB0tb1:1:1) = 'A'
025300050623     c                             or     DB0tb2     <> *blanks
025400050621     c                   eval      Pxas   =  'X'
025500050704      * - porto franco
025600050704w   1c                   other
025700050621     c                   eval      Pxfr   =  'X'
025800050704e   1c                   endsl
025900050621      *
026000050621      * Servizio (espresso)
026100050623if  1c                   if            DB0fst =  'S'
026200050623     c                             and DB0tsp =  'E'
026300050621     c                   eval      Pxse   =  'X'
026400050621e   1c                   endif
026500050621      *
026600050621      * Fermo deposito
026700050621if  1c                   if        DB0ffd <> *blanks
026800050621     c                   eval      Pxfd   =  'X'
026900050621e   1c                   endif
027000050621      *
027100050621      * Consegna richiesta
027200050621sel 1c                   select
027300050621w   1c                   when      DB0tcr = *blanks
027400050621      * - giorno si
027500050621w   1c                   when      DB0dcr <> *zeros  and  DB0tcr = 'P'
027600050621     c                   eval      Ptdr   =  'PRIMA'
027700050621w   1c                   when      DB0dcr <> *zeros  and  DB0tcr = 'D'
027800050621     c                   eval      Ptdr   =  'DOPO '
027900050621     *** * - giorno no  ora si
028000050621w   1***c                   when      DB0dcr =  *zeros  and  DB0tcr = 'P'
028100050621     ***c                   eval      Ptdr   =  'PRIMA DELLE'
028200050621w   1***c                   when      DB0dcr =  *zeros  and  DB0tcr = 'D'
028300050621     ***c                   eval      Ptdr   =  'DOPO LE    '
028400050621e   1c                   endsl
028500050621      * - Data
028600050621if  1c                   if        DB0dcr <> *zeros
028700050621     c     *iso          movel     DB0dcr        Data_EUR
028800050630     ***c     *eur          movel     Data_EUR      Pdcr
028900050630     c                   movel     Data_EUR      Data_DMY
029000050630     c     *dmy          movel     Data_DMY      Pdcr
029100050621e   1c                   endif
029200050630      * - Ora
029300050630if  1c                   if        DB0hcr <> *zeros
029400050630     c                   eval      Phcr   =  %editw(DB0hcr:'0 :  ')
029500050630e   1c                   endif
029600050621      *
029700050621      * Turni di chiusura
029800050621      * - 1° turno di chiusura
029900050621if  1c                   IF        DB0gc1 <> *blanks
030000050621if  2c                   if        %subst(DB0gc1:1:1) = *blank
030100050621      *   - Nessun giorno specificato - Solo 'M' o 'P'
030200050621if  3c                   if        DB0gc1 =  ' M'
030300050621     c                   eval      Pgc1   =  %trim($Gio(8))
030400050621x   3c                   else
030500050621     c                   eval      Pgc1   =  %trim($Gio(9))
030600050621e   3c                   endif
030700050621x   2c                   else
030800050621      *   - Giorno specificato
030900050621     c                   movel     DB0gc1        w001a
031000050621     c                   eval      XX     =  1
031100050621     c                   eval      *in31  =  *off
031200050621     c     w001a         lookup    $GgS(xx)                               31
031300050621     c   31              movel     $Gio(xx)      Pgc1
031400050621      *     + 'M' o 'P'
031500050621sel 3c                   select
031600050621w   3c                   when      %subst(DB0gc1:2:1) = 'M'
031700050624     c                   eval      Pgc1   =  %trim(Pgc1)
031800050621     c                                    +  %trim($Gio(8))
031900050621w   3c                   when      %subst(DB0gc1:2:1) = 'P'
032000050624     c                   eval      Pgc1   =  %trim(Pgc1)
032100050621     c                                    +  %trim($Gio(9))
032200050621e   3c                   endsl
032300050621e   2c                   endif
032400050621e   1c                   ENDIF
032500050621      * - 2° turno di chiusura
032600050624if  1c                   IF        DB0gc2 <> *blanks
032700050621if  2c                   if        %subst(DB0gc2:1:1) = *blank
032800050621      *   - Nessun giorno specificato - Solo 'M' o 'P'
032900050624if  3c                   if        DB0gc2 =  ' M'
033000050624     c                   eval      Pgc2   =  %trim($Gio(8))
033100050621x   3c                   else
033200050624     c                   eval      Pgc2   =  %trim($Gio(9))
033300050621e   3c                   endif
033400050621x   2c                   else
033500050621      *   - Giorno specificato
033600050621     c                   movel     DB0gc2        w001a
033700050621     c                   eval      XX     =  1
033800050621     c                   eval      *in31  =  *off
033900050621     c     w001a         lookup    $GgS(xx)                               31
034000050624     c   31              movel     $Gio(xx)      Pgc2
034100050621      *     + 'M' o 'P'
034200050621sel 3c                   select
034300050621w   3c                   when      %subst(DB0gc2:2:1) = 'M'
034400050624     c                   eval      Pgc2   =  %trim(Pgc2)
034500050621     c                                    +  %trim($Gio(8))
034600050621w   3c                   when      %subst(DB0gc2:2:1) = 'P'
034700050624     c                   eval      Pgc2   =  %trim(Pgc2)
034800050621     c                                    +  %trim($Gio(9))
034900050621e   3c                   endsl
035000050621e   2c                   endif
035100050621e   1c                   ENDIF
035200050621      *
035300050621      * Valore da assicurare e relativa divisa
035400050628if  1c                   if        DB0ias <> *zeros
035500050622if  2c***                if        DB0vas <> §GEDcn
035600050621     c                   eval      Pdas   =  DB0vas
035700050622e   2c***                endif
035800050622     c*** PIAS ridotto:  eval(h)   Pias   =  DB0ias
035900050622     c                   z-add     DB0ias        Pias
036000050628e   1c                   endif
036100050621      *
036200050621      * Contrassegno e relativa divisa
036300050628if  1c                   if        DB0cas <> *zeros
036400050622if  2c***                if        DB0vca <> §GEDcn
036500050621     c                   eval      Pdca   =  DB0vca
036600050622e   2c***                endif
036700050624     c*** PCAS ridotto:  eval(h)   Pcas   =  DB0cas
036800050624     c                   z-add     DB0cas        Pcas
036900050628e   1c                   endif
037000050621      *
037100050621      * - Colli: numero, peso, volume, flag tipo-volume
037200050621     c                   eval      Pncl   =  DB0ncl
037300050621     c                   eval      Ppkf   =  DB0pkf
037400050621     c                   eval      Pvlf   =  DB0vlf
037500050621      *
037600050621      * - Natura merce
037700050621     c                   eval      Pnat   =  DB0nas
037800050620      *
037900050621      * - Note
038000050623     c                   eval      Pnp1   =  %trim(DB0nt1)
038100050620      *
038200050620     c                   ENDSR
038300050623      *
038400050623      *---------------------------------------------------------------*
038500050623      *?Chain su TABEL per le PARTICOLARITA'                         ?*
038600050623      *---------------------------------------------------------------*
038700050623     c     TABxPAR       BEGSR
038800050623      *
038900050623     c                   movel(p)  WCdPar        w035a
039000050623     c                   movel     '7V'          TBLcod
039100050623     c                   movel(p)  KeyPAR        TBLkey
039200050623     c     K03TABEL      chain     TABEL
039300050623      *
039400050623if  1c                   if        %found(TABEL00F)
039500050623     c                             and TBLflg = *blanks
039600050623     c                   eval      w035a = WCdPar + '-'
039700050623     c                                   + %trim(%subst(TBLuni:1:30))
039800050623x   1c                   else
039900050623     c                   clear                   TBLuni
040000050623e   1c                   endif
040100050623      *
040200050623     c                   ENDSR
040300050620
040400050620** > $GgS/$Gio * --------------------------------------------------------------*
0405000506241LUN
0406000506242MAR
0407000506243MER
0408000506244GIO
0409000506245VEN
0410000506246SAB
0411000506247DOM
041200050624M   -MATT.
041300050624P   -POM.
