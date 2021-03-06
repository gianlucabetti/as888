000100060105     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200060105
000300060105      *------------------------------------------------------------------------*
000400060206      *           Stampa    manca Tariffa Filiale / Sede                       *
000500060105      *------------------------------------------------------------------------*
000600060105
000700060105      *------------------------------------------------------------------------*
000800060105      *   N O T E
000900060105      *------------------------------------------------------------------------*
001000060105      *
001100060105      *------------------------------------------------------------------------*
001200060105
001300060105     fAzorg01l  if   e           k Disk
001400060126     fTabel00f  if   e           k Disk
001500060208     fWfmtc01l  if   e           k Disk    usropn
001600060206     fTnsb46p   o    e             Printer Oflind(*IN63) usropn
001700060208     fTnsb46pa  o    e             Printer Oflind(*IN64) usropn
001800060105
001900060105      *------------------------------------------------------------------------*
002000060105      *   C A M P I   D I   L A V O R O
002100060105      *------------------------------------------------------------------------*
002200060105     d dateu           s              8  0
002300060105     d w0140           s             14  0
002400060105     d wdata           s              8  0
002500060202     d SavPo           s              3  0
002600060202     d SavCar          s              3  0
002700060210     d Almeno1         s              1
002800060105     d codut           s              1  0 Inz(1)
002900060111
003000060306     d comman          s             80
003100060306     d lun             s             15  5 inz(80)
003200060306     d comman1         s             91
003300060306     d lun1            s             15  5 inz(91)
003400060120
003500060109     d Tabcod          s                   like(Tblcod)
003600060109     d Tabkey          s                   like(Tblkey)
003700060105
003800060105      *------------------------------------------------------------------------*
003900060105      *   S C H I E R E
004000060105      *------------------------------------------------------------------------*
004100060105
004200060206      * Ovrprtf
004300060208     d cmdo            s             90    dim(2) ctdata perrcd(1)
004400060227     d cmdc            s             91    dim(1) ctdata perrcd(1)
004500060113
004600060105      *------------------------------------------------------------------------*
004700060105      *   D S   I N T E R N E / E S T E R N E
004800060105      *------------------------------------------------------------------------*
004900060105
005000060105     d wlbdat          ds                  Inz
005100060105     d  g02dat                 1      8  0
005200060105     d  g02inv                 9     16  0
005300060105     d  g02err                17     17
005400060105     d  g02tgi                18     22  0
005500060207
005600060207     d                 ds
005700060207     d  Mtcspe                 1     16  0
005800060207     d  MtcAas                 1      4  0
005900060207     d  MtcLnp                 5      7  0
006000060207     d  MtcNrs                 8      9  0
006100060207     d  Mtcnsp                10     16  0
006200060207     d                 ds                  Inz
006300060207     d  Savspe                 1     16  0
006400060207     d  SavAas                 1      4  0
006500060207     d  SavLnp                 5      7  0
006600060207     d  SavNrs                 8      9  0
006700060207     d  Savnsp                10     16  0
006800060207
006900060119
007000060105     d AzuteDs       e ds                  ExtName(AzUte00F)
007100060105     d DDatiUte      e ds
007200060113
007300060206     d Tibs02ds      e ds
007400060206     d Tibs34ds      e ds
007500060206
007600060206     d Tnsb44ds      e ds
007700060206
007800060109     d Dged          e ds
007900060109     d Dgei          e ds
008000060206     d Dlia          e ds
008100060208     d Ds05          e ds
008200060206
008300060202     d Og141         e ds
008400060202     d Og144         e ds
008500060113
008600060105     d Kpjba         e ds
008700060105
008800060105      *------------------------------------------------------------------------*
008900060105      *   I N D I C A T O R I
009000060105      *------------------------------------------------------------------------*
009100060203
009200060203      * 05    Sede
009300060203      * 20    Stampa Imponibile
009400060207      * 40    Alta intensit� su peso fatturabile
009500060207      * 41    Stessa spedizione 2 errori
009600060208      * 63    OF  tnsb46p
009700060208      * 64    OF  tnsb46pa
009800060105
009900060105      *------------------------------------------------------------------------*
010000060105      * CICLO PRINCIPALE
010100060105      *------------------------------------------------------------------------*
010200060105
010300060110      * carico le tabelle ed imposto i punti operativi da elaborare
010400060110     c                   exsr      Sr_imposta
010500060117      * stampa dal file di lavoro
010600060206     c                   exsr      Sr_Stampa
010700060208      * se devo creare il file
010800060208     c                   exsr      SR_Creapf
010900060119
011000060117      *
011100060117     c                   seton                                        lr
011200060206
011300060126      *------------------------------------------------------------------------*
011400060126      * Stampa manca tariffa
011500060126      *------------------------------------------------------------------------*
011600060126     c     SR_Stampa     BegSr
011700060202
011800060202     c                   clear                   savpo
011900060202
012000060202      * lettura del file di lavoro in ordine di Area/P.O.
012100060126
012200060126     c                   Do        *hival
012300060126
012400060126     c                   Read      Wfmtc01l
012500060126
012600060126     c                   If        %eof(Wfmtc01l)
012700060126     c                   leave
012800060126     c                   endif
012900060202      * Cambio P.O. Totali e chiudo il file printer
013000060202     c                   If        Savpo <> Mtcpoc and Savpo > *zeros
013100060207     c                   Exsr      Sr_totali
013200060206      * se stampa separata per P.O.
013300060206     c                   If        D44spp = 'S'
013400060206     c                   Close     tnsb46p
013500060206     c                   endif
013600060206
013700060206    1c                   endif
013800060202      * Cambio P.O.
013900060206    1c                   If        Savpo <> Mtcpoc
014000060202
014100060202     c                   eval      Savpo = Mtcpoc
014200060202     c     Savpo         chain     Azorg01l
014300060206    2c                   if        %found(azorg01l)
014400060202     c                   eval      Despoc = orgdes
014500060202
014600060202      * se stampa separata per P.O. recupero la coda del P.O.
014700060202
014800060206    3c                   If        d44spp = 'S'
014900060202     c                   movel     orgde1        og141
015000060202
015100060206    4c                   If        �ogssc = *blanks
015200060202      * recupero la stampante generica
015300060202     c                   movel     orgde4        og144
015400060202     c                   eval      �ogssc = �ogssp
015500060206    4c                   endif
015600060202
015700060206     c                   clear                   comman
015800060206     c                   movea     cmdo(1)       comman
015900060206     c                   eval      %subst(comman:23:10) = �ogssc
016000060206     c                   Call      'QCMDEXC'                            99
016100060206     c                   Parm                    Comman
016200060206     c                   Parm                    Lun
016300060206
016400060206     c                   open      tnsb46p
016500060206
016600060206    3c                   endif
016700060206
016800060206    2c                   endif
016900060206      * verifico se cambiata  l'area per recuperare la decodifica ma anche per fare i totali
017000060208
017100060208     c                   If        D44tot = 'S'  and Savcar <> Mtccar
017200060208     c                   Exsr      SR_Area
017300060208     c                   endif
017400060203
017500060203      * stampo testata
017600060203
017700060206     c                   write     sb46t
017800060206     c                   write     sb46t1
017900060206     c                   write     sb46tdcm
018000060206     c                   write     sb46t2
018100060206     c                   write     sb46tdcm
018200060206
018300060206    1c                   endif
018400060202      * Preparo i campi di Stampa
018500060207
018600090327     c                   clear                   stpori
018700060207     c                   eval      mtclnp1 = mtclnp
018800060202
018900060202      * Data spedizione
019000060202     c                   Clear                   wlbdat
019100060202     c                   Z-add     MTCdsp        g02inv
019200060202     c                   Movel     '3'           g02err
019300060202     c                   Call      'XSRDA8'
019400060202     c                   Parm                    wlbdat
019500060202     c                   Z-add     g02dat        DatDsp
019600060203      * Imponibile
019700120417     c                   If        Mtcimp = 'IMP'  or Mtcimp = 'IMF'
019800060203     c                   eval      stpvar = 'IMP:'
019900060203     c                   z-add     mtcimv        stpimv
020000060206     c                   movel     mtcdiv        stpdiv
020100060203     c                   else
020200060206     c                   clear                   stpvar
020300060203     c                   clear                   stpimv
020400060206     c                   clear                   stpdiv
020500060203     c                   endif
020600060223
020700060223      * se devo stampare imponibile per cliente e l'imponibile non c'� accendo il 21
020800121031      * se c'� un errore
020900121031     c                   If        *in20 and mtcimv=*zeros and mtcerr <> *blanks
021000060223     c                   eval      *in21 = *on
021100060223     c                   else
021200060223     c                   eval      *in21 = *off
021300060223     c                   endif
021400060203      * verifico se ci sono errori diversi da IMP
021500060203     c                   If        Mtcimp = '   ' and mtcass <> '   '
021600060206     c                   eval      stpvar = 'ASS:'
021700060206     c                   z-add     mtcias        stpimv
021800060206     c                   movel     mtcvas        stpdiv
021900060206     c                   endif
022000060207      * se media del collo sballata evidenzio il campo peso fatturabile
022100090327     c***                eval      *in40 = (mtcesc <> '   ')
022200090327     c                   eval      *in40 = (mtcesc =  '!!!')
022300090327      * se segnalato il fatto che sono stati utilizzati i colli originali
022400090327      * lo stampo
022500090327     c                   if        %subst(mtcesc:1:1) = 'o'
022600090327     c                   eval      stpori = 'o'
022700090327     c                   endif
022800060207      * verifico se si tratta della stessa spedizione che ho letto prima
022900060207     c                   If        Savspe <> Mtcspe
023000060207     c                   eval      *in41 = *off
023100060207     c                   eval      Savspe = Mtcspe
023200060207     c                   else
023300060207     c                   eval      *in41 = *on
023400060207     c                   endif
023500060203      * scrivo la riga di dettaglio
023600060203
023700060203      * overflow
023800060203     c                   if        *in63
023900060203
024000060206     c                   write     sb46tleg
024100060206     c                   write     sb46t
024200060206     c                   write     sb46t1
024300060206     c                   write     sb46tdcm
024400060206     c                   write     sb46t2
024500060206     c                   write     sb46tdcm
024600060203     c                   eval      *in63 = *off
024700060203
024800060203     c                   endif
024900060202
025000060206     c                   write     sb46r
025100060210     c                   eval      almeno1 = '1'
025200060210
025300060206      * se per la stessa spedizione c'� anche errore di assicurazione stampo
025400060206     c                   If        Mtcass <> '   '
025500060227     c                   z-add     mtcias        stpias
025600060206     c                   write     sb46r2
025700060227     c                   else
025800060227     c                   clear                   stpias
025900060206     c                   endif
026000060207      * calcolo totali   franchi/assegnati
026100060207     c                   If        %subst(mtctbl:1:1) = 'F'
026200060207     c                   add       1             totfra
026300060207     c                   else
026400060207     c                   add       1             totass
026500060207     c                   endif
026600060207      * calcolo totali spedizioni
026700060207     c                   add       1             totfin
026800060207      * calcolo totali imponibile
026900060207     c                   If        *in20
027000121031      * calcolo totali spedizioni con imponibile o con imponibile a zero ma senza errore
027100121031     c                   If        mtcimv > 0 or
027200121031     c                             (mtcerr = *blanks and mtcimv = 0)
027300060223     c                   add       1             tottas
027400060207     c                   add       mtcimv        totimp
027500060223     c                   else
027600060223     c                   add       1             totnotas
027700060223     c                   endif
027800060207     c                   endif
027900060207
028000060206     c                   enddo
028100060207
028200060210     c                   SELECT
028300060210      * se ho scritto almeno una riga faccio anche i totali
028400060210     c                   when      Almeno1 <> ' '
028500060208      * fine lettura faccio i totali del p.o. se sto stampando senza totali  imponibile
028600060207     c                   if        not *in20
028700060207     c                   Exsr      Sr_totali
028800060207      * se stampa separata per P.O.
028900060207     c                   If        D44spp = 'S'
029000060207     c                   Close     tnsb46p
029100060207     c                   endif
029200060208      * se stampa dei totali per area
029300060208     c                   If        D44tot = 'S'
029400060208      * overflow
029500060208     c                   If        *in64
029600060208
029700060208     c                   write     sb46at
029800060208     c                   write     sb46at1
029900060208     c                   write     sb46atdc
030000060208     c                   write     sb46at2
030100060208     c                   write     sb46atdc
030200060208     c                   eval      *in64 = *off
030300060208
030400060208     c                   endif
030500060208      * totali per area
030600060209     c                   write     sb46atdc
030700060208     c                   write     sb46atfi
030800060208     c                   close     tnsb46pa
030900060208     c                   endif
031000060208      * totali per cliente
031100060207     c                   else
031200060207     c                   if        *in63
031300060207
031400060207     c                   write     sb46t
031500060207     c                   write     sb46t1
031600060207     c                   write     sb46tdcm
031700060207     c                   write     sb46t2
031800060207     c                   write     sb46tdcm
031900060207     c                   eval      *in63 = *off
032000060207
032100060207     c                   endif
032200060207
032300060207     c                   write     sb46tfii
032400060207
032500060207     c                   close     tnsb46p
032600060207
032700060207     c                   endif
032800060210
032900060210     c                   other
033000060223      * solo se la stampa � separata per PO apro il file di spool
033100060223     c                   If        D44spp = 'S'
033200060210     c                   open      tnsb46p
033300060223     c                   endif
033400060210
033500060210     c                   write     sb46t
033600060210     c                   write     sb46par
033700060210     c                   write     sb46e
033800060210     c                   close     tnsb46p
033900060210
034000060210     c                   endsl
034100060207      *
034200060206
034300060206     c                   endsr
034400060207      *------------------------------------------------------------------------*
034500060207      * Totali per Punto operativo
034600060207      *------------------------------------------------------------------------*
034700060207     c     SR_Totali     BegSr
034800060207
034900060207      * overflow
035000060207     c                   if        *in63
035100060207
035200060207     c                   write     sb46tleg
035300060207     c                   write     sb46t
035400060207     c                   write     sb46t1
035500060207     c                   write     sb46tdcm
035600060207     c                   write     sb46t2
035700060207     c                   write     sb46tdcm
035800060207     c                   eval      *in63 = *off
035900060207
036000060207     c                   endif
036100060207
036200060207     c                   write     sb46tfig
036300060207     c                   write     sb46tleg
036400060207     c                   eval      *in63 = *off
036500060207
036600060207      * Verifico se devo fare i totali di area
036700060207
036800060207     c                   If        D44tot = 'S'
036900060208     c                   write     sb46ar
037000060208      * sommo i totali per area
037100060208     c                   add       totfra        toafra
037200060208     c                   add       totass        toaass
037300060208     c                   add       totfin        toafin
037400060208
037500060207     c                   endif
037600060207      * pulisco i campi di output
037700060207     c                   clear                   totfra
037800060207     c                   clear                   totass
037900060207     c                   clear                   totfin
038000060207
038100060207     c                   endsr
038200060208      *------------------------------------------------------------------------*
038300060208      * GESTIONE AREA
038400060208      *------------------------------------------------------------------------*
038500060208     c     SR_AREA       BegSr
038600060208
038700060208      * se diverso da zero devo fare i totali e chiudere il file di spool
038800060208     c                   If        Savcar > 0
038900060208      * overflow
039000060208     c                   If        *in64
039100060208
039200060208     c                   write     sb46at
039300060208     c                   write     sb46at1
039400060208     c                   write     sb46atdc
039500060208     c                   write     sb46at2
039600060208     c                   write     sb46atdc
039700060208     c                   eval      *in64 = *off
039800060208
039900060208     c                   endif
040000060208      * totali per area
040100060208     c                   write     sb46atfi
040200060208     c                   close     tnsb46pa
040300060208
040400060208     c                   clear                   toafra
040500060208     c                   clear                   toaass
040600060208     c                   clear                   toafin
040700060208
040800060208     c                   endif
040900060208      * decodifico la nuova area e recupero la stampante
041000060208     c                   eval      savcar = Mtccar
041100060222      * se la stampante del Totale per area � in tabella  faccio ovrprtf
041200060222     c                   If        D44Tou = 'T'
041300060208     c                   movel     '05'          Tabcod
041400060208     c                   movel(P)  savcar        Tabkey
041500060208     c     keytab        chain     tabel00f
041600060208     c                   if        %found(tabel00f)
041700060208     c                   movel     tbluni        ds05
041800060208     c                   else
041900060208     c                   clear                   ds05
042000060208     c                   endif
042100060208
042200060208     c                   eval      descar = �05des
042300060208      * se esiste la coda di output per l'area eveguo la ovrprtf
042400060208     c                   If        �05otq <> *blanks
042500060208
042600060208     c                   clear                   comman
042700060208     c                   movea     cmdo(2)       comman
042800060208     c                   eval      %subst(comman:23:10) = �05otq
042900060208     c                   Call      'QCMDEXC'                            99
043000060208     c                   Parm                    Comman
043100060208     c                   Parm                    Lun
043200060208     c                   endif
043300060222     c                   endif
043400060208
043500060208     c                   open      tnsb46pa
043600060208
043700060208     c                   write     sb46at
043800060208     c                   write     sb46at1
043900060208     c                   write     sb46atdc
044000060208     c                   write     sb46at2
044100060208     c                   write     sb46atdc
044200060208
044300060208     c                   endsr
044400060109      *------------------------------------------------------------------------*
044500060109      * IMPOSTAZIONI INIZIALI
044600060109      *------------------------------------------------------------------------*
044700060109     c     SR_Imposta    BegSr
044800060109
044900060105      * Utente di sede
045000060110     c                   If        DutPou = 46
045100060105     c                   Eval      *In05 = *On
045200060110     c                   EndIf
045300060203
045400060203      * verifico se devo stampare il totale imponibile
045500060203     c                   eval      *in20 = (d44fsi = 'S')
045600060109
045700060109      * Recupero la moneta di conto
045800060109     c                   clear                   Tibs02ds
045900060109     c                   clear                   Dged
046000060109
046100060109     c                   Movel     'C'           T02mod
046200060109     c                   Movel     KNSIF         T02sif
046300060109     c                   Movel     'GED'         T02cod
046400060109     c                   Movel     '1'           T02ke1
046500060109      *
046600060109     c                   Call      'TIBS02R'
046700060109     c                   Parm                    Kpjba
046800060109     c                   Parm                    Tibs02ds
046900060109      *
047000060109     c                   If        T02err = *blanks
047100060109     c                   Movel     T02uni        Dged
047200060109     c                   Endif
047300060109
047400060109      * Ricerca degli importi standard nella moneta di conto
047500060109
047600060109     c                   Clear                   Tibs02ds
047700060109     c                   Clear                   Dgei
047800060109     c                   Movel     'L'           T02tla
047900060109     c                   Movel     'C'           T02mod
048000060109     c                   Movel     KNSIF         T02sif
048100060109     c                   Movel     'GEI'         T02cod
048200060109     c                   Movel     �GEDCN        T02ke1
048300060109
048400060109     c                   Call      'TIBS02R'
048500060109     c                   Parm                    Kpjba
048600060109     c                   Parm                    Tibs02ds
048700060109
048800060109     c                   If        T02err =  *blanks
048900060109     c                   Movel     T02uni        Dgei
049000060109     c                   Endif
049100060109
049200060119      * Ricerco il limite standard dell'importo da assicurare
049300060119     c                   Clear                   Tibs02ds
049400060119     c                   Clear                   Dlia
049500060119     c                   Eval      T02Mod = 'C'
049600060119     c                   Eval      T02Sif = Knsif
049700060119     c                   Eval      T02Cod = 'LIA'
049800060119     c                   Movel     '001'         T02Ke1
049900060119     c                   Call      'TIBS02R'
050000060119     c                   Parm                    Kpjba
050100060119     c                   Parm                    Tibs02ds
050200060119     c                   If        T02Err = *Blanks
050300060119     c                   Movel     T02Uni        Dlia
050400060119     c                   EndIf
050500060119
050600060105
050700060109     c                   endsr
050800060208      *------------------------------------------------------------------------*
050900060208      * Crea File in Gaitraazm
051000060208      *------------------------------------------------------------------------*
051100060208     c     SR_Creapf     BegSr
051200060208
051300060208     c                   close     wfmtc01l
051400060208
051500060208     c                   If        d44crf = 'S'
051600060208
051700060306     c                   clear                   comman1
051800060306     c                   movea     cmdc(1)       comman1
051900060306     c                   eval      %subst(comman1:64:10) = D44mbr
052000060208     c                   Call      'QCMDEXC'                            99
052100060306     c                   Parm                    Comman1
052200060306     c                   Parm                    Lun1
052300060208
052400060208     c                   endif
052500060208
052600060208     c                   Endsr
052700060109      *------------------------------------------------------------------------*
052800060109      * ROUTINE INIZIALE
052900060109      *------------------------------------------------------------------------*
053000060109     c     *Inzsr        BegSr
053100060109
053200060109     c     *entry        plist
053300060109     c                   parm                    Kpjba
053400060119     c                   eval      Tnsb44ds = Kpjbu
053500060109      * Reperisco data corrente
053600060109     c                   time                    w0140
053700060109     c                   move      w0140         wData
053800060206      * ora
053900060206     c                   movel     w0140         wora
054000060206
054100060202
054200060109     c                   clear                   WLBdat
054300060109     c                   eval      G02dat = wData
054400060109     c                   call      'XSRDA8'
054500060109     c                   parm                    WLBdat
054600060109     c                   z-add     G02inv        DateU
054700060109
054800060109     c     *dtaara       define    �azute        azuteds
054900060109     c     *dtaara       define    �datiute      ddatiute
055000060109     c                   in(E)     *dtaara
055100060109     c                   If        %error  or RSUT = *blanks
055200060109     c                   CLEAR                   tibs34ds
055300060109     c                   CALL      'TIBS34R'
055400060109     c                   PARM                    tibs34ds
055500060109     c                   in        *dtaara
055600060109     c                   EndIf
055700060206      *
055800060206      * Preparo i campi data da stampare
055900060206      * Data spedizione merce dal
056000060206     c                   Clear                   wlbdat
056100060206     c                   Z-add     D44dsd        g02inv
056200060206     c                   Movel     '3'           g02err
056300060206     c                   Call      'XSRDA8'
056400060206     c                   Parm                    wlbdat
056500060206     c                   Z-add     g02dat        DatDcd
056600060206      * Data spedizione merce al
056700060206     c                   Clear                   wlbdat
056800060206     c                   Z-add     D44dsa        g02inv
056900060206     c                   Movel     '3'           g02err
057000060206     c                   Call      'XSRDA8'
057100060206     c                   Parm                    wlbdat
057200060206     c                   Z-add     g02dat        DatDca
057300060206
057400060207      * se stampa unica apro il file di spool
057500060207
057600060207     c                   If        d44spp <> 'S'
057700060207     c                   open      tnsb46p
057800060207     c                   endif
057900060208
058000060208     c                   open      wfmtc01l
058100060208
058200060105      *
058300060105      *------------------------------------------------------------------------*
058400060105      * KLIST
058500060105      *------------------------------------------------------------------------*
058600060105
058700060109
058800060109      *---- File Tabel00f
058900060109
059000060109     c     KeyTab        Klist
059100060109     c                   Kfld                    Codut
059200060109     c                   Kfld                    Tabcod
059300060109     c                   Kfld                    Tabkey
059400060206
059500060105     c                   EndSr
059600060202** cmd ovrprtf
059700060206OVRPRTF TNSB46P  OUTQ(xxxxxxxxxx) USRDTA('MancaTarif') SAVE(*YES) HOLD(*YES)       1
059800060208OVRPRTF TNSB46PA OUTQ(xxxxxxxxxx) USRDTA('Tot.ManTar') SAVE(*YES) HOLD(*YES)       1
059900060208** cmdc
060000060227CPYF FROMFILE(QTEMP/WFMTC00F) TOFILE(GAITRAAZM/WFMTC00F) TOMBR(          ) MBROPT(*REPLACE)
