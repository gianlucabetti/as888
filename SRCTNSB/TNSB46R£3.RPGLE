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
009400060207      * 40    Alta intensità su peso fatturabile
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
015100060206    4c                   If        §ogssc = *blanks
015200060202      * recupero la stampante generica
015300060202     c                   movel     orgde4        og144
015400060202     c                   eval      §ogssc = §ogssp
015500060206    4c                   endif
015600060202
015700060206     c                   clear                   comman
015800060206     c                   movea     cmdo(1)       comman
015900060206     c                   eval      %subst(comman:23:10) = §ogssc
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
018501090327     c                   clear                   stpori
018600060207     c                   eval      mtclnp1 = mtclnp
018700060202
018800060202      * Data spedizione
018900060202     c                   Clear                   wlbdat
019000060202     c                   Z-add     MTCdsp        g02inv
019100060202     c                   Movel     '3'           g02err
019200060202     c                   Call      'XSRDA8'
019300060202     c                   Parm                    wlbdat
019400060202     c                   Z-add     g02dat        DatDsp
019500060203      * Imponibile
019600120417     c                   If        Mtcimp = 'IMP'  or Mtcimp = 'IMF'
019700060203     c                   eval      stpvar = 'IMP:'
019800060203     c                   z-add     mtcimv        stpimv
019900060206     c                   movel     mtcdiv        stpdiv
020000060203     c                   else
020100060206     c                   clear                   stpvar
020200060203     c                   clear                   stpimv
020300060206     c                   clear                   stpdiv
020400060203     c                   endif
020500060223
020600060223      * se devo stampare imponibile per cliente e l'imponibile non c'è accendo il 21
020700120416     c                   If        *in20 and mtcimv = *zeros
020800060223     c                   eval      *in21 = *on
020900060223     c                   else
021000060223     c                   eval      *in21 = *off
021100060223     c                   endif
021200060203      * verifico se ci sono errori diversi da IMP
021300060203     c                   If        Mtcimp = '   ' and mtcass <> '   '
021400060206     c                   eval      stpvar = 'ASS:'
021500060206     c                   z-add     mtcias        stpimv
021600060206     c                   movel     mtcvas        stpdiv
021700060206     c                   endif
021800060207      * se media del collo sballata evidenzio il campo peso fatturabile
021900090327     c***                eval      *in40 = (mtcesc <> '   ')
021901090327     c                   eval      *in40 = (mtcesc =  '!!!')
021902090327      * se segnalato il fatto che sono stati utilizzati i colli originali
021903090327      * lo stampo
021904090327     c                   if        %subst(mtcesc:1:1) = 'o'
021905090327     c                   eval      stpori = 'o'
021906090327     c                   endif
022000060207      * verifico se si tratta della stessa spedizione che ho letto prima
022100060207     c                   If        Savspe <> Mtcspe
022200060207     c                   eval      *in41 = *off
022300060207     c                   eval      Savspe = Mtcspe
022400060207     c                   else
022500060207     c                   eval      *in41 = *on
022600060207     c                   endif
022700060203      * scrivo la riga di dettaglio
022800060203
022900060203      * overflow
023000060203     c                   if        *in63
023100060203
023200060206     c                   write     sb46tleg
023300060206     c                   write     sb46t
023400060206     c                   write     sb46t1
023500060206     c                   write     sb46tdcm
023600060206     c                   write     sb46t2
023700060206     c                   write     sb46tdcm
023800060203     c                   eval      *in63 = *off
023900060203
024000060203     c                   endif
024100060202
024200060206     c                   write     sb46r
024300060210     c                   eval      almeno1 = '1'
024400060210
024500060206      * se per la stessa spedizione c'è anche errore di assicurazione stampo
024600060206     c                   If        Mtcass <> '   '
024700060227     c                   z-add     mtcias        stpias
024800060206     c                   write     sb46r2
024900060227     c                   else
025000060227     c                   clear                   stpias
025100060206     c                   endif
025200060207      * calcolo totali   franchi/assegnati
025300060207     c                   If        %subst(mtctbl:1:1) = 'F'
025400060207     c                   add       1             totfra
025500060207     c                   else
025600060207     c                   add       1             totass
025700060207     c                   endif
025800060207      * calcolo totali spedizioni
025900060207     c                   add       1             totfin
026000060207      * calcolo totali imponibile
026100060207     c                   If        *in20
026200060223      * calcolo totali spedizioni con imponibile
026300060223     c                   If        mtcimv > 0
026400060223     c                   add       1             tottas
026500060207     c                   add       mtcimv        totimp
026600060223     c                   else
026700060223     c                   add       1             totnotas
026800060223     c                   endif
026900060207     c                   endif
027000060207
027100060206     c                   enddo
027200060207
027300060210     c                   SELECT
027400060210      * se ho scritto almeno una riga faccio anche i totali
027500060210     c                   when      Almeno1 <> ' '
027600060208      * fine lettura faccio i totali del p.o. se sto stampando senza totali  imponibile
027700060207     c                   if        not *in20
027800060207     c                   Exsr      Sr_totali
027900060207      * se stampa separata per P.O.
028000060207     c                   If        D44spp = 'S'
028100060207     c                   Close     tnsb46p
028200060207     c                   endif
028300060208      * se stampa dei totali per area
028400060208     c                   If        D44tot = 'S'
028500060208      * overflow
028600060208     c                   If        *in64
028700060208
028800060208     c                   write     sb46at
028900060208     c                   write     sb46at1
029000060208     c                   write     sb46atdc
029100060208     c                   write     sb46at2
029200060208     c                   write     sb46atdc
029300060208     c                   eval      *in64 = *off
029400060208
029500060208     c                   endif
029600060208      * totali per area
029700060209     c                   write     sb46atdc
029800060208     c                   write     sb46atfi
029900060208     c                   close     tnsb46pa
030000060208     c                   endif
030100060208      * totali per cliente
030200060207     c                   else
030300060207     c                   if        *in63
030400060207
030500060207     c                   write     sb46t
030600060207     c                   write     sb46t1
030700060207     c                   write     sb46tdcm
030800060207     c                   write     sb46t2
030900060207     c                   write     sb46tdcm
031000060207     c                   eval      *in63 = *off
031100060207
031200060207     c                   endif
031300060207
031400060207     c                   write     sb46tfii
031500060207
031600060207     c                   close     tnsb46p
031700060207
031800060207     c                   endif
031900060210
032000060210     c                   other
032100060223      * solo se la stampa è separata per PO apro il file di spool
032200060223     c                   If        D44spp = 'S'
032300060210     c                   open      tnsb46p
032400060223     c                   endif
032500060210
032600060210     c                   write     sb46t
032700060210     c                   write     sb46par
032800060210     c                   write     sb46e
032900060210     c                   close     tnsb46p
033000060210
033100060210     c                   endsl
033200060207      *
033300060206
033400060206     c                   endsr
033500060207      *------------------------------------------------------------------------*
033600060207      * Totali per Punto operativo
033700060207      *------------------------------------------------------------------------*
033800060207     c     SR_Totali     BegSr
033900060207
034000060207      * overflow
034100060207     c                   if        *in63
034200060207
034300060207     c                   write     sb46tleg
034400060207     c                   write     sb46t
034500060207     c                   write     sb46t1
034600060207     c                   write     sb46tdcm
034700060207     c                   write     sb46t2
034800060207     c                   write     sb46tdcm
034900060207     c                   eval      *in63 = *off
035000060207
035100060207     c                   endif
035200060207
035300060207     c                   write     sb46tfig
035400060207     c                   write     sb46tleg
035500060207     c                   eval      *in63 = *off
035600060207
035700060207      * Verifico se devo fare i totali di area
035800060207
035900060207     c                   If        D44tot = 'S'
036000060208     c                   write     sb46ar
036100060208      * sommo i totali per area
036200060208     c                   add       totfra        toafra
036300060208     c                   add       totass        toaass
036400060208     c                   add       totfin        toafin
036500060208
036600060207     c                   endif
036700060207      * pulisco i campi di output
036800060207     c                   clear                   totfra
036900060207     c                   clear                   totass
037000060207     c                   clear                   totfin
037100060207
037200060207     c                   endsr
037300060208      *------------------------------------------------------------------------*
037400060208      * GESTIONE AREA
037500060208      *------------------------------------------------------------------------*
037600060208     c     SR_AREA       BegSr
037700060208
037800060208      * se diverso da zero devo fare i totali e chiudere il file di spool
037900060208     c                   If        Savcar > 0
038000060208      * overflow
038100060208     c                   If        *in64
038200060208
038300060208     c                   write     sb46at
038400060208     c                   write     sb46at1
038500060208     c                   write     sb46atdc
038600060208     c                   write     sb46at2
038700060208     c                   write     sb46atdc
038800060208     c                   eval      *in64 = *off
038900060208
039000060208     c                   endif
039100060208      * totali per area
039200060208     c                   write     sb46atfi
039300060208     c                   close     tnsb46pa
039400060208
039500060208     c                   clear                   toafra
039600060208     c                   clear                   toaass
039700060208     c                   clear                   toafin
039800060208
039900060208     c                   endif
040000060208      * decodifico la nuova area e recupero la stampante
040100060208     c                   eval      savcar = Mtccar
040200060222      * se la stampante del Totale per area è in tabella  faccio ovrprtf
040300060222     c                   If        D44Tou = 'T'
040400060208     c                   movel     '05'          Tabcod
040500060208     c                   movel(P)  savcar        Tabkey
040600060208     c     keytab        chain     tabel00f
040700060208     c                   if        %found(tabel00f)
040800060208     c                   movel     tbluni        ds05
040900060208     c                   else
041000060208     c                   clear                   ds05
041100060208     c                   endif
041200060208
041300060208     c                   eval      descar = §05des
041400060208      * se esiste la coda di output per l'area eveguo la ovrprtf
041500060208     c                   If        §05otq <> *blanks
041600060208
041700060208     c                   clear                   comman
041800060208     c                   movea     cmdo(2)       comman
041900060208     c                   eval      %subst(comman:23:10) = §05otq
042000060208     c                   Call      'QCMDEXC'                            99
042100060208     c                   Parm                    Comman
042200060208     c                   Parm                    Lun
042300060208     c                   endif
042400060222     c                   endif
042500060208
042600060208     c                   open      tnsb46pa
042700060208
042800060208     c                   write     sb46at
042900060208     c                   write     sb46at1
043000060208     c                   write     sb46atdc
043100060208     c                   write     sb46at2
043200060208     c                   write     sb46atdc
043300060208
043400060208     c                   endsr
043500060109      *------------------------------------------------------------------------*
043600060109      * IMPOSTAZIONI INIZIALI
043700060109      *------------------------------------------------------------------------*
043800060109     c     SR_Imposta    BegSr
043900060109
044000060105      * Utente di sede
044100060110     c                   If        DutPou = 46
044200060105     c                   Eval      *In05 = *On
044300060110     c                   EndIf
044400060203
044500060203      * verifico se devo stampare il totale imponibile
044600060203     c                   eval      *in20 = (d44fsi = 'S')
044700060109
044800060109      * Recupero la moneta di conto
044900060109     c                   clear                   Tibs02ds
045000060109     c                   clear                   Dged
045100060109
045200060109     c                   Movel     'C'           T02mod
045300060109     c                   Movel     KNSIF         T02sif
045400060109     c                   Movel     'GED'         T02cod
045500060109     c                   Movel     '1'           T02ke1
045600060109      *
045700060109     c                   Call      'TIBS02R'
045800060109     c                   Parm                    Kpjba
045900060109     c                   Parm                    Tibs02ds
046000060109      *
046100060109     c                   If        T02err = *blanks
046200060109     c                   Movel     T02uni        Dged
046300060109     c                   Endif
046400060109
046500060109      * Ricerca degli importi standard nella moneta di conto
046600060109
046700060109     c                   Clear                   Tibs02ds
046800060109     c                   Clear                   Dgei
046900060109     c                   Movel     'L'           T02tla
047000060109     c                   Movel     'C'           T02mod
047100060109     c                   Movel     KNSIF         T02sif
047200060109     c                   Movel     'GEI'         T02cod
047300060109     c                   Movel     §GEDCN        T02ke1
047400060109
047500060109     c                   Call      'TIBS02R'
047600060109     c                   Parm                    Kpjba
047700060109     c                   Parm                    Tibs02ds
047800060109
047900060109     c                   If        T02err =  *blanks
048000060109     c                   Movel     T02uni        Dgei
048100060109     c                   Endif
048200060109
048300060119      * Ricerco il limite standard dell'importo da assicurare
048400060119     c                   Clear                   Tibs02ds
048500060119     c                   Clear                   Dlia
048600060119     c                   Eval      T02Mod = 'C'
048700060119     c                   Eval      T02Sif = Knsif
048800060119     c                   Eval      T02Cod = 'LIA'
048900060119     c                   Movel     '001'         T02Ke1
049000060119     c                   Call      'TIBS02R'
049100060119     c                   Parm                    Kpjba
049200060119     c                   Parm                    Tibs02ds
049300060119     c                   If        T02Err = *Blanks
049400060119     c                   Movel     T02Uni        Dlia
049500060119     c                   EndIf
049600060119
049700060105
049800060109     c                   endsr
049900060208      *------------------------------------------------------------------------*
050000060208      * Crea File in Gaitraazm
050100060208      *------------------------------------------------------------------------*
050200060208     c     SR_Creapf     BegSr
050300060208
050400060208     c                   close     wfmtc01l
050500060208
050600060208     c                   If        d44crf = 'S'
050700060208
050800060306     c                   clear                   comman1
050900060306     c                   movea     cmdc(1)       comman1
051000060306     c                   eval      %subst(comman1:64:10) = D44mbr
051100060208     c                   Call      'QCMDEXC'                            99
051200060306     c                   Parm                    Comman1
051300060306     c                   Parm                    Lun1
051400060208
051500060208     c                   endif
051600060208
051700060208     c                   Endsr
051800060109      *------------------------------------------------------------------------*
051900060109      * ROUTINE INIZIALE
052000060109      *------------------------------------------------------------------------*
052100060109     c     *Inzsr        BegSr
052200060109
052300060109     c     *entry        plist
052400060109     c                   parm                    Kpjba
052500060119     c                   eval      Tnsb44ds = Kpjbu
052600060109      * Reperisco data corrente
052700060109     c                   time                    w0140
052800060109     c                   move      w0140         wData
052900060206      * ora
053000060206     c                   movel     w0140         wora
053100060206
053200060202
053300060109     c                   clear                   WLBdat
053400060109     c                   eval      G02dat = wData
053500060109     c                   call      'XSRDA8'
053600060109     c                   parm                    WLBdat
053700060109     c                   z-add     G02inv        DateU
053800060109
053900060109     c     *dtaara       define    §azute        azuteds
054000060109     c     *dtaara       define    §datiute      ddatiute
054100060109     c                   in(E)     *dtaara
054200060109     c                   If        %error  or RSUT = *blanks
054300060109     c                   CLEAR                   tibs34ds
054400060109     c                   CALL      'TIBS34R'
054500060109     c                   PARM                    tibs34ds
054600060109     c                   in        *dtaara
054700060109     c                   EndIf
054800060206      *
054900060206      * Preparo i campi data da stampare
055000060206      * Data spedizione merce dal
055100060206     c                   Clear                   wlbdat
055200060206     c                   Z-add     D44dsd        g02inv
055300060206     c                   Movel     '3'           g02err
055400060206     c                   Call      'XSRDA8'
055500060206     c                   Parm                    wlbdat
055600060206     c                   Z-add     g02dat        DatDcd
055700060206      * Data spedizione merce al
055800060206     c                   Clear                   wlbdat
055900060206     c                   Z-add     D44dsa        g02inv
056000060206     c                   Movel     '3'           g02err
056100060206     c                   Call      'XSRDA8'
056200060206     c                   Parm                    wlbdat
056300060206     c                   Z-add     g02dat        DatDca
056400060206
056500060207      * se stampa unica apro il file di spool
056600060207
056700060207     c                   If        d44spp <> 'S'
056800060207     c                   open      tnsb46p
056900060207     c                   endif
057000060208
057100060208     c                   open      wfmtc01l
057200060208
057300060105      *
057400060105      *------------------------------------------------------------------------*
057500060105      * KLIST
057600060105      *------------------------------------------------------------------------*
057700060105
057800060109
057900060109      *---- File Tabel00f
058000060109
058100060109     c     KeyTab        Klist
058200060109     c                   Kfld                    Codut
058300060109     c                   Kfld                    Tabcod
058400060109     c                   Kfld                    Tabkey
058500060206
058600060105     c                   EndSr
058700060202** cmd ovrprtf
058800060206OVRPRTF TNSB46P  OUTQ(xxxxxxxxxx) USRDTA('MancaTarif') SAVE(*YES) HOLD(*YES)       1
058900060208OVRPRTF TNSB46PA OUTQ(xxxxxxxxxx) USRDTA('Tot.ManTar') SAVE(*YES) HOLD(*YES)       1
059000060208** cmdc
059100060227CPYF FROMFILE(QTEMP/WFMTC00F) TOFILE(GAITRAAZM/WFMTC00F) TOMBR(          ) MBROPT(*REPLACE)
