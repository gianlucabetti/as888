000100060320     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200060320      *------------------------------------------------------------------------*
000300060320      *
000400060928      *  Pulizia Tariffe obsolete
000500060320      *
000600060320      *------------------------------------------------------------------------*
000700060215
000800060215     ftntam04l  if   e           k disk
000900060320     fazorg01l  if   e           k disk
001000060320     ftabel00f  if   e           k disk
001100080704     fcnacvt1l  if   e           k disk
001200080703     fwftan00f  o    e             disk    usropn
001300080708     ftnta93p   o    e             printer oflind(*in05)
001400060323      *------------------------------------------------------------------------*
001500060323      *  RIEPILOGO INDICATORI
001600060323      *------------------------------------------------------------------------*
001700060323      * 12 - utente di sede
001800080926      * 20 - proteggo in stampa il campo del commerciale
001900060323      * 30 - comodo
002000060215
002100060215      *------------------------------------------------------------------------*
002200060215      *   V A R I A B I L I
002300060215      *------------------------------------------------------------------------*
002400080710     d comman          s            110
002500060320     d codut           s              1  0 inz(1)
002600060320     d kcar            s                   like(orgcar)
002700060320     d kcod            s                   like(tblcod)
002800060320     d kksc            s                   like(tamksc)
002900060320     d kkey            s                   like(tblkey)
003000080710     d lenght          s             15  5 inz(110)
003100080708     d savcli          s                   like(tanksc)
003200080708     d savcmm          s                   like(tancmm)
003300080708     d savfil          s                   like(tanfil)
003400060215     d wctr            s                   like(tamctr)
003500060928     d wprg            s                   like(tamprg)
003600060320     d wdata           s              8  0
003700080708     d wora            s              6  0
003800060215     d wksc            s                   like(tamksc)
003900060320     d wlib            s              9
004000060320     d w003a           s              3
004100060320     d w0030           s              3  0
004200060320     d w0140           s             14  0
004300060320     d xx              s              3  0
004400060320     d yy              s              3  0
004500080704     d numsped         s             11  0
004600080704     d numtarcli       s             11  0
004700080704     d numtarann       s             11  0
004800080704     d dtaacquisi      s              8  0
004900060320
005000060320      *------------------------------------------------------------------------*
005100060320      *   S C H I E R E
005200060320      *------------------------------------------------------------------------*
005300080709     d cmd             s             80    dim(3) ctdata perrcd(1)
005400060320     d skpoel          s              3    dim(250)
005500060215
005600060215      *------------------------------------------------------------------------*
005700060215      *   D S   I N T E R N E / E S T E R N E
005800060215      *------------------------------------------------------------------------*
005900060320
006000060320     d parm01          ds
006100080709     d  parfil                 1      3  0
006200080709     d  parcar                 4    153    dim(50)
006300080709     d  pardus               154    161  0
006400080709     d  pardna               162    169  0
006500080709     d  parfile              170    170
006600080709     d  parabi               171    172
006700080710     d  parcrf               173    173
006800060321
006900060321     d wlbdat          ds                  inz
007000060321     d  g02dat                 1      8  0
007100060321     d  g02inv                 9     16  0
007200060321     d  g02err                17     17
007300060321     d  g02tgi                18     22  0
007400060320
007500060320     d azuteds       e ds                  extname(azute00f)
007600060320     d ddatiute      e ds
007700060320     d ds_cnaco      e ds                  inz  extname(Cnaco00f)
007800060320     d ds_cnind      e ds                  inz  extname(Cnind00f)
007900060320     d ds_cnclp      e ds                  inz  extname(Cnclp00f)
008000060320     d ds_fncls      e ds                  inz  extname(Fncls00f)
008100080709     d azorgds       e ds                  extname(azorg02l)
008200080708     d wftands       e ds                  extname(wftan00f)
008300060215     d kpjba         e ds
008400060320     d og143         e ds
008500060726     d tibs02ds      e ds
008600060320     d tibs34ds      e ds
008700060320     d tibs69ds      e ds
008800060320     d trul31ds      e ds                  inz
008900061002     d ds01          e ds
009000080711     d dsta01        e ds
009100060215
009200060215      *------------------------------------------------------------------------*
009300080707     C*
009400080707     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni
009500080707     C
009600080707     C/EXEC SQL
009700080707     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
009800080707     C/END-EXEC
009900080707     C*
010000060215
010100060320      * ciclo per i p.o. caricati in schiera
010200060320    1c                   do        250           xx
010300060320     c                   if        skpoel(xx) = *blanks
010400060320     c                   leave
010500060320     c                   endif
010600060320
010700060320     c                   clear                   wksc
010800060215     c                   eval      wctr = 999
010900060320     c                   move      *all'0'       kksc
011000060320     c                   movel     skpoel(xx)    kksc
011100060215
011200060320     c     kksc          setll     tntam04l
011300060320    2c                   do        *hival
011400060215     c                   read      tntam04l
011500060215      * fine file
011600060215     c                   if        %eof(tntam04l)
011700080926      * verifico se wksc > 0 se iltotale delle tariffe annullabili è uguale
011800080926      * al totale delle tariffe del cliente in questo caso devo aggiornare
011900080926      * un flag nel file wftan che il cliente è annullabile
012000080926     c                   if        wksc > 0 and numtarcli = numtarann
012100080926
012200080926     c/exec Sql
012300080926     c+ update wftan00f set tanann = '*'
012400080926     c+ where tanksc = :wksc
012500080926     c/End-Exec
012600080926
012700080926     c                   endif
012800060215     c                   leave
012900060215     c                   endif
013000060320
013100080704      * cambio cliente
013200080704    3c                   if        tamksc <> wksc
013300080707      * verifico se wksc > 0 se iltotale delle tariffe annullabili è uguale
013400080707      * al totale delle tariffe del cliente in questo caso devo aggiornare
013500080707      * un flag nel file wftan che il cliente è annullabile
013600080707     c                   if        wksc > 0 and numtarcli = numtarann
013700080707
013800080707     c/exec Sql
013900080707     c+ update wftan00f set tanann = '*'
014000080707     c+ where tanksc = :wksc
014100080707     c/End-Exec
014200080707
014300080707     c                   endif
014400080707
014500080704     c                   clear                   numtarcli
014600080704     c                   clear                   numtarann
014700080704      * se p.o. maggiore esco
014800080704     c                   movel     tamksc        w003a
014900080704     c                   if        w003a > skpoel(xx)
015000080704     c                   leave
015100080704     c                   endif
015200080704      * cerco i dati anagrafici
015300080704     c                   clear                   tibs69ds
015400080704     c                   move      tamksc        i69kac
015500080704     c                   move      tamksc        i69kin
015600080704     c                   move      tamksc        i69kcp
015700080704     c                   call      'TIBS69R'
015800080704     c                   parm                    tibs69ds
015900080704     c                   parm                    ds_cnaco
016000080704     c                   parm                    ds_cnind
016100080704     c                   parm                    ds_cnclp
016200080704     c                   parm                    ds_fncls
016300080704      * controllo data acquisizione
016400080704      * verifico se ha un record nel file variazioni di inserimento
016500080704
016600080704      * valorizzo il campo di comodo DTAACQUISI con un valore minimo in quanto
016700080704      * il file delle variazioni è noto poco tempo fa e prima non c'era nulla
016800080704      * visto che ci serve la data di acquisizione del cliente e ci serve
016900080704      * all'incirca al massimo 6 mesi indietro e il file delle variazioni
017000080704      * esisteva già ... tutto quello che è nato prima è comunque da prendere
017100080704      * in considerazione
017200080704
017300080704     c                   eval      dtaacquisi = 00010101
017400080704     c     Kacvt         setll     cnacvt1l
017500080704    4c                   do        *hival
017600080704     c     kacvt         reade     cnacvt1l
017700080704      * fine file
017800080704     c                   if        %eof(cnacvt1l)
017900080704     c                   leave
018000080704     c                   endif
018100080704
018200080704      * verifico se c'è stato un inserimento
018300080704     c                   If        acvcta <> 'M'
018400080704      * valorizzo la dta acquisizione
018500080704     c                   eval      dtaacquisi = acvdav
018600080704     c                   leave
018700080704     c                   endif
018800080704
018900080704    4c                   enddo
019000080704
019100080704      * controllo la data acquisizione se maggiore del limite impostato
019200080704     c                   if        dtaacquisi >= pardna
019300080704     c                   iter
019400080704     c                   endif
019500080704
019600080704    3c                   endif
019700080704
019800060320      * cambio tariffa
019900080704    3c                   if        tamctr <> wctr or tamksc <> wksc
020000060320
020100060320      * escludo le annullate
020200060215     c                   if        tamatb <> *blanks
020300060215     c                   iter
020400060215     c                   endif
020500080704      * memorizzo il numero di tariffe del cliente
020600080704     c                   add       1             numtarcli
020700080704      * se la tariffa che sto leggendo deve ancora andare in vigore la escludo
020800080704     c                   if        tamddt > *date
020900080704     c                   eval      wctr = tamctr
021000080704     c                   eval      wksc = tamksc
021100080704     c                   iter
021200080704     c                   endif
021300080703      * considero le tariffe dei clienti che non hanno avuto spedizioni
021400080703      * dopo la data inserita a video controllando sul file titas
021500080704
021600080704     c                   clear                   numsped
021700080704
021800080708      * se sono su AS888 punto su titas in gaitragrps altrimenti su
021900080708      * gaitragru
022000080708
022100080708     c                   if        %subst(knsif:7:1) = 'P'
022200080703     c/exec sql
022300080710     c+     select count(*) into :NumSped
022400080710     c+     from    GAITRAGRPS/TITAS00F
022500080707     c+     where   TASksc = :TAMksc and TASctr = :TAMctr
022600080703     c+       and ((TASaas*10000)+TASmgs)  >=  :pardus
022700080710     c/end-exec
022800080710      * se non trovata nessuna spedizione vado a verificare in TITAS10
022900080710     c                   if        numsped = 0
023000080710     c/exec sql
023100080710     c+     select count(*) into :NumSped
023200080708     c+     from    GAITRAGRPS/TITAS10F
023300080707     c+     where   TASksc = :TAMksc and TASctr = :TAMctr
023400080703     c+       and ((TASaas*10000)+TASmgs)  >=  :pardus
023500080710     c/end-exec
023600080710      * se non trovata ANCORA nessuna spedizione vado a verificare in TITASP
023700080710     c                   if        numsped = 0
023800080710     c/exec sql
023900080710     c+     select count(*) into :NumSped
024000080710     c+     from    GAITRAGRPS/TITASP0F
024100080707     c+     where   TASksc = :TAMksc and TASctr = :TAMctr
024200080703     c+       and ((TASaas*10000)+TASmgs)  >=  :pardus
024300080703     c/end-exec
024400080710     c                   endif
024500080710     c                   endif
024600080708
024700080708     c                   else
024800080708     c/exec sql
024900080710     c+     select count(*) into :NumSped
025000080710     c+     from    GAITRAGRU/TITAS00F
025100080708     c+     where   TASksc = :TAMksc and TASctr = :TAMctr
025200080708     c+       and ((TASaas*10000)+TASmgs)  >=  :pardus
025300080710     c/end-exec
025400080710      * se non trovata nessuna spedizione vado a verificare in TITAS10
025500080710     c                   if        numsped = 0
025600080710     c/exec sql
025700080710     c+     select count(*) into :NumSped
025800080708     c+     from    GAITRAGRU/TITAS10F
025900080708     c+     where   TASksc = :TAMksc and TASctr = :TAMctr
026000080708     c+       and ((TASaas*10000)+TASmgs)  >=  :pardus
026100080710     c/end-exec
026200080710      * se non trovata ANCORA nessuna spedizione vado a verificare in TITASP0
026300080710     c                   if        numsped = 0
026400080710     c/exec sql
026500080710     c+     select count(*) into :NumSped
026600081030     c+     from    GAITRAGRUS/TITASP0F
026700080708     c+     where   TASksc = :TAMksc and TASctr = :TAMctr
026800080708     c+       and ((TASaas*10000)+TASmgs)  >=  :pardus
026900080708     c/end-exec
027000080710     c                   endif
027100080710     c                   endif
027200080708
027300080708     c                   endif
027400080703      *
027500080704      * se numero spedizione maggiore di 0 passo alla tariffa successiva
027600080703      *
027700080704     c                   if        numsped > *zeros
027800080704     c                   eval      wksc = tamksc
027900080704     c                   eval      wctr = tamctr
028000080704     c                   iter
028100080704     c                   endif
028200080704      * se passati tutti i controlli rendo la tariffa per il cliente annullabile
028300080704      *
028400080704     c                   add       1             numtarann
028500060929      * scrivo il file
028600060320     c                   exsr      scrivi
028700060215
028800060320     c                   eval      wksc = tamksc
028900060320     c                   eval      wctr = tamctr
029000060215
029100060320    3c                   endif
029200060320
029300060320    2c                   enddo
029400060215
029500060320    1c                   enddo
029600060320
029700080707     c                   exsr      sr_Stampa
029800060215
029900080710      * se si desidera il file devo copiarlo
030000080710     c                   if        parcrf = 'S'
030100080710     c                   clear                   comman
030200080710     c                   EVAL      comman = 'CPYF FROMFILE(QTEMP/WFTAN00F) '
030300080711     c                             + 'TOFILE('+ %TRIMR(WLIB) + '/WFTAN00F) '
030400080710     c                             + 'FROMMBR(WFTAN00F) MBROPT(*REPLACE)'
030500080710
030600080710     c                   call      'QCMDEXC'
030700080710     c                   parm                    comman
030800080710     c                   parm                    lenght
030900080710     c                   endif
031000080710
031100060215     c                   eval      *inlr = *on
031200060215
031300060215      *------------------------------------------------------------------------*
031400060320      * SCRIVO IL FILE PER LA STAMPA
031500060215      *------------------------------------------------------------------------*
031600060215     c     scrivi        begsr
031700060215
031800060323
031900060929     c                   clear                   wftan000
032000060215
032100061002      * job/utente/data/ora elaborazione
032200061002     c                   eval      tannoj = knmeb
032300061002     c                   eval      tanpru = knmus
032400061002     c                   eval      tandta = wdata
032500061002     c                   eval      tanora = wora
032600060320      * commerciale
032700060928     c                   eval      tancmm = clpage
032800060320     c                   eval      kcod = '01'
032900060928     c                   movel     tancmm        kkey
033000060320     c     ktabel        chain     tabel00f
033100060320     c                   if        not %found(tabel00f) or tblflg <> *blanks
033200060928     c                   eval      tandcmm = *all'*'
033300060320     c                   else
033400060928     c                   eval      tandcmm = tbluni
033500061002     c                   movel     tbluni        ds01
033600061002     c                   movel     §01rgf        tancmu
033700061002      * codice commerciale unificante decodifica
033800061002     c                   movel     tancmu        kkey
033900061002     c     ktabel        chain     tabel00f
034000061002     c                   if        not %found(tabel00f) or tblflg <> *blanks
034100061002     c                   eval      tandcmm = *all'*'
034200061002     c                   else
034300061002     c                   eval      tandcmu = tbluni
034400060320     c                   endif
034500061002     c                   endif
034600080707      * cliente
034700080707     c                   eval      tanksc = tamksc
034800080707     c                   eval      tanrag = acorag
034900080707      * distretto area po codice cliente
035000080707     c                   movel     tanksc        w0030
035100061002     c     w0030         chain     azorg01l
035200061002
035300061002      * distretto
035400061002     c                   eval      tandis = orgfl3
035500061002     c                   eval      kcod = '17'
035600061002     c                   eval      kkey = tandis
035700061002     c     ktabel        chain     tabel00f
035800061002     c                   if        not %found(tabel00f) or tblflg <> *blanks
035900061002     c                   eval      tanddis = *all'*'
036000061002     c                   else
036100061002     c                   eval      tanddis = tbluni
036200061002     c                   endif
036300061002      * area
036400061002     c                   eval      tancar = orgcar
036500061002     c                   eval      kcod = '05'
036600061002     c                   movel     tancar        kkey
036700061002     c     ktabel        chain     tabel00f
036800061002     c                   if        not %found(tabel00f) or tblflg <> *blanks
036900061002     c                   eval      tandcar = *all'*'
037000061002     c                   else
037100061002     c                   eval      tandcar = tbluni
037200061002     c                   endif
037300061002      * p.o.
037400061002     c                   eval      tanfil = orgfil
037500061002     c                   if        not %found(azorg01l) or orgfva <> *blanks
037600061002     c                   eval      tandfil = *all'*'
037700061002     c                   else
037800061002     c                   eval      tandfil = orgdes
037900061002     c                   endif
038000060215      * data ultima spedizione fattura
038100060928     c                   eval      tandus = clpdus
038200060215      * tariffa
038300060928     c                   eval      tanctr = tamctr
038400080711
038500080711     c                   movel     tamflo        dsta01
038600080711      * Tipo tariffa
038700080711     c                   select
038800080711     c                   when      §tadpd = 'S'
038900080711     c                   eval      tanfie = 'D'
039000080711     c                   when      §tafed = 'S'
039100080711     c                   eval      tanfie = 'F'
039200080711     c                   other
039300080711     c                   eval      tanfie = tamfie
039400080711     c                   endsl
039500080711      * descrizione
039600080711     c                   eval      tandsc = tamdcv
039700080711
039800060928     c                   eval      tanddt = tamddt
039900060928     c                   eval      tandst = tamdst
040000060323      * tariffa bloccata partenza/arrivo
040100060928     c                   eval      tanbap = tambap
040200060215      * scrivo file
040300060929     c                   write     wftan000
040400080707     c                   feod      wftan00f
040500060320
040600060215     c                   endsr
040700060320
040800080707      *------------------------------------------------------------------------*
040900080707      * Stampo in ordine di Area / P.O. / Codice commerciale / cliente
041000080707      *------------------------------------------------------------------------*
041100080707     c     sr_stampa     begsr
041200080707
041300080711      * Spedizione del
041400080711     c                   Clear                   wlbdat
041500080711     c                   Z-add     pardus        g02inv
041600080711     c                   Movel     '3'           g02err
041700080711     c                   Call      'XSRDA8'
041800080711     c                   Parm                    wlbdat
041900080711     c                   Z-add     g02dat        Stpdus
042000080711      * Data nascita da
042100080711     c                   Clear                   wlbdat
042200080711     c                   Z-add     pardna        g02inv
042300080711     c                   Movel     '3'           g02err
042400080711     c                   Call      'XSRDA8'
042500080711     c                   Parm                    wlbdat
042600080711     c                   Z-add     g02dat        Stpdna
042700080707     c/exec SQL
042800080707     c+ declare C1 cursor for
042900080708     c+ select * from wftan00f
043000080707     c+ order by tancar, tanfil, tancmm, tanksc, tanctr
043100080707     c+ for read only
043200080707     c/end-exec
043300080708
043400080708     c/exec sql
043500080708     c+ open c1
043600080708     c/end-exec
043700080707
043800080707     c/exec SQL
043900080708     c+ Fetch next from C1 into :wftands
044000080707     c/end-exec
044100080707
044200080707      * in fase di lettura
044300080707     c                   dow       sqlcod = *zeros
044400080707
044500080708      * a rottura di commerciale o di filiale cambio pagina
044600080708     c                   if        savcmm <> tancmm or
044700080708     c                             savfil <> tanfil
044800080926      *
044900080926     c                   exsr      sr_totali
045000080708      * valorizzo i campi in stampa
045100080708     c                   eval      agente = tancmm
045200080708     c                   eval      desage = tandcmm
045300080708     c                   eval      keypo  = tanfil
045400080708     c                   eval      despoc = tandfil
045500080707      * stampo la testata
045600080708     c                   write     ta93t
045700080708     c                   write     ta93t1
045800080708     c                   write     ta93tdcm
045900080708     c                   write     ta93t2
046000080708     c                   write     ta93tdcm
046100080710     c                   eval      *in05 = *off
046200080708      * Salvo i campi
046300080708     c                   eval      savcmm = tancmm
046400080708     c                   eval      savfil = tanfil
046500080708
046600080708     c                   endif
046700080708      * a cambio cliente stampo i dati del cliente
046800080708     c                   if        savcli <> tanksc
046900080708     c                   seton                                        40
047000080708     c                   eval      savcli = tanksc
047100080926      * preparo i totali clienti   / tariffe
047200080926     c                   add       1             totcli
047300080926     c                   if        tanann <> ' '
047400080926     c                   add       1             tottan
047500080926     c                   endif
047600080708     c                   endif
047700080708      * preparo i dati del dettaglio
047800080708     c                   eval      stpann = tanann
047900080708     c                   eval      stpksc = tanksc
048000080708     c                   eval      stprag = tanrag
048100080708      * Data decorrenza
048200080708     c                   Clear                   wlbdat
048300080708     c                   Z-add     Tanddt        g02inv
048400080708     c                   Movel     '3'           g02err
048500080708     c                   Call      'XSRDA8'
048600080708     c                   Parm                    wlbdat
048700080708     c                   Z-add     g02dat        Stpddt
048800080708      * Data scadenza
048900080708     c                   Clear                   wlbdat
049000080708     c                   Z-add     Tandst        g02inv
049100080708     c                   Movel     '3'           g02err
049200080708     c                   Call      'XSRDA8'
049300080708     c                   Parm                    wlbdat
049400080708     c                   Z-add     g02dat        Stpdst
049500080708      * scrivo la riga di dettaglio
049600080708
049700080708      * overflow
049800080708     c                   if        *in05
049900080708
050000080708     c                   write     ta93t
050100080708     c                   write     ta93t1
050200080708     c                   write     ta93tdcm
050300080708     c                   write     ta93t2
050400080708     c                   write     ta93tdcm
050500080708     c                   eval      *in05 = *off
050600080708     c                   eval      *in40 = *on
050700080708
050800080708     c                   endif
050900080708
051000080708     c                   write     ta93r
051100080708     c                   eval      *in40 = *off
051200080926     c                   add       1             tottar
051300080708
051400080707     c/exec sql
051500080708     c+ Fetch next from C1 into :wftands
051600080707     c/end-exec
051700080707     c                   enddo
051800080707     c
051900080926      *
052000080926     c                   eval      tanfil = 999
052100080926     c                   exsr      sr_totali
052200080707     c/exec sql
052300080707     c+ close C1
052400080707     c/end-exec
052500080708
052600080708     c                   endsr
052700080926      *------------------------------------------------------------------------*
052800080926      * Stampo i totali per commerciale e filiale
052900080926      *------------------------------------------------------------------------*
053000080926     c     sr_totali     begsr
053100080926
053200080926      * prima faccio i totali del commerciale precedente
053300080926     c                   if        savcmm <> *zeros
053400080926      * overflow
053500080926     c                   if        *in05
053600080926
053700080926     c                   write     ta93t
053800080926     c                   write     ta93t1
053900080926     c                   write     ta93tdcm
054000080926     c                   write     ta93t2
054100080926     c                   write     ta93tdcm
054200080926     c                   eval      *in05 = *off
054300080926     c                   endif
054400080926     c                   write     ta93tt
054500080926     c                   add       tottar        tottar_f
054600080926     c                   add       totcli        totcli_f
054700080926     c                   add       tottan        tottan_f
054800080926     c                   clear                   tottar
054900080926     c                   clear                   totcli
055000080926     c                   clear                   tottan
055100080926     c                   endif
055200080926
055300080926      * rottura di filiale
055400080926      * faccio i totali della filiale precedente
055500080926     c                   if        savfil <> tanfil and
055600080926     c                             savfil <> *zeros
055700080926
055800080926     c                   write     ta93t
055900080926     c                   eval      *in20 = *on
056000080926     c                   write     ta93t1
056100080926     c                   eval      *in20 = *off
056200080926     c                   write     ta93tdcm
056300080926     c                   eval      *in05 = *off
056400080926
056500080926     c                   write     ta93ttf
056600080926     c                   clear                   tottar_f
056700080926     c                   clear                   totcli_f
056800080926     c                   clear                   tottan_f
056900080926     c                   endif
057000080926     c                   endsr
057100060320      *--------------------------------------------------------------*
057200060320      * ROUTINE INIZIALE
057300060320      *--------------------------------------------------------------*
057400060320     c     *inzsr        begsr
057500060320
057600060320     c     *entry        plist
057700060320     c                   parm                    kpjba
057800060320     c                   movel     kpjbu         parm01
057900060320
058000060320     c     *dtaara       define    §azute        azuteds
058100060320     c     *dtaara       define    §datiute      ddatiute
058200060320
058300060320     c                   in(e)     *dtaara
058400060320     c                   if        %error or rsut = *blanks
058500060320     c                   clear                   tibs34ds
058600060320     c                   call      'TIBS34R'
058700060320     c                   parm                    tibs34ds
058800060320     c                   in        *dtaara
058900060320     c                   endif
059000060320
059100060320
059200060320      * se s.i. di prova imposto la libreria di prova
059300060320     c                   if        %subst(knsif:7:1) = 'P'
059400060320     c                   eval      wlib = 'GAITRAAZP '
059500060320     c                   else
059600060320     c                   eval      wlib = 'GAITRAAZM '
059700060320     c                   endif
059800080710      * creo il file in qtemp copiandolo dalla gaitraazm o gaitraazp
059900080710     c                   clear                   comman
060000080710     c                   EVAL      comman = 'CPYF FROMFILE('
060100080710     c                             + %TRIMR(WLIB)
060200080710     c                             + '/WFTAN00F) TOFILE(QTEMP/WFTAN00F) '
060300080710     c                             + 'FROMMBR(WFTAN00F) CRTFILE(*YES)'
060400080710
060500060320     c                   call      'QCMDEXC'
060600060320     c                   parm                    comman
060700060320     c                   parm                    lenght
060800080710      * pulisco il file
060900080710     c                   clear                   comman
061000080710     c                   EVAL      comman = 'CLRPFM FILE(QTEMP/WFTAN00F)'
061100080710
061200080710     c                   call      'QCMDEXC'
061300080710     c                   parm                    comman
061400080710     c                   parm                    lenght
061500080710
061600060929     c                   open      wftan00f
061700060320
061800060320      * reperisco data e ora del lancio
061900060320     c                   time                    w0140
062000060320     c                   move      w0140         wdata
062100061002     c                   movel     w0140         wora
062200060320
062300060320      * pulisco la sk dei p.o. da elaborare
062400060320     c                   clear                   skpoel
062500060320
062600060320      * se ho le aree carico una sk con tutti i p.o. relativi alle aree
062700060320      * richieste
062800060321    1c                   if        parcar(01) <> *blanks
062900080709    2c                   do        50            xx
063000060321    3c                   if        parcar(xx) <> *blanks
063100080709
063200080709     c                   move      parcar(xx)    kcar
063300080709     c/exec SQL
063400080709     c+ declare C2 cursor for
063500080709     c+ select * from azorg02l
063600080709     c+ where  orgcar = :Kcar
063700080709     c+ for read only
063800080709     c/end-exec
063900080709
064000080709     c/exec sql
064100080709     c+ open c2
064200080709     c/end-exec
064300080709
064400080709     c/exec SQL
064500080709     c+ Fetch next from C2 into :azorgds
064600080709     c/end-exec
064700080709
064800080709      * in fase di lettura
064900080709    4c                   dow       sqlcod = *zeros
065000080709    5c                   if        orgfva <> *blanks or
065100080709     c                             (orgfag <> 'A' and orgfag <> 'F')
065200080709     c                   iter
065300080709    5c                   endif
065400080709
065500080709     c                   move      orgfil        w003a
065600080709     c     w003a         lookup    skpoel                                 30
065700080709    5c                   if        not *in30
065800080709     c                   eval      yy = 1
065900080709     c     *blanks       lookup    skpoel(yy)                             30
066000080709    6c                   if        *in30
066100080709     c                   eval      skpoel(yy) = w003a
066200080709    6c                   endif
066300080709    5c                   endif
066400080709     c/exec sql
066500080709     c+ Fetch next from C2 into :azorgds
066600080709     c/end-exec
066700080709    4c                   enddo
066800080709     c
066900080709     c/exec sql
067000080709     c+ close C2
067100080709     c/end-exec
067200080709
067300060320    3c                   endif
067400060320    2c                   enddo
067500060320
067600060320   x1c                   else
067700060320      * carico il p.o. richiesto a video
067800060321    2c                   if        parfil <>  *zeros
067900060321     c                   move      parfil        skpoel(1)
068000060320   x2c                   else
068100060320     c                   clear                   trul31ds
068200060321     c                   eval      i31abi = parabi
068300060320     c                   eval      i31cdi = dutdis
068400060320     c                   eval      i31car = dutare
068500060320     c                   eval      i31cpo = dutpou
068600060320     c                   call      'TRUL31R'
068700060320     c                   parm                    kpjba
068800060320     c                   parm                    trul31ds
068900060320     c                   if        o31pog > *zeros
069000060320     c                   movea     o31pog        skpoel
069100060320     c                   endif
069200060320     c                   endif
069300060320     c                   endif
069400060321
069500060321     c                   eval      *in12 = (simfel = *zeros)
069600060928
069700060928     c     ktam          klist
069800060928     c                   kfld                    tamksc
069900060928     c                   kfld                    tamctr
070000060928     c                   kfld                    Wprg
070100060320
070200080704     c     kacvt         klist
070300080704     c                   kfld                    acokcc
070400080704     c                   kfld                    tamksc
070500080704
070600060320     c     ktabel        klist
070700060320     c                   kfld                    codut
070800060320     c                   kfld                    kcod
070900060320     c                   kfld                    kkey
071000060320
071100060320     c                   endsr
071200060320      *--------------------------------------------------------------*
071300060320**   cmd - comando
071400080709CPYF FROMFILE(         /WFTAN00F) TOFILE(QTEMP/WFTAN00F) FROMMBR(WFTAN00F) CRTFI
071500080709CRTDUPOBJ OBJ(WFTAN00F) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
071600080709CLRPFM FILE(QTEMP/WFTAN00F)
