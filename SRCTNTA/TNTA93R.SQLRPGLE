000100060320     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200060320      *------------------------------------------------------------------------*
000300060320      *
000400060928      *  Pulizia Tariffe obsolete
000500060320      *
000600060320      *------------------------------------------------------------------------*
000700060215
000800060215     ftntam04l  if   e           k disk
000900130905     fAZCMM01L  if   e           k disk
001000060320     fazorg01l  if   e           k disk
001100060320     ftabel00f  if   e           k disk
001200080704     fcnacvt1l  if   e           k disk
001300080703     fwftan00f  o    e             disk    usropn
001400080708     ftnta93p   o    e             printer oflind(*in05)
001500060323      *------------------------------------------------------------------------*
001600060323      *  RIEPILOGO INDICATORI
001700060323      *------------------------------------------------------------------------*
001800060323      * 12 - utente di sede
001900080926      * 20 - proteggo in stampa il campo del commerciale
002000060323      * 30 - comodo
002100060215
002200060215      *------------------------------------------------------------------------*
002300060215      *   V A R I A B I L I
002400060215      *------------------------------------------------------------------------*
002500080710     d comman          s            110
002600060320     d codut           s              1  0 inz(1)
002700060320     d kcar            s                   like(orgcar)
002800060320     d kcod            s                   like(tblcod)
002900060320     d kksc            s                   like(tamksc)
003000060320     d kkey            s                   like(tblkey)
003100080710     d lenght          s             15  5 inz(110)
003200080708     d savcli          s                   like(tanksc)
003300080708     d savcmm          s                   like(tancmm)
003400080708     d savfil          s                   like(tanfil)
003500060215     d wctr            s                   like(tamctr)
003600060928     d wprg            s                   like(tamprg)
003700060320     d wdata           s              8  0
003800080708     d wora            s              6  0
003900060215     d wksc            s                   like(tamksc)
004000060320     d wlib            s              9
004100060320     d w003a           s              3
004200060320     d w0030           s              3  0
004300060320     d w0140           s             14  0
004400060320     d xx              s              3  0
004500060320     d yy              s              3  0
004600080704     d numsped         s             11  0
004700080704     d numtarcli       s             11  0
004800080704     d numtarann       s             11  0
004900080704     d dtaacquisi      s              8  0
005000060320
005100060320      *------------------------------------------------------------------------*
005200060320      *   S C H I E R E
005300060320      *------------------------------------------------------------------------*
005400080709     d cmd             s             80    dim(3) ctdata perrcd(1)
005500060320     d skpoel          s              3    dim(250)
005600060215
005700060215      *------------------------------------------------------------------------*
005800060215      *   D S   I N T E R N E / E S T E R N E
005900060215      *------------------------------------------------------------------------*
006000060320
006100060320     d parm01          ds
006200080709     d  parfil                 1      3  0
006300080709     d  parcar                 4    153    dim(50)
006400080709     d  pardus               154    161  0
006500080709     d  pardna               162    169  0
006600080709     d  parfile              170    170
006700080709     d  parabi               171    172
006800080710     d  parcrf               173    173
006900060321
007000060321     d wlbdat          ds                  inz
007100060321     d  g02dat                 1      8  0
007200060321     d  g02inv                 9     16  0
007300060321     d  g02err                17     17
007400060321     d  g02tgi                18     22  0
007500060320
007600060320     d azuteds       e ds                  extname(azute00f)
007700060320     d ddatiute      e ds
007800060320     d ds_cnaco      e ds                  inz  extname(Cnaco00f)
007900060320     d ds_cnind      e ds                  inz  extname(Cnind00f)
008000060320     d ds_cnclp      e ds                  inz  extname(Cnclp00f)
008100060320     d ds_fncls      e ds                  inz  extname(Fncls00f)
008200130905     d azorgds       e ds                  extname(azorg00f)
008300080708     d wftands       e ds                  extname(wftan00f)
008400060215     d kpjba         e ds
008500060320     d tibs34ds      e ds
008600060320     d tibs69ds      e ds
008700060320     d trul31ds      e ds                  inz
008800080711     d dsta01        e ds
008900060215
009000060215      *------------------------------------------------------------------------*
009100080707     C*
009200080707     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni
009300080707     C
009400080707     C/EXEC SQL
009500080707     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
009600080707     C/END-EXEC
009700080707     C*
009800060215
009900060320      * ciclo per i p.o. caricati in schiera
010000060320    1c                   do        250           xx
010100060320     c                   if        skpoel(xx) = *blanks
010200060320     c                   leave
010300060320     c                   endif
010400060320
010500060320     c                   clear                   wksc
010600060215     c                   eval      wctr = 999
010700060320     c                   move      *all'0'       kksc
010800060320     c                   movel     skpoel(xx)    kksc
010900060215
011000060320     c     kksc          setll     tntam04l
011100060320    2c                   do        *hival
011200060215     c                   read      tntam04l
011300060215      * fine file
011400060215     c                   if        %eof(tntam04l)
011500080926      * verifico se wksc > 0 se iltotale delle tariffe annullabili è uguale
011600080926      * al totale delle tariffe del cliente in questo caso devo aggiornare
011700080926      * un flag nel file wftan che il cliente è annullabile
011800080926     c                   if        wksc > 0 and numtarcli = numtarann
011900080926
012000080926     c/exec Sql
012100080926     c+ update wftan00f set tanann = '*'
012200080926     c+ where tanksc = :wksc
012300080926     c/End-Exec
012400080926
012500080926     c                   endif
012600060215     c                   leave
012700060215     c                   endif
012800060320
012900080704      * cambio cliente
013000080704    3c                   if        tamksc <> wksc
013100080707      * verifico se wksc > 0 se iltotale delle tariffe annullabili è uguale
013200080707      * al totale delle tariffe del cliente in questo caso devo aggiornare
013300080707      * un flag nel file wftan che il cliente è annullabile
013400080707     c                   if        wksc > 0 and numtarcli = numtarann
013500080707
013600080707     c/exec Sql
013700080707     c+ update wftan00f set tanann = '*'
013800080707     c+ where tanksc = :wksc
013900080707     c/End-Exec
014000080707
014100080707     c                   endif
014200080707
014300080704     c                   clear                   numtarcli
014400080704     c                   clear                   numtarann
014500080704      * se p.o. maggiore esco
014600080704     c                   movel     tamksc        w003a
014700080704     c                   if        w003a > skpoel(xx)
014800080704     c                   leave
014900080704     c                   endif
015000080704      * cerco i dati anagrafici
015100080704     c                   clear                   tibs69ds
015200080704     c                   move      tamksc        i69kac
015300080704     c                   move      tamksc        i69kin
015400080704     c                   move      tamksc        i69kcp
015500080704     c                   call      'TIBS69R'
015600080704     c                   parm                    tibs69ds
015700080704     c                   parm                    ds_cnaco
015800080704     c                   parm                    ds_cnind
015900080704     c                   parm                    ds_cnclp
016000080704     c                   parm                    ds_fncls
016100080704      * controllo data acquisizione
016200080704      * verifico se ha un record nel file variazioni di inserimento
016300080704
016400080704      * valorizzo il campo di comodo DTAACQUISI con un valore minimo in quanto
016500080704      * il file delle variazioni è noto poco tempo fa e prima non c'era nulla
016600080704      * visto che ci serve la data di acquisizione del cliente e ci serve
016700080704      * all'incirca al massimo 6 mesi indietro e il file delle variazioni
016800080704      * esisteva già ... tutto quello che è nato prima è comunque da prendere
016900080704      * in considerazione
017000080704
017100080704     c                   eval      dtaacquisi = 00010101
017200080704     c     Kacvt         setll     cnacvt1l
017300080704    4c                   do        *hival
017400080704     c     kacvt         reade     cnacvt1l
017500080704      * fine file
017600080704     c                   if        %eof(cnacvt1l)
017700080704     c                   leave
017800080704     c                   endif
017900080704
018000080704      * verifico se c'è stato un inserimento
018100080704     c                   If        acvcta <> 'M'
018200080704      * valorizzo la dta acquisizione
018300080704     c                   eval      dtaacquisi = acvdav
018400080704     c                   leave
018500080704     c                   endif
018600080704
018700080704    4c                   enddo
018800080704
018900080704      * controllo la data acquisizione se maggiore del limite impostato
019000080704     c                   if        dtaacquisi >= pardna
019100080704     c                   iter
019200080704     c                   endif
019300080704
019400080704    3c                   endif
019500080704
019600060320      * cambio tariffa
019700080704    3c                   if        tamctr <> wctr or tamksc <> wksc
019800060320
019900060320      * escludo le annullate
020000060215     c                   if        tamatb <> *blanks
020100060215     c                   iter
020200060215     c                   endif
020300080704      * memorizzo il numero di tariffe del cliente
020400080704     c                   add       1             numtarcli
020500080704      * se la tariffa che sto leggendo deve ancora andare in vigore la escludo
020600080704     c                   if        tamddt > *date
020700080704     c                   eval      wctr = tamctr
020800080704     c                   eval      wksc = tamksc
020900080704     c                   iter
021000080704     c                   endif
021100080703      * considero le tariffe dei clienti che non hanno avuto spedizioni
021200080703      * dopo la data inserita a video controllando sul file titas
021300080704
021400080704     c                   clear                   numsped
021500080704
021600080708      * se sono su AS888 punto su titas in gaitragrps altrimenti su
021700080708      * gaitragru
021800080708
021900080708     c                   if        %subst(knsif:7:1) = 'P'
022000080703     c/exec sql
022100080710     c+     select count(*) into :NumSped
022200080710     c+     from    GAITRAGRPS/TITAS00F
022300080707     c+     where   TASksc = :TAMksc and TASctr = :TAMctr
022400080703     c+       and ((TASaas*10000)+TASmgs)  >=  :pardus
022500080710     c/end-exec
022600080710      * se non trovata nessuna spedizione vado a verificare in TITAS10
022700080710     c                   if        numsped = 0
022800080710     c/exec sql
022900080710     c+     select count(*) into :NumSped
023000080708     c+     from    GAITRAGRPS/TITAS10F
023100080707     c+     where   TASksc = :TAMksc and TASctr = :TAMctr
023200080703     c+       and ((TASaas*10000)+TASmgs)  >=  :pardus
023300080710     c/end-exec
023400080710      * se non trovata ANCORA nessuna spedizione vado a verificare in TITASP
023500080710     c                   if        numsped = 0
023600080710     c/exec sql
023700080710     c+     select count(*) into :NumSped
023800080710     c+     from    GAITRAGRPS/TITASP0F
023900080707     c+     where   TASksc = :TAMksc and TASctr = :TAMctr
024000080703     c+       and ((TASaas*10000)+TASmgs)  >=  :pardus
024100080703     c/end-exec
024200080710     c                   endif
024300080710     c                   endif
024400080708
024500080708     c                   else
024600080708     c/exec sql
024700080710     c+     select count(*) into :NumSped
024800080710     c+     from    GAITRAGRU/TITAS00F
024900080708     c+     where   TASksc = :TAMksc and TASctr = :TAMctr
025000080708     c+       and ((TASaas*10000)+TASmgs)  >=  :pardus
025100080710     c/end-exec
025200080710      * se non trovata nessuna spedizione vado a verificare in TITAS10
025300080710     c                   if        numsped = 0
025400080710     c/exec sql
025500080710     c+     select count(*) into :NumSped
025600080708     c+     from    GAITRAGRU/TITAS10F
025700080708     c+     where   TASksc = :TAMksc and TASctr = :TAMctr
025800080708     c+       and ((TASaas*10000)+TASmgs)  >=  :pardus
025900080710     c/end-exec
026000080710      * se non trovata ANCORA nessuna spedizione vado a verificare in TITASP0
026100080710     c                   if        numsped = 0
026200080710     c/exec sql
026300080710     c+     select count(*) into :NumSped
026400081030     c+     from    GAITRAGRUS/TITASP0F
026500080708     c+     where   TASksc = :TAMksc and TASctr = :TAMctr
026600080708     c+       and ((TASaas*10000)+TASmgs)  >=  :pardus
026700080708     c/end-exec
026800080710     c                   endif
026900080710     c                   endif
027000080708
027100080708     c                   endif
027200080703      *
027300080704      * se numero spedizione maggiore di 0 passo alla tariffa successiva
027400080703      *
027500080704     c                   if        numsped > *zeros
027600080704     c                   eval      wksc = tamksc
027700080704     c                   eval      wctr = tamctr
027800080704     c                   iter
027900080704     c                   endif
028000080704      * se passati tutti i controlli rendo la tariffa per il cliente annullabile
028100080704      *
028200080704     c                   add       1             numtarann
028300060929      * scrivo il file
028400060320     c                   exsr      scrivi
028500060215
028600060320     c                   eval      wksc = tamksc
028700060320     c                   eval      wctr = tamctr
028800060215
028900060320    3c                   endif
029000060320
029100060320    2c                   enddo
029200060215
029300060320    1c                   enddo
029400060320
029500080707     c                   exsr      sr_Stampa
029600060215
029700080710      * se si desidera il file devo copiarlo
029800080710     c                   if        parcrf = 'S'
029900080710     c                   clear                   comman
030000080710     c                   EVAL      comman = 'CPYF FROMFILE(QTEMP/WFTAN00F) '
030100080711     c                             + 'TOFILE('+ %TRIMR(WLIB) + '/WFTAN00F) '
030200080710     c                             + 'FROMMBR(WFTAN00F) MBROPT(*REPLACE)'
030300080710
030400080710     c                   call      'QCMDEXC'
030500080710     c                   parm                    comman
030600080710     c                   parm                    lenght
030700080710     c                   endif
030800080710
030900060215     c                   eval      *inlr = *on
031000060215
031100060215      *------------------------------------------------------------------------*
031200060320      * SCRIVO IL FILE PER LA STAMPA
031300060215      *------------------------------------------------------------------------*
031400060215     c     scrivi        begsr
031500060215
031600060323
031700060929     c                   clear                   wftan000
031800060215
031900061002      * job/utente/data/ora elaborazione
032000061002     c                   eval      tannoj = knmeb
032100061002     c                   eval      tanpru = knmus
032200061002     c                   eval      tandta = wdata
032300061002     c                   eval      tanora = wora
032400060320      * commerciale
032500060928     c                   eval      tancmm = clpage
032600130905     c     TANcmm        chain     AZCMM000
032700130905     c                   if        not %found(AZCMM01L)
032800060928     c                   eval      tandcmm = *all'*'
032900060320     c                   else
033000130905     c                   eval      tandcmm = CMMdes
033100130905     c                   movel     CMMuni        tancmu
033200061002      * codice commerciale unificante decodifica
033300130905     c     TANcmu        chain     AZCMM000
033400130905     c                   if        not %found(AZCMM01L)
033500061002     c                   eval      tandcmm = *all'*'
033600061002     c                   else
033700130905     c                   eval      tandcmu = CMMdes
033800060320     c                   endif
033900061002     c                   endif
034000080707      * cliente
034100080707     c                   eval      tanksc = tamksc
034200080707     c                   eval      tanrag = acorag
034300080707      * distretto area po codice cliente
034400080707     c                   movel     tanksc        w0030
034500061002     c     w0030         chain     azorg01l
034600061002
034700061002      * distretto
034800061002     c                   eval      tandis = orgfl3
034900061002     c                   eval      kcod = '17'
035000061002     c                   eval      kkey = tandis
035100061002     c     ktabel        chain     tabel00f
035200061002     c                   if        not %found(tabel00f) or tblflg <> *blanks
035300061002     c                   eval      tanddis = *all'*'
035400061002     c                   else
035500061002     c                   eval      tanddis = tbluni
035600061002     c                   endif
035700061002      * area
035800061002     c                   eval      tancar = orgcar
035900061002     c                   eval      kcod = '05'
036000061002     c                   movel     tancar        kkey
036100061002     c     ktabel        chain     tabel00f
036200061002     c                   if        not %found(tabel00f) or tblflg <> *blanks
036300061002     c                   eval      tandcar = *all'*'
036400061002     c                   else
036500061002     c                   eval      tandcar = tbluni
036600061002     c                   endif
036700061002      * p.o.
036800061002     c                   eval      tanfil = orgfil
036900061002     c                   if        not %found(azorg01l) or orgfva <> *blanks
037000061002     c                   eval      tandfil = *all'*'
037100061002     c                   else
037200061002     c                   eval      tandfil = orgdes
037300061002     c                   endif
037400060215      * data ultima spedizione fattura
037500060928     c                   eval      tandus = clpdus
037600060215      * tariffa
037700060928     c                   eval      tanctr = tamctr
037800080711
037900080711     c                   movel     tamflo        dsta01
038000080711      * Tipo tariffa
038100080711     c                   select
038200080711     c                   when      §tadpd = 'S'
038300080711     c                   eval      tanfie = 'D'
038400080711     c                   when      §tafed = 'S'
038500080711     c                   eval      tanfie = 'F'
038600080711     c                   other
038700080711     c                   eval      tanfie = tamfie
038800080711     c                   endsl
038900080711      * descrizione
039000080711     c                   eval      tandsc = tamdcv
039100080711
039200060928     c                   eval      tanddt = tamddt
039300060928     c                   eval      tandst = tamdst
039400060323      * tariffa bloccata partenza/arrivo
039500060928     c                   eval      tanbap = tambap
039600060215      * scrivo file
039700060929     c                   write     wftan000
039800080707     c                   feod      wftan00f
039900060320
040000060215     c                   endsr
040100060320
040200080707      *------------------------------------------------------------------------*
040300080707      * Stampo in ordine di Area / P.O. / Codice commerciale / cliente
040400080707      *------------------------------------------------------------------------*
040500080707     c     sr_stampa     begsr
040600080707
040700080711      * Spedizione del
040800080711     c                   Clear                   wlbdat
040900080711     c                   Z-add     pardus        g02inv
041000080711     c                   Movel     '3'           g02err
041100080711     c                   Call      'XSRDA8'
041200080711     c                   Parm                    wlbdat
041300080711     c                   Z-add     g02dat        Stpdus
041400080711      * Data nascita da
041500080711     c                   Clear                   wlbdat
041600080711     c                   Z-add     pardna        g02inv
041700080711     c                   Movel     '3'           g02err
041800080711     c                   Call      'XSRDA8'
041900080711     c                   Parm                    wlbdat
042000080711     c                   Z-add     g02dat        Stpdna
042100080707     c/exec SQL
042200080707     c+ declare C1 cursor for
042300080708     c+ select * from wftan00f
042400080707     c+ order by tancar, tanfil, tancmm, tanksc, tanctr
042500080707     c+ for read only
042600080707     c/end-exec
042700080708
042800080708     c/exec sql
042900080708     c+ open c1
043000080708     c/end-exec
043100080707
043200080707     c/exec SQL
043300080708     c+ Fetch next from C1 into :wftands
043400080707     c/end-exec
043500080707
043600080707      * in fase di lettura
043700080707     c                   dow       sqlcod = *zeros
043800080707
043900080708      * a rottura di commerciale o di filiale cambio pagina
044000080708     c                   if        savcmm <> tancmm or
044100080708     c                             savfil <> tanfil
044200080926      *
044300080926     c                   exsr      sr_totali
044400080708      * valorizzo i campi in stampa
044500080708     c                   eval      agente = tancmm
044600080708     c                   eval      desage = tandcmm
044700080708     c                   eval      keypo  = tanfil
044800080708     c                   eval      despoc = tandfil
044900080707      * stampo la testata
045000080708     c                   write     ta93t
045100080708     c                   write     ta93t1
045200080708     c                   write     ta93tdcm
045300080708     c                   write     ta93t2
045400080708     c                   write     ta93tdcm
045500080710     c                   eval      *in05 = *off
045600080708      * Salvo i campi
045700080708     c                   eval      savcmm = tancmm
045800080708     c                   eval      savfil = tanfil
045900080708
046000080708     c                   endif
046100080708      * a cambio cliente stampo i dati del cliente
046200080708     c                   if        savcli <> tanksc
046300080708     c                   seton                                        40
046400080708     c                   eval      savcli = tanksc
046500080926      * preparo i totali clienti   / tariffe
046600080926     c                   add       1             totcli
046700080926     c                   if        tanann <> ' '
046800080926     c                   add       1             tottan
046900080926     c                   endif
047000080708     c                   endif
047100080708      * preparo i dati del dettaglio
047200080708     c                   eval      stpann = tanann
047300080708     c                   eval      stpksc = tanksc
047400080708     c                   eval      stprag = tanrag
047500080708      * Data decorrenza
047600080708     c                   Clear                   wlbdat
047700080708     c                   Z-add     Tanddt        g02inv
047800080708     c                   Movel     '3'           g02err
047900080708     c                   Call      'XSRDA8'
048000080708     c                   Parm                    wlbdat
048100080708     c                   Z-add     g02dat        Stpddt
048200080708      * Data scadenza
048300080708     c                   Clear                   wlbdat
048400080708     c                   Z-add     Tandst        g02inv
048500080708     c                   Movel     '3'           g02err
048600080708     c                   Call      'XSRDA8'
048700080708     c                   Parm                    wlbdat
048800080708     c                   Z-add     g02dat        Stpdst
048900080708      * scrivo la riga di dettaglio
049000080708
049100080708      * overflow
049200080708     c                   if        *in05
049300080708
049400080708     c                   write     ta93t
049500080708     c                   write     ta93t1
049600080708     c                   write     ta93tdcm
049700080708     c                   write     ta93t2
049800080708     c                   write     ta93tdcm
049900080708     c                   eval      *in05 = *off
050000080708     c                   eval      *in40 = *on
050100080708
050200080708     c                   endif
050300080708
050400080708     c                   write     ta93r
050500080708     c                   eval      *in40 = *off
050600080926     c                   add       1             tottar
050700080708
050800080707     c/exec sql
050900080708     c+ Fetch next from C1 into :wftands
051000080707     c/end-exec
051100080707     c                   enddo
051200080707     c
051300080926      *
051400080926     c                   eval      tanfil = 999
051500080926     c                   exsr      sr_totali
051600080707     c/exec sql
051700080707     c+ close C1
051800080707     c/end-exec
051900080708
052000080708     c                   endsr
052100080926      *------------------------------------------------------------------------*
052200080926      * Stampo i totali per commerciale e filiale
052300080926      *------------------------------------------------------------------------*
052400080926     c     sr_totali     begsr
052500080926
052600080926      * prima faccio i totali del commerciale precedente
052700080926     c                   if        savcmm <> *zeros
052800080926      * overflow
052900080926     c                   if        *in05
053000080926
053100080926     c                   write     ta93t
053200080926     c                   write     ta93t1
053300080926     c                   write     ta93tdcm
053400080926     c                   write     ta93t2
053500080926     c                   write     ta93tdcm
053600080926     c                   eval      *in05 = *off
053700080926     c                   endif
053800080926     c                   write     ta93tt
053900080926     c                   add       tottar        tottar_f
054000080926     c                   add       totcli        totcli_f
054100080926     c                   add       tottan        tottan_f
054200080926     c                   clear                   tottar
054300080926     c                   clear                   totcli
054400080926     c                   clear                   tottan
054500080926     c                   endif
054600080926
054700080926      * rottura di filiale
054800080926      * faccio i totali della filiale precedente
054900080926     c                   if        savfil <> tanfil and
055000080926     c                             savfil <> *zeros
055100080926
055200080926     c                   write     ta93t
055300080926     c                   eval      *in20 = *on
055400080926     c                   write     ta93t1
055500080926     c                   eval      *in20 = *off
055600080926     c                   write     ta93tdcm
055700080926     c                   eval      *in05 = *off
055800080926
055900080926     c                   write     ta93ttf
056000080926     c                   clear                   tottar_f
056100080926     c                   clear                   totcli_f
056200080926     c                   clear                   tottan_f
056300080926     c                   endif
056400080926     c                   endsr
056500060320      *--------------------------------------------------------------*
056600060320      * ROUTINE INIZIALE
056700060320      *--------------------------------------------------------------*
056800060320     c     *inzsr        begsr
056900060320
057000060320     c     *entry        plist
057100060320     c                   parm                    kpjba
057200060320     c                   movel     kpjbu         parm01
057300060320
057400060320     c     *dtaara       define    §azute        azuteds
057500060320     c     *dtaara       define    §datiute      ddatiute
057600060320
057700060320     c                   in(e)     *dtaara
057800060320     c                   if        %error or rsut = *blanks
057900060320     c                   clear                   tibs34ds
058000060320     c                   call      'TIBS34R'
058100060320     c                   parm                    tibs34ds
058200060320     c                   in        *dtaara
058300060320     c                   endif
058400060320
058500060320
058600060320      * se s.i. di prova imposto la libreria di prova
058700060320     c                   if        %subst(knsif:7:1) = 'P'
058800060320     c                   eval      wlib = 'GAITRAAZP '
058900060320     c                   else
059000060320     c                   eval      wlib = 'GAITRAAZM '
059100060320     c                   endif
059200080710      * creo il file in qtemp copiandolo dalla gaitraazm o gaitraazp
059300080710     c                   clear                   comman
059400080710     c                   EVAL      comman = 'CPYF FROMFILE('
059500080710     c                             + %TRIMR(WLIB)
059600080710     c                             + '/WFTAN00F) TOFILE(QTEMP/WFTAN00F) '
059700080710     c                             + 'FROMMBR(WFTAN00F) CRTFILE(*YES)'
059800080710
059900060320     c                   call      'QCMDEXC'
060000060320     c                   parm                    comman
060100060320     c                   parm                    lenght
060200080710      * pulisco il file
060300080710     c                   clear                   comman
060400080710     c                   EVAL      comman = 'CLRPFM FILE(QTEMP/WFTAN00F)'
060500080710
060600080710     c                   call      'QCMDEXC'
060700080710     c                   parm                    comman
060800080710     c                   parm                    lenght
060900080710
061000060929     c                   open      wftan00f
061100060320
061200060320      * reperisco data e ora del lancio
061300060320     c                   time                    w0140
061400060320     c                   move      w0140         wdata
061500061002     c                   movel     w0140         wora
061600060320
061700060320      * pulisco la sk dei p.o. da elaborare
061800060320     c                   clear                   skpoel
061900060320
062000060320      * se ho le aree carico una sk con tutti i p.o. relativi alle aree
062100060320      * richieste
062200060321    1c                   if        parcar(01) <> *blanks
062300080709    2c                   do        50            xx
062400060321    3c                   if        parcar(xx) <> *blanks
062500080709
062600080709     c                   move      parcar(xx)    kcar
062700080709     c/exec SQL
062800080709     c+ declare C2 cursor for
062900080709     c+ select * from azorg02l
063000080709     c+ where  orgcar = :Kcar
063100080709     c+ for read only
063200080709     c/end-exec
063300080709
063400080709     c/exec sql
063500080709     c+ open c2
063600080709     c/end-exec
063700080709
063800080709     c/exec SQL
063900080709     c+ Fetch next from C2 into :azorgds
064000080709     c/end-exec
064100080709
064200080709      * in fase di lettura
064300080709    4c                   dow       sqlcod = *zeros
064400080709    5c                   if        orgfva <> *blanks or
064500080709     c                             (orgfag <> 'A' and orgfag <> 'F')
064600080709     c                   iter
064700080709    5c                   endif
064800080709
064900080709     c                   move      orgfil        w003a
065000080709     c     w003a         lookup    skpoel                                 30
065100080709    5c                   if        not *in30
065200080709     c                   eval      yy = 1
065300080709     c     *blanks       lookup    skpoel(yy)                             30
065400080709    6c                   if        *in30
065500080709     c                   eval      skpoel(yy) = w003a
065600080709    6c                   endif
065700080709    5c                   endif
065800080709     c/exec sql
065900080709     c+ Fetch next from C2 into :azorgds
066000080709     c/end-exec
066100080709    4c                   enddo
066200080709     c
066300080709     c/exec sql
066400080709     c+ close C2
066500080709     c/end-exec
066600080709
066700060320    3c                   endif
066800060320    2c                   enddo
066900060320
067000060320   x1c                   else
067100060320      * carico il p.o. richiesto a video
067200060321    2c                   if        parfil <>  *zeros
067300060321     c                   move      parfil        skpoel(1)
067400060320   x2c                   else
067500060320     c                   clear                   trul31ds
067600060321     c                   eval      i31abi = parabi
067700060320     c                   eval      i31cdi = dutdis
067800060320     c                   eval      i31car = dutare
067900060320     c                   eval      i31cpo = dutpou
068000060320     c                   call      'TRUL31R'
068100060320     c                   parm                    kpjba
068200060320     c                   parm                    trul31ds
068300060320     c                   if        o31pog > *zeros
068400060320     c                   movea     o31pog        skpoel
068500060320     c                   endif
068600060320     c                   endif
068700060320     c                   endif
068800060321
068900060321     c                   eval      *in12 = (simfel = *zeros)
069000060320
069100080704     c     kacvt         klist
069200080704     c                   kfld                    acokcc
069300080704     c                   kfld                    tamksc
069400080704
069500060320     c     ktabel        klist
069600060320     c                   kfld                    codut
069700060320     c                   kfld                    kcod
069800060320     c                   kfld                    kkey
069900060320
070000060320     c                   endsr
070100060320      *--------------------------------------------------------------*
070200060320**   cmd - comando
070300080709CPYF FROMFILE(         /WFTAN00F) TOFILE(QTEMP/WFTAN00F) FROMMBR(WFTAN00F) CRTFI
070400080709CRTDUPOBJ OBJ(WFTAN00F) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
070500080709CLRPFM FILE(QTEMP/WFTAN00F)
