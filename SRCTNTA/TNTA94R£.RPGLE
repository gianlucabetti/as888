000100060320     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200060320      *------------------------------------------------------------------------*
000300060320      *
000400061017      *  Annullamento Tariffe Clienti Bloccati
000500060320      *
000600060320      *------------------------------------------------------------------------*
000700060215
000800061017     fcnaco00f  if   e           k disk
000900061017     ftitas02l  if   e           k disk
001000061017     fcnacvt1l  if   e           k disk
001100061017     fcnacvd1l  if   e           k disk
001200060320     fazorg01l  if   e           k disk
001300061019     ftabel00f  if   e           k disk
001400061023     ftntam00l  uf   e           k disk
001500061027     ftitad04l  uf   e           k disk
001600061023     ftitpt01l  uf   e           k disk
001700061023     ftitpd01l  uf   e           k disk
001800061023     ftitgc01l  uf   e           k disk
001900061023     ftitav01l  uf a e           k disk
002000061023     ftfntc01l  uf   e           k disk
002100061023     ftnta94p   o    e             Printer Oflind(*IN63) usropn
002200061025     fsysprt    o    f  132        Printer
002300060323
002400060323      *------------------------------------------------------------------------*
002500060323      *  RIEPILOGO INDICATORI
002600060323      *------------------------------------------------------------------------*
002700060323      * 30 - comodo
002800061026      * 41 - stampa anagrafica
002900061026      * 42 - SIMULAZIONE
003000061024      * 63 - Over flow
003100060215
003200060215      *------------------------------------------------------------------------*
003300060215      *   V A R I A B I L I
003400060215      *------------------------------------------------------------------------*
003500061026     d simula          s              1
003600061018     d w003a           s              3
003700060320     d w0140           s             14  0
003800061024     d xx              s              3  0
003900061018     d yy              s              2  0
004000061017     d codut           s              1  0
004100061023     d dateu           s              8  0
004200061023     d wdata           s              8  0
004300061023     d wora            s              6  0
004400061024     d savpo           s              3
004500061025     d gentar          s             11  0
004600061025     d gencli          s             11  0
004700061018     d Cli_ann         s              1    Inz(*off)
004800061023     d Data_no         s              1    Inz(*off)
004900061023     d Wta47           s              1    Inz(*off)
005000061019
005100061024     d comman          s             80
005200061024     d lun             s             15  5 inz(80)
005300061024
005400061019     d kcod            s                   like(tblcod) inz('5A')
005500061019     d kkey            s                   like(tblkey) inz('SEDE1')
005600061019
005700061024     d keypo           s                   like(orgfil)
005800061024
005900061018     d Kksc            s                   like(acoksc)
006000061019     d K_yda           s                   like(acvyda) inz('B')
006100061019     d K_prg           s                   like(acvprg) inz(0)
006200061019
006300061023     d Appl            s                   like(ntcapl) inz('C')
006400061023     d Notkey          s                   like(ntcnk1)
006500061023     d Notke2          s                   like(ntcnk2)
006600100125     d TipoNota        s                   like(ntctnt) inz('10')
006700061023
006800061019     d Wdataeur        s               d   datfmt(*eur)
006900061019     d Wdataoggi       s               d   datfmt(*eur)
007000100125
007100100125     d tariffe         s              1n
007200060320
007300060320      *------------------------------------------------------------------------*
007400060320      *   S C H I E R E
007500060320      *------------------------------------------------------------------------*
007600060320     d skpoel          s              3    dim(250)
007700061018     d datvar          s              1    dim(9)
007800061024      * Ovrprtf
007900061024     d cmdo            s             90    dim(1) ctdata perrcd(1)
008000060215
008100060215      *------------------------------------------------------------------------*
008200060215      *   D S   I N T E R N E / E S T E R N E
008300060215      *------------------------------------------------------------------------*
008400060321
008500060321     d wlbdat          ds                  inz
008600060321     d  g02dat                 1      8  0
008700060321     d  g02inv                 9     16  0
008800060321     d  g02err                17     17
008900060321     d  g02tgi                18     22  0
009000060320
009100060320     d azuteds       e ds                  extname(azute00f)
009200060320     d ddatiute      e ds
009300061024     d tibs34ds      e ds                  inz
009400060215     d kpjba         e ds
009500061024     d og141         e ds
009600061024     d og144         e ds
009700060320     d trul31ds      e ds                  inz
009800061023     d tnta47ds      e ds
009900061024     d dsta01        e ds
010000061019
010100061019     d dacv_B        e ds
010200061019     d ds5as1        e ds
010300060215
010400061023      * Reperimento dati del PGM
010500061023     d pgmstatus      sds
010600061023     d job_name              244    253
010700061023
010800060215      *------------------------------------------------------------------------*
010900060215
011000060320      * ciclo per i p.o. caricati in schiera
011100060320    1c                   do        250           xx
011200060320     c                   if        skpoel(xx) = *blanks
011300060320     c                   leave
011400060320     c                   endif
011500060320
011600061024     c                   movel(p)  skpoel(xx)    kksc
011700060215
011800061018     c     kaco          setll     Cnaco00f
011900060320    2c                   do        *hival
012000061018     c                   read      cnaco00f
012100060215      * fine file
012200061018     c                   if        %eof(cnaco00f)
012300060215     c                   leave
012400060215     c                   endif
012500061018      * se p.o. maggiore esco
012600061018     c                   movel     acoksc        w003a
012700061018     c                   if        w003a > skpoel(xx)
012800061018     c                   leave
012900061018     c                   endif
013000060320
013100061018      * verifico se cliente bloccato
013200130315    3c                   if        Acoabl <> *blanks
013300061024     c                   seton                                        41
013400061018      * verifico la data di immissione del blocco
013500061018     c                   exsr      SR_ctrdta
013600061020      * se cliente annullabile oppure data non trovata  eseguo annullamento
013700061020      * e stampa
013800061023     c                   if        Cli_ann = *on or Data_no = *off
013900061023      * verifico se ha delle spedizioni ancora da fatturare
014000061023     c     Acoksc        setll     titas02l
014100061023
014200061023     c                   if        not %equal(titas02l)
014300061020      * eseguo annullamento
014400061020     c                   exsr      Annulla
014500061023     c                   endif
014600061023
014700061020     c                   endif
014800060215
014900060320    3c                   endif
015000060320
015100060320    2c                   enddo
015200060215
015300060320    1c                   enddo
015400060320
015500061023      *
015600061024      * se file stampa aperto stampo l'ultima riga e chiudo
015700061024      *
015800061024     c                   If        %open(tnta94p)
015900061024     c                   write     Ta94e
016000061024     c                   Close     tnta94p
016100061025     c                   add       tottar        gentar
016200061025     c                   add       totcli        gencli
016300061025     c                   except    totali
016400061024     c                   endif
016500061024
016600061023     c                   If        Wta47 = '1'
016700061023     c                   clear                   Tnta47ds
016800061023     c                   eval      D47tla = 'C'
016900061023     c                   movel     Tnta47ds      Kpjbu
017000061026     c                   If        simula = ' '
017100061026     c                   call      'TNTA47R'
017200061026     c                   parm                    Kpjba
017300061026     c                   endif
017400061023     c                   endif
017500060215
017600060215     c                   eval      *inlr = *on
017700060215
017800060215      *------------------------------------------------------------------------*
017900061018      * CONTROLLO DATA IMMISSIONE DEL BLOCCO
018000060215      *------------------------------------------------------------------------*
018100061018     c     SR_Ctrdta     begsr
018200060215
018300061020     c                   eval      Cli_ann = *off
018400061023     c                   eval      Data_no  = *off
018500061018      * verifico se inserita la data della variazione nella testata variazioni
018600061018     c     kacvt         setll     cnacvt1l
018700061018      * se non trovato il record di variazione per me è già annullabile
018800061020i   1c                   If        not %equal(cnacvt1l)
018900061018     c                   eval      Cli_ann = *on
019000061018
019100061020   x1c                   else
019200061018      * se trovato leggo
019300061020i   2c                   do        *hival
019400061018     c     kacvt         reade     cnacvt1l
019500061020      * fne lettura
019600061025i   3c                   if        %eof(cnacvt1l)
019700061020     c                   leave
019800061020e   3c                   endif
019900061018      * annullato rileggo
020000061020i   3c                   if        acvatb <> ' '
020100061018     c                   iter
020200061020e   3c                   endif
020300061018      * se record che segnala l'inserimento do per ok annullamento
020400061018      * per inserimento si intende tutto quello che è diverso da "M"
020500061020i   3c                   if        acvcta <> 'M'
020600061018     c                   eval      Cli_ann = *on
020700061018     c                   leave
020800061020e   3c                   endif
020900061018      * verifico che tipo dati sono stati variati
021000061018     c                   movea     acvflo        DatVar
021100061018      * controllo se esiste blocco cliente
021200061018     c     'B'           lookup    datvar                                 30
021300061018      * se trovato la "B" come dato variato leggo il dettaglio
021400061020i   3c                   If        %found
021500061019
021600061019     c     Kacvd         chain     Cnacvd1l
021700061019      * se trovato non annullato
021800061020i   4c                   If        %found(Cnacvd1l) and acvatb = ' '
021900061019      * verifico come era il codice del blocco prima della variazione "B"
022000061019      *
022100061019     c                   movel     acvprima      dacv_B
022200061019      * se il blocco prima non era valorizzato vuol dire che è avvenuto
022300061019      * in quel momento e quindi prendo come data immissione blocco
022400061019      * quella della variazione
022500061020i   5c                   If        acoabl = ' '
022600061019     c     *iso          movel     acvdav        Wdataeur
022700061024     c                   adddur    §5as1atb:*d   Wdataeur
022800151117      * se sono trascorsi già xx giorni dal blocco del cliente
022900061019      * annulliamo tariffe
023000061020i   6c                   if        Wdataeur < Wdataoggi
023100061019     c                   eval      Cli_ann = *on
023200061019     c                   leave
023300061020     c                   else
023400061023     c                   eval      data_no = *on
023500061020     c                   leave
023600061020e   6c                   endif
023700061019
023800061020      * se invece prima il cliente risulta bloccato leggo un'altra variazione
023900061020     c                   else
024000061020     c                   iter
024100061020e   5c                   endif
024200061020
024300061020      * se non trova il record del dettaglio  leggo un'altra variazione
024400061020     c                   else
024500061020     c                   iter
024600061020e   4c                   endif
024700061020
024800061020      * se non trovato variazione blocco cliente leggo un'altra variazione
024900061020     c                   else
025000061020     c                   iter
025100061020e   3c                   endif
025200061020
025300061020e   2c                   enddo
025400061019
025500061020e   1c                   endif
025600060320
025700060215     c                   endsr
025800060320
025900061020      *------------------------------------------------------------------------*
026000061020      * ANNULLAMENTO TARIFFE CLIENTI BLOCCATI E STAMPA
026100061020      *------------------------------------------------------------------------*
026200061020     c     Annulla       begsr
026300061023
026400061023      * Tntam00l
026500061024     c     acoksc        setll     Tntam00l
026600061020
026700061023d   1c                   do        *hival
026800061023     c     acoksc        reade     Tntam00l
026900061023
027000061023     c                   If        %eof(Tntam00l)
027100061023     c                   leave
027200061023     c                   endif
027300061023      * Annullo Titpt
027400061023     c     ktariffa      setll     Titpt01l
027500061023d   2c                   do        *hival
027600061023     c     ktariffa      reade     Titpt01l
027700061023
027800061023     c                   if        %eof(titpt01l)
027900061023     c                   leave
028000061023     c                   endif
028100061023      * Annullo Titpd Dettaglio tariffa particolare
028200061023     c     ktpd          setll     Titpd01l
028300061023d   3c                   do        *hival
028400061023     c     ktpd          reade     Titpd01l
028500061023
028600061023     c                   if        %eof(titpd01l)
028700061023     c                   leave
028800061023     c                   endif
028900061023      * annullo
029000061023i   4c                   If        Tpdatb = ' '
029100061023     c                   eval      tpdatb = 'A'
029200061023     c                   clear                   tpdftr
029300061023     c                   z-add     dateu         tpddtr
029400061026     c                   if        simula = ' '
029500061026     c                   update    titpd000
029600061026e   4c                   endif
029700061023e   4c                   endif
029800061023
029900061023e   3c                   enddo
030000061023      * annullo Titpt testata tariffa particolare
030100061023     c                   If        Tptatb = ' '
030200061023     c                   eval      tptatb = 'A'
030300061023     c                   clear                   tptftr
030400061023     c                   z-add     dateu         tptdtr
030500061026     c                   if        simula = ' '
030600061026     c                   update    titpt000
030700061026     c                   endif
030800061023     c                   endif
030900061023
031000061023e   2c                   enddo
031100061023      * Annullo Titad
031200061027     c     ktariffa      setll     Titad04l
031300061023d   2c                   do        *hival
031400061027     c     ktariffa      reade     Titad04l
031500061023
031600061027     c                   if        %eof(titad04l)
031700061023     c                   leave
031800061023     c                   endif
031900061023      * annullo
032000061023     c                   If        Tadatb = ' '
032100061023     c                   eval      tadatb = 'A'
032200061023     c                   clear                   tadftr
032300061023     c                   z-add     dateu         taddtr
032400061026     c                   if        simula = ' '
032500061026     c                   update    titad000
032600061026     c                   endif
032700061023     c                   endif
032800061023
032900061023e   2c                   enddo
033000061023      * annullo Titgc
033100061023     c     Ktariffa      Chain     Titgc01l
033200061023      * annullo
033300061023     c                   If        %found(Titgc01l) and Tgcatb = ' '
033400061023     c                   eval      tgcatb = 'A'
033500061023     c                   clear                   tgcftr
033600061023     c                   z-add     dateu         tgcdtr
033700061026     c                   if        simula = ' '
033800061026     c                   update    titgc000
033900061026     c                   endif
034000061023     c                   endif
034100061023
034200061023      * annullo Tfntc note legate alla tariffa
034300061023     c                   movel     acokcc        notkey
034400061023     c                   move      acoksc        notkey
034500061023     c                   movel     tamctr        notke2
034600061023
034700061023     c     Kntc          setll     tfntc01l
034800061023      *
034900061023d   2c                   do        *hival
035000061023     c     Kntc          reade     tfntc01l
035100061023     c                   If        %eof(tfntc01l)
035200061023     c                   leave
035300061023     c                   endif
035400061023
035500061026     c                   if        simula = ' '
035600061026     c                   delete    tfntc
035700061026     c                   endif
035800061023e   2c                   enddo
035900061023      * annullo tntam
036000061023     c                   eval      tamatb = 'A'
036100061023     c                   clear                   tamftr
036200061023     c                   z-add     dateu         tamdtr
036300061026     c                   if        simula = ' '
036400061026     c                   update    tntam000
036500061026     c                   endif
036600061023
036700061023      * Annullo Titav Variazioni tariffe e scrivo record
036800061023     c     Ktariffa      setll     Titav01l
036900061023
037000061023d   2c                   do        *hival
037100061023     c     Ktariffa      reade     Titav01l
037200061023
037300061023     c                   if        %eof(titav01l)
037400061023     c                   leave
037500061023     c                   endif
037600061023
037700061023     c                   eval      Tavatb = 'A'
037800061023     c                   clear                   Tavftr
037900061023     c                   z-add     dateu         Tavdtr
038000061023
038100061026     c                   if        simula = ' '
038200061026     c                   update    titav000
038300061026     c                   endif
038400061023
038500061023e   2c                   enddo
038600061023      * scrivo un nuovo record di TITAV (annullato) per l'annullamento
038700061023      * della tariffa
038800061023     c                   clear                   Titav000
038900061023     C                   eval      Tavksc = Tamksc
039000061023     C                   eval      Tavctr = Tamctr
039100061023     C                   eval      Tavprg = Tamprg
039200061023      * data variazione
039300061023     C                   eval      tavdav = dateu
039400061023     C                   eval      tavnoj = job_name
039500061023     C                   eval      tavpru = 'BATCH     '
039600061023     c                   eval      tavAtb = 'A'
039700061023     c                   eval      tavtrd = 'TES'
039800061023     c                   eval      tavcta = 'A'
039900061023      * recupero ora di variazione
040000061023     c                   eval      TAVorv = Wora
040100061023     c                   z-add     dateu         tavdtr
040200061023      *
040300061026     c                   if        simula = ' '
040400061026     c                   write     titav000
040500061026     c                   endif
040600061023
040700061023      * Annullo file dati TIPO: TNETC/TNETT/TNETS  ****
040800061027     c                   if        simula = ' '
040900061023     c                   clear                   Tnta47ds
041000061023     c                   eval      Wta47 = *on
041100061023     c                   eval      D47tup = '1'
041200061023     c                   eval      D47cto = 'T'
041300061023     c                   eval      D47dsf = 'S'
041400061023     c                   eval      D47ksc = Tamksc
041500061023     c                   eval      D47ctr = Tamctr
041600061023     c                   eval      D47prg = Tamprg
041700061024     c                   movel     tnta47ds      kpjbu
041800061023     c                   call      'TNTA47R'
041900061023     c                   parm                    kpjba
042000061027     c                   endif
042100061024      * STAMPA
042200120207     c**!!!              exsr      Sr_Stampa
042300061023
042400061023e   1c                   enddo
042500100125
042600100125      * dopo che ho letto tutte le tariffe del cliente
042700100125      * controllo se ho note di tipo 10 aperte solo sul cliente
042800100125     c                   movel     acokcc        notkey
042900100125     c                   move      acoksc        notkey
043000100125     c                   clear                   notke2
043100100125      * azzero flag di appoggio per leggere solo 1 volta le tariffe
043200100125     c                   eval      tariffe = *off
043300100125     c     Kntc10        setll     tfntc01l
043400100125    1c                   do        *hival
043500100125     c     Kntc10        reade     tfntc01l
043600100125     c                   If        %eof(tfntc01l)
043700100125     c                   leave
043800100125     c                   endif
043900100125      * trovo nota di tipo 10 verifico se il cliente ha ancora tariffe
044000100125      * non annullate
044100100125    2c                   if        not tariffe
044200100125     c                   eval      tariffe = *on
044300100125     c     acoksc        setll     tntam00l
044400100125      * anche una sola tariffa non annullata esco dal giro
044500100125      * perchè devo tenere la nota di tipo 10
044600100125    3c                   if        %equal(tntam00l)
044700100125     c                   leavesr
044800100125     c                   endif
044900100125    2c                   endif
045000100125      * se arrivo qua vuol dire che non ho trovato tariffe valide
045100100125      * quindi posso cancellare la nota di tipo 10
045200100125     c                   if        simula = ' '
045300100125     c                   delete    tfntc
045400100125     c                   endif
045500100125    1c                   enddo
045600061023
045700061023     c                   endsr
045800061024
045900061024      *--------------------------------------------------------------*
046000061024      * Stampa tariffe annullate
046100061024      *--------------------------------------------------------------*
046200061024     c     Sr_Stampa     begsr
046300061020
046400061024      * Cambio P.O. chiudo il file printer
046500061024    1c                   If        Savpo <> W003a  and Savpo <> *blanks
046600061024      *
046700061024     c                   If        %open(tnta94p)
046800061024     c                   write     Ta94e
046900061024     c                   Close     tnta94p
047000061025     c                   add       tottar        gentar
047100061025     c                   add       totcli        gencli
047200061025     c                   clear                   tottar
047300061025     c                   clear                   totcli
047400061024     c                   endif
047500061024
047600061024    1c                   endif
047700061024      * Cambio P.O.
047800061024    1c                   If        Savpo <> w003a
047900061024
048000061024     c                   eval      Savpo = w003a
048100061024     c                   move      Savpo         keypo
048200061024     c     Keypo         chain     Azorg01l
048300061024    2c                   if        %found(azorg01l)
048400061024     c                   eval      Despoc = orgdes
048500061024
048600061024      * stampa separata per P.O. recupero la coda del P.O.
048700061024
048800061024     c                   movel     orgde1        og141
048900061024
049000061024    3c                   If        §ogssc = *blanks
049100061024      * recupero la stampante generica
049200061024     c                   movel     orgde4        og144
049300061024     c                   eval      §ogssc = §ogssp
049400061024    3c                   endif
049500061024
049600061024     c                   clear                   comman
049700061024     c                   movea     cmdo(1)       comman
049800061024     c                   eval      %subst(comman:23:10) = §ogssc
049900061024     c                   Call      'QCMDEXC'                            99
050000061024     c                   Parm                    Comman
050100061024     c                   Parm                    Lun
050200061024
050300061024     c                   open      tnta94p
050400061025     c                   eval      *in63 = *off
050500061024
050600061024    2c                   endif
050700061024
050800061024      * stampo testata
050900061024     c                   write     ta94t
051000061024     c                   write     ta94t1
051100061024     c                   write     ta94tdcm
051200061024     c                   write     ta94t2
051300061024     c                   write     ta94tdcm
051400061024
051500061024    1c                   endif
051600061024      * Preparo i campi di Stampa
051700061024
051800061024     c                   eval      stpksc  = acoksc
051900061024     c                   eval      stprag  = acorag
052000061024
052100061024     c                   movel     tamflo        dsta01
052200061024      * Tipo tariffa
052300061024     c                   select
052400061024     c                   when      §tadpd = 'S'
052500061024     c                   eval      stptip = 'D'
052600061024     c                   when      §tafed = 'S'
052700061024     c                   eval      stptip = 'F'
052800061024     c                   other
052900061024     c                   eval      stptip = tamfie
053000061024     c                   endsl
053100061024      * Data decorrenza
053200061024     c                   Clear                   wlbdat
053300061024     c                   Z-add     Tamddt        g02inv
053400061024     c                   Movel     '3'           g02err
053500061024     c                   Call      'XSRDA8'
053600061024     c                   Parm                    wlbdat
053700061024     c                   Z-add     g02dat        Stpddt
053800061024      * Data scadenza
053900061024     c                   Clear                   wlbdat
054000061024     c                   Z-add     Tamdst        g02inv
054100061024     c                   Movel     '3'           g02err
054200061024     c                   Call      'XSRDA8'
054300061024     c                   Parm                    wlbdat
054400061024     c                   Z-add     g02dat        Stpdst
054500061024      * scrivo la riga di dettaglio
054600061024
054700061024      * overflow
054800061024     c                   if        *in63
054900061024
055000061024     c                   write     ta94t
055100061024     c                   write     ta94t1
055200061024     c                   write     ta94tdcm
055300061024     c                   write     ta94t2
055400061024     c                   write     ta94tdcm
055500061024     c                   eval      *in63 = *off
055600061024     c                   eval      *in41 = *on
055700061024
055800061024     c                   endif
055900061024
056000061025     c   41              add       1             totcli
056100061025     c                   add       1             tottar
056200061024     c                   write     ta94r
056300061024     c                   eval      *in41 = *off
056400061024
056500061024     c                   endsr
056600060320      *--------------------------------------------------------------*
056700060320      * ROUTINE INIZIALE
056800060320      *--------------------------------------------------------------*
056900060320     c     *inzsr        begsr
057000060320
057100060320     c     *entry        plist
057200060320     c                   parm                    kpjba
057300061026     c                   movel     kpjbu         simula
057400061026     c                   eval      *in42 = (simula = 'S')
057500061026      *
057600060320     c     *dtaara       define    §azute        azuteds
057700060320     c     *dtaara       define    §datiute      ddatiute
057800060320
057900060320     c                   in(e)     *dtaara
058000060320     c                   if        %error or rsut = *blanks
058100060320     c                   clear                   tibs34ds
058200060320     c                   call      'TIBS34R'
058300060320     c                   parm                    tibs34ds
058400060320     c                   in        *dtaara
058500060320     c                   endif
058600060320
058700060320
058800060320      * reperisco data e ora del lancio
058900060320     c                   time                    w0140
059000060320     c                   move      w0140         wdata
059100061002     c                   movel     w0140         wora
059200061019      * UDATE in AAAAMMGG
059300061019     c                   eval      g02dat = wdata
059400061019     c                   clear                   g02err
059500061019     c                   call      'XSRDA8'
059600061019     c                   parm                    wlbdat
059700061019     c                   movel     g02inv        dateu
059800061019     c     *iso          movel     dateu         Wdataoggi
059900061019
060000060320
060100060320      * pulisco la sk dei p.o. da elaborare
060200060320     c                   clear                   skpoel
060300061017      * Reperimento dei P.O. dell'Azienda
060400061017     c                   clear                   TRUL31DS
060500061017     c                   eval      I31abi = 'AZ'
060600061017     c                   call      'TRUL31R'
060700061017     c                   parm                    KPJBA
060800061017     c                   parm                    TRUL31DS
060900061017     c                   if        O31pog > *zeros
061000061017     c                   movea     O31pog        Skpoel
061100061017     c                   endif
061200061019
061300061019     c                   z-add     1             codut
061400061019
061500061019     c                   Clear                   ds5as1
061600061019     c     ktabel        chain     Tabel00f
061700061019     c                   If        %Found(Tabel00f)
061800061019     c                   Eval      ds5as1 = Tbluni
061900061019     c                   EndIf
062000061019
062100061017
062200061018      * Cnaco00f
062300061017     c     kaco          klist
062400061017     c                   kfld                    codut
062500061017     c                   kfld                    dutkci
062600061018     c                   kfld                    kksc
062700061018      * CnAcvt1l
062800061018     c     kacvt         klist
062900061018     c                   kfld                    acokcc
063000061018     c                   kfld                    acoksc
063100061019      * CnAcvd1l
063200061019     c     kacvd         klist
063300061019     c                   kfld                    acvkcc
063400061019     c                   kfld                    acvksc
063500061019     c                   kfld                    acvdav
063600061019     c                   kfld                    acvorv
063700061019     c                   kfld                    acvnoj
063800061019     c                   kfld                    k_yda
063900061019     c                   kfld                    k_prg
064000061020      * Tabel00f
064100061019     c     ktabel        klist
064200061019     c                   kfld                    codut
064300061019     c                   kfld                    kcod
064400061019     c                   kfld                    kkey
064500061023      * Titad
064600061023     c     ktariffa      klist
064700061023     c                   kfld                    tamksc
064800061023     c                   kfld                    tamctr
064900061023     c                   kfld                    tamprg
065000061023      * Titpd
065100061023     c     ktpd          klist
065200061023     c                   kfld                    tptksc
065300061023     c                   kfld                    tptctr
065400061023     c                   kfld                    tptprg
065500061023     c                   kfld                    tptftc
065600061023      * Tfntc01l
065700061023     c     Kntc          klist
065800061023     c                   kfld                    Appl
065900061023     c                   kfld                    Notkey
066000061023     c                   kfld                    Notke2
066100100125      * Tfntc01l - note 10
066200100125     c     Kntc10        klist
066300100125     c                   kfld                    Appl
066400100125     c                   kfld                    Notkey
066500100125     c                   kfld                    Notke2
066600100125     c                   kfld                    TipoNota
066700060928
066800060320     c                   endsr
066900061025     osysprt    E            totali           02
067000061025     o                                           +2 'Totali Clienti'
067100061025     o                       gencli        z     +2
067200061025     o                                          +10 'Totali Tariffe'
067300061025     o                       gentar        z     +2
067400060320      *--------------------------------------------------------------*
067500061023** cmd ovrprtf
067600061023OVRPRTF TNTA94P  OUTQ(xxxxxxxxxx) USRDTA('Tarclibloc') SAVE(*YES) HOLD(*YES)
