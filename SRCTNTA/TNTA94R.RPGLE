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
010300160209
010400160209       // -?ds dati passati al TRSM90R - Cancella file Tariffe Inserite
010500160209     d TRSM90DS      e ds
010600060215
010700061023      * Reperimento dati del PGM
010800061023     d pgmstatus      sds
010900061023     d job_name              244    253
011000160209
011100160209      //---------------------------------------------------------------
011200160209      //?Definizione procedure esterne.
011300160209      //---------------------------------------------------------------
011400160209      // - Pgm Cancella file Tariffe Inserite
011500160209     d trsm90r         pr                  extpgm('TRSM90R')
011600160209     d  kpjba                              likeds(KPJBA)
011700160209     d  trsm90ds                           likeds(TRSM90DS)
011800061023
011900060215      *------------------------------------------------------------------------*
012000060215
012100060320      * ciclo per i p.o. caricati in schiera
012200060320    1c                   do        250           xx
012300060320     c                   if        skpoel(xx) = *blanks
012400060320     c                   leave
012500060320     c                   endif
012600060320
012700061024     c                   movel(p)  skpoel(xx)    kksc
012800060215
012900061018     c     kaco          setll     Cnaco00f
013000060320    2c                   do        *hival
013100061018     c                   read      cnaco00f
013200060215      * fine file
013300061018     c                   if        %eof(cnaco00f)
013400060215     c                   leave
013500060215     c                   endif
013600061018      * se p.o. maggiore esco
013700061018     c                   movel     acoksc        w003a
013800061018     c                   if        w003a > skpoel(xx)
013900061018     c                   leave
014000061018     c                   endif
014100060320
014200061018      * verifico se cliente bloccato
014300130315    3c                   if        Acoabl <> *blanks
014400061024     c                   seton                                        41
014500061018      * verifico la data di immissione del blocco
014600061018     c                   exsr      SR_ctrdta
014700061020      * se cliente annullabile oppure data non trovata  eseguo annullamento
014800061020      * e stampa
014900061023     c                   if        Cli_ann = *on or Data_no = *off
015000061023      * verifico se ha delle spedizioni ancora da fatturare
015100061023     c     Acoksc        setll     titas02l
015200061023
015300061023     c                   if        not %equal(titas02l)
015400061020      * eseguo annullamento
015500061020     c                   exsr      Annulla
015600061023     c                   endif
015700061023
015800061020     c                   endif
015900060215
016000060320    3c                   endif
016100060320
016200060320    2c                   enddo
016300060215
016400060320    1c                   enddo
016500060320
016600061023      *
016700061024      * se file stampa aperto stampo l'ultima riga e chiudo
016800061024      *
016900061024     c                   If        %open(tnta94p)
017000061024     c                   write     Ta94e
017100061024     c                   Close     tnta94p
017200061025     c                   add       tottar        gentar
017300061025     c                   add       totcli        gencli
017400061025     c                   except    totali
017500061024     c                   endif
017600061024
017700061023     c                   If        Wta47 = '1'
017800061023     c                   clear                   Tnta47ds
017900061023     c                   eval      D47tla = 'C'
018000061023     c                   movel     Tnta47ds      Kpjbu
018100061026     c                   If        simula = ' '
018200061026     c                   call      'TNTA47R'
018300061026     c                   parm                    Kpjba
018400061026     c                   endif
018500061023     c                   endif
018600060215
018700060215     c                   eval      *inlr = *on
018800060215
018900060215      *------------------------------------------------------------------------*
019000061018      * CONTROLLO DATA IMMISSIONE DEL BLOCCO
019100060215      *------------------------------------------------------------------------*
019200061018     c     SR_Ctrdta     begsr
019300060215
019400061020     c                   eval      Cli_ann = *off
019500061023     c                   eval      Data_no  = *off
019600061018      * verifico se inserita la data della variazione nella testata variazioni
019700061018     c     kacvt         setll     cnacvt1l
019800061018      * se non trovato il record di variazione per me è già annullabile
019900061020i   1c                   If        not %equal(cnacvt1l)
020000061018     c                   eval      Cli_ann = *on
020100061018
020200061020   x1c                   else
020300061018      * se trovato leggo
020400061020i   2c                   do        *hival
020500061018     c     kacvt         reade     cnacvt1l
020600061020      * fne lettura
020700061025i   3c                   if        %eof(cnacvt1l)
020800061020     c                   leave
020900061020e   3c                   endif
021000061018      * annullato rileggo
021100061020i   3c                   if        acvatb <> ' '
021200061018     c                   iter
021300061020e   3c                   endif
021400061018      * se record che segnala l'inserimento do per ok annullamento
021500061018      * per inserimento si intende tutto quello che è diverso da "M"
021600061020i   3c                   if        acvcta <> 'M'
021700061018     c                   eval      Cli_ann = *on
021800061018     c                   leave
021900061020e   3c                   endif
022000061018      * verifico che tipo dati sono stati variati
022100061018     c                   movea     acvflo        DatVar
022200061018      * controllo se esiste blocco cliente
022300061018     c     'B'           lookup    datvar                                 30
022400061018      * se trovato la "B" come dato variato leggo il dettaglio
022500061020i   3c                   If        %found
022600061019
022700061019     c     Kacvd         chain     Cnacvd1l
022800061019      * se trovato non annullato
022900061020i   4c                   If        %found(Cnacvd1l) and acvatb = ' '
023000061019      * verifico come era il codice del blocco prima della variazione "B"
023100061019      *
023200061019     c                   movel     acvprima      dacv_B
023300061019      * se il blocco prima non era valorizzato vuol dire che è avvenuto
023400061019      * in quel momento e quindi prendo come data immissione blocco
023500061019      * quella della variazione
023600061020i   5c                   If        acoabl = ' '
023700061019     c     *iso          movel     acvdav        Wdataeur
023800061024     c                   adddur    §5as1atb:*d   Wdataeur
023900151117      * se sono trascorsi già xx giorni dal blocco del cliente
024000061019      * annulliamo tariffe
024100061020i   6c                   if        Wdataeur < Wdataoggi
024200061019     c                   eval      Cli_ann = *on
024300061019     c                   leave
024400061020     c                   else
024500061023     c                   eval      data_no = *on
024600061020     c                   leave
024700061020e   6c                   endif
024800061019
024900061020      * se invece prima il cliente risulta bloccato leggo un'altra variazione
025000061020     c                   else
025100061020     c                   iter
025200061020e   5c                   endif
025300061020
025400061020      * se non trova il record del dettaglio  leggo un'altra variazione
025500061020     c                   else
025600061020     c                   iter
025700061020e   4c                   endif
025800061020
025900061020      * se non trovato variazione blocco cliente leggo un'altra variazione
026000061020     c                   else
026100061020     c                   iter
026200061020e   3c                   endif
026300061020
026400061020e   2c                   enddo
026500061019
026600061020e   1c                   endif
026700060320
026800060215     c                   endsr
026900060320
027000061020      *------------------------------------------------------------------------*
027100061020      * ANNULLAMENTO TARIFFE CLIENTI BLOCCATI E STAMPA
027200061020      *------------------------------------------------------------------------*
027300061020     c     Annulla       begsr
027400061023
027500061023      * Tntam00l
027600061024     c     acoksc        setll     Tntam00l
027700061020
027800061023d   1c                   do        *hival
027900061023     c     acoksc        reade     Tntam00l
028000061023
028100061023     c                   If        %eof(Tntam00l)
028200061023     c                   leave
028300061023     c                   endif
028400061023      * Annullo Titpt
028500061023     c     ktariffa      setll     Titpt01l
028600061023d   2c                   do        *hival
028700061023     c     ktariffa      reade     Titpt01l
028800061023
028900061023     c                   if        %eof(titpt01l)
029000061023     c                   leave
029100061023     c                   endif
029200061023      * Annullo Titpd Dettaglio tariffa particolare
029300061023     c     ktpd          setll     Titpd01l
029400061023d   3c                   do        *hival
029500061023     c     ktpd          reade     Titpd01l
029600061023
029700061023     c                   if        %eof(titpd01l)
029800061023     c                   leave
029900061023     c                   endif
030000061023      * annullo
030100061023i   4c                   If        Tpdatb = ' '
030200061023     c                   eval      tpdatb = 'A'
030300061023     c                   clear                   tpdftr
030400061023     c                   z-add     dateu         tpddtr
030500061026     c                   if        simula = ' '
030600061026     c                   update    titpd000
030700061026e   4c                   endif
030800061023e   4c                   endif
030900061023
031000061023e   3c                   enddo
031100061023      * annullo Titpt testata tariffa particolare
031200061023     c                   If        Tptatb = ' '
031300061023     c                   eval      tptatb = 'A'
031400061023     c                   clear                   tptftr
031500061023     c                   z-add     dateu         tptdtr
031600061026     c                   if        simula = ' '
031700061026     c                   update    titpt000
031800061026     c                   endif
031900061023     c                   endif
032000061023
032100061023e   2c                   enddo
032200061023      * Annullo Titad
032300061027     c     ktariffa      setll     Titad04l
032400061023d   2c                   do        *hival
032500061027     c     ktariffa      reade     Titad04l
032600061023
032700061027     c                   if        %eof(titad04l)
032800061023     c                   leave
032900061023     c                   endif
033000061023      * annullo
033100061023     c                   If        Tadatb = ' '
033200061023     c                   eval      tadatb = 'A'
033300061023     c                   clear                   tadftr
033400061023     c                   z-add     dateu         taddtr
033500061026     c                   if        simula = ' '
033600061026     c                   update    titad000
033700061026     c                   endif
033800061023     c                   endif
033900061023
034000061023e   2c                   enddo
034100061023      * annullo Titgc
034200061023     c     Ktariffa      Chain     Titgc01l
034300061023      * annullo
034400061023     c                   If        %found(Titgc01l) and Tgcatb = ' '
034500061023     c                   eval      tgcatb = 'A'
034600061023     c                   clear                   tgcftr
034700061023     c                   z-add     dateu         tgcdtr
034800061026     c                   if        simula = ' '
034900061026     c                   update    titgc000
035000061026     c                   endif
035100061023     c                   endif
035200061023
035300061023      * annullo Tfntc note legate alla tariffa
035400061023     c                   movel     acokcc        notkey
035500061023     c                   move      acoksc        notkey
035600061023     c                   movel     tamctr        notke2
035700061023
035800061023     c     Kntc          setll     tfntc01l
035900061023      *
036000061023d   2c                   do        *hival
036100061023     c     Kntc          reade     tfntc01l
036200061023     c                   If        %eof(tfntc01l)
036300061023     c                   leave
036400061023     c                   endif
036500061023
036600061026     c                   if        simula = ' '
036700061026     c                   delete    tfntc
036800061026     c                   endif
036900061023e   2c                   enddo
037000061023      * annullo tntam
037100061023     c                   eval      tamatb = 'A'
037200061023     c                   clear                   tamftr
037300061023     c                   z-add     dateu         tamdtr
037400061026     c                   if        simula = ' '
037500061026     c                   update    tntam000
037600160209      /free
037700160209       //?richiamo pgm per cancellare:
037800160209       //?file Tariffe Inserite
037900160209         clear TRSM90DS;
038000160209         ISM90ksc = TAMksc;
038100160209         ISM90ctr = TAMctr;
038200160209         ISM90prg = TAMprg;
038300160209         trsm90r (kpjba:TRSM90DS);
038400160209      /end-free
038500061026     c                   endif
038600061023
038700061023      * Annullo Titav Variazioni tariffe e scrivo record
038800061023     c     Ktariffa      setll     Titav01l
038900061023
039000061023d   2c                   do        *hival
039100061023     c     Ktariffa      reade     Titav01l
039200061023
039300061023     c                   if        %eof(titav01l)
039400061023     c                   leave
039500061023     c                   endif
039600061023
039700061023     c                   eval      Tavatb = 'A'
039800061023     c                   clear                   Tavftr
039900061023     c                   z-add     dateu         Tavdtr
040000061023
040100061026     c                   if        simula = ' '
040200061026     c                   update    titav000
040300061026     c                   endif
040400061023
040500061023e   2c                   enddo
040600061023      * scrivo un nuovo record di TITAV (annullato) per l'annullamento
040700061023      * della tariffa
040800061023     c                   clear                   Titav000
040900061023     C                   eval      Tavksc = Tamksc
041000061023     C                   eval      Tavctr = Tamctr
041100061023     C                   eval      Tavprg = Tamprg
041200061023      * data variazione
041300061023     C                   eval      tavdav = dateu
041400061023     C                   eval      tavnoj = job_name
041500061023     C                   eval      tavpru = 'BATCH     '
041600061023     c                   eval      tavAtb = 'A'
041700061023     c                   eval      tavtrd = 'TES'
041800061023     c                   eval      tavcta = 'A'
041900061023      * recupero ora di variazione
042000061023     c                   eval      TAVorv = Wora
042100061023     c                   z-add     dateu         tavdtr
042200061023      *
042300061026     c                   if        simula = ' '
042400061026     c                   write     titav000
042500061026     c                   endif
042600061023
042700061023      * Annullo file dati TIPO: TNETC/TNETT/TNETS  ****
042800061027     c                   if        simula = ' '
042900061023     c                   clear                   Tnta47ds
043000061023     c                   eval      Wta47 = *on
043100061023     c                   eval      D47tup = '1'
043200061023     c                   eval      D47cto = 'T'
043300061023     c                   eval      D47dsf = 'S'
043400061023     c                   eval      D47ksc = Tamksc
043500061023     c                   eval      D47ctr = Tamctr
043600061023     c                   eval      D47prg = Tamprg
043700061024     c                   movel     tnta47ds      kpjbu
043800061023     c                   call      'TNTA47R'
043900061023     c                   parm                    kpjba
044000061027     c                   endif
044100061024      * STAMPA
044200120207     c**!!!              exsr      Sr_Stampa
044300061023
044400061023e   1c                   enddo
044500100125
044600100125      * dopo che ho letto tutte le tariffe del cliente
044700100125      * controllo se ho note di tipo 10 aperte solo sul cliente
044800100125     c                   movel     acokcc        notkey
044900100125     c                   move      acoksc        notkey
045000100125     c                   clear                   notke2
045100100125      * azzero flag di appoggio per leggere solo 1 volta le tariffe
045200100125     c                   eval      tariffe = *off
045300100125     c     Kntc10        setll     tfntc01l
045400100125    1c                   do        *hival
045500100125     c     Kntc10        reade     tfntc01l
045600100125     c                   If        %eof(tfntc01l)
045700100125     c                   leave
045800100125     c                   endif
045900100125      * trovo nota di tipo 10 verifico se il cliente ha ancora tariffe
046000100125      * non annullate
046100100125    2c                   if        not tariffe
046200100125     c                   eval      tariffe = *on
046300100125     c     acoksc        setll     tntam00l
046400100125      * anche una sola tariffa non annullata esco dal giro
046500100125      * perchè devo tenere la nota di tipo 10
046600100125    3c                   if        %equal(tntam00l)
046700100125     c                   leavesr
046800100125     c                   endif
046900100125    2c                   endif
047000100125      * se arrivo qua vuol dire che non ho trovato tariffe valide
047100100125      * quindi posso cancellare la nota di tipo 10
047200100125     c                   if        simula = ' '
047300100125     c                   delete    tfntc
047400100125     c                   endif
047500100125    1c                   enddo
047600061023
047700061023     c                   endsr
047800061024
047900061024      *--------------------------------------------------------------*
048000061024      * Stampa tariffe annullate
048100061024      *--------------------------------------------------------------*
048200061024     c     Sr_Stampa     begsr
048300061020
048400061024      * Cambio P.O. chiudo il file printer
048500061024    1c                   If        Savpo <> W003a  and Savpo <> *blanks
048600061024      *
048700061024     c                   If        %open(tnta94p)
048800061024     c                   write     Ta94e
048900061024     c                   Close     tnta94p
049000061025     c                   add       tottar        gentar
049100061025     c                   add       totcli        gencli
049200061025     c                   clear                   tottar
049300061025     c                   clear                   totcli
049400061024     c                   endif
049500061024
049600061024    1c                   endif
049700061024      * Cambio P.O.
049800061024    1c                   If        Savpo <> w003a
049900061024
050000061024     c                   eval      Savpo = w003a
050100061024     c                   move      Savpo         keypo
050200061024     c     Keypo         chain     Azorg01l
050300061024    2c                   if        %found(azorg01l)
050400061024     c                   eval      Despoc = orgdes
050500061024
050600061024      * stampa separata per P.O. recupero la coda del P.O.
050700061024
050800061024     c                   movel     orgde1        og141
050900061024
051000061024    3c                   If        §ogssc = *blanks
051100061024      * recupero la stampante generica
051200061024     c                   movel     orgde4        og144
051300061024     c                   eval      §ogssc = §ogssp
051400061024    3c                   endif
051500061024
051600061024     c                   clear                   comman
051700061024     c                   movea     cmdo(1)       comman
051800061024     c                   eval      %subst(comman:23:10) = §ogssc
051900061024     c                   Call      'QCMDEXC'                            99
052000061024     c                   Parm                    Comman
052100061024     c                   Parm                    Lun
052200061024
052300061024     c                   open      tnta94p
052400061025     c                   eval      *in63 = *off
052500061024
052600061024    2c                   endif
052700061024
052800061024      * stampo testata
052900061024     c                   write     ta94t
053000061024     c                   write     ta94t1
053100061024     c                   write     ta94tdcm
053200061024     c                   write     ta94t2
053300061024     c                   write     ta94tdcm
053400061024
053500061024    1c                   endif
053600061024      * Preparo i campi di Stampa
053700061024
053800061024     c                   eval      stpksc  = acoksc
053900061024     c                   eval      stprag  = acorag
054000061024
054100061024     c                   movel     tamflo        dsta01
054200061024      * Tipo tariffa
054300061024     c                   select
054400061024     c                   when      §tadpd = 'S'
054500061024     c                   eval      stptip = 'D'
054600061024     c                   when      §tafed = 'S'
054700061024     c                   eval      stptip = 'F'
054800061024     c                   other
054900061024     c                   eval      stptip = tamfie
055000061024     c                   endsl
055100061024      * Data decorrenza
055200061024     c                   Clear                   wlbdat
055300061024     c                   Z-add     Tamddt        g02inv
055400061024     c                   Movel     '3'           g02err
055500061024     c                   Call      'XSRDA8'
055600061024     c                   Parm                    wlbdat
055700061024     c                   Z-add     g02dat        Stpddt
055800061024      * Data scadenza
055900061024     c                   Clear                   wlbdat
056000061024     c                   Z-add     Tamdst        g02inv
056100061024     c                   Movel     '3'           g02err
056200061024     c                   Call      'XSRDA8'
056300061024     c                   Parm                    wlbdat
056400061024     c                   Z-add     g02dat        Stpdst
056500061024      * scrivo la riga di dettaglio
056600061024
056700061024      * overflow
056800061024     c                   if        *in63
056900061024
057000061024     c                   write     ta94t
057100061024     c                   write     ta94t1
057200061024     c                   write     ta94tdcm
057300061024     c                   write     ta94t2
057400061024     c                   write     ta94tdcm
057500061024     c                   eval      *in63 = *off
057600061024     c                   eval      *in41 = *on
057700061024
057800061024     c                   endif
057900061024
058000061025     c   41              add       1             totcli
058100061025     c                   add       1             tottar
058200061024     c                   write     ta94r
058300061024     c                   eval      *in41 = *off
058400061024
058500061024     c                   endsr
058600060320      *--------------------------------------------------------------*
058700060320      * ROUTINE INIZIALE
058800060320      *--------------------------------------------------------------*
058900060320     c     *inzsr        begsr
059000060320
059100060320     c     *entry        plist
059200060320     c                   parm                    kpjba
059300061026     c                   movel     kpjbu         simula
059400061026     c                   eval      *in42 = (simula = 'S')
059500061026      *
059600060320     c     *dtaara       define    §azute        azuteds
059700060320     c     *dtaara       define    §datiute      ddatiute
059800060320
059900060320     c                   in(e)     *dtaara
060000060320     c                   if        %error or rsut = *blanks
060100060320     c                   clear                   tibs34ds
060200060320     c                   call      'TIBS34R'
060300060320     c                   parm                    tibs34ds
060400060320     c                   in        *dtaara
060500060320     c                   endif
060600060320
060700060320
060800060320      * reperisco data e ora del lancio
060900060320     c                   time                    w0140
061000060320     c                   move      w0140         wdata
061100061002     c                   movel     w0140         wora
061200061019      * UDATE in AAAAMMGG
061300061019     c                   eval      g02dat = wdata
061400061019     c                   clear                   g02err
061500061019     c                   call      'XSRDA8'
061600061019     c                   parm                    wlbdat
061700061019     c                   movel     g02inv        dateu
061800061019     c     *iso          movel     dateu         Wdataoggi
061900061019
062000060320
062100060320      * pulisco la sk dei p.o. da elaborare
062200060320     c                   clear                   skpoel
062300061017      * Reperimento dei P.O. dell'Azienda
062400061017     c                   clear                   TRUL31DS
062500061017     c                   eval      I31abi = 'AZ'
062600061017     c                   call      'TRUL31R'
062700061017     c                   parm                    KPJBA
062800061017     c                   parm                    TRUL31DS
062900061017     c                   if        O31pog > *zeros
063000061017     c                   movea     O31pog        Skpoel
063100061017     c                   endif
063200061019
063300061019     c                   z-add     1             codut
063400061019
063500061019     c                   Clear                   ds5as1
063600061019     c     ktabel        chain     Tabel00f
063700061019     c                   If        %Found(Tabel00f)
063800061019     c                   Eval      ds5as1 = Tbluni
063900061019     c                   EndIf
064000061019
064100061017
064200061018      * Cnaco00f
064300061017     c     kaco          klist
064400061017     c                   kfld                    codut
064500061017     c                   kfld                    dutkci
064600061018     c                   kfld                    kksc
064700061018      * CnAcvt1l
064800061018     c     kacvt         klist
064900061018     c                   kfld                    acokcc
065000061018     c                   kfld                    acoksc
065100061019      * CnAcvd1l
065200061019     c     kacvd         klist
065300061019     c                   kfld                    acvkcc
065400061019     c                   kfld                    acvksc
065500061019     c                   kfld                    acvdav
065600061019     c                   kfld                    acvorv
065700061019     c                   kfld                    acvnoj
065800061019     c                   kfld                    k_yda
065900061019     c                   kfld                    k_prg
066000061020      * Tabel00f
066100061019     c     ktabel        klist
066200061019     c                   kfld                    codut
066300061019     c                   kfld                    kcod
066400061019     c                   kfld                    kkey
066500061023      * Titad
066600061023     c     ktariffa      klist
066700061023     c                   kfld                    tamksc
066800061023     c                   kfld                    tamctr
066900061023     c                   kfld                    tamprg
067000061023      * Titpd
067100061023     c     ktpd          klist
067200061023     c                   kfld                    tptksc
067300061023     c                   kfld                    tptctr
067400061023     c                   kfld                    tptprg
067500061023     c                   kfld                    tptftc
067600061023      * Tfntc01l
067700061023     c     Kntc          klist
067800061023     c                   kfld                    Appl
067900061023     c                   kfld                    Notkey
068000061023     c                   kfld                    Notke2
068100100125      * Tfntc01l - note 10
068200100125     c     Kntc10        klist
068300100125     c                   kfld                    Appl
068400100125     c                   kfld                    Notkey
068500100125     c                   kfld                    Notke2
068600100125     c                   kfld                    TipoNota
068700060928
068800060320     c                   endsr
068900061025     osysprt    E            totali           02
069000061025     o                                           +2 'Totali Clienti'
069100061025     o                       gencli        z     +2
069200061025     o                                          +10 'Totali Tariffe'
069300061025     o                       gentar        z     +2
069400060320      *--------------------------------------------------------------*
069500061023** cmd ovrprtf
069600061023OVRPRTF TNTA94P  OUTQ(xxxxxxxxxx) USRDTA('Tarclibloc') SAVE(*YES) HOLD(*YES)
