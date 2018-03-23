000100101012     h Decedit('0,') Datedit(*ymd/) Option(*nodebugio)
000200130902
000300061227      *------------------------------------------------------------------------*
000400061227      *
000500061227      *          PREPARAZIONE DATI PER PDA AUT
000600061227      *
000700061227      *------------------------------------------------------------------------*
000800080304     ffidst01l  if   e           k disk
000801170728     ffiapd61l  if   e           k disk
000900101012     ffiarg01l  if   e           k disk
001000101012     ffnart01l  if   e           k disk
001100080321     ffnarb01l  if   e           k disk
001200080227     ffiar601l  if   e           k disk
001300080227     ffiar901l  if   e           k disk
001400080227     ffiar501l  if   e           k disk
001500080227     ffiar401l  if   e           k disk
001600080304     ffiarn11l  if   e           k disk
001700080321     ffnlbl01l  if   e           k disk
001800080418     ffnevb05l  if   e           k disk
001900080505     ffnbrv01l  if   e           k disk
002000080403     fazcln01l  if   e           k disk
002100080422     fazorg01l  if   e           k disk
002200080303     ftabel00f  if   e           k disk
002300080403     ftntbe01l  if   e           k disk
002400080326     ffipdc02t  if   e           k disk    rename(fipdc000:fipdc2) prefix(x)
002500130724     ffipdc01t  if   e           k disk    infds(pdc01)
002600080904     ffipdc03t  uf a e           k disk    rename(fipdc000:fipdc3)
002602170728     ffipdc61t  if a e           k disk
002700130724     ffipdd01t  if   e           k disk    infds(pdd01)
002701170728     ffipdc00t  uf a e             disk    rename(fipdc000:fipdc)
002900130724     ffipdd00t  uf a e             disk    rename(fipdd000:fipdd)
003000131128     ffidsf01l  if   e           k disk
003100140120     fficau02l  if   e           k disk
003200061227
003300061227      *------------------------------------------------------------------------*
003400061227      *  RIEPILOGO INDICATORI
003500061227      *------------------------------------------------------------------------*
003600071205
003700061227      *------------------------------------------------------------------------*
003800130724     D pdc01           DS
003900130724     D  pdc01nrr             397    400B 0
004000130724     D pdd01           DS
004100130724     D  pdd01nrr             397    400B 0
004200061227
004300061227      *   S C H I E R E
004400080227
004500080227      *   D S   I N T E R N E / E S T E R N E
004600100317     d c_NoContr       c                   const('NO CONTRATTO')
004700100317     d WX              s              3  0 inz
004800080320     d tmp             s             17
004900110408     d climit          s              7
005000170728     d pdc             s                     like(pdc6pdr)
005001170728     d climitt         s                     like(arbksc)
005100100317     d pesparz         s                     like(arbpkc)
005200080320     d volparz         s                     like(arbvlc)
005300080320     d pescons         s                     like(arbpkc)
005400080320     d volcons         s                     like(arbvlc)
005500100914     d $mamma          s                     like(§3Abcm)
005600080320     d wpes            s                     like(arbpkc)
005700080320     d wvol            s                     like(arbvlc)
005800080320     d wcolli          s                     like(arbncl)
005900080422     d wcollip         s                     like(arbncl)
006000080422     d wdatap          s                     like(artdcm)
006100101014     d luncgi          s              2  0
006200101014     d lunlod          s              2  0
006300101014     d luntot          s              2  0
006400100902     d ritinc          s              1
006500101129     d comddt          s              1
006600120412     d compkl          s              1
006700080403     d x               s              3  0
006800080403     D MES             S              1    DIM(31)
006900080417     D TSA             S              1    DIM(30)
007000080403     D dtaiso          S               d
007100080418     D dataiso         S               d
007200080418     D dataeur         S               d   datfmt(*eur)
007300080403     D oggi            S              8  0
007400140411      *ds composizione campo
007500140416     d orastd28        ds
007600140416     d  orastd13                     13
007700140416     d  orastd15                     15
007800080418      * - Tab. "2A" (Causali Eventi): causali con tipo consegna = "G"
007900080418     d $2Acev_G        s                   dim(999) inz like(evbcev)
008000101118      * - Tab. "3E" (Anamalie da spunte
008100101118     d $3Ecan          s                   dim(999) inz like(artcan)
008200080418
008300080403     d doccod          s              1      dim(50)
008400080422     d docflg          s              5      dim(50)
008500080403     d comflg          ds
008600080403     d  flgFAT                        1
008700080403     d  flgDDT                        1
008800080403     d  flgPKL                        1
008900080403     d  flgFAS                        1
009000080422     d  flgDAS                        1
009100080227
009200080304     d noteldv         ds
009300080304     d  noteldv1                     70
009400080304     d  noteldv2                     70
009500080304     d  noteldv3                     70
009600080304     d  noteldv4                     70
009700061227
009800080430     d fipdcds       e ds                  extname(fipdc01t)
009900080904     d fipdcdsB      e ds                  extname(fipdc03t) prefix(b)
010000080904     d fipdcdsA      e ds                  extname(fipdc03t) prefix(a)
010100080430     d fipddds       e ds                  extname(fipdd01t)
010200080430     d fipdddsB      e ds                  extname(fipdd01t) prefix(b)
010300080430     d fipdddsA      e ds                  extname(fipdd01t) prefix(a)
010400130906     d fipdddsav     e ds                  extname(fipdd01t) prefix(s)
010500080430     d fnarbds       e ds                  extname(fnarb00f)
010600130325     d fnarbds$      e ds                  extname(fnarb00f) prefix($)
010700080321     d fnarbdsx      e ds                  extname(fnarb00f) prefix(x)
010800100914     d arbmamds      e ds                  extname(fnarb00f) prefix(m)
010900080630     d fidg45ds      e ds
011000080227     d fidg42ds      e ds
011100080304     d fnlv80ds      e ds
011200080418     d fidn12ds      e ds
011300080707     d fior56ds      e ds
011400061227     d kpjba         e ds
011500080630     d kpjbus          s                   like(kpjbu)
011600080422     d dsbl4a        e ds
011700141110     d dsbl4y        e ds
011800080227     d dar5gen       e ds
011900080303     d dar5rcc       e ds
012000080303     d dar5bnb       e ds
012100140120     d dar5emd       e ds
012200100317     d OG143ccm      e ds                  inz     extname(OG143)
012300100317     d                                             prefix(§CM:3)
012400080418     d ds2a          e ds
012500080304     d ds3a          e ds
012600101118     d ds3e          e ds
012700100914     d dstb          e ds
012800080304     d ds7v          e ds
012900080521     d ds7r          e ds
013000080304     d ds1p          e ds
013100080403     d ddoc          e ds
013200100827     d dfir          e ds
013300100827     d dpxapda       e ds
013400080404     d dta4q         e ds
013500140314     d trulorsds     e ds
013600140314     d trulor2ds     e ds
013700150804     d diore01       e ds
013800100325      * - Parametri per TNSD99R (calcolo tempi di delivery)
013900100325     d TNSD99ds      e ds                  inz
014000100325     d  D98tla       e                     inz('C')
014100100914      * - Parametri per TRUL21R (calcolo limiti importo c/assegno)
014200100914     d TRUL21ds      e ds                  inz
014300100914     d  I21tla       e                     inz('L')
014400100914     d*
014500100315     d TRULINTds     e ds                  inz
014600100315     d   iINTtip     e                     inz('I')
014700080422      * Codice cliente "PraOil oleodotti italiani"?
014800080422     d CliINT          c                   const(0201515)
014900080428      * Codice cliente "FedEx"
015000080428     d CliFed          c                   const(0501331)
015100100317      * Codice cliente "S G Soluzioni Logistiche"
015200100317     d c_CliSG         c                   const(0634923)
015300100317     d c_CliSG_2       c                   const(0635042)
015400100317      * Codice cliente "Consorzio Lotterie Nazionali - Gratta & Vinci"
015500100317     d c_CliCLN        c                   const(0632910)
015600100914     d Status         sds
015700100914     d  SDSpgm           *proc
015800100914     d  SDSusr               254    263
015900140120      *
016000140120     d weditora        c                   '  :  '
016100080227      * ?_______________________________________________________________________
016200080227     c     *entry        plist
016300080227     c                   parm                    kpjba
016400130325     c                   parm                    fnarbds$
016500080227
016600080227     c                   clear                   fidg42ds
016700080227     c                   eval      fidg42ds = kpjbu
016800130724      * chiamata per chiusura
016900130724     c                   if        co42tla = 'C'
017000130724     c                   goto      chiudi
017100130724     c                   endif
017200130322      * se non ricevo ds come secondo parametro la riempio con una chain su arb
017300130322     c                   if        %parms = 1
017400130322     C     karb          chain     fnarb01l
017500130322     c                   if        not%found(fnarb01l)
017600130325     c                   clear                   fnarbds
017700130325     c                   eval      co42err = 'E'
017800130325     c                   eval      co42msg = 'Non trovata spedizione '
017900130325     c                   return
018000130325     c                   else
018100140103     c                   if        arbndc <> co42ndc
018200140103     c                             and arbndc <> 8888888
018300130507     c                   eval      co42err = 'E'
018400130507     c                   eval      co42msg = 'Invio dati a Pda non completato'
018500130507     c                   return
018600130515     c                   endif
018700130515     c                   endif
018800130507     c                   else
018900130325     c                   eval      fnarbds = fnarbds$
019000130322     c                   endif
019100140103     c                   if        arbndc = 8888888
019200140103     c                   move      'A'           co42tla
019300140103     c                   endif
019400131128      * verifica se presente fase CRI per la distinta nel caso esce
019500131128     c                   z-add     4             DSFNPG
019600131128     c                   z-add     arbndc        DSFNFV
019700131128     c                   z-add     arbifp        DSFFGS
019800131128     c                   move      'CRI'         DSFTRD
019900131128     c     kdsf          setll     fidsf01l                               98
020000131128     c                   if        *in98
020100131128     c                   eval      co42err = 'E'
020200131128     c                   eval      co42msg = 'Distinta già in fase CRI aggiorna-
020300131128     c                             mento non possibile'
020400131128     c                   goto      fine
020500131128     c                   endif
020600080227
020700080227      * Richiamato da preparazione dati di massa
020800080227
020900080227     c                   clear                   co42err
021000080227     c                   clear                   co42msg
021100080227      * verifico se la distinta è già stata lanciata in maniera globale
021200080227      * se non c'è e sto trattando la singola spedizione non inserisco il rekord
021300080227     c                   if        co42tla <> 'T'
021400080310
021500130724     c     kfipdc1       chain     fipdc01t
021600080227      * se non esiste su FIPDC torno errore ai programmi chiamanti
021700080227     c                   if        not %found(fipdc01t)
021800080227     c                   eval      co42err = 'E'
021900080227     c                   eval      co42msg = 'Spedizione non scaricata sul PDA -
022000080311     c                                        distinta non ancora trasferita'
022100080227     c                   goto      fine
022200080227     c                   endif
022300080227      * se la chiamata è per annullamento elimino i dettagli e la spedizione e non faccio altro
022400080227     c                   if        co42tla = 'A'
022500080227     c                   exsr      annulla
022600080227     c                   goto      fine
022700080227     c                   endif
022800080227
022900080227     c                   endif
023000070301
023100080227     c                   exsr      scrivi
023200080227
023300080227     c     fine          tag
023400080227
023500080227     c                   eval      kpjbu = fidg42ds
023600080227     c                   return
023700080506     c     chiudi        tag
023800080506
023900080506     c                   seton                                        lr
024000080227      * ?_____________________________________________________
024100080227     c     annulla       begsr
024200080227      * ?_____________________________________________________
024300080904     c     kfipdc        setgt     fipdc01t
024400080904     c     kfipdc        readpe    fipdc01t
024500080904     c                   if        not %eof(fipdc01t)
024600130724     c     pdc01nrr      chain     fipdc00t
024700080227     c* annulla dettaglio
024800080904     c     kfipdc        setll     fipdd01t
024900080227     c                   do        *hival
025000080904     c     kfipdc        reade     fipdd01t
025100080227     c                   if        %eof(fipdd01t)
025200080227     c                   leave
025300080227     c                   endif
025400130724     c     pdd01nrr      chain     fipdd00t
025500080313     c                   eval      pddatb = 'A'
025600080307     c                   eval      tmp= %char(%timestamp: *iso0)
025700080313     c                   eval      PDdDATORA =   tmp
025800130724     c                   update    fipdd
025801170728     c                   feod      fipdd00t
025900080227     c                   enddo
026000080227     c* annulla spedizione
026100080313     c                   eval      pdcatb = 'A'
026200080704      *
026300080704     c     karb          chain     fipdc02t
026400080704     c                   if        %found(fipdc02t)
026500080704     c                   eval      PDCIDROW =   xPDCIDROW + 1
026600080704     c                   endif
026700080307     c                   eval      tmp= %char(%timestamp: *iso0)
026800080313     c                   eval      PDcDATORA =   tmp
026900130724     c                   update    fipdc
026901170728     c                   feod      fipdc00t
027000080630      *
027100080707     c* chiamata annullamento ORM su FIPDO
027200080707     c                   if        pdcnor > 0
027300080707     c                   clear                   fior56ds
027400080707     c                   move      'A'           OR56TLA
027500080707     c                   z-add     pdcfgs        OR56FGS
027600080707     c                   z-add     pdcpoe        OR56POE
027700080707     c                   z-add     pdcnsr        OR56NSR
027800080707     c                   z-add     pdcnor        OR56NOR
027900080707     c                   z-add     pdcnrv        OR56NRV
028000080707     c                   z-add     pdcndc        OR56NDCD
028100080707     c                   call      'FIOR56R'
028200080707     c                   parm                    kpjba
028300080707     c                   parm                    fior56ds
028400080707     c                   endif
028500080630      * annulla anche eventuali info da Visionare sul FIPCT
028600080630     c                   clear                   fidg45ds
028700080805     c                   z-add     PDCfgs        C45fgs
028800080805     c                   z-add     PDCndc        C45ndc
028900080805     c                   z-add     PDCcodaut     C45codaut
029000080805     c                   z-add     PDCAAS        C45AAS
029100080630     c                   z-add     PDCLNP        C45LNP
029200080630     c                   z-add     PDCNRS        C45NRS
029300080630     c                   z-add     PDCNSP        C45NSP
029400080630     c                   move      'S'           C45AGGVIS
029500080630     c                   eval      kpjbus = kpjbu
029600080630     c                   eval      kpjbu  = fidg45ds
029700080630     c                   call      'FIDG45R'
029800080630     c                   parm                    kpjba
029900080630     c                   eval      fidg45ds = Kpjbu
030000080630     c                   eval      kpjbu = kpjbus
030100080630      *
030200080227     c                   endif
030300080227     c                   endsr
030400080227      * ?_____________________________________________________
030500080227     c     scrivi        begsr
030600100317      * ?_____________________________________________________
030700100914      * salvo dati bolla nel caso mi trovi poi con la bolla mamma
030800100914     c                   movel     fnarbds       fnarbdsx
030900101129     c                   clear                   comddt
031000120412     c                   clear                   compkl
031100101129     c                   clear                   ritinc
031200100915     c                   clear                   comflg
031300100317      *  TIPO PORTO
031400100317     c                   clear                   ds3a
031500100317     C                   movel     '3A'          tblcod
031600100317     C                   movel(P)  ARBcbo        tblkey
031700100317     C     ktab          chain     TABEL00f
031800100317     c                   if        %found(tabel00f)
031900100317     C                   movel     TBLUNI        DS3A
032000100317     c                   endif
032100100914     c*
032200100915     c                   clear                   dstb
032300100914     c                   movel     'TB'          TBLcod
032400100914if  2c                   if        §3Atb2 <> *blanks
032500100914     c                   movel(p)  §3Atb2        TBLkey
032600100914x   2c                   else
032700100914     c                   movel(p)  §3Atb1        TBLkey
032800100914e   2c                   endif
032900100914     c     ktab          chain     TABEL00f
033000100914if  2c                   if        %found(TABEL00F)
033100100914     c                   movel     TBLuni        dsTB
033200100914e   2c                   endif
033300100914     c* verifico se spedizione con mamma
033400100914     c                   eval      $Mamma = §3Abcm
033500100914     c                   if        $mamma = 'S'
033600100914     c                   exsr      srmamma
033700100914     c                   end
033800100317      *  CLIENTE MITTENTE
033900100317     c                   clear                   climitt
034000100317if  1c                   If        ARBccm <> *zeros
034100100317     c                   movel(p)  ARBccm        climitt
034200100317x   1c                   Else
034300100317if  2c                   if        %subst(§3Atb1:1:1) = 'F'
034400100317     c                   movel     ARBksc        climitt
034500100317e   2c                   endif
034600100317e   2c                   endif
034700100317     c* dati network
034800100317     c     arblnp        chain     azorg01l
034900100317      * Dati della filiale del mittente
035000100317     c                   if        climitt <> 0
035100100318     c                   eval      WX     =  climitt / 10000
035200100317if  1c                   if        WX     <> arblnp
035300100319     c     WX            chain     AZORG01l
035400100317e   1c                   endif
035500100317e   1c                   endif
035600100317     C*
035700100317if  1c                   if        %found(AZORG01L)
035800100317     c                   movel     ORGde3        OG143ccm
035900100317x   1c                   else
036000100317     c                   clear                   OG143ccm
036100100317e   1c                   endif
036200101012     C* giro spedizione
036300101012     c     karb          chain     fiarg01l
036400101012     c                   if        not %found(fiarg01l)
036500101012     c                   clear                   argcgi
036600101012     c                   end
036700101012     C*
036800080304     c     kdst          chain     fidst01l
036900080304     c                   if        %found(fidst01l) and dstatb = *blank
037000080304     c                             and (dstpda = 'C' or dstpda = 'E')
037100100915     c* dettaglio colli
037200080304     c                   exsr      elab_dett
037300100915     c* testata
037400080904     c     kfipdc3       chain     fipdc03t
037500100915     c* salvo ds x confrontare se apportate modifiche e quindi aggiornare il
037600100915     c* ticket oppure se non aggiornare il record, questo perchè in alcuni
037700100915     c* pgm (fnlr50) chiamano il pgm per tutte le spedizioni in distinta
037800100915     c* anche se non toccate
037900140129     c                   if        %found(fipdc03t)
038000140129     c                   movel     fipdcds       fipdcdsB
038100100826     c                   else
038200100826     c                   clear                   fipdcdsB
038300100826     c                   endif
038400140129     c                   clear                   fipdc
038500080307     c                   eval      PDCatb    =   *blank
038600080321     c                   eval      PDCFGS    =   arbifp
038700080321     c                   eval      PDCNDC    =   arbNDC
038800080321     c                   eval      PDCAAS    =   arbAAS
038900080321     c                   eval      PDCLNP    =   arbLNP
039000080321     c                   eval      PDCNRS    =   arbNRS
039100080321     c                   eval      PDCNSP    =   arbNSP
039200080321     c                   eval      PDCATB    =   *blank
039300080321     c                   eval      PDCTDC    =   dstfpp
039400080321     c                   eval      PDCRSM    =   arbRSM
039500080321     c                   eval      PDCLOM    =   arbLOM
039600080321     c                   eval      PDCPRM    =   arbPRM
039700080321     c                   eval      PDCNZM    =   arbNZM
039800080321     c                   eval      PDCRMN    =   arbRMN
039900080321     c                   eval      PDCRMA    =   arbRMA
040000080321     c                   eval      PDCRMO    =   arbRMO
040100080321     c                   eval      PDCRSD    =   arbRSD
040200080321     c                   eval      PDCIND    =   arbIND
040300080321     c                   eval      PDCLOD    =   arbLOD
040400110121     c*                  if        argcgi <> ' '
040500110121     c*                  eval      luncgi = %len(%trimr(argcgi))
040600110121     c*                  eval      lunlod = %len(%trimr(arblod))
040700110121     c*                  eval      luntot = luncgi + lunlod + 1
040800101014     c* se la lunghezza della località + '-' + del giro supera 35 byte
040900101014     c* tolgo un po' dalla località
041000110121     c*                  if        luntot > 35
041100110121     c*                  eval      lunlod = 35 - luncgi - 1
041200110121     c*                  end
041300110121     c*                  eval      pdclod = %subst(arblod: 1: lunlod) +
041400110121     c*                            '-' +  %subst(argcgi: 1: luncgi)
041500110121     c*                  end
041600101108     c                   eval      pdcgiro = argcgi
041700101108     c                   eval      pdcznc  = arbznc
041800101108     c* aggiorno il campo ordinamento solo se sto passando l'intera distinta
041900101108     c* nel chiamante FIDG41R i record vengono già letti ordinati per
042000101108     c* località, giro, destinatario, quindi viene passatio il contatore
042100101109     c* altrimenti in inserimento azzero
042200101108     c                   eval      pdcordina  = co42ord
042300080321     c                   eval      PDCPRD    =   arbPRD
042400080321     c                   eval      PDCNZD    =   arbNZD
042500080321     c                   eval      PDCGC1    =   arbGC1
042600080321     c                   eval      PDCGC2    =   arbGC2
042700170206      * l'ara viene passata solo se non è una data già passata
042800100913     c                   if        arbdcr >= oggi
042900170206     c*                  eval      PDCTCR    =   arbTCR
043000170206     c*                  eval      PDCDCR    =   arbDCR
043100080321     c                   eval      PDCHCR    =   arbHCR
043200080404     c                   endif
043201170206      * oppure se ho una data ma è "DOPO", quindi l'ora va sempre bene
043202170206     c                   if        arbdcr <> 0 and
043203170206     c                             arbTCR    =   'D'
043204170206     c                   eval      PDCHCR    =   arbHCR
043205170206     c                   endif
043300140129      * il ficau viene chainato solo se il rekord è in aggiornamento
043400140129     c                   if        %found(fipdc03t)
043500140129      * in presenza di record su PDC verifico che non siano state fatte variazioni  da telefonate
043600140129      * se le trovo imposto i campi dal file telefonate
043700140129     c                   clear                   oggsped
043800140129     c                   eval      oggsped = %editc(arblnp:'X') +
043900140129     c                                       %editc(arbnrs:'X') +
044000140129     c                                       %editc(arbnsp:'X') +
044100140129     c                                       %editc(arbaas:'X')
044200140129     c     kficau        chain     ficau02l
044300140129     c                   if        %found(ficau02l)
044400140129     c                   eval      PDCric    =   caucmd
044500140129     c                   eval      PDCcmd    =   caudescmd
044600140129     c                   if        caudesva <> *blank
044700140129     c                   eval      pdcvar = 'Variato: ' + caudesva
044800140129     c                   endif
044900140129     c                   endif
045000140129     c                   endif
045100080304      *  TIPO consegna particolare verifico se deve essere passata al PDA
045200100915     c                   clear                   ds1p
045300080304     c                   if        arbtc1 <> *blank
045400080304     C                   movel     '1P'          tblcod
045500080304     C                   movel(P)  arbtc1        tblkey
045600080304     C     ktab          chain     TABEL00f
045700080304     c                   if        %found(tabel00f)
045800080304     C                   movel     TBLUNI        DS1P
045900080304     c                   if        §1ppda <> 'N'
046000080321     c                   eval      PDCTC1    =   arbTC1
046100080304     c                   endif
046200080304     c                   endif
046300080304     c                   endif
046400100915     c                   clear                   ds1p
046500080304     c                   if        arbtc2 <> *blank
046600080304     C                   movel(P)  arbtc2        tblkey
046700080304     C     ktab          chain     TABEL00f
046800080304     c                   if        %found(tabel00f)
046900080304     C                   movel     TBLUNI        DS1P
047000080304     c                   if        §1ppda <> 'N'
047100080304     c                   eval      PDCTC2    =   arbTC2
047200080304     c                   endif
047300080304     c                   endif
047400080304     c                   endif
047500080403      * seconda ragione sociale
047600080403     c                   move      'D'           trek4
047700080403     c     kar4          chain     fiar401l
047800080403     c                   if        %found(fiar401l)
047900080403     c                   eval      PDCrs2    =   AR4NOT
048000080403     C                   endif
048100080304      * note bolla ar4 trek 8/9
048200080227     c                   move      '8'           trek4
048300080303     c     kar4          chain     fiar401l
048400080303     c                   if        %found(fiar401l)
048500080303     c                   eval      PDCNO1    =   AR4NOT
048600080303     C                   endif
048700080303     c                   move      '9'           trek4
048800080303     c     kar4          chain     fiar401l
048900080303     c                   if        %found(fiar401l)
049000080303     c                   eval      PDCNO2    =   AR4NOT
049100080303     C                   endif
049200100317      * Flag DDT SI  ar4 trek A solo se 1° tentativo
049300100915     c                   clear                   dsbl4a
049400080403     c                   move      'A'           trek4
049500080403     c     kar4          chain     fiar401l
049600080403     c                   if        %found(fiar401l)
049700080422     c                   movel     AR4NOT        dsbl4a
049800100916     c                   if        §B4ABM = 'S' or §B4ABM = 'P' or
049900100916     c                             §B4ABM = 'C' or §B4ABM = 'Q'
050000101129     c                   eval      comddt = 'S'
050100100916     c                   if        arbtmc = ' '
050200080403     c                   eval      flgddt = 'S'
050300080403     c                   else
050400100916     c                   if        pdcnote <> *blank
050500100916     c                   eval      pdcnote= %trim(pdcnote) +
050600100916     c                             ' DDT sul collo'
050700100916     c                   else
050800100916     c                   eval      pdcnote= 'DDT sul collo'
050900100916     c                   endif
051000080403     C                   endif
051100080403     C                   endif
051200100317     C                   endif
051300141110      *tipo rekord Y per Pin_Code
051400141110     c                   move      'Y'           trek4
051500141110     c                   clear                   dsbl4y
051600141110     c     kar4          chain     fiar401l
051700141110     c                   if        %found(fiar401l)
051800141110     c                   movel     AR4NOT        dsbl4y
051900141110     c                   eval      PDCpin    =   §B4PCD
052000141110     C                   endif
052100080422      * Flag documento das
052200080422      * LDV con mittente "PraOil oleodotti italiani"
052300100317w   1c                   if        climitt= CliINT or
052400100318     c                             (arbfbr = 'S' and  §B4Aks = CliINT)
052500080422     c                   eval      flgdas = 'S'
052600080422     c                   endif
052700130321      * Flag packing list
052800140714     c*                  if        climitt = CliFed
052900140714     c*                  eval      flgpkl = 'S'
053000140714     c*                  eval      compkl = 'S'
053100140714     C*                  endif
053200100316      * ar5 trek GEN referente, telefono, intestatario assegno
053300080407     c                   move      'GEN'         trek5
053400100316     c                   clear                   dar5gen
053500080303     c     kar5          chain     fiar501l
053600080303     c                   if        %found(fiar501l)
053700080303     c                   movel     ar5uni        dar5gen
053800080303     c                   eval      PDCTELEF  =   §AR5TELD
053900080303     c                   eval      PDCNOMIN  =   §AR5REF
054000130320      * Flag packing list
054100130321     c                   if        §AR5als ='S'
054200130320     c                             or §AR5alx  ='S'
054300130320     c                   eval      flgpkl = 'S'
054400130320     c                   eval      compkl = 'S'
054500130320     C                   endif
054600080227     c                   endif
054700080303      * ar5 trek BNB colli originali prima di imbancalare
054800080417     c                   CLEAR                   dar5bnb
054900080417     C                   IF        %subst(arbgva: 1: 1) = 'O'
055000080303     c                   move      'BNB'         trek5
055100080303     c     kar5          chain     fiar501l
055200080303     c                   if        %found(fiar501l)
055300080303     c                   movel     ar5uni        dar5bnb
055400080303     c                   eval      PDCnclori =   §AR5bcor
055500080303     c                   endif
055600080417     c                   endif
055700080303      * ar5 trek RCC ritiro contestuale
055800080303     c                   move      'RCC'         trek5
055900100316     c                   clear                   dar5rcc
056000080303     c     kar5          chain     fiar501l
056100080303     c                   if        %found(fiar501l)
056200080303     c                   movel     ar5uni        dar5Rcc
056300080303     c                   eval      PDCpoe    =   §AR5RPOE
056400080303     c                   eval      PDCnsr    =   §AR5RNSR
056500080303     c                   eval      PDCnor    =   §AR5RNOR
056600080303     c                   eval      PDCnrv    =   §AR5RNRV
056700080303     c                   endif
056800140120      * ar5 trek EMD campo ALERT
056900140120     c                   move      'EMD'         trek5
057000140120     c                   clear                   dar5EMD
057100140120     c     kar5          chain     fiar501l
057200140120     c                   if        %found(fiar501l)
057300140121     c                   movel     ar5uni        dar5emd
057400140120     c                   if        §AR5MMP = 'S' or
057500140120     c                             §AR5SMP = 'S'
057600140120     c                   eval      PDCalert  =   '01'
057700140120     c                   endif
057800140120     c                   endif
057900100914      * ar5 trek SCR modifica appuntamento supermaercato note aut
058000100914     c* ????????????     move      'SCR'         trek5
058100140120      * reperimento ora standard
058200140314     c                   clear                   TNSD99ds
058300140314     c                   eval      D98tbo = 'A'
058400140314     c                   eval      D98aas = arbaas
058500140314     c                   eval      D98lnp = arblnp
058600140314     c                   eval      D98nrs = arbnrs
058700140314     c                   eval      D98nsp = arbnsp
058800140314     c                   call      'TNSD99R'
058900140314     c                   parm                    tnsd99ds
059000140314      *
059100140314     C                   clear                   trulorsds
059200140314     C                   clear                   trulor2ds
059300140317     c                   eval      kpjbus = kpjbu
059400140314     c                   eval      IORETLA   =   ' '
059500140314     c                   eval      IOREDTA   =   arbddc
059600140314     c                   eval      IOREFIL   =   arblna
059700140314     c                   eval      IORECAP   =   arbcad
059800140314     c                   eval      IORELOC   =   arblod
059900140314     c                   eval      IORENAR   =   arbnzd
060000140314     c                   eval      IORETSEr  =   'C'
060100140314     c                   eval      IORECONs  =   'S'
060200140314     c                   eval      IOREDDC   =   arbddc
060300140314     c                   eval      IORENDC   =   arbndc
060400140314     c                   eval      IORETFP   =   arbtfp
060500140314     c                   eval      IORETFA   =   arbtfa
060600140314     c                   eval      IOREDTI   =   arbdti
060700140314     c                   eval      IOREHTI   =   arbhti
060800140314     c                   eval      IOREDCR   =   arbdcr
060900140314     c                   eval      IOREHCR   =   arbhcr
061000140314     c                   eval      IORETCR   =   arbtcr
061100140314     c                   eval      IOREDEI   =   d98dei
061200140314     c                   eval      IOREOEI   =   d98oei
061300140314     c                   eval      IORETSP   =   arbtsp
061400150804     c                   eval      §IORETRAZC = %editc(D98TTC:'X')
061500150804     c                   eval      §IORECONSC = %editc(D98TCC:'X')
061600150804     c                   eval      IOREFLO  = diore01
061700140314     c                   call      'TRULORSR'
061800140317     C                   PARM                    kpjba
061900140314     C                   PARM                    trulorsds
062000140314     C                   PARM                    trulor2ds
062100140411      *? attenzione a modificare il riempimento della variabile pdcorastd si sposiziona la paginaPDA
062200140314     c                   if        ooreerr <> '1' and oor2stis > 0
062300140411     c                   eval      pdcorastd = 'Di norma -
062400140314     c                             dalle ' + %editw(oor2stis:weditora)
062500140411     c                             + ' alle ' + %editw(oor2stfs:weditora)
062600140120     c                   endif
062700140411     c                   eval      kpjbu = kpjbus
062800140411      * imposta ora richiesta e giorni di chiusura
062900140416     c                   clear                   orastd28
063000140411     c                   if        pdcgc1 <> *blank or  pdcgc2 <> *blank
063100140411     c                   eval      orastd13 = 'GG ch. '+ pdcgc1 + ' ' + pdcgc2
063200140411     c                   endif
063300140411     c                   if        pdchcr > 0
063400140416     c                   eval      orastd15 = ' Ora ric. ' +
063500140411     c                             %editw(pdchcr:weditora)
063600140411     c                   endif
063700140416     c                   move      orastd28      pdcorastd
063800140411      *?
063900140120     c
064000101119      *obbligo firmatario sempre
064100101119     c                   eval      pdcrnomfir = 'S'
064200080404      *  particolarità consegna
064300080304     c                   clear                   ds7v
064400100517     c                   clear                   ds7r
064500080304     C                   movel     '7V'          tblcod
064600080304     C                   eval      tblkey = 'R' + arbgma + 'S' + '0001'
064700080304     C     ktab          chain     TABEL00f
064800080304     c                   if        %found(tabel00f)
064900080304     C                   movel     TBLUNI        DS7v
065000080304     c                   if        §7vpda <> 'N'
065100080404     c                   eval      PDCGMA    =   arbGMA
065200120427     c                   endif
065300120427     c                   endif
065400101118     c*
065500080521     C                   movel     '7R'          tblcod
065600080521     C                   eval      tblkey =  arbgma
065700080521     C     ktab          chain     TABEL00f
065800080521     c                   if        %found(tabel00f)
065900080521     C                   movel     TBLUNI        DS7r
066000101118     c*                  if        §7r2fi = 'S'
066100101118     c*                  eval      pdcrnomfir = 'S'
066200101118     c*                  endif
066300120427     c                   eval      pdcldvfir = §7rldvfir
066400080304     c                   endif
066500080521      *  particolarità giacenza                  11
066600080404     c                   clear                   ds7v
066700080404     C                   movel     '7V'          tblcod
066800100317     C                   eval      tblkey = 'Q' + arbgga + 'S' + '0001'
066900080404     C     ktab          chain     TABEL00f
067000080404     c                   if        %found(tabel00f)
067100080404     C                   movel     TBLUNI        DS7v
067200080404     c                   if        §7vpda <> 'N'
067300080404     c                   eval      PDCGGA    =   arbGGA
067400080404     c                   endif
067500080404     c                   endif
067600080404      *  particolarità varie
067700080404     c                   clear                   ds7v
067800080404     C                   movel     '7V'          tblcod
067900100317     C                   eval      tblkey = 'S' + arbgva + 'S' + '0001'
068000080404     C     ktab          chain     TABEL00f
068100080404     c                   if        %found(tabel00f)
068200080404     C                   movel     TBLUNI        DS7v
068300100913     c                   if        §7vpda <> 'N'
068400080404     c                   eval      PDCGVA    =   arbGVA
068500080404     c                   endif
068600080404     c                   endif
068700080403      * reperisce C/A se non lo ha già impostato dalla bolla mamma se presente
068800100914     c                   if        $mamma <> 'S'
068900080227     c     karb          chain     fiar901l
069000100914     c                   else
069100100914     c     karbm         chain     fiar901l
069200100914     c                   end
069300080227     c                   if        %found(fiar901l)
069400080403     c                             and (arbicc=' ' or arbicc = 'R')
069500080227     c                   eval      PDCTIC    =   ar9TIC
069600080227     c                   eval      PDCCAS    =   ar9CAS
069700080227     c                   eval      PDCVCA    =   ar9VCA
069800080304     c                   eval      PDCGCA    =   ar9GCA
069900100902     c* mi serve poi per non richiedere la firma
070000100902     c                   if        arbicc = 'R'
070100100902     c                   eval      ritinc = 'S'
070200080403     c                   endif
070300100914     c* se incassi imposto la ragione sociale intestatario assegno.
070400100914     c                   reset                   trulintds
070500100914     c                   eval      iINTtic  = pdctic
070600100914     c                   eval      iINTrscM = pdcrsm
070700100914     c                   eval      iINTrscO = pdcrmo
070800100914     c                   eval      iINTcdi  = §AR5cdi
070900100914     c                   call      'TRULINTR'
071000100914     c                   parm                    TRULINTDS
071100100914     c                   if        oINTerr = *blank
071200100914     c                   eval      pdcragcdi = oINTrsci
071300100914     c                   end
071400100914     c* flag fotocopia assegno circolare
071500100914     c* vanno bene i dati della figlia perchè nel pgm gli altri non li usa
071600100914     c                   reset                   TRUL21DS
071700130902     c                   if        %subst(§3atb1:1:1)='F'
071800130902     c                   z-add     arbksc        i21ksc
071900130902     c                   else
072000130902     c                   z-add     arbccm        i21ksc
072100130902     c                   if        i21ksc=0
072200130902     c                   z-add     8888          i21ksc
072300130902     c                   movel     arblnp        i21ksc
072400130902     c                   endif
072500130902     c                   endif
072600100914     c                   eval      I21cbo =  arbcbo
072700130902     c                   eval      I21tsp =  arbtsp
072800100914     c                   eval      I21lnp =  arblnp
072900100914     c                   eval      I21nzm =  arbnzm
073000100914     c                   eval      I21lna =  arblna
073100100914     c                   eval      I21nzd =  arbnzd
073200100914     c                   eval      I21tic =  pdctic
073300100914     c                   eval      I21imp =  pdccas
073400100914     c                   eval      I21div =  pdcvca
073500100914     c                   eval      I21ute =  KNMUS
073600100914     c                   eval      I21pgm =  SDSpgm
073700100914     c                   call      'TRUL21R'
073800100914     c                   parm                    TRUL21ds
073900100914     c                   if        O21fca <> *blanks
074000080403     c                             and (arbicc=' ' or arbicc = 'R')
074100080403     c                   move      'S'           flgfas
074200100914     c                   endif
074300080403     c                   endif
074400100914     c* prima fattura
074500100914     c                   if        %subst(§3Atb1: 1: 1) ='A'
074600100914     c                   eval      trek6= '1'
074700100914     c                   exsr      srar6
074800100914     c                   end
074900100914     c* seconda fattura
075000111013     c                   if        %subst(§3Atb2: 1: 1) ='A'
075100100914     c                   eval      trek6= '2'
075200100914     c                   exsr      srar6
075300100914     c                   end
075400120502     c* se non ho la partita iva chiedo il codice fiscale
075500120502     c                   if        arbcpi = ' '
075600120427     c* prima bolla
075700080421      * codice fiscale verifico se presente solo in caso di assegnato
075800080417     c                   clear                   dta4q
075900080417     c                   if        %subst(§3Atb1: 1: 1) ='A'
076000080421     c                             and ar6dft <> 0
076100080404     c                   move      'Q'           trek4
076200080404     c     kar4          chain     fiar401l
076300080404     c                   if        %found(fiar401l)
076400080404     c                   eval      dta4q     =   AR4NOT
076500080417     C                   endif
076600080404      * se non presente richiedo Cod. Fiscale
076700120502     c                   if        %subst(§TA4QCFD: 12: 1) = ' '
076800120502     c                             or not%found(fiar401l)
076900120427     c                   eval      PDCSICFIS  =  'S'
077000080404     C                   endif
077100080404     C                   endif
077200120427     c* seconda bolla
077300080521      * codice fiscale verifico se presente solo in caso di assegnato
077400080417     c                   clear                   dta4q
077500080417     c                   if        %subst(§3Atb2: 1: 1) ='A'
077600080421     c                             and ar6dft <> 0
077700080417     c                   move      'Q'           trek4
077800080417     c     kar4          chain     fiar401l
077900080417     c                   if        %found(fiar401l)
078000080417     c                   eval      dta4q     =   AR4NOT
078100080417     C                   endif
078200080417      * se non presente richiedo Cod. Fiscale
078300120502     c                   if        %subst(§TA4QCFD: 12: 1) = ' '
078400120502     c                             or not%found(fiar401l)
078500120427     c                   eval      PDCSICFIS  =  'S'
078600080417     C                   endif
078700080417     C                   endif
078800120502     C                   endif
078900120427      * se assegnato verifico se non presente il codice fiscale oppure,
079000120427      * se presente deve essere > 11 altrimenti richiedo la partita iva
079100120427     c                   if        arbcpi = *blank and %subst(§3Atb1: 1: 1) ='A'
079200120427     c                             and ar6dft <> 0 and
079300120427     c                             %subst(§TA4QCFD: 12: 1) = ' '
079400120427     c                   eval      PDCSIPIVA  =  'S'
079500120427     c                   endif
079600120427     c                   if        arbcpi = *blank and %subst(§3Atb2: 1: 1) ='A'
079700120427     c                             and ar6dft <> 0 and
079800120427     c                             %subst(§TA4QCFD: 12: 1) = ' '
079900120427     c                   eval      PDCSIPIVA  =  'S'
080000120427     c                   endif
080100080404      *colli bolla
080200080303     c                   eval      PDCNCL     =  arbNCL
080300080320     c* richiamo routine per dati consegnabili colli/volumi/pesi
080400080320     c                   clear                   pesparz
080500080320     c                   clear                   volparz
080600080320      * nessun collo consegnabile azzero anche volumi e pesi per la bolla
080700080320     c                   if        wcolli = 0
080800080320     c                   clear                   pescons
080900080320     c                   clear                   volcons
081000080320     c                   else
081100080320      * prendo pesi e volumi e li imposto come totali
081200080320     c                   exsr      calpes
081300080320     c                   exsr      calvol
081400080320     c                   z-add     wpes          pescons
081500080320     c                   z-add     wvol          volcons
081600080320      * calcolo pesi e volumi reali se colli consegnabili diversi da totale colli
081700080320     c                   if        arbncl > wcolli
081800080320     c     wpes          div       arbncl        pesparz
081900080320     c     wvol          div       arbncl        volparz
082000080507     c     pesparz       mult(h)   wcolli        pescons
082100080507     c     volparz       mult(h)   wcolli        volcons
082200080320     c                   endif
082300080320     c                   endif
082400080303
082500080320     c                   eval      PDCNCLDAC  =  wcolli
082600080320     c                   eval      PDCPKCDAC  =  pescons
082700080320     c                   eval      PDCVLCDAC  =  volcons
082800080303
082900080417     c     arbtsp        lookup    tsa                                    65
083000080417     c   65              eval      PDCSERVIZ  =  arbtsp
083100080304      *verifica consegna tassativa
083200080304     c                   clear                   fnlv80ds
083300080304     c                   z-add     arbAAS        ILV80AAS
083400080304     c                   z-add     arbLNP        ILV80LNP
083500080304     c                   z-add     arbNRS        ILV80NRS
083600080304     c                   z-add     arbNSP        ILV80NSP
083700080304     c                   z-add     arbMGS        ILV80MGS
083800080304     c                   move      arbTCR        ILV80TCR
083900080304     c                   z-add     arbDCR        ILV80DCR
084000080304     c                   z-add     arbHCR        ILV80HCR
084100080304     c                   move      arbTC1        ILV80TC1
084200080304     c                   move      arbTC2        ILV80TC2
084300080304     c                   call      'FNLV80R'
084400080304     c                   parm                    kpjba
084500080304     c                   parm                    fnlv80ds
084600080304     c                   eval      PDCTASSAT  =  olv80tat
084700100317      * reperisce note LDV
084800080305     c                   clear                   noteldv
084900080304     c     karn          setll     fiarn11l
085000080304     c                   do        *hival
085100080304     c     karn          reade     fiarn11l
085200080304     c                   if        %eof(fiarn11l)
085300080304     c                   leave
085400080304     c                   endif
085500080304     c                   select
085600080304     c                   when      noteldv1 = *blank
085700080304     c                   movel     arnnob        noteldv1
085800080304     c                   when      noteldv2 = *blank
085900080304     c                   movel     arnnob        noteldv2
086000080304     c                   when      noteldv3 = *blank
086100080304     c                   movel     arnnob        noteldv3
086200080304     c                   when      noteldv4 = *blank
086300080304     c                   movel     arnnob        noteldv4
086400080304     c                   endsl
086500080304     C                   enddo
086600080304     c                   eval      PDCNOTAUT  =  NOTeldv
086700140218      * imposta icona per consegna documenti
086800140218     c                   z-add     1             x
086900140218     c     comflg        lookup    docflg(x)                              66
087000140218     c   66              move      doccod(x)     pdcdocum
087100080418      * promemoria autista
087200080418     c                   exsr      noteAvviso
087300100907      * gestione maschera firma, note firmatario
087400100907     c                   exsr      srfirma
087500100506      * spedizione da trattare singolarmente incasso da effettuare
087600120521      * ritiro contestuale, parziale, bancali da rendere, note cliente
087700120521      * ritorni all'incasso, particolarità consegna
087800100506     c                   if        pdccas > 0 or pdcift > 0 or
087900150224     c                             arbdcp > 0 or
088000100514     c                             §7RRC = 'S' or pdcnor > 0 or
088100140218     c                             pdcnotpxa <> ' ' or pdcflgfir = 'N' or
088200140221     c                             pdcdocum <> ' ' or  pdcpin <> ' ' or
088300140218     c                             pdcldvfir = 'S' or pdcgma <> ' '
088400100506     c                   eval      PDCSPESING =  'S'
088500100506     c                   endif
088600080402      *
088700080403      * icone da impostare
088800080403     c                   eval      PDCFLGINFO =  *blank
088900080403     c                   eval      PDCFLGSX   =  *blank
089000080403     c                   exsr      icone
089100100826     c*
089200100826     c                   eval      PDcCODAUT =   arbpdc
089300080403      *
089400080326     c     karb          chain     fipdc02t
089500080326     c                   if        %found(fipdc02t)
089600100826     c                   eval      PDCIDROW =   xPDCIDROW + 1
089700101119     c                   eval      PDCordina=   xPDCordina
089800080326     c                   else
089900100826     c                   eval      PDCIDROW =    PDCIDROW + 1
090000080326     c                   endif
090100080304     c                   eval      tmp= %char(%timestamp: *iso0)
090200100826     c                   eval      PDcDATORA =   tmp
090300100826      * confronto le immagini da rekord esistente e attuale se non varia non aggiorno
090400080904     c                   if        %found(fipdc03t)
090500080430     c                   movel     fipdcds       fipdcdsA
090600100827     c* forzo i campi idrow e dataora uguali (perchè questi sicuramente
090700100826     c* sono variati) per poter confrontare le due ds
090800110505     c* forzo anche i campi del peso e del volume perchè non voglio
090900110505     c* tracciarne le variazioni
091000100826     c                   eval      aPDCIDROW =    bPDCIDROW
091100100826     c                   eval      aPDCdatora=    bPDCdatora
091200110505     c                   eval      aPDCPKCDAC =   bPDCPKCDAC
091300110505     c                   eval      aPDCVLCDAC =   bPDCVLCDAC
091400130221     c                   eval      aPDCtdc   =    bPDCtdc
091500130221     c                   eval      aPDCRSM   =    bPDCRSM
091600130221     c                   eval      aPDCLOM   =    bPDCLOM
091700130221     c                   eval      aPDCPRM   =    bPDCPRM
091800130221     c                   eval      aPDCNZM   =    bPDCNZM
091900130221     c                   eval      aPDCRMN   =    bPDCRMN
092000130221     c                   eval      aPDCRMA   =    bPDCRMA
092100130221     c                   eval      aPDCRMO   =    bPDCRMO
092200130221     c                   eval      aPDCGC1   =    bPDCGC1
092300130221     c                   eval      aPDCGC2   =    bPDCGC2
092400130221     c                   eval      aPDCTCR   =    bPDCTCR
092500130221     c                   eval      aPDCDCR   =    bPDCDCR
092600130221     c                   eval      aPDCHCR   =    bPDCHCR
092700130221     c                   eval      aPDCTC1   =    bPDCTC1
092800130221     c                   eval      aPDCTC2   =    bPDCTC2
092900130221     c                   eval      aPDCNO1   =    bPDCNO1
093000130221     c                   eval      aPDCNO2   =    bPDCNO2
093100130221     c                   eval      aPDCTELEF =    bPDCTELEF
093200130221     c                   eval      aPDCNOMIN =    bPDCNOMIN
093300130221     c                   eval      aPDCGGA   =    bPDCGGA
093400130221     c                   eval      aPDCGMA   =    bPDCGMA
093500130221     c                   eval      aPDCGVA   =    bPDCGVA
093600130221     c                   eval      aPDCNCL     =  bPDCNCL
093700130221     c                   eval      aPDCNCLORI  =  bPDCNCLORI
093800130221     c                   eval      aPDCNCLDAC  =  bPDCNCLDAC
093900130221     c                   eval      aPDCSPESING =  bPDCSPESING
094000130221     c                   eval      aPDCSINOAVV =  bPDCSINOAVV
094100130221     c                   eval      aPDCSERVIZ  =  bPDCSERVIZ
094200130221     c                   eval      aPDCTASSAT  =  bPDCTASSAT
094300130221     c                   eval      aPDCINFOSUB =  bPDCINFOSUB
094400130221     c                   eval      aPDCRNOMFIR =  bPDCRNOMFIR
094500130221     c                   eval      aPDCNOTE    =  bPDCNOTE
094600130221     c                   eval      aPDCpoe     =  bPDCPoe
094700130221     c                   eval      aPDCnsr     =  bPDCnsr
094800130221     c                   eval      aPDCNOR     =  bPDCNOR
094900130221     c                   eval      aPDCNRV     =  bPDCNRV
095000130221     c                   eval      aPDCRAGCDI  =  bPDCRAGCDI
095100130221     c                   eval      aPDCNOTPXA  =  bPDCNOTPXA
095200130221     c                   eval      aPDCNOTFIR  =  bPDCNOTFIR
095300130221     c                   eval      aPDCFLGFIR  =  bPDCFLGFIR
095400130221     c                   eval      aPDCLDVFIR  =  bPDCLDVFIR
095500130221     c                   eval      aPDCGIRO    =  bPDCGIRO
095600130221     c                   eval      aPDCORDINA  =  bPDCORDINA
095700130221     c                   eval      aPDCZNC     =  bPDCZNC
095800130221     c                   if        fipdcdsB <>
095900100826     c                             fipdcdsA
096000080904     c                   update    fipdc3
096001170728     c                   feod      fipdc03t
096002170728     c* ora stimata
096003170728     c                   exsr      orastima
096004170915     c                   if        pdcnor > 0
096005170915     c                   clear                   fior56ds
096006170915     c                   move      'R'           OR56TLA
096007170915     c                   z-add     pdcfgs        OR56FGS
096008170915     c                   z-add     pdcpoe        OR56POE
096009170915     c                   z-add     pdcnsr        OR56NSR
096010170915     c                   z-add     pdcnor        OR56NOR
096011170915     c                   z-add     pdcnrv        OR56NRV
096012170915     c                   z-add     pdcndc        OR56NDCD
096013170915     c                   call      'FIOR56R'
096014170915     c                   parm                    kpjba
096015170915     c                   parm                    fior56ds
096016170915     c                   endif
096017170915      *
096100130906     c                   else
096101170728     c* non è cambiato nulla di importante
096200130906     c                   unlock    fipdc03t
096300080430     c                   endif
096400080227     c                   else
096500080904     c                   write     fipdc3
096502170728     c                   feod      fipdc03t
096503170728     c* ora stimata
096504170728     c                   exsr      orastima
096602170728     c* chiamata inserimento ORM su FIPDO solo per WRITE
096700080707     c                   if        pdcnor > 0
096800080707     c                   clear                   fior56ds
096900080708     c                   move      'R'           OR56TLA
097000080707     c                   z-add     pdcfgs        OR56FGS
097100080707     c                   z-add     pdcpoe        OR56POE
097200080707     c                   z-add     pdcnsr        OR56NSR
097300080707     c                   z-add     pdcnor        OR56NOR
097400080707     c                   z-add     pdcnrv        OR56NRV
097500080707     c                   z-add     pdcndc        OR56NDCD
097600080707     c                   call      'FIOR56R'
097700080707     c                   parm                    kpjba
097800080707     c                   parm                    fior56ds
097900080707     c                   endif
098000080227     c                   endif
098100080304      *
098200080304     c                   endif
098300080227     c                   endsr
098400080227     c* ?_____________________________________________________
098500170728     c     orastima      begsr
098600080227     c* ?_____________________________________________________
098601170728     c     kpdc6         setll     fipdc61t
098602170728     c                   if        not %equal(fipdc61t)
098603170728     c                   eval      pdc=pdccodaut
098604170728     c     kapd6         chain     fiapd61l
098605170728     c                   if        not %found(fiapd61l)
098608170728     c                   eval      pdc=0
098609170728     c     kapd6         chain     fiapd61l
098611170728     c                   end
098612170728     c                   if        %found(fiapd61l)
098613170728     c                   eval      pdc6pdr =pdccodaut
098614170728     c                   eval      pdc6tsp =APD6TSP
098615170728     c                   eval      pdc6fgsg=APD6FGSG
098616170728     c                   eval      pdc6cgi =APD6CGI
098619170728     c                   eval      pdc6ora =APD6ORA
098620170728     c                   write     fipdc600
098621170728     c                   feod      fipdc61t
098622170728     c                   end
098623170728     c                   end
098624170728     c                   endsr
098625170728     c* ?_____________________________________________________
098626170728     c     elab_dett     begsr
098627170728     c* ?_____________________________________________________
098700080304     c                   clear                   fipdd000
098800080320     c                   clear                   wcolli
098900080422     c                   clear                   wcollip
099000080422     c                   clear                   wdatap
099100100914     c                   if        $mamma <> 'S'
099200080325     c     kart          setll     fnart01l
099300100914     c                   else
099400100914     c     karbm         setll     fnart01l
099500100914     c                   end
099600080227     c                   do        *hival
099700100914     c                   if        $mamma <> 'S'
099800080325     c     kart          reade     fnart01l
099900100914     c                   else
100000100914     c     karbm         reade     fnart01l
100100100914     c                   end
100200080227     c                   if        %eof(fnart01l)
100300080227     c                   leave
100400080227     c                   endif
100500080227     c                   if        artdcm > 0
100600080422     c                   add       1             wcollip
100700080422     c                   z-add     artdcm        wdatap
100800080227     c                   iter
100900080227     c                   endif
101000080320     c                   add       1             wcolli
101100080227     c     kfipdd        chain     fipdd01t
101200080430     c                   movel     fipddds       fipdddsB
101300080307     c                   eval      PDdatb    =   *blank
101400080321     c                   eval      PDDFGS    =  xarbifp
101500080321     c                   eval      PDDNDC    =  xarbndc
101600080321     c                   eval      PDDAAS    =  xarbaas
101700080321     c                   eval      PDDLNP    =  xarblnp
101800080321     c                   eval      PDDNRS    =  xarbnrs
101900080321     c                   eval      PDDNSP    =  xarbnsp
102000080227     c                   eval      PDDFLS    =   artfls
102100080227     c                   eval      PDDLNA    =   artlna
102200080508     c                   eval      PDDNRSER  =   artnrs
102300080227     c                   eval      PDDNSC    =   artnsc
102400101118     c                   eval      PDDCAN    =   artcan
102500101118     c                   if        artcan <> ' '
102600101118     c     artcan        lookup    $3ecan                                 45
102700101118     c                   if        not *in45
102800101118     c                   clear                   pddcan
102900101118     c                   end
103000101118     c                   end
103100080414      * per tipo lancio totale imposto il dettaglio come spuntato
103200080411     c                   if        co42tla = 'T'
103300080411     c                   eval      PDDSPUMAG =  'S'
103400080505     c                   else
103500080505      * se non è lancio globale verifico se esiste la spunta
103600080505     c     kbrv          chain     fnbrv01l
103700080505     c                   if        %found(fnbrv01l)
103800080505     c                   eval      PDDSPUMAG =  'S'
103900080509     c                   else
104000080509     c                   clear                   pddspumag
104100080505     c                   endif
104200080411     c                   endif
104300080321     c                   eval      PDDCODAUT =  xarbpdc
104400080304     c                   eval      tmp= %char(%timestamp: *iso0)
104500080304     c                   eval      PDDDATORA =   tmp
104600080227     c
104700080506     c                   if        %found(fipdd01t)
104800080430      * carico l'immagine attuale del rekord
104900080430     c                   movel     fipddds       fipdddsA
105000130906     c                   movel     fipddds       fipdddsav
105100100827     c* forzo il campo dataora uguale (perchè questo sicuramente
105200100827     c* è variati) per poter confrontare le due ds
105300100827     c                   eval      aPDDdatora=    bPDDdatora
105400080430      * confronto le immagini da rekord esistente e attuale se non varia non aggiorno
105500100827     c                   if        fipdddsB <>
105600100827     c                             fipdddsA
105700130906     c     pdd01nrr      chain     fipdd00t
105800130906     c                   movel     fipdddsav     fipddds
105900130724     c                   update    fipdd
106000080430     c                   endif
106100080227     c                   else
106200130724     c                   write     fipdd
106300080227     c                   endif
106301170728     c                   feod      fipdd00t
106400080227     c                   enddo
106500080227     c                   endsr
106600080320     c* ?_____________________________________________________
106700080320     c     calpes        begsr
106800080320      * ?_____________________________________________________
106900080320     C     arbncp        ifeq      arbncl
107000080321     C                   z-add     arbPKC        wpes
107100080321     C                   else
107200080320     C     arbPKC        ifge      arbPKF
107300080321     C                   z-add     arbPKC        wpes
107400080320     C                   else
107500080321     C                   z-add     arbPKF        wpes
107600080320     C                   end
107700080321     C                   endif
107800080320     c                   endsr
107900080320     c* ?_____________________________________________________
108000080320     c     calvol        begsr
108100080320      * ?_____________________________________________________
108200080320     c* t=volume da fatturare
108300080321     C     arbfvf        ifeq      'T'
108400080321     C                   z-add     arbvlf        wvol
108500080321     C                   else
108600080321     C     arbncr        ifeq      arbncl
108700080321     C                   z-add     arbvlc        wvol
108800080321     C                   else
108900080321     C     arbvlc        ifge      arbvlf
109000080321     C                   z-add     arbvlc        wvol
109100080321     C                   else
109200080321     C                   z-add     arbvlf        wvol
109300080321     C                   endif
109400080321     C                   endif
109500080321     C                   endif
109600080320     C*
109700080320     c                   endsr
109800080321     c* ?_____________________________________________________
109900100914     C     srar6         begsr
110000080321     c* ?_____________________________________________________
110100100914if  1c                   IF        §TBfcb =  '1'
110200100914     c     kar6          chain     fiar601l
110300100914     c                   if        %found(fiar601l)
110400100914     c                             and (arbica=' ' or arbica = 'R')
110500100914     c                             and ar6dft <> 0
110600100914     c                   eval      PDCIFT    =   ar6IFT
110700100914     c                   eval      PDCDIV    =   ar6DIV
110800100914     c                   move      'S'           flgfat
110900100914     c* mi serve poi per non richiedere la firma
111000100914     c                   if        arbica = 'R'
111100100914     c                   eval      ritinc = 'S'
111200100914     c                   endif
111300100914     c                   endif
111400100914     c                   endif
111500100914     C*
111600100914     c                   endsr
111700100914     c* ?_____________________________________________________
111800100914     C     srmamma       begsr
111900100914     c* ?_____________________________________________________
112000080321     C*
112100080321     C     karb          chain     fnlbl01l
112200080321     C                   IF        %found(fnlbl01l)
112300080321     C     klbl          chain     fnarb01l
112400100914     C                   IF        %found(fnarb01l)
112500100914     c                   eval      arbmamds = fnarbds
112600100914     c                   end
112700100914     C* RImposto i dati della FIGLIA
112800080321     C                   movel     fnarbdsx      fnarbds
112900100914     c                   end
113000080321     C*
113100080321     C                   endsr
113200080403     c* ?_____________________________________________________
113300080403     c     icone         begsr
113400080403     c* ?_____________________________________________________
113500101206     c* ?               ATTENZIONE
113600101206     c* ?ORDINAMENTO delle specifiche
113700101206     c* ?che l'icona sia presente nelle versioni utilizzate
113800101206     c* ?dai palmari
113900101118     c* DX TIPO SERVIZIO E ROSSA
114000100429     c                   if        pdcserviz = 'E'
114100080403     c                   move      '01'          pdcflginfo
114200120528     c                   eval      pdcinfosub = 'S'
114300080403     c                   endif
114400101118     c* DX TIPO SERVIZIO H ROSSA
114500100409     c                   if        pdcserviz = 'H'
114600100409     c                   move      '06'          pdcflginfo
114700120528     c                   eval      pdcinfosub = 'S'
114800100409     c                   endif
114900101118     c* DX TASSATIVA T ROSSA
115000080404     c                   if        pdctassat = 'S' and pdcflginfo = *blank
115100080404     c                   move      '02'          pdcflginfo
115200080404     c                   endif
115300101118     c* DX TURNO mattina M GIALLA
115400080404     c                   if        arbgc1 <> *blanks and pdcflginfo = *blank
115500080404     c                   if        %subst(arbgc1: 1: 1) = WGG
115600080404     c                   if        %subst(arbgc1: 2: 1) = 'M'
115700080403     c                   eval      pdcflginfo = '03'
115800080404     c                   endif
115900101118     c* DX TURNO pomeriggio P GIALLA
116000080404     c                   if        %subst(arbgc1: 2: 1) = 'P'
116100080404     c                   eval      pdcflginfo = '04'
116200080404     c                   endif
116300080403     c                   endif
116400080404     c                   endif
116500101118     c* DX TURNO mattina M GIALLA
116600080404     c                   if        arbgc2 <> *blanks and pdcflginfo = *blank
116700080404     c                   IF        %subst(arbgc2: 1: 1) = WGG
116800080403     c                   if        %subst(arbgc2: 2: 1) = 'M'
116900080404     c                   eval      pdcflginfo = '03'
117000080404     c                   endif
117100101118     c* DX TURNO pomeriggio P GIALLA
117200080404     c                   if        %subst(arbgc2: 2: 1) = 'P'
117300080404     c                   eval      pdcflginfo = '04'
117400080404     c                   endif
117500080404     c                   endif
117600080404     c                   endif
117700101118     c* SX DOCUMENTO ICONA DOCUMENTO
117800080403     c                   if        pdcdocum <> *blank
117900080403     c                   move      '05'          pdcflgsx
118000080403     c                   endif
118100100316     c*                  if        pdcnote <> *blank and pdcflginfo = *blank
118200101118     c* DX INCASSI "I" ROSSA
118300101206     c                   if        pdcflginfo = *blank and
118400101206     c                             (pdccas > 0 or pdcift > 0)
118500101206     c                   move      '07'          pdcflginfo
118600101206     c                   endif
118700101118     c* DX NOTE I BLU
118800101118     c                   if        pdcflginfo = *blank and
118900100316     c                             (pdcnote <> ' ' or pdcnotpxa <> ' ' or
119000140218     c                             pdcnotaut <> ' '   or
119100100316     c                             pdcno1 <> ' ' or pdcno2 <> ' ' )
119200080403     c                   move      '00'          pdcflginfo
119300080403     c                   endif
119400080403     C                   endsr
119500080418     c* ?_____________________________________________________
119600080418     c     noteAvviso    begsr
119700131205      * si può lasciare lasciato avviso
119800131205     c*                  if        arbntc = 0
119900131205     c                   eval      PDCSINOAVV =  'S'
120000131205     c*                  else
120100131205     c*                  eval      PDCSINOAVV =  'N'
120200131205     c*                  endif
120300080418      * ?_____________________________________________________
120400080421      * COMUNICAZIONE ANOMALIA
120500080421      * Richiamo PGM per recuperare le CA legate alla spedizione in esame
120600080418     c                   clear                   fidn12ds
120700100914     c                   if        $mamma <> 'S'
120800080418     c                   eval      i12aas = arbaas
120900080418     c                   eval      i12lnp = arblnp
121000080418     c                   eval      i12nrs = arbnrs
121100080418     c                   eval      i12nsp = arbnsp
121200100914     c                   else
121300100914     c                   eval      i12aas = marbaas
121400100914     c                   eval      i12lnp = marblnp
121500100914     c                   eval      i12nrs = marbnrs
121600100914     c                   eval      i12nsp = marbnsp
121700100914     c                   end
121800080418     c                   eval      i12fel = 'E'
121900080418     c                   eval      i12fan = 'E'
122000080418     c                   eval      i12fch = 'E'
122100080418     c                   call      'FIDN12R'
122200080418     c                   parm                    fidn12ds
122300080418      * se c'è almeno una C.A. aperta
122400080418     c     o12nca        ifgt      0
122500100915     c                   if        pdcnote <> *blank
122600100915     c                   eval      pdcnote= %trim(pdcnote) +
122700100915     c                             ' Presente Comunicazione di Anomalia'
122800100915     c                   else
122900080418     c                   eval      pdcnote= 'Presente Comunicazione di Anomalia'
123000080418     c                   endif
123100100915     c                   endif
123200080421      * LASCIATO AVVISO
123300080418      * verifico se precedente lasciato avviso
123400080418     c     Karb          setgt     fnevb05l
123500080418     c     Karb          readpe    fnevb05l
123600080418      *
123700080418do  1c                   dow       not  %eof(fnevb05l)
123800080418      *
123900080418      * Verifico se evento di tipo Giacenza
124000080418     c                   eval      *in32    = *off
124100080418     c     evbcev        lookup    $2Acev_G                               32
124200080418      **
124300080418sel 2c                   Select
124400080418      * - Evento GIACENZA --> NO LASCIATO AVVISO
124500080418w   2c                   When      *in32
124600080418     c                   leave
124700080418      * - Evento RIC --> 1° LASCIATO AVVISO
124800080418w   2c                   When      evbcev   = 'RIC'
124900080418      * - - reperimento data/ora del 1° lasciato avviso
125000131206     c                   move      evbdev        dataiso
125100080418     c                   move      dataiso       dataeur
125200080418     c                   move      dataeur       dteve             8 0
125300080421     c                   if        pdcnote <> *blank
125400080418     c                   eval      pdcnote= %trim(pdcnote) +
125500080418     c                             ' Lasciato Avviso il ' +
125600080422     c                             %EDITW(dteve : '  -  -    ') + ' alle ' +
125700131206     c                             %EDITW(evbhev : '  :  ')
125800080421     c                   else
125900080421     c                   eval      pdcnote= 'Lasciato Avviso il ' +
126000080422     c                             %EDITW(dteve : '  -  -    ') + ' alle ' +
126100131206     c                             %EDITW(evbhev : '  :  ')
126200080421     c                   endif
126300131205     c                   eval      PDCSINOAVV =  'N'
126400080418     c                   leave
126500080418      * - Evento DA NON ELABORARE
126600080418x   2c                   Other
126700080418     c     Karb          readpe    fnevb05l
126800080418e   2c                   Endsl
126900080418e   1c                   enddo
127000080421      * CONSEGNA GARANTITA
127100080421      *
127200080421     c                   if        arbnrs = 40 and arbtsp = 'E'and
127300120809     c                             (arblnp = 43 or arblnp = 277 or arblnp = 063)
127400080421     c                   if        pdcnote <> *blank
127500080418     c                   eval      pdcnote= %trim(pdcnote) +
127600090520     c                             ' CONSEGNA ESPRESSA WUERTH'
127700080421     c                   else
127800090520     c                   eval      pdcnote= 'CONSEGNA ESPRESSA WUERTH'
127900080421     c                   endif
128000080418     c                   endif
128100101020      * NOTA DPD INDIRIZZO PACCO (eliminato perchè altrimenti non si può
128200101020      * fare multiconsegna
128300101020     c*                  if        §cmntw = 'DPD'
128400101020     c*                  if        pdcnote <> *blank
128500101020     c*                  eval      pdcnote= %trim(pdcnote) +
128600101020     c*                            ' Controllare indirizzo pacco'
128700101020     c*                  else
128800101020     c*                  eval      pdcnote= 'Controllare indirizzo pacco'
128900101020     c*                  endif
129000101020     c*                  endif
129100080422      * CONSEGNA parziale di colli in data
129200080422      *
129300080422     c                   if        wcollip > 0
129400080422     c                   move      wdatap        dataiso
129500080422     c                   move      dataiso       dataeur
129600080422     c                   move      dataeur       wdatapg           8 0
129700080422     c                   if        pdcnote <> *blank
129800080422     c                   eval      pdcnote= %trim(pdcnote) +
129900080422     c                             ' Consegnati colli ' +
130000080422     c                             %EDITc(wcollip : 'Z') + ' in data ' +
130100080422     c                             %EDITW(wdatapg : '  -  -    ')
130200080422     c                   else
130300080422     c                   eval      pdcnote= 'Consegnati colli ' +
130400080422     c                             %EDITc(wcollip : 'Z') + ' in data ' +
130500080422     c                             %EDITW(wdatapg : '  -  -    ')
130600080422     c                   endif
130700080422     c                   endif
130800080428      * Nota attenzione PROCEDURA D.A.S
130900080428      *
131000080428     c                   if        flgdas <> *blank
131100080428     c                   if        pdcnote <> *blank
131200080428     c                   eval      pdcnote= %trim(pdcnote) +
131300080428     c                             ' v. Procedura D.A.S.'
131400080428     c                   else
131500080428     c                   eval      pdcnote= 'v. Procedura D.A.S.'
131600080428     c                   endif
131700080428     c                   endif
131800100325     c* per priority calcolo i tempi di delivery per segnalare se consegna
131900100325     c* al MATTINO no per Wurth
132000100325     c                   if        arbtsp = 'E' and (arbnrs <> 40 or
132100100325     c                             (arblnp <> 043   and
132200120809     c                             arblnp <> 277   and
132300100325     c                             arblnp <> 063))
132400140314      *questo campo è reperito dal tnsd99 che era precedentemente lanciato dentro questa IF
132500140314      * ma ora serve per tutte le bolle ed è stato spostato sopra
132600100325     c                   if        D98tci = 12
132700100325     c                   if        pdcnote <> *blank
132800100325     c                   eval      pdcnote= %trim(pdcnote) +
132900100325     c                             ' CONSEGNA MATTINO '
133000100325     c                   else
133100100325     c                   eval      pdcnote= 'CONSEGNA MATTINO'
133200100325     c                   end
133300100325     c                   end
133400100325     c                   end
133500100325     c* estensione note TABELLA PXA
133600110411     c* particolarità + cliente
133700100325     c                   clear                   pdcnotpxa
133800110408     c                   movel     climitt       climit
133900110408     c                   eval      tbeke2 = 'PDA'
134000100325     c                   eval      tbecod = 'PXA'
134100110408     c                   if        arbgma <> ' '
134200110411     c                   eval      tbeke1 = arbgma
134300110408     c     ktbe2         setll     tntbe01l
134400110408     c                   do        *hival
134500110408     c     ktbe2         reade     tntbe01l
134600110408     c                   if        %eof(tntbe01l)
134700110408     c                   leave
134800110408     c                   end
134900110408     c                   if        tbeatb <> ' '
135000110408     c                   iter
135100110408     c                   end
135200110408     c                   movel     tbeuni        dpxapda
135300110408     c                   eval      pdcnotpxa = §pxades
135400110408     c                   leave
135500110408     c                   enddo
135600110408     c                   end
135700110411     c* aggiungo il cliente e aggiungo quel che ci sta ...
135800110408     c                   eval      tbeke1 = climit
135900100325     c     ktbe2         setll     tntbe01l
136000100325     c                   do        *hival
136100100325     c     ktbe2         reade     tntbe01l
136200100325     c                   if        %eof(tntbe01l)
136300100325     c                   leave
136400100317     c                   end
136500100317     c                   if        tbeatb <> ' '
136600100317     c                   iter
136700100317     c                   end
136800100827     c                   movel     tbeuni        dpxapda
136900110411     c                   if        pdcnotpxa <> *blank
137000110411     c                   eval      pdcnotpxa = %trim(pdcnotpxa) +
137100110411     c                             ' ' + §pxades
137200110411     c                   else
137300110411     c                   eval      pdcnotpxa = §pxades
137400110411     c                   endif
137500110408     c                   leave
137600100317     c                   enddo
137700080418     c                   endsr
137800100317     c* ?_____________________________________________________
137900100317     c     srfirma       begsr
138000100317      * ?_____________________________________________________
138100100317     c* NON RICHIEDERE FIRMA
138200100921     c* per fedex (cliente mittente) si stampa solo packing list ?????????
138300100902     c* per ritorni all'incasso
138400100921     c*                  if        climitt = clifed or
138500100921     c                   if        ritinc = 'S'
138600100826     c                   eval      pdcflgfir ='N'
138700100826     c                   end
138800100827     c* NOTE FIRMA tabella FIR
138900100827     c                   do
139000100827     c* personalizzazione cliente mittente
139100100827     c                   movel(p)  climitt       tbeke1
139200100827     c                   clear                   tbeke2
139300100827     c                   exsr      srtbefir
139400140221     c                   if        pdcnotfir <> ' '
139500100902     c* se personalizzazione firma no multiconsegna
139600100902     c                   eval      PDCSPESING =  'S'
139700100827     c                   leave
139800100827     c                   end
139900100827     c* personalizzazione network (DPD)
140000100827     c                   movel(p)  §cmntw        tbeke1
140100100827     c                   clear                   tbeke2
140200100827     c                   exsr      srtbefir
140300100827     c                   if        pdcnotfir <> ' '
140400101022     c* se personalizzazione firma no multiconsegna (Bocchi ha detto di no)
140500101022     c*                  eval      PDCSPESING =  'S'
140600100827     c                   leave
140700100827     c                   end
140800100827     c* standard Bartolini con o senza DDT
140900100827     c                   movel(p)  'BARTOLINI'   tbeke1
141000120412     c* DDT faccio comunque il doppio
141100100827     c* per evitare che se non trova il record non metta lo standard
141200101129w   1c                   if        comddt =  'S'
141300100827     c                   movel(p)  'DDT'         tbeke2
141400100827     c                   exsr      srtbefir
141500100827     c                   if        pdcnotfir <> ' '
141600100827     c                   leave
141700100827     c                   end
141800100827     c                   end
141900120412     c* PACKING LIST faccio comunque il doppio
142000120412     c* per evitare che se non trova il record non metta lo standard
142100120412w   1c                   if        compkl =  'S'
142200120412     c                   movel(p)  'PKL'         tbeke2
142300120412     c                   exsr      srtbefir
142400120412     c                   if        pdcnotfir <> ' '
142500120412     c                   leave
142600120412     c                   end
142700120412     c                   end
142800100827     c* standard bartolini
142900100827     c                   clear                   tbeke2
143000100827     c                   exsr      srtbefir
143100100827     c                   enddo
143200100317     c*
143300100317     c                   endsr
143400080227     c* ?_____________________________________________________
143500100827     c     srtbefir      begsr
143600080227      * ?_____________________________________________________
143700100827     c                   clear                   pdcnotfir
143800100827     c                   eval      tbecod = 'FIR'
143900100827     c     ktbe2         chain     tntbe01l
144000100827     c                   if        %found(tntbe01l) and
144100100827     c                             tbeatb = ' '
144200100827     c                   movel     tbeuni        dfir
144300100827     c                   eval      pdcnotfir = §firdes
144400100827     c                   end
144500100827     c                   endsr
144600100827     c* ?_____________________________________________________
144700100827     c     *inzsr        begsr
144800100827      * ?_____________________________________________________
144900061227
145000080227     c     karb          klist
145100080227     c                   kfld                    CO42AAS
145200080227     c                   kfld                    CO42LNP
145300080227     c                   kfld                    CO42NRS
145400080227     c                   kfld                    CO42NSP
145500131128     c     kdsf          klist
145600131128     c                   kfld                    DSFNPG
145700131128     c                   kfld                    DSFNFV
145800131128     c                   kfld                    DSFFGS
145900131128     c                   kfld                    DSFTRD
146000100914     c     karbm         klist
146100100914     c                   kfld                    marbAAS
146200100914     c                   kfld                    marbLNP
146300100914     c                   kfld                    marbNRS
146400100914     c                   kfld                    marbNSP
146500080321
146600080325     c     kart          klist
146700080325     c                   kfld                    arbAAS
146800080325     c                   kfld                    arbLNP
146900080325     c                   kfld                    arbNRS
147000080325     c                   kfld                    arbNSP
147100080325
147200080505     c     kbrv          klist
147300080505     c                   kfld                    cat4
147400080505     c                   kfld                    co42ndc
147500080505     c                   kfld                    co42fgs
147600080505     c                   kfld                    artLNP
147700080505     c                   kfld                    artLNA
147800080505     c                   kfld                    artNRS
147900080505     c                   kfld                    artNSC
148000080505
148100080321     C     klbl          klist
148200080321     C                   kfld                    lblaap
148300080321     C                   kfld                    lbllpp
148400080321     C                   kfld                    lblnrp
148500080321     C                   kfld                    lblnsp
148600080321     C*
148700061227
148800080227     c     kar5          klist
148900080227     c                   kfld                    CO42AAS
149000080227     c                   kfld                    CO42LNP
149100080227     c                   kfld                    CO42NRS
149200080227     c                   kfld                    CO42NSP
149300080227     c                   kfld                    trek5             3
149400080227
149500080227     c     kar4          klist
149600080227     c                   kfld                    CO42AAS
149700080227     c                   kfld                    CO42LNP
149800080227     c                   kfld                    CO42NRS
149900080227     c                   kfld                    CO42NSP
150000080227     c                   kfld                    trek4             1
150100100914      *
150200100914     c     kar6          klist
150300100914     c                   kfld                    CO42AAS
150400100914     c                   kfld                    CO42LNP
150500100914     c                   kfld                    CO42NRS
150600100914     c                   kfld                    CO42NSP
150700100914     c                   kfld                    trek6             1
150800080304      *
150900080304     c     kdst          klist
151000080304     c                   kfld                    Cat4              1 0
151100080304     c                   kfld                    CO42ndc
151200080304     c                   kfld                    CO42fgs
151300080304     c                   z-add     4             cat4
151400080304      *
151500080304     c     karn          klist
151600080304     c                   kfld                    CO42AAS
151700080304     c                   kfld                    CO42LNP
151800080304     c                   kfld                    CO42NRS
151900080304     c                   kfld                    CO42NSP
152000080304     c                   kfld                    cdn               3
152100080304     c                   kfld                    gst               1
152200080304     c                   move      'LDV'         cdn
152300080304     c                   move      'S'           gst
152400080304      *
152500080227
152600080227     c     kfipdc1       klist
152700080227     c                   kfld                    CO42fgs
152800080227     c                   kfld                    CO42ndc
152900080227      *
153000080904     c     kfipdc        klist
153100080904     c                   kfld                    CO42fgs
153200080904     c                   kfld                    CO42ndc
153300080904     c                   kfld                    CO42AAS
153400080904     c                   kfld                    CO42LNP
153500080904     c                   kfld                    CO42NRS
153600080904     c                   kfld                    CO42NSP
153700080904     c     kfipdc3       klist
153800080227     c                   kfld                    CO42fgs
153900080227     c                   kfld                    CO42ndc
154000080904     c                   kfld                    arbpdc
154100080227     c                   kfld                    CO42AAS
154200080227     c                   kfld                    CO42LNP
154300080227     c                   kfld                    CO42NRS
154400080227     c                   kfld                    CO42NSP
154401170728     c     kpdc6         klist
154402170728     c                   kfld                    pdccodaut
154403170728     c                   kfld                    PDCSERVIZ
154404170728     c                   kfld                    pdcfgs
154405170728     c                   kfld                    pdcgiro
154406170728     c     kapd6         klist
154407170728     c                   kfld                    pdc
154408170728     c                   kfld                    PDCSERVIZ
154409170728     c                   kfld                    pdcfgs
154410170728     c                   kfld                    pdcgiro
154500080903      *
154600080227     c     kfipdd        klist
154700080227     c                   kfld                    CO42fgs
154800080227     c                   kfld                    CO42ndc
154900080227     c                   kfld                    CO42AAS
155000080227     c                   kfld                    CO42LNP
155100080227     c                   kfld                    CO42NRS
155200080227     c                   kfld                    CO42NSP
155300080227     c                   kfld                    artnsc
155400080303     C     ktab          KLIST
155500080303     C                   kfld                    codut
155600080303     C                   kfld                    tblcod
155700080303     C                   kfld                    tblkey
155800080303     c                   z-add     1             codut             1 0
155900130222
156000080418     C     ktab2         KLIST
156100080418     C                   kfld                    codut
156200080418     C                   kfld                    tblcod
156300080403
156400080403     c     ktbe          klist
156500080403     c                   kfld                    tbecod
156600080403     c                   kfld                    tbeke1
156700100316     c     ktbe2         klist
156800100316     c                   kfld                    tbecod
156900100316     c                   kfld                    tbeke1
157000100316     c                   kfld                    tbeke2
157100080403
157200080403     C     KCLN          KLIST
157300080403     C                   kfld                    CLNTFP
157400080403     C                   kfld                    CLNTFA
157500080403     C                   kfld                    CLNANN
157600080403     C                   kfld                    CLNMES
157700140120     c     kficau        klist
157800140120     c                   kfld                    CO42fgs
157900140120     c                   kfld                    CO42ndc
158000140120     c                   kfld                    arbpdc
158100140120     c                   kfld                    tipooggt          1
158200140120     c                   kfld                    oggsped          20
158300140120     c                   move      'C'           tipooggt          1
158400140120      *
158500080403     c*giorno da trattare
158600080403     c                   move      *date         dtaiso
158700080403     c                   move      dtaiso        oggi              8 0
158800080403     c                   Z-ADD     oggi          WMMGG             4 0
158900080403     c                   Z-ADD     oggi          WG                2 0
159000080403     c                   MOVEL     oggi          clnann
159100080403     c                   Z-ADD     0             CLNTFP
159200080403     c                   Z-ADD     0             CLNTFA
159300080403     c                   MOVEL     WMMGG         clnmes
159400080403     c     KCLN          CHAIN     AZCLN01L                           31
159500080403     c                   MOVEA     CLNMAT        MES
159600080403     c                   MOVE      MES(WG)       WGG               1
159700080403     c
159800080403     c
159900080403     C* tabelle caricate UNA-TANTUM
160000080418      * Carico le causali eventi con tipo consegna = "G"
160100080418     c                   movel     '2A'          TBLcod
160200080418     c                   clear                   TBLkey
160300080418     c                   clear                   XX                3 0
160400080418     c     Ktab          setll     tabel00f
160500080418     c     Ktab2         reade     tabel00f
160600080418      *
160700080418do  1c                   dow       not %eof(tabel00F)
160800080418      * Deve essere NON annullata e con flag §2AFTC = "G"
160900080418if  2c                   If        TBLflg = *blanks
161000080418     c                   movel     TBLuni        ds2A
161100080418if  3c                   if        §2Aftc = 'G'
161200080418     c                   add       1             xx
161300080418     c                   movel     TBLkey        $2Acev_G(xx)
161400080418e   3c                   endif
161500080418e   2c                   EndIf
161600080418      *
161700080418     c     ktab2         reade     tabel00f
161800080418e   1c                   ENDDO
161900101118      * Carico le anomalia dei colli
162000101118     c                   movel     '3E'          TBLcod
162100101118     c                   clear                   TBLkey
162200101118     c                   clear                   XX                3 0
162300101118     c     Ktab          setll     tabel00f
162400101118     c     Ktab2         reade     tabel00f
162500101118      *
162600101118do  1c                   dow       not %eof(tabel00F)
162700101118      * Deve essere passata a PDA e no di tipo spunta 8
162800101118if  2c                   If        TBLflg = *blanks
162900101118     c                   movel     TBLuni        ds3E
163000101118if  3c                   if        §3EPDA <> 'N' and §3ECAT8 = ' '
163100101118     c                   add       1             xx
163200101118     c                   movel     TBLkey        $3Ecan(xx)
163300101118e   3c                   endif
163400101118e   2c                   EndIf
163500101118      *
163600101118     c     ktab2         reade     tabel00f
163700101118e   1c                   ENDDO
163800061227
163900080417      * tipi servizio da esporre
164000080417     c                   clear                   x
164100080417     c                   movel     'TSA'         tbecod
164200080417     c                   clear                   tbeke1
164300080417     c     ktbe          setll     tntbe01l
164400080417     c                   do        *hival
164500080417     c                   read      tntbe01l
164600080417     c                   if        %eof(tntbe01l) or tbecod <> 'TSA'
164700080417     c                   leave
164800080417     c                   endif
164900080417     c                   if        tbeatb <> *blank
165000080417     c                   iter
165100080417     c                   endif
165200080417     c                   eval      x = x + 1
165300080417     c                   movel     tbeke1        TSA(x)
165400080417     c                   enddo
165500080417
165600080403      * documenti da consegnare
165700080417     c                   clear                   ddoc
165800080403     c                   clear                   x
165900080403     c                   movel     'DOC'         tbecod
166000080403     c                   movel(p)  *blank        tbeke1
166100080403     c     ktbe          setll     tntbe01l
166200080403     c                   do        *hival
166300080403     c                   read      tntbe01l
166400080403     c                   if        %eof(tntbe01l) or tbecod <> 'DOC'
166500080403     c                   leave
166600080403     c                   endif
166700080403     c                   if        tbeatb <> *blank or §docpda = 'N'
166800080403     c                   iter
166900080403     c                   endif
167000080403     c                   eval      x = x + 1
167100080403     c                   movel     tbeuni        ddoc
167200080403     c                   movel     tbeke1        doccod(x)
167300080422     c                   eval      docflg(x) = §DOCFAT+§DOCDDT+§DOCPKL+
167400080422     c                             §DOcfas + §docdas
167500080403     c                   enddo
167600080403
167700080403     c                   endsr
