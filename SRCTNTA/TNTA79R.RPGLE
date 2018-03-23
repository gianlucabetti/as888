000100070712     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200070712      *------------------------------------------------------------------------*
000300070712      *
000400070713      *  Stato applicazione ISTAT per tariffa
000500070712      *
000600070712      *------------------------------------------------------------------------*
000700070712
000800121224     ftntam04l  if   e           k disk
000900130905     fAZCMM01L  if   e           k disk
001000070712     fazorg01l  if   e           k disk
001100070712     ftabel00f  if   e           k disk
001200101015     ftisis01l  if   e           k disk
001300070712     fwfist00f  o    e             disk    usropn
001400121128     f                                     extfile(wLibWF)
001500070712     ftnta79p   o    e             printer oflind(*in90)
001600070712
001700070712      *------------------------------------------------------------------------*
001800070712      *  RIEPILOGO INDICATORI
001900070712      *------------------------------------------------------------------------*
002000070712      * 02 - ho scritto almeno 1 rcd
002100070712
002200070712      *------------------------------------------------------------------------*
002300070712      *   V A R I A B I L I
002400070712      *------------------------------------------------------------------------*
002500070712     d comman          s             80
002600070712     d codut           s              1  0 inz(1)
002700070712     d kcar            s                   like(orgcar)
002800070712     d kcod            s                   like(tblcod)
002900070712     d kksc            s                   like(tamksc)
003000070712     d kkey            s                   like(tblkey)
003100070712     d lenght          s             15  5 inz(80)
003200070712     d kkcc            s                   like(acokcc) inz(151)
003300070712     d kkut            s                   like(acokut) inz(1)
003400070712     d wctr            s                   like(tamctr)
003500070712     d wdata           s              8  0
003600070712     d wksc            s                   like(tamksc)
003700070712     d wlib            s              9
003800070712     d wora            s              6  0
003900070712     d w0030           s              3  0
004000070712     d w0140           s             14  0
004100101015     d $EoF            s              1n   inz(*off)
004200121128       // -?Nomi estesi libreria/file dei file?
004300121128     d wLibWF          s             21a   inz
004400070712
004500070712      *------------------------------------------------------------------------*
004600070712      *   S C H I E R E
004700070712      *------------------------------------------------------------------------*
004800121128     d cmd             s             80    dim(1) ctdata perrcd(1)
004900101015     d dia             s              8  0 dim(999)
005000070712
005100070712      *------------------------------------------------------------------------*
005200070712      *   D S   I N T E R N E / E S T E R N E
005300070712      *------------------------------------------------------------------------*
005400080924
005500080924     d wlbdat          ds                  inz
005600080924     d  g02dat                 1      8  0
005700080924     d  g02inv                 9     16  0
005800080924     d  g02err                17     17
005900080924     d  g02tgi                18     22  0
006000080924
006100070712     d tnta79ds        ds                  inz
006200070712     d  dsdip                         6  0
006300070712     d  dsdfp                         6  0
006400081009     d  dsdru                         8  0
006500121211     d  dsDIV                         8  0
006600121128     d  ds10F                         1
006700070712
006800070712     d azuteds       e ds                  extname(azute00f)
006900070712     d ddatiute      e ds
007000070712     d ds_cnaco      e ds                  inz extname(cnaco00f)
007100070712     d ds_cnind      e ds                  inz extname(cnind00f)
007200070712     d ds_cnclp      e ds                  inz extname(cnclp00f)
007300070712     d ds_fncls      e ds                  inz extname(fncls00f)
007400070712     d kpjba         e ds
007500070712     d dsta01        e ds
007600070712     d tibs10ds      e ds
007700070712     d tibs34ds      e ds
007800070712     d tibs69ds      e ds
007900070712     d tise70ds      e ds
008000070712
008100070712      *------------------------------------------------------------------------*
008200070712
008300070712     c                   clear                   wksc
008400070712     c                   eval      wctr = 999
008500070712     c                   move      *all'0'       kksc
008600070712
008700121224     c     kksc          setll     tntam04l
008800070712    1c                   do        *hival
008900121224     c                   read      tntam04l
009000070712      * fine file
009100121224     c                   if        %eof(tntam04l)
009200070712     c                   leave
009300070712     c                   endif
009400121224      * Verifico data decorrenza e scadenza della tariffa con i dati passati
009500070712
009600121224      * se data decorrenza e scadenza della tariffa è maggiore della data
009700121224      * validità della tariffa leggo la successiva
009800121224     c                   if        tamddt > dsdiv and tamdst > dsdiv
009900121224     c                   iter
010000121224     c                   endif
010100070712      * cambio tariffa
010200070712    2c                   if        tamctr <> wctr or tamksc <> wksc
010300070712
010400070712     c                   movel     tamflo        dsta01
010500070712
010600070712      * escludo le annullate
010700070712     c                   if        tamatb <> *blanks
010800070712     c                   iter
010900070712     c                   endif
011000070712      * escludo le bloccate
011100070712     c                   if        tambap = 'B'
011200070712     c                   iter
011300070712     c                   endif
011400070712
011500070712      * cerco i dati anagrafici
011600070712     c                   clear                   tibs69ds
011700070712     c                   move      tamksc        i69kac
011800070712     c                   move      tamksc        i69kcp
011900070712     c                   call      'TIBS69R'
012000070712     c                   parm                    tibs69ds
012100070712     c                   parm                    ds_cnaco
012200070712     c                   parm                    ds_cnind
012300070712     c                   parm                    ds_cnclp
012400070712     c                   parm                    ds_fncls
012500070712      * escludo le tariffe dei clienti bloccati
012600130315     c                   if        acoabl <> *blanks
012700070712     c                   iter
012800070712     c                   endif
012900070712      * scrivo il file
013000070712     c                   exsr      scrivi
013100070712
013200070712     c                   eval      wksc = tamksc
013300070712     c                   eval      wctr = tamctr
013400070712
013500070712      * se la tariffa che sto leggendo deve ancora andare in vigore la elaboro
013600070712      * ma devo elaborare anche la tariffa precedente che è in vigore ora
013700121211      * => In realtà ora considero la data (ricevuta come parametro) in cui
013800121211      *    entra in vigore la tariffa - non più la data odierna
013900121211     c*//                if        tamddt > *date
014000121224     c*//                if        tamddt > dsDIV
014100121224     c*//                eval      wctr = 999
014200121224     c*//                endif
014300070712    2c                   endif
014400070712
014500070712    1c                   enddo
014600070712
014700070712      * stampo  la fine elaborazione
014800070712     c                   write     ta79s1
014900070712
015000070712     c                   eval      *inlr = *on
015100070712
015200070712      *------------------------------------------------------------------------*
015300070712      * SCRIVO IL FILE PER LA STAMPA
015400070712      *------------------------------------------------------------------------*
015500070712     c     scrivi        begsr
015600070712
015700070712     c                   clear                   wfist000
015800070712
015900070712      * utente/data/ora elaborazione
016000070712     c                   eval      istpru = knmus
016100070712     c                   eval      istdte = wdata
016200070712     c                   eval      istore = wora
016300070712      * cliente
016400070712     c                   eval      istksc  = tamksc
016500070712     c                   eval      istkscd = acorag
016600070712      * importanza cliente
016700070712     c                   eval      istclv = clpclv
016800070712      * data ultima spedizione fattura
016900080924     c                   if        clpdus > *zeros
017000080924      * controllo la data lunga 8
017100080924     c                   clear                   wlbdat
017200080924     c                   z-add     clpdus        g02inv
017300080924     c                   eval      g02err = '3'
017400080924     c                   call      'XSRDA8'
017500080924     c                   parm                    wlbdat
017600080924     c                   eval      istdus = g02inv
017700080924     c                   endif
017800070712      * tariffa
017900070712     c                   eval      istctr = tamctr
018000070712     c                   eval      istprg = tamprg
018100070712     c                   eval      istddt = tamddt
018200070712     c                   eval      istdst = tamdst
018300070712      * tipo tariffa
018400070712     c                   select
018500070712     c                   when      §tadpd = 'S'
018600070712     c                   eval      isttpt = 'D'
018700070712     c                   when      §tafed = 'S'
018800070712     c                   eval      isttpt = 'F'
018900070712     c                   other
019000070712     c                   eval      isttpt = tamfie
019100070712     c                   endsl
019200070712      * istat presente
019300101015     c                   if        tamrct <> 0
019400070712     c                   eval      istist = *on
019500101015     c                   eval      istfli = tamrct
019600070712     c                   eval      istdsi = dia(istfli)
019700070712     c                   eval      istpri = tampri
019800070712      * istat assente
019900070712     c                   else
020000070712     c                   eval      istist = *off
020100070712     c                   endif
020200070712      * cerco il codice cliente unificante
020300070712     c                   clear                   tibs10ds
020400070712     c                   eval      d10tle = 'ST'
020500070712     c                   eval      d10paf = 'P'
020600070712     c                   z-add     istksc        d10cod
020700081009     c                   z-add     dsdru         d10drf
020800070712     c                   call      'TIBS10R'
020900070712     c                   parm                    tibs10ds
021000070712     c                   parm      'A'           partpexe          1
021100070712      * se ci sono errori valorizzo come unificante il cliente
021200070712     c                   if        d10err <> ' '
021300070712     c                   eval      istkun = istksc
021400070712     c                   eval      istkund = istkscd
021500070712      * se tutto ok valorizzo l'unificante trovato
021600070712     c                   else
021700070712     c                   eval      istkun = d10cop
021800070712      * cerco i dati anagrafici
021900070712     c                   clear                   tibs69ds
022000070712     c                   move      istkun        i69kac
022100070712     c                   move      istkun        i69kcp
022200070712     c                   call      'TIBS69R'
022300070712     c                   parm                    tibs69ds
022400070712     c                   parm                    ds_cnaco
022500070712     c                   parm                    ds_cnind
022600070712     c                   parm                    ds_cnclp
022700070712     c                   parm                    ds_fncls
022800070712     c                   eval      istkund = acorag
022900100917     c                   eval      istclv = clpclv
023000070712     c                   endif
023100070712      * verifico il codice commerciale unificante
023200130905     c     CLPage        chain     AZCMM000
023300130905     c                   if        not %found(AZCMM01L)
023400070712     c                   eval      istcmu  = clpage
023500070712     c                   eval      istcmud = *all'*'
023600070712     c                   else
023700130905     c                   eval      istcmu  = CMMuni
023800130905     c     ISTcmu        chain     AZCMM000
023900130905     c                   if        not %found(AZCMM01L)
024000070712     c                   eval      istcmud = *all'*'
024100070712     c                   else
024200130905     c                   eval      istcmud = CMMdes
024300070712     c                   endif
024400070712     c                   endif
024500070712
024600070712      * albero commerciale unificante
024700070712     c                   movel     istcmu        w0030
024800070712     c     w0030         chain     azorg01l
024900070712
025000070712      * distretto
025100070712     c                   eval      istdis = orgfl3
025200070712     c                   eval      kcod = '17'
025300070712     c                   eval      kkey = istdis
025400070712     c     ktabel        chain     tabel00f
025500070712     c                   if        not %found(tabel00f) or tblflg <> *blanks
025600070712     c                   eval      istdisd = *all'*'
025700070712     c                   else
025800070712     c                   eval      istdisd = tbluni
025900070712     c                   endif
026000070712      * area
026100070712     c                   eval      istcar = orgcar
026200070712     c                   eval      kcod = '05'
026300070712     c                   movel     istcar        kkey
026400070712     c     ktabel        chain     tabel00f
026500070712     c                   if        not %found(tabel00f) or tblflg <> *blanks
026600070712     c                   eval      istcard = *all'*'
026700070712     c                   else
026800070712     c                   eval      istcard = tbluni
026900070712     c                   endif
027000070712      * filiale
027100070712     c                   eval      istfil = orgfil
027200070712     c                   if        not %found(azorg01l) or orgfva <> *blanks
027300070712     c                   eval      istfild = *all'*'
027400070712     c                   else
027500070712     c                   eval      istfild = orgdes
027600070712     c                   endif
027700070712
027800070712      * imposto i dati relativi al fatturato
027900070712     c                   eval      istdip = dsdip
028000070712     c                   eval      istdfp = dsdfp
028100070712     c                   clear                   tise70ds
028200070712     c                   movel     'S'           i70tla
028300070712     c                   z-add     istksc        i70ksc
028400070712     c                   move      istctr        i70ctr
028500070712     c                   z-add     istdip        i70dai
028600070712     c                   z-add     istdfp        i70daf
028700070712     c                   call      'TISE70R'
028800070712     c                   parm                    tise70ds
028900070712     c                   z-add     o70ric        istfat
029000070712     c                   z-add     o70rir        istrir
029100070712     c                   z-add     o70rip        istrip
029200070712      * scrivo file
029300070712     c                   add       1             tottar
029400070712     c                   write     wfist000
029500070712
029600070712     c                   endsr
029700070712
029800070712      *--------------------------------------------------------------*
029900070712      * ROUTINE INIZIALE
030000070712      *--------------------------------------------------------------*
030100070712     c     *inzsr        begsr
030200070712
030300070712     c     *entry        plist
030400070712     c                   parm                    kpjba
030500070712     c                   movel     kpjbu         tnta79ds
030600070712
030700070712     c     *dtaara       define    §azute        azuteds
030800070712     c     *dtaara       define    §datiute      ddatiute
030900070712
031000070712     c                   in(e)     *dtaara
031100070712     c                   if        %error or rsut = *blanks
031200070712     c                   clear                   tibs34ds
031300070712     c                   call      'TIBS34R'
031400070712     c                   parm                    tibs34ds
031500070712     c                   in        *dtaara
031600070712     c                   endif
031700070712
031800101015      /free
031900101015       //?Carico scatti ISTAT
032000101015         SISsca = 0;
032100101015         clear DIA;
032200101015         setll (SISsca) TISIS01L;
032300101015
032400101015         DOW  not $EoF;
032500101015
032600101015           read TISIS01L;
032700101015
032800101015           IF  %Eof(TISIS01L);
032900101015             $EoF = *on;
033000101015             leave;
033100101015           ENDIF;
033200101015
033300101015           DIA(SISsca) = SISdata;
033400101015
033500101015         ENDDO;
033600101015
033700101015      /end-free
033800070712
033900070712      * se s.i. di prova imposto la libreria di prova
034000070712     c                   if        %subst(knsif:7:1) = 'P'
034100070712     c                   eval      wlib = 'GAITRAAZP '
034200070712     c                   else
034300070712     c                   eval      wlib = 'GAITRAAZM '
034400070712     c                   endif
034500121128
034600121128      * se lavoro lanciato da TISE01R imposto il wrkf a WFIST10F
034700121128      * altrimenti lo imposto a WFIST00F
034800121128     c                   if        ds10F = 'S'
034900121128     c                   eval      wLibWF = %trim(wLib) + '/WFIST10F'
035000121128     c                   else
035100121128     c                   eval      wLibWF = %trim(wLib) + '/WFIST00F'
035200121128     c                   endif
035300070712
035400070712      * pulisco il file
035500121128     c                   eval      Comman = %trimr(Cmd(1)) +
035600121128     c                                      %trimr(wLibWF) + ')'
035700070712     c                   call      'QCMDEXC'
035800070712     c                   parm                    comman
035900070712     c                   parm                    lenght
036000070712
036100070712     c                   open      wfist00f
036200070712
036300070712      * reperisco data e ora del lancio
036400070712     c                   time                    w0140
036500070712     c                   move      w0140         wdata
036600070712     c                   movel     w0140         wora
036700070712
036800070712     c     kazorg        klist
036900070712     c                   kfld                    dutdis
037000070712     c                   kfld                    kcar
037100070712
037200070712     c     ktabel        klist
037300070712     c                   kfld                    codut
037400070712     c                   kfld                    kcod
037500070712     c                   kfld                    kkey
037600070712
037700070712     c                   endsr
037800121128
037900121128** - Cmd - Comandi ------------------------------------------------------------*
038000121128CLRPFM FILE(
