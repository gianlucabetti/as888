000100070712     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200070712      *------------------------------------------------------------------------*
000300070712      *
000400070713      *  Stato applicazione ISTAT per tariffa
000500070712      *
000600070712      *------------------------------------------------------------------------*
000700070712
000800121224     ftntam04l  if   e           k disk
000900070712     fazorg01l  if   e           k disk
001000070712     fazorg02l  if   e           k disk    rename(azorg:azorg02)
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
003800070712     d wok             s              1    inz(*off)
003900070712     d wora            s              6  0
004000070712     d w0030           s              3  0
004100070712     d w0140           s             14  0
004200101015     d $EoF            s              1n   inz(*off)
004300121128       // -?Nomi estesi libreria/file dei file?
004400121128     d wLibWF          s             21a   inz
004500070712
004600070712      *------------------------------------------------------------------------*
004700070712      *   S C H I E R E
004800070712      *------------------------------------------------------------------------*
004900121128     d cmd             s             80    dim(1) ctdata perrcd(1)
005000101015     d dia             s              8  0 dim(999)
005100070712
005200070712      *------------------------------------------------------------------------*
005300070712      *   D S   I N T E R N E / E S T E R N E
005400070712      *------------------------------------------------------------------------*
005500080924
005600080924     d wlbdat          ds                  inz
005700080924     d  g02dat                 1      8  0
005800080924     d  g02inv                 9     16  0
005900080924     d  g02err                17     17
006000080924     d  g02tgi                18     22  0
006100080924
006200070712     d tnta79ds        ds                  inz
006300070712     d  dsdip                         6  0
006400070712     d  dsdfp                         6  0
006500081009     d  dsdru                         8  0
006600121211     d  dsDIV                         8  0
006700121128     d  ds10F                         1
006800070712
006900070712     d azuteds       e ds                  extname(azute00f)
007000070712     d ddatiute      e ds
007100070712     d ds_cnaco      e ds                  inz extname(cnaco00f)
007200070712     d ds_cnind      e ds                  inz extname(cnind00f)
007300070712     d ds_cnclp      e ds                  inz extname(cnclp00f)
007400070712     d ds_fncls      e ds                  inz extname(fncls00f)
007500070712     d kpjba         e ds
007600070712     d dsta01        e ds
007700070712     d ds01          e ds
007800070712     d og143         e ds
007900070712     d tibs02ds      e ds
008000070712     d tibs10ds      e ds
008100070712     d tibs34ds      e ds
008200070712     d tibs69ds      e ds
008300070712     d tise70ds      e ds
008400070712
008500070712      *------------------------------------------------------------------------*
008600070712
008700070712     c                   clear                   wksc
008800070712     c                   eval      wctr = 999
008900070712     c                   move      *all'0'       kksc
009000070712
009100121224     c     kksc          setll     tntam04l
009200070712    1c                   do        *hival
009300121224     c                   read      tntam04l
009400070712      * fine file
009500121224     c                   if        %eof(tntam04l)
009600070712     c                   leave
009700070712     c                   endif
009800121224      * Verifico data decorrenza e scadenza della tariffa con i dati passati
009900070712
010000121224      * se data decorrenza e scadenza della tariffa è maggiore della data
010100121224      * validità della tariffa leggo la successiva
010200121224     c                   if        tamddt > dsdiv and tamdst > dsdiv
010300121224     c                   iter
010400121224     c                   endif
010500070712      * cambio tariffa
010600070712    2c                   if        tamctr <> wctr or tamksc <> wksc
010700070712
010800070712     c                   movel     tamflo        dsta01
010900070712
011000070712      * escludo le annullate
011100070712     c                   if        tamatb <> *blanks
011200070712     c                   iter
011300070712     c                   endif
011400070712      * escludo le bloccate
011500070712     c                   if        tambap = 'B'
011600070712     c                   iter
011700070712     c                   endif
011800070712
011900070712      * cerco i dati anagrafici
012000070712     c                   clear                   tibs69ds
012100070712     c                   move      tamksc        i69kac
012200070712     c                   move      tamksc        i69kcp
012300070712     c                   call      'TIBS69R'
012400070712     c                   parm                    tibs69ds
012500070712     c                   parm                    ds_cnaco
012600070712     c                   parm                    ds_cnind
012700070712     c                   parm                    ds_cnclp
012800070712     c                   parm                    ds_fncls
012900070712      * escludo le tariffe dei clienti bloccati
013000130315     c                   if        acoabl <> *blanks
013100070712     c                   iter
013200070712     c                   endif
013300070712      * scrivo il file
013400070712     c                   exsr      scrivi
013500070712
013600070712     c                   eval      wksc = tamksc
013700070712     c                   eval      wctr = tamctr
013800070712
013900070712      * se la tariffa che sto leggendo deve ancora andare in vigore la elaboro
014000070712      * ma devo elaborare anche la tariffa precedente che è in vigore ora
014100121211      * => In realtà ora considero la data (ricevuta come parametro) in cui
014200121211      *    entra in vigore la tariffa - non più la data odierna
014300121211     c*//                if        tamddt > *date
014400121224     c*//                if        tamddt > dsDIV
014500121224     c*//                eval      wctr = 999
014600121224     c*//                endif
014700070712    2c                   endif
014800070712
014900070712    1c                   enddo
015000070712
015100070712      * stampo  la fine elaborazione
015200070712     c                   write     ta79s1
015300070712
015400070712     c                   eval      *inlr = *on
015500070712
015600070712      *------------------------------------------------------------------------*
015700070712      * SCRIVO IL FILE PER LA STAMPA
015800070712      *------------------------------------------------------------------------*
015900070712     c     scrivi        begsr
016000070712
016100070712     c                   clear                   wfist000
016200070712
016300070712      * utente/data/ora elaborazione
016400070712     c                   eval      istpru = knmus
016500070712     c                   eval      istdte = wdata
016600070712     c                   eval      istore = wora
016700070712      * cliente
016800070712     c                   eval      istksc  = tamksc
016900070712     c                   eval      istkscd = acorag
017000070712      * importanza cliente
017100070712     c                   eval      istclv = clpclv
017200070712      * data ultima spedizione fattura
017300080924     c                   if        clpdus > *zeros
017400080924      * controllo la data lunga 8
017500080924     c                   clear                   wlbdat
017600080924     c                   z-add     clpdus        g02inv
017700080924     c                   eval      g02err = '3'
017800080924     c                   call      'XSRDA8'
017900080924     c                   parm                    wlbdat
018000080924     c                   eval      istdus = g02inv
018100080924     c                   endif
018200070712      * tariffa
018300070712     c                   eval      istctr = tamctr
018400070712     c                   eval      istprg = tamprg
018500070712     c                   eval      istddt = tamddt
018600070712     c                   eval      istdst = tamdst
018700070712      * tipo tariffa
018800070712     c                   select
018900070712     c                   when      §tadpd = 'S'
019000070712     c                   eval      isttpt = 'D'
019100070712     c                   when      §tafed = 'S'
019200070712     c                   eval      isttpt = 'F'
019300070712     c                   other
019400070712     c                   eval      isttpt = tamfie
019500070712     c                   endsl
019600070712      * istat presente
019700101015     c                   if        tamrct <> 0
019800070712     c                   eval      istist = *on
019900101015     c                   eval      istfli = tamrct
020000070712     c                   eval      istdsi = dia(istfli)
020100070712     c                   eval      istpri = tampri
020200070712      * istat assente
020300070712     c                   else
020400070712     c                   eval      istist = *off
020500070712     c                   endif
020600070712      * cerco il codice cliente unificante
020700070712     c                   clear                   tibs10ds
020800070712     c                   eval      d10tle = 'ST'
020900070712     c                   eval      d10paf = 'P'
021000070712     c                   z-add     istksc        d10cod
021100081009     c                   z-add     dsdru         d10drf
021200070712     c                   call      'TIBS10R'
021300070712     c                   parm                    tibs10ds
021400070712     c                   parm      'A'           partpexe          1
021500070712      * se ci sono errori valorizzo come unificante il cliente
021600070712     c                   if        d10err <> ' '
021700070712     c                   eval      istkun = istksc
021800070712     c                   eval      istkund = istkscd
021900070712      * se tutto ok valorizzo l'unificante trovato
022000070712     c                   else
022100070712     c                   eval      istkun = d10cop
022200070712      * cerco i dati anagrafici
022300070712     c                   clear                   tibs69ds
022400070712     c                   move      istkun        i69kac
022500070712     c                   move      istkun        i69kcp
022600070712     c                   call      'TIBS69R'
022700070712     c                   parm                    tibs69ds
022800070712     c                   parm                    ds_cnaco
022900070712     c                   parm                    ds_cnind
023000070712     c                   parm                    ds_cnclp
023100070712     c                   parm                    ds_fncls
023200070712     c                   eval      istkund = acorag
023300100917     c                   eval      istclv = clpclv
023400070712     c                   endif
023500070712      * verifico il codice commerciale unificante
023600070712     c                   eval      kcod = '01'
023700070712     c                   movel     clpage        kkey
023800070712     c     ktabel        chain     tabel00f
023900070712     c                   if        not %found(tabel00f) or tblflg <> *blanks
024000070712     c                   eval      istcmu  = clpage
024100070712     c                   eval      istcmud = *all'*'
024200070712     c                   else
024300070712     c                   eval      ds01    = tbluni
024400070712     c                   eval      istcmu  = §01rgf
024500070712     c                   movel     istcmu        kkey
024600070712     c     ktabel        chain     tabel00f
024700070712     c                   if        not %found(tabel00f) or tblflg <> *blanks
024800070712     c                   eval      istcmud = *all'*'
024900070712     c                   else
025000070712     c                   eval      ds01    = tbluni
025100070712     c                   eval      istcmud = §01age
025200070712     c                   endif
025300070712     c                   endif
025400070712
025500070712      * albero commerciale unificante
025600070712     c                   movel     istcmu        w0030
025700070712     c     w0030         chain     azorg01l
025800070712
025900070712      * distretto
026000070712     c                   eval      istdis = orgfl3
026100070712     c                   eval      kcod = '17'
026200070712     c                   eval      kkey = istdis
026300070712     c     ktabel        chain     tabel00f
026400070712     c                   if        not %found(tabel00f) or tblflg <> *blanks
026500070712     c                   eval      istdisd = *all'*'
026600070712     c                   else
026700070712     c                   eval      istdisd = tbluni
026800070712     c                   endif
026900070712      * area
027000070712     c                   eval      istcar = orgcar
027100070712     c                   eval      kcod = '05'
027200070712     c                   movel     istcar        kkey
027300070712     c     ktabel        chain     tabel00f
027400070712     c                   if        not %found(tabel00f) or tblflg <> *blanks
027500070712     c                   eval      istcard = *all'*'
027600070712     c                   else
027700070712     c                   eval      istcard = tbluni
027800070712     c                   endif
027900070712      * filiale
028000070712     c                   eval      istfil = orgfil
028100070712     c                   if        not %found(azorg01l) or orgfva <> *blanks
028200070712     c                   eval      istfild = *all'*'
028300070712     c                   else
028400070712     c                   eval      istfild = orgdes
028500070712     c                   endif
028600070712
028700070712      * imposto i dati relativi al fatturato
028800070712     c                   eval      istdip = dsdip
028900070712     c                   eval      istdfp = dsdfp
029000070712     c                   clear                   tise70ds
029100070712     c                   movel     'S'           i70tla
029200070712     c                   z-add     istksc        i70ksc
029300070712     c                   move      istctr        i70ctr
029400070712     c                   z-add     istdip        i70dai
029500070712     c                   z-add     istdfp        i70daf
029600070712     c                   call      'TISE70R'
029700070712     c                   parm                    tise70ds
029800070712     c                   z-add     o70ric        istfat
029900070712     c                   z-add     o70rir        istrir
030000070712     c                   z-add     o70rip        istrip
030100070712      * scrivo file
030200070712     c                   add       1             tottar
030300070712     c                   write     wfist000
030400070712
030500070712     c                   endsr
030600070712
030700070712      *--------------------------------------------------------------*
030800070712      * ROUTINE INIZIALE
030900070712      *--------------------------------------------------------------*
031000070712     c     *inzsr        begsr
031100070712
031200070712     c     *entry        plist
031300070712     c                   parm                    kpjba
031400070712     c                   movel     kpjbu         tnta79ds
031500070712
031600070712     c     *dtaara       define    §azute        azuteds
031700070712     c     *dtaara       define    §datiute      ddatiute
031800070712
031900070712     c                   in(e)     *dtaara
032000070712     c                   if        %error or rsut = *blanks
032100070712     c                   clear                   tibs34ds
032200070712     c                   call      'TIBS34R'
032300070712     c                   parm                    tibs34ds
032400070712     c                   in        *dtaara
032500070712     c                   endif
032600070712
032700101015      /free
032800101015       //?Carico scatti ISTAT
032900101015         SISsca = 0;
033000101015         clear DIA;
033100101015         setll (SISsca) TISIS01L;
033200101015
033300101015         DOW  not $EoF;
033400101015
033500101015           read TISIS01L;
033600101015
033700101015           IF  %Eof(TISIS01L);
033800101015             $EoF = *on;
033900101015             leave;
034000101015           ENDIF;
034100101015
034200101015           DIA(SISsca) = SISdata;
034300101015
034400101015         ENDDO;
034500101015
034600101015      /end-free
034700070712
034800070712      * se s.i. di prova imposto la libreria di prova
034900070712     c                   if        %subst(knsif:7:1) = 'P'
035000070712     c                   eval      wlib = 'GAITRAAZP '
035100070712     c                   else
035200070712     c                   eval      wlib = 'GAITRAAZM '
035300070712     c                   endif
035400121128
035500121128      * se lavoro lanciato da TISE01R imposto il wrkf a WFIST10F
035600121128      * altrimenti lo imposto a WFIST00F
035700121128     c                   if        ds10F = 'S'
035800121128     c                   eval      wLibWF = %trim(wLib) + '/WFIST10F'
035900121128     c                   else
036000121128     c                   eval      wLibWF = %trim(wLib) + '/WFIST00F'
036100121128     c                   endif
036200070712
036300070712      * pulisco il file
036400121128     ***c                   clear                   comman
036500121128     ***c                   movel(p)  cmd(1)        comman
036600121128     ***c                   eval      %subst(comman:13:9) = wlib
036700121128     c                   eval      Comman = %trimr(Cmd(1)) +
036800121128     c                                      %trimr(wLibWF) + ')'
036900070712     c                   call      'QCMDEXC'
037000070712     c                   parm                    comman
037100070712     c                   parm                    lenght
037200070712
037300070712     c                   open      wfist00f
037400070712
037500070712      * reperisco data e ora del lancio
037600070712     c                   time                    w0140
037700070712     c                   move      w0140         wdata
037800070712     c                   movel     w0140         wora
037900070712
038000070712     c     kazorg        klist
038100070712     c                   kfld                    dutdis
038200070712     c                   kfld                    kcar
038300070712
038400070712     c     kcli          klist
038500070712     c                   kfld                    kkut
038600070712     c                   kfld                    kkcc
038700070712     c                   kfld                    tamksc
038800070712
038900070712     c     ktabel        klist
039000070712     c                   kfld                    codut
039100070712     c                   kfld                    kcod
039200070712     c                   kfld                    kkey
039300070712
039400070712     c                   endsr
039500121128
039600121128** - Cmd - Comandi ------------------------------------------------------------*
039700121128CLRPFM FILE(
