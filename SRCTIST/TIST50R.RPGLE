000100060315     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200060315
000300060315      *------------------------------------------------------------------------*
000400060315      * VERIFICA/AGGIORNA CODICI IMPORTANZA CLIENTI
000500060315      *------------------------------------------------------------------------*
000600060315
000700060315     ftist50d   cf   e             workstn
000800060315     fazorg01l  if   e           k disk
000900060315
001000060315      *------------------------------------------------------------------------*
001100060315      *  RIEPILOGO INDICATORI
001200060315      *------------------------------------------------------------------------*
001300060316      * 28 - emette msg.
001400060316      * 40 - fatturato cat.clienti
001500060316      * 41 - mm/aaaa inizio
001600060316      * 42 - mm/aaaa fine
001700060614      * 43 - stampa variati
001800060315      *------------------------------------------------------------------------*
001900060315
002000060315      *   V A R I A B I L I
002100060315     d dataiso1        s               d   datfmt(*iso)
002200060315     d dataiso2        s               d   datfmt(*iso)
002300060315     d wmesi           s              2  0
002400060315
002500060315      *   S C H I E R E
002600060315     d msg             s             78    dim(20) ctdata perrcd(1)
002700060315
002800060315      *   D S   I N T E R N E / E S T E R N E
002900060315     d                 ds
003000060315     d  waa1                   1      4  0
003100060315     d  wmm1                   5      6  0
003200060315     d  wgg1                   7      8  0
003300060315     d  wdata1                 1      8  0
003400060315
003500060315     d                 ds
003600060315     d  wgg2                   1      2  0
003700060315     d  wmm2                   3      4  0
003800060315     d  waa2                   5      8  0
003900060315     d  wdata2                 1      8  0
004000060315
004100060315     d wlbdat          ds
004200060315     d  g08dat                 1      8  0
004300060315     d  g08inv                 9     16  0
004400060315     d  g08err                17     17
004500060315     d  g08tgi                18     22  0
004600060315
004700060315     d azuteds       e ds                  extname(azute00f)
004800060315     d ddatiute      e ds
004900060315     d kpjba         e ds
005000060315     d og143         e ds
005100060315     d tibs34ds      e ds
005200060315     d tist50ds      e ds
005300060315
005400060315      *------------------------------------------------------------------------*
005500060315
005600060316     c                   exsr      sr_inzvid
005700060315
005800060315      * emetto videata
005900060315     c                   do        *hival
006000060315
006100060315     c                   exfmt     st50d01
006200060315     c                   eval      *in28 = *off
006300060315     c                   clear                   vidmsg
006400060315
006500060315      * f3=fine
006600060315     c   kc              leave
006700060315
006800060315      * controllo i dati immessi
006900060316     c                   exsr      sr_ctrvid
007000060315     c   28              iter
007100060315
007200060315      * f6=lancio
007300060315     c                   if        *inkf
007400060316     c                   exsr      sr_lancio
007500060315     c                   leave
007600060315     c                   endif
007700060315
007800060315     c                   enddo
007900060315
008000060315     c                   eval      *inlr = *on
008100060315
008200060315      *------------------------------------------------------------------------*
008300060316      *  imposto i dati di dft nella videata di lancio
008400060315      *------------------------------------------------------------------------*
008500060316     c     sr_inzvid     begsr
008600060315
008700060315      * fatturati per categoria cliente
008800060315      * direzionali
008900110613     c**!!!              eval      vididd = 1300
009000110613     c                   eval      vididd = 1292
009100060315     c                   eval      vidida = 9999
009200060315      * top
009300060315     c                   eval      viditd = 250
009400110613     c**!!!              eval      vidita = 1300
009500110613     c                   eval      vidita = 1292
009600060315      * A
009700060315     c                   eval      vidiad = 65
009800060315     c                   eval      vidiaa = 250
009900060315      * B
010000060315     c                   eval      vidibd = 15
010100060315     c                   eval      vidiba = 65
010200060315      * C
010300060315     c                   eval      vidicd = 0
010400060315     c                   eval      vidica = 15
010500060315
010600060315      * periodo da controllare
010700060315      * calcolo mese/anno corrente - 1 anno
010800060315     c                   move      *date         dataiso1
010900060315     c     dataiso1      subdur    1:*y          dataiso2
011000060315     c                   move      dataiso2      wdata1
011100060315     c                   movel     wmm1          vidmai
011200060315     c                   move      waa1          vidmai
011300060315      * calcolo mese/anno corrente -1 mese
011400060315     c                   subdur    1:*m          dataiso1
011500060315     c                   move      dataiso1      wdata1
011600060315     c                   movel     wmm1          vidmaf
011700060315     c                   move      waa1          vidmaf
011800060315
011900060315      * ultimi mesi fatturazione da controllare
012000060315     c                   eval      vidumes = 3
012100060315
012200060315      * aggiorna categoria clienti
012300060315     c                   eval      vidagg = 'N'
012400060315
012500060315      * stampa solo variazioni
012600060315     c                   eval      vidvar = 'S'
012700060614      * escludi bloccati senza fatturato nella stampa
012800060614     c                   eval      vidblc = 'N'
012900060315
013000060315     c                   endsr
013100060315
013200060315      *------------------------------------------------------------------------*
013300060315      * controllo i dati del video
013400060315      *------------------------------------------------------------------------*
013500060316     c     sr_ctrvid     begsr
013600060315
013700060315     c                   setoff                                       404142
013800060331     c                   setoff                                       43
013900060315
014000060315     c                   clear                   tist50ds
014100060315
014200060315      * controllo i fatturati massimi
014300060315     c                   if        vidida > vididd and vididd >= vidita and
014400060315     c                             vidita > viditd and viditd >= vidiaa and
014500060315     c                             vidiaa > vidibd and vidibd >= vidica and
014600060315     c                             vidica > vidicd and vidicd >= 0
014700060315     c                   else
014800060315     c                   eval      *in28 = *on
014900060315     c                   eval      *in40 = *on
015000060315     c                   eval      vidmsg = msg(01)
015100060315     c                   leavesr
015200060315     c                   endif
015300060315
015400060315      * controllo il periodo di fatturazione
015500060315      * periodo dal
015600060315      * obbligatorio
015700060315     c                   if        vidmai = *zeros
015800060315     c                   eval      *in28 = *on
015900060315     c                   eval      *in41 = *on
016000060315     c                   eval      vidmsg = msg(02)
016100060315     c                   leavesr
016200060315     c                   endif
016300060315      * valido
016400060315     c                   clear                   wlbdat
016500060315     c                   z-add     vidmai        g08dat
016600060315     c                   if        vidmai > 9999
016700060315     c                   add       01000000      g08dat
016800060315     c                   else
016900060315     c                   add       00010000      g08dat
017000060315     c                   endif
017100060315     c                   call      'XSRDA8'
017200060315     c                   parm                    wlbdat
017300060315     c                   if        g08err = *on
017400060315     c                   eval      *in28 = *on
017500060315     c                   eval      *in41 = *on
017600060315     c                   eval      vidmsg = msg(02)
017700060315     c                   leavesr
017800060315     c                   endif
017900060315     c                   z-add     g08dat        wdata2
018000060315     c                   move      wdata2        vidmai
018100060315     c                   z-add     g08inv        wdata1
018200060315     c                   movel     wdata1        i50ami
018300060315      * periodo al
018400060315      * obbligatorio
018500060315     c                   if        vidmaf = *zeros
018600060315     c                   eval      *in28 = *on
018700060315     c                   eval      *in42 = *on
018800060315     c                   eval      vidmsg = msg(02)
018900060315     c                   leavesr
019000060315     c                   endif
019100060315      * valido
019200060315     c                   clear                   wlbdat
019300060315     c                   z-add     vidmaf        g08dat
019400060315     c                   if        vidmaf > 9999
019500060315     c                   add       01000000      g08dat
019600060315     c                   else
019700060315     c                   add       00010000      g08dat
019800060315     c                   endif
019900060315     c                   call      'XSRDA8'
020000060315     c                   parm                    wlbdat
020100060315     c                   if        g08err = *on
020200060315     c                   eval      *in28 = *on
020300060315     c                   eval      *in42 = *on
020400060315     c                   eval      vidmsg = msg(02)
020500060315     c                   leavesr
020600060315     c                   endif
020700060315     c                   z-add     g08dat        wdata2
020800060315     c                   move      wdata2        vidmaf
020900060315     c                   z-add     g08inv        wdata1
021000060315     c                   movel     wdata1        i50amf
021100060315      * dal maggiore di al
021200060315     c                   if        i50ami > i50amf
021300060315     c                   eval      *in28 = *on
021400060315     c                   eval      *in41 = *on
021500060315     c                   eval      *in42 = *on
021600060315     c                   eval      vidmsg = msg(03)
021700060315     c                   leavesr
021800060315     c                   endif
021900060315      * il periodo da elaborare deve essere di 12 mesi
022000060315     c                   movel     i50ami        wdata1
022100060315     c                   move      '01'          wdata1
022200060315     c                   move      wdata1        dataiso1
022300060315     c                   movel     i50amf        wdata1
022400060315     c                   move      '01'          wdata1
022500060315     c                   move      wdata1        dataiso2
022600060315     c                   adddur    1:*m          dataiso2
022700060315     c     dataiso2      subdur    dataiso1      wmesi:*m
022800060315     c                   if        wmesi <> 12
022900060315     c                   eval      *in28 = *on
023000060315     c                   eval      *in41 = *on
023100060315     c                   eval      *in42 = *on
023200060315     c                   eval      vidmsg = msg(04)
023300060315     c                   leavesr
023400060315     c                   endif
023500060316
023600060614      * se richiesto aggiornamento non devo fare selezioni per stampare
023700060614      * solo le categorie variate
023800060331     c                   if        vidagg = 'S' and vidvar <> 'S'
023900060316     c                   eval      *in28 = *on
024000060331     c                   eval      *in43 = *on
024100060331     c                   eval      vidmsg = msg(05)
024200060316     c                   leavesr
024300060316     c                   endif
024400060315
024500060315     c                   endsr
024600060315
024700060315      *------------------------------------------------------------------------*
024800060315      * lancio la stampa/aggiornamento
024900060315      *------------------------------------------------------------------------*
025000060316     c     sr_lancio     begsr
025100060315
025200060315     c                   eval      i50idd = vididd
025300060315     c                   eval      i50ida = vidida
025400060315     c                   eval      i50itd = viditd
025500060315     c                   eval      i50ita = vidita
025600060315     c                   eval      i50iad = vidiad
025700060315     c                   eval      i50iaa = vidiaa
025800060315     c                   eval      i50ibd = vidibd
025900060315     c                   eval      i50iba = vidiba
026000060315     c                   eval      i50icd = vidicd
026100060315     c                   eval      i50ica = vidica
026200060315     c                   eval      i50umes = vidumes
026300060315     c                   eval      i50agg = vidagg
026400060315     c                   eval      i50var = vidvar
026500060614     c                   eval      i50blc = vidblc
026600060315
026700060315     c                   eval      kcoaz = 'ST51'
026800060315     c                   eval      kpjbu = tist50ds
026900060503     c                   call      'BCH09'
027000060503     c                   parm                    kpjba
027100060315     c                   call      'BCH10'
027200060315     c                   parm                    kpjba
027300060315
027400060315     c                   endsr
027500060315
027600060315      *------------------------------------------------------------------------*
027700060315      * routine iniziale
027800060315      *------------------------------------------------------------------------*
027900060316     c     *inzsr        begsr
028000060315
028100060316     c     *entry        plist
028200060316     c                   parm                    kpjba
028300060315
028400060315     c     *dtaara       define    §azute        azuteds
028500060315     c     *dtaara       define    §datiute      ddatiute
028600060315     c                   in(e)     *dtaara
028700060315     c                   if        %error  or rsut = *blanks
028800060315     c                   clear                   tibs34ds
028900060315     c                   call      'TIBS34R'
029000060315     c                   parm                    tibs34ds
029100060315     c                   in        *dtaara
029200060315     c                   endif
029300060315
029400060315     c                   endsr
029500060315      *------------------------------------------------------------------------*
029600060315** msg
029700060315Fascie di fatturato non progressive.                                          01
029800060315Mese/Anno errati                                                              02
029900060315Il Mese/Anno iniziale non puo' essere maggiore del Mese/Anno finale           03
030000060315Il periodo da elaborare deve essere di 12 mesi                                04
030100060614Selezione valida solo se non è richiesto l'aggiornamento                      05
