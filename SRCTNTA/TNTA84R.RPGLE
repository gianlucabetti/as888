000100070319     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200070319
000300070319      *------------------------------------------------------------------------*
000400070319      *
000500070319      *                 GESTIONE STAMPA LUOGHI             ?
000600070319      *
000700070319      *------------------------------------------------------------------------*
000800070320     fazorg01l  if   e           k disk
000900070320     ftabel00f  if   e           k disk
001000070319     ftnta84d   cf   e             workstn
001100070319
001200070319      *------------------------------------------------------------------------*
001300070319      *  RIEPILOGO INDICATORI
001400070319      *------------------------------------------------------------------------*
001500070319      * 01 - utente non abilitato - esce dal pgm
001600070320      * 12 - utente di sede
001700070320      * 40 - filiale
001800070320      * 41 - luogo
001900070320      * 42 - cliente
002000070320      * 43 - selezione
002100070319      *------------------------------------------------------------------------*
002200070319
002300070319      *   V A R I A B I L I
002400070319     d wabi            s                   like(UteAut)
002500070319     d w003a           s              3
002600070319
002700070319      *   S C H I E R E
002800070319     d msg             s             78    dim(10) ctdata perrcd(1)
002900070319     d skpog           s              3    dim(250) inz(*zeros)
003000070319
003100070319      *   D S   I N T E R N E / E S T E R N E
003200070320     d param85         ds
003300070320     d  ta85fil                       3  0
003400070326     d  ta85luo                       3
003500070320     d  ta85ksc                       7  0
003600070320     d  ta85sel                       1
003700070320     d  ta85cgis                      1
003800070320     d  ta85abi                       2
003900070320
004000070319     d azuteds       e ds                  extname(azute00f)
004100070319     d ddatiute      e ds
004200070319     d dlat          e ds
004300070319     d ds4l          e ds
004400070319     d ds_cnaco      e ds                  inz  extname(cnaco00f)
004500070319     d ds_cnind      e ds                  inz  extname(cnind00f)
004600070319     d ds_cnclp      e ds                  inz  extname(cnclp00f)
004700070319     d ds_fncls      e ds                  inz  extname(fncls00f)
004800070319     d dute01        e ds
004900070319     d kpjba         e ds
005000070319     d tibs02ds      e ds
005100070319     d tibs34ds      e ds
005200070319     d tibs69ds      e ds
005300070319     d trul31ds      e ds
005400070319
005500070319      *   C O S T A N T I
005600070319
005700070319      *------------------------------------------------------------------------*
005800070319
005900070319     c                   exsr      sr_puld01
006000070319
006100070319     c                   do        *hival
006200070319     c                   exfmt     ta84d01
006300070319     c                   eval      *in28 = *off
006400070319     c                   clear                   v1cmsg
006500070319
006600070319      * f3=fine  ?
006700070319     c                   if        *inkc or
006800070319      * 01=utente non abilitato
006900070319     c                             *in01
007000070319     c                   leave
007100070319     c                   endif
007200070319
007300070319      * controllo  ?
007400070319     c                   exsr      sr_ctrd01
007500070319     c   28              iter
007600070319
007700070319      * f6=stampa    ?
007800070319     c                   if        *inkf
007900070319     c                   exsr      sr_stampa
008000070319     c                   leave
008100070319     c                   endif
008200070319
008300070319     c                   enddo
008400070319
008500070319     c                   eval      *inlr = *on
008600070319
008700070319      *------------------------------------------------------------------------*
008800070319      * pulizia videata
008900070319      *------------------------------------------------------------------------*
009000070319     c     sr_puld01     begsr
009100070319
009200070320     c  n12              eval      v1cfil = dutpou
009300070320     c  n12              eval      v1dfil = dutdpo
009400070320     c   12              eval      v1cfil = 999
009500070320     c   12              eval      v1dfil = 'Filiali abilitate'
009600070319     c                   clear                   v1cluo
009700070319     c                   clear                   v1dluo
009800070319     c                   clear                   v1cksc
009900070319     c                   clear                   v1dksc
010000070320     c                   clear                   v1csel
010100070320     c                   clear                   v1cgis
010200070319
010300070319     c                   endsr
010400070319
010500070319      *------------------------------------------------------------------------*
010600070319      * controllo la videata
010700070319      *------------------------------------------------------------------------*
010800070319     c     sr_ctrd01     begsr
010900070319
011000070319     c                   eval      *in40 = *off
011100070319     c                   eval      *in41 = *off
011200070320     c                   eval      *in42 = *off
011300070320     c                   eval      *in43 = *off
011400070320     c                   clear                   v1dfil
011500070319     c                   clear                   v1dluo
011600070319     c                   clear                   v1dksc
011700070320
011800070320      * controllo filiale
011900070320     c                   if        v1cfil = *zeros
012000070320     c                   eval      *in28 = *on
012100070320     c                   eval      *in40 = *on
012200070320     c                   eval      v1cmsg = msg(02)
012300070320     c                   leavesr
012400070320     c                   endif
012500070320      * tutte le filiali abilitate
012600070320     c                   if        v1cfil = 999
012700070320     c                   eval      v1dfil = 'Filiali abilitate'
012800070320     c                   endif
012900070320      * controllo
013000070320     c                   if        v1cfil <> 999
013100070320     c     v1cfil        chain     azorg01l
013200070320     c                   if        not %found(azorg01l) or orgfva <> *blanks or
013300070320     c                             (orgfag <> 'F' and orgfag <> 'A')
013400070320     c                   eval      *in28 = *on
013500070320     c                   eval      *in40 = *on
013600070320     c                   eval      v1cmsg = msg(03)
013700070320     c                   leavesr
013800070320     c                   endif
013900070320      * decodifico
014000070320     c                   eval      v1dfil = orgdes
014100070320      * abilitata
014200070320     c                   movel     v1cfil        w003a
014300070320     c     w003a         lookup    skpog                                  30
014400070320     c                   if        not *in30
014500070320     c                   eval      *in28 = *on
014600070320     c                   eval      *in40 = *on
014700070320     c                   eval      v1cmsg = msg(03)
014800070320     c                   leavesr
014900070320     c                   endif
015000070320     c                   endif
015100070319
015200070319      * almeno una scelta
015300070326     c                   if        v1cluo = *blanks and v1cksc = *zeros and
015400070320     c                             v1csel = *blanks
015500070319     c                   eval      *in28 = *on
015600070320     c                   eval      *in41 = *on
015700070320     c                   eval      v1cmsg = msg(04)
015800070319     c                   leavesr
015900070319     c                   endif
016000070319
016100070319      * controllo luogo
016200070326     c                   if        v1cluo <> *blanks
016300070319     c                   clear                   ds4l
016400070319     c                   eval      tblcod = '4L'
016500070320     c                   movel     v1cluo        tblkey
016600070319     c     ktabel        chain     tabel00f
016700070319if  2c                   if        not %found(tabel00f) or
016800070319     c                             tblflg <> *blanks
016900070319     c                   eval      *in28 = *on
017000070320     c                   eval      *in41 = *on
017100070320     c                   eval      v1cmsg = msg(05)
017200070319     c                   leavesr
017300070319e   2c                   endif
017400070319     c                   eval      ds4l = tbluni
017500070319if  2c                   if        not *in12 and §4lufi <> 'S'
017600070319     c                   eval      *in28 = *on
017700070320     c                   eval      *in41 = *on
017800070320     c                   eval      v1cmsg = msg(05)
017900070319     c                   leavesr
018000070319e   2c                   endif
018100070319      * decodifico
018200070319     c                   eval      v1dluo = §4ldes
018300070319e   1c                   endif
018400070319
018500070319      * controllo cliente
018600070319     c                   if        v1cksc <> *zeros
018700070319     c                   clear                   tibs69ds
018800070319     c                   eval      i69kac = v1cksc
018900070319     c                   call      'TIBS69R'
019000070319     c                   parm                    tibs69ds
019100070319     c                   parm                    ds_cnaco
019200070319     c                   parm                    ds_cnind
019300070319     c                   parm                    ds_cnclp
019400070319     c                   parm                    ds_fncls
019500070319if  1c                   if        o69err =  *on
019600070319     c                   eval      *in28 = *on
019700070320     c                   eval      *in42 = *on
019800070320     c                   eval      v1cmsg = msg(06)
019900070319     c                   leavesr
020000070319     c                   endif
020100070319      * decodifico
020200070319     c                   eval      v1dksc = acorag
020300070319      * controllo se gestibile dall'utente
020400070319     c                   movel     v1cksc        w003a
020500070319     c     w003a         lookup    skpog                                  30
020600070320     c                   if        not *in30
020700070319     c                   eval      *in28 = *on
020800070320     c                   eval      *in42 = *on
020900070320     c                   eval      v1cmsg = msg(06)
021000070319     c                   leavesr
021100070320     c                   endif
021200070319     c                   endif
021300070321
021400070321      * se inserito un cliente non devo impostare nessuna selezione
021500070321     c                   if        v1cksc <> *zeros and
021600070321     c                             (v1csel <> *blanks or v1cgis <> *blanks)
021700070321     c                   eval      *in28 = *on
021800070321     c                   eval      *in43 = *on
021900070321     c                   eval      v1cmsg = msg(09)
022000070321     c                   leavesr
022100070321     c                   endif
022200070320
022300070321      * se richiesto luogo 300 devo fare una selezione di stampa
022400070326     c                   if        v1cluo = '300' and v1csel = *blanks
022500070326     c                             and v1cksc = *zeros
022600070320     c                   eval      *in28 = *on
022700070320     c                   eval      *in43 = *on
022800070321     c                   eval      v1cmsg = msg(07)
022900070320     c                   leavesr
023000070320     c                   endif
023100070320
023200070321      * se non richiesto luogo 300 non devo fare una selezione di stampa
023300070326     c                   if        v1cluo <> '300' and v1cluo <> *zeros and
023400070320     c                             v1csel <> *blanks
023500070320     c                   eval      *in28 = *on
023600070320     c                   eval      *in43 = *on
023700070320     c                   eval      v1cmsg = msg(08)
023800070320     c                   leavesr
023900070320     c                   endif
024000070319
024100070319     c                   endsr
024200070319
024300070319      *------------------------------------------------------------------------*
024400070319      * stampo
024500070319      *------------------------------------------------------------------------*
024600070319     c     sr_stampa     begsr
024700070320
024800070320     c                   clear                   param85
024900070320     c                   eval      ta85fil = v1cfil
025000070320     c                   eval      ta85luo = v1cluo
025100070320     c                   eval      ta85ksc = v1cksc
025200070320     c                   eval      ta85sel = v1csel
025300070320     c                   eval      ta85cgis = v1cgis
025400070320     c                   eval      ta85abi = wabi
025500070320     c                   eval      kpjbu = param85
025600070320     c                   eval      kcoaz = 'TA85'
025700070320     c**!!!              call      'BCH10'
025800070320     c                   call      'TNTA85R'
025900070320     c                   parm                    kpjba
026000070319
026100070319     c                   endsr
026200070319
026300070319      *------------------------------------------------------------------------*
026400070319      * routine iniziale
026500070319      *------------------------------------------------------------------------*
026600070319     c     *inzsr        begsr
026700070319
026800070319     c     *entry        plist
026900070319     c                   parm                    kpjba
027000070319
027100070319     c     *dtaara       define    §azute        azuteds
027200070319     c     *dtaara       define    §datiute      ddatiute
027300070319     c                   in(e)     *dtaara
027400070319     c                   if        %error or rsut = *blanks
027500070319     c                   clear                   tibs34ds
027600070319     c                   call      'TIBS34R'
027700070319     c                   parm                    tibs34ds
027800070319     c                   in        *dtaara
027900070319     c                   endif
028000070319
028100070319     c                   clear                   wabi
028200070319     c                   clear                   dlat
028300070319
028400070319      * verifica errori e autorità profilo
028500070319     c                   select
028600070319      * se ho errori nei dati utente esco dal pgm
028700070319     c                   when      duterr = 'E'
028800070319     c                   eval      *in01 = *on
028900070319     c                   eval      *in28 = *on
029000070319     c                   eval      v1cmsg = msg(01)
029100070319     c                   leavesr
029200070319      * se non c'è l'abilitazione
029300070319      * --> se 1° livello, abilitazioni al terminal
029400070319      *     se 2° livello, abilitazioni al punto operativo
029500070319      *     se sede è impossibile ma se capita mando a fine il pgm
029600070319     c                   when      uteaut = *blanks
029700070319     c                   if        dutlpo = '1'
029800070319     c                   eval      wabi   = 'TP'
029900070319     c                   endif
030000070319     c                   if        dutlpo = '2'
030100070319     c                   eval      wabi   = 'PO'
030200070319     c                   endif
030300070319     c                   if        dutlpo = 'S'
030400070319     c                   eval      *in01 = *on
030500070319     c                   eval      *in28 = *on
030600070319     c                   eval      v1cmsg = msg(01)
030700070319     c                   leavesr
030800070319     c                   endif
030900070319      * carica le abilitazioni del profilo
031000070319     c                   other
031100070319     c                   movel     utefaf        dute01
031200070319     c                   if        §utecli <> *blanks
031300070319     c                   eval      wabi = §utecli
031400070319     c                   else
031500070319     c                   eval      wabi = uteaut
031600070319     c                   endif
031700070319     c                   endsl
031800070319
031900070319      * controllo se ok l'abilitazione dell'utente
032000070319     c                   clear                   tibs02ds
032100070319     c                   eval      t02mod = 'C'
032200070319     c                   eval      t02sif = knsif
032300070319     c                   eval      t02cod = 'LAT'
032400070319     c                   movel(p)  wabi          t02ke1
032500070319     c                   call      'TIBS02R'
032600070319     c                   parm                    kpjba
032700070319     c                   parm                    tibs02ds
032800070319     c                   if        t02err = *blanks
032900070319     c                   eval      dlat = t02uni
033000070319     c                   endif
033100070319      * errore o non abilitato
033200070319     c                   if        t02err <> *blanks or §latabi = 'S'
033300070319     c                   eval      *in01 = *on
033400070319     c                   eval      *in28 = *on
033500070319     c                   eval      v1cmsg = msg(01)
033600070319     c                   leavesr
033700070319     c                   endif
033800070319
033900070319      * reperimento delle filiali abilitate per l'utente
034000070319     c                   clear                   trul31ds
034100070319     c                   eval      i31abi = wabi
034200070319     c                   eval      i31cdi = dutdis
034300070319     c                   eval      i31car = dutare
034400070319     c                   eval      i31cpo = dutpou
034500070319     c                   call      'TRUL31R'
034600070319     c                   parm                    kpjba
034700070319     c                   parm                    trul31ds
034800070319     c                   if        o31pog > *zeros
034900070319     c                   movea     o31pog        skpog
035000070319     c                   else
035100070319     c                   eval      *in01 = *on
035200070319     c                   eval      *in28 = *on
035300070319     c                   eval      v1cmsg = msg(01)
035400070319     c                   leavesr
035500070319     c                   endif
035600070319
035700070319      * controllo se sono in sede
035800070319     c                   if        simfel = *zeros
035900070319     c                   eval      *in12 = *on
036000070319     c                   endif
036100070319
036200070319     c     ktabel        klist
036300070319     c                   kfld                    tblkut
036400070320     c                   kfld                    tblcod
036500070319     c                   kfld                    tblkey
036600070319
036700070319     c                   eval      tblkut = 1
036800070319
036900070319     c                   endsr
037000070319
037100070319** MSG  Lungh. 78                                                            *
037200070319Utente non autorizzato                                                        01
037300070320Immettere la filiale                                                          02
037400070320Filiale errata o non gestibile                                                03
037500070320Effettuare una scelta                                                         04
037600070320Codice luogo errato o non utilizzabile                                        05
037700070320Cliente errato o non gestibile                                                06
037800070320Immettere il tipo di selezione                                                07
037900070320Tipo selezione ammesso solo per luogo 300                                     08
038000070321Immesso il cliente le selezioni per il luogo 300 sono in alternativa          09
