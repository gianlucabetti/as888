000100051201     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200051201
000300051201      *------------------------------------------------------------------------*
000400051201      *                                                                        *
000500051201      *                 GESTIONE CONTATTI                  ?                   *
000600051201      *                                                                        *
000700051201      *------------------------------------------------------------------------*
000800051201
000900051205     fTfntc00f  uf a e             disk    rename(tfntc:tfntc0)
001000051205     fTfntc01l  if   e           k disk    infds(ntcds)
001100051202     fTncpo01l  if   e           k disk
001200051201     fTabel00f  if   e           k disk
001300051206     fTnta12D   cf   e             workstn sfile(ta12s02:nrr1)
001400051206     f                                     sfile(ta12s03:nrr2)
001500051201
001600051201      *------------------------------------------------------------------------*
001700051201      *  RIEPILOGO INDICATORI
001800051201      *------------------------------------------------------------------------*
001900051201      * 01 - Pgm richiamato
002000051201      * 02 - Utente non abilitato
002100051201      * 03 - Cliente
002200051201      * 04 - Potenziale
002300051202      * 06 - Seconda parte della prima videata
002400051202      * 07 - Proteggo campo nota a video
002500140530      * 08 - Eccezione per la modifica del contatto in Rubrica
002600140530      *      (altrimenti SOLO visualizzabile)
002700051207      * 09 - Solo interrogazione
002800051207      * 10 - Sottolineo campo nota sel sbfl 02 x interrogazione
002900090915      * 11 - Richiamato per trattativa
003000051201      * 12 - Sede - N12 Filiale
003100051201      * 20 - GESTIONE SUBFILE
003200051201      * 21 - GESTIONE SUBFILE
003300051201      * 22 - GESTIONE SUBFILE
003400051201      * 28 - ERRORE GENERICO DSPF
003500051201      * 30 - Comodo
003600051202      * 31 - Fine File
003700051202      * 32 - Fine Subfile
003800051201      * 40 - Cliente/Potenziale/Visita
003900051201      * 41 - Contatto
004000051202      * 42 - Nota
004100051201      * 31 - Fine file
004200051201      * 90 - Riemetto la videata
004300051201      *------------------------------------------------------------------------*
004400051201
004500051201      *   V A R I A B I L I
004600051201     d comando         s              1
004700051202     d kapl            s                   like(ntcapl)
004800051202     d kcod            s                   like(tblcod)
004900051201     d kkey            s                   like(tblkey)
005000051201     d kkut            s                   like(tblkut) inz(1)
005100051202     d knk1            s                   like(ntcnk1)
005200051202     d knk2            s                   like(ntcnk2)
005300051202     d ktnt            s                   like(ntctnt)
005400051206     d nrr1            s                   like(recsf1)
005500051206     d nrr2            s                   like(recsf2)
005600051201     d ordina          s              1
005700051202     d riga            s                   like(§1trig)
005800051201     d tiporich        s              1    inz('C')
005900051202     d wcmm            s                   like(clpage)
006000051201     d wpos            s              2  0
006100051202     d wporagru        s              3
006200051201     d wtibs69         s              1    inz('0')
006300051202     d w003a           s              3
006400051202     d xx              s              3  0
006500051202     d yy              s              3  0
006600090406     d zz              s              3  0
006700051201
006800051201      *   S C H I E R E
006900051201     d msg             s             78    Dim(60) ctdata perrcd(1)
007000051206     d skceml          s              1    Dim(99)
007100051206     d skdtnt          s             25    Dim(99)
007200060329     d skmult          s              1    Dim(99)
007300051201     d sktnt           s              2    Dim(99)
007400051202     d sktnt1          s              2    Dim(99)
007500090406     d skord           s              5    dim(99)
007600090415     d skord1          s              5    dim(99)
007700051201
007800051201      *   D S   I N T E R N E / E S T E R N E
007900051205     d ntcds           ds
008000051205     d ntcnrr                397    400B 0
008100051202
008200051201     d                 ds
008300051202     d v1cnrv1                 1      6  0
008400051202     d v1cnrv2                 7      7  0
008500051202     d v1cnrv                  1      7  0
008600051202     d                 ds
008700051202     d v2cnrv1                 1      6  0
008800051202     d v2cnrv2                 7      7  0
008900051202     d v2cnrv                  1      7  0
009000051206     d                 ds
009100051206     d v3cnrv1                 1      6  0
009200051206     d v3cnrv2                 7      7  0
009300051206     d v3cnrv                  1      7  0
009400051201
009500051201     d                sds
009600051201     d  Vtcpgm                 1     10
009700051201
009800051201     d Azuteds       e ds                  Extname(Azute00f)
009900051201     d dDatiute      e ds
010000051202     d dsemail       e ds
010100051201     d ds1t          e ds
010200051201     d ds1u          e ds
010300051201     d ds_cnaco      e ds                  inz  extname(Cnaco00f)
010400051201     d ds_cnind      e ds                  inz  extname(Cnind00f)
010500051201     d ds_cnclp      e ds                  inz  extname(Cnclp00f)
010600051201     d ds_fncls      e ds                  inz  extname(Fncls00f)
010700051201     d Kpjba         e ds
010800051201     d Tnta12ds      e ds
010900051201     d Tibs02ds      e ds
011000051201     d Tibs34ds      e ds
011100051201     d Tibs69ds      e ds
011200090630     d tntaa1ds      e ds
011300051201
011400051201      *   C O S T A N T I
011500051201      * titolo videata (lunghezza massima 21)
011600051201     d VtcTit          C                   CONST('* GESTIONE CONTATTI *')
011700051201
011800051201      *------------------------------------------------------------------------*
011900051201
012000051202     c                   Clear                   v1cksc
012100051202     c                   Clear                   v1cpot
012200051202     c                   Clear                   v1cnrv
012300051202     c                   Clear                   v1dksc
012400051202     c                   Clear                   v1dpot
012500051202     c                   Clear                   v1dnrv
012600051202     c                   Clear                   v1ctnt
012700090707     c                   Clear                   v1dtnt
012800051202
012900051202      * Se richiamato imposto i dati e vado subito al subfile
013000060216      * se ho i dati validi altrimenti vado in seconda videata
013100051202if  1c                   If        *In01
013200051202     c                   Eval      v1capl = ta12apl
013300051202     c                   Eval      *In03 = (v1capl = 'C')
013400051202     c                   Eval      *In04 = (v1capl = 'P')
013500091112     c                   Eval      *In11 = (v1capl = 'T')
013600060216      * se non c'è la chiave impostata vado in seconda videata
013700060216     c                   If        *In03 and ta12ksc = *Zeros
013800060216     c                   Goto      vid02
013900060216     c                   EndIf
014000060216     c                   If        *In04 and ta12pot = *Zeros
014100060216     c                   Goto      vid02
014200060216     c                   EndIf
014300091112     c                   If        *In11 and ta12nrv = *Zeros
014400091112     c                   Goto      vid02
014500091112     c                   EndIf
014600051202     c   03              Eval      v1cksc = ta12ksc
014700051202     c   04              Eval      v1cpot = ta12pot
014800091112     c   11              Eval      v1cnrv = ta12nrv
014900051202     c                   Eval      v1ctnt = ta12tnt
015000051202      * Decodifico i dati passati
015100051202     c                   Eval      *In06 = *On
015200090701     c                   ExSr      Sr_Ctraut
015300100331     c                   IF        not *in02
015400100331     c                   ExSr      Sr_Ctrd01
015500100331     c                   ENDIF
015600090701     C
015700060215     c                   If        *In28
015800060215     c                   Eval      ta12err = '1'
015900060215     c                   Eval      ta12msg = v1cmsg
016000060215     c                   Goto      Fine
016100060215     c                   EndIf
016200051202     c                   Goto      carsfl
016300101019    1c                   EndIf
016400051201
016500051201      * --> Prima videata
016600051201     c     emid01        Tag
016700051201      * Pulisco la videata
016800051201     c                   If        Not *In28 and Not *In90
016900101019     c                   Setoff                                       0306
017000051201     c                   Eval      v1capl = 'C'
017100101020     c                   EndIf
017200051201
017300051202      * --> Seconda parte
017400051201     c     vid02         Tag
017500051202     c                   Eval      *In06 = *On
017600051201     c                   Eval      *In03 = (v1capl = 'C')
017700051201     c                   Eval      *In04 = (v1capl = 'P')
017800091112     c                   Eval      *In11 = (v1capl = 'T')
017900051202     c                   Clear                   v1cksc
018000051202     c                   Clear                   v1cpot
018100051202     c                   Clear                   v1cnrv
018200051202     c                   Clear                   v1dksc
018300051202     c                   Clear                   v1dpot
018400051202     c                   Clear                   v1dnrv
018500051202     c                   Clear                   v1ctnt
018600090707     c                   Clear                   v1dtnt
018700090701      * Controllo se utente abilitato
018800090701     c                   ExSr      Sr_Ctraut
018900051201
019000051201      * Emetto la videata
019100051201     c     emid02        Tag
019200051201     c                   Write     Ta12t01
019300051202     c                   Exfmt     Ta12d01
019400090630     c                   Eval      *In28 = *Off
019500090630     c                   Eval      *In90 = *Off
019600090630     c                   Clear                   v1cmsg
019700051201
019800051201      * F3=Fine
019900090701      * *IN02 - esco lo stesso dal pgm perchè utente non abilitato
020000090701     c   02
020100090701     cor kc              Goto      fine
020200051201
020300051201      * F12=Ritorno
020400051201     c                   If        *Inkl
020500060216      * se richiamato da gestione note esco
020600060216     c                   If        *in01 and ta12note = 'S'
020700060216     c                   Goto      fine
020800060216     c                   EndIf
020900051201     c                   Setoff                                       2890
021000051201     c                   Goto      emid01
021100051201     c                   EndIf
021200051201
021300051201      * Controllo i dati della videata
021400051202     c                   ExSr      Sr_Ctrd01
021500051201     c   28
021600051201     cor 90              Goto      emid02
021700051206
021800051206      * F10=Inserimento Multiplo E-Mail
021900051206     c                   If        *Inkj
022000051206     c                   ExSr      Sr_F10
022100051206     c                   Goto      emid02
022200051206     c                   EndIf
022300051201
022400051201      * --> Subfile
022500051201     c     Carsfl        Tag
022600051201     c                   ExSr      Sr_Pulsfl
022700051201     c                   ExSr      Sr_Carsfl
022800051201     c                   ExSr      Sr_Emisfl
022900051201
023000051206     c                   Goto      vid02
023100051201
023200051201     c     Fine          Tag
023300051201
023400051201      * Chiude i file dei pgm richiamati
023500051201     c                   If        wtibs69 = *On
023600051201     c                   Eval      i69tla = 'C'
023700051201     c                   Call      'TIBS69R'
023800051201     c                   Parm                    tibs69ds
023900051201     c                   EndIf
024000090630     C*
024100090630     C                   clear                   tntaa1ds
024200090630     C                   movel     'C'           Itaa1Tla
024300090630     C                   movel(p)  tntaa1ds      kpjbu
024400090703     C                   CALL      'TNTAA1C'
024500090630     C                   PARM                    KPJBA
024600090630     c
024700051201
024800051201     c                   Eval      *InLr = *On
024900051201
025000051201      *------------------------------------------------------------------------*
025100051201      * CONTROLLO LA PRIMA VIDEATA
025200051201      *------------------------------------------------------------------------*
025300051201     c     Sr_Ctrd01     BegSr
025400051202
025500051202      * Spengo indicatori di posizionamento cursore
025600051202     c                   Setoff                                       404143
025700051201
025800051202if  1c                   If        Not *In06
025900051202
026000051202   x1c                   Else
026100051202
026200051202      * --> Codice Cliente/Potenziale/Nr.Visita
026300051202      * Cliente
026400051202     c   03              ExSr      Sr_Ctrksc
026500051202      * Potenziale
026600051202     c   04              ExSr      Sr_Ctrpot
026700090915      * Trattativa
026800090915     c                   if        *in11
026900090915     c                   eval      v1dnrv = ta12rag
027000090915     c                   endif
027100051202
027200051202      * --> Tipo contatto
027300090707     c                   clear                   v1dtnt
027400051202if  2c                   If        v1ctnt <> *Blanks
027500051202     c                   ExSr      Sr_Ctrtnt
027600051202     c                   Else
027700051202     c                   Movea     sktnt         sktnt1
027800090415     c                   Movea     skord         skord1
027900051202    2c                   EndIf
028000051202
028100051202    1c                   EndIf
028200051201
028300051201     c                   EndSr
028400051201
028500051202      *------------------------------------------------------------------------*
028600051202      * CONTROLLO CLIENTE
028700051202      *------------------------------------------------------------------------*
028800051202     c     Sr_Ctrksc     BegSr
028900051202
029000051202     c                   If        v1cksc = *Zeros
029100051202     c                   Eval      *In28 = *On
029200051202     c                   Eval      *In40 = *On
029300051202     c                   Eval      v1cmsg = msg(04)
029400051202     c                   Leavesr
029500051202     c                   EndIf
029600051202
029700060216      * se richiamato da immissione cliente non controllo
029800060216     c                   If        *In01 and
029900060216     c                             ta12gcli = '1' and ta12icli = '1'
030000060217      * imposto la ragione sociale
030100060217     c                   Eval      v1dksc = ta12rag
030200060216     c                   goto      noksc
030300060216     c                   endif
030400060216
030500051202     c                   Clear                   Tibs69ds
030600051202     c                   Move      v1cksc        I69kac
030700051202     c                   Call      'TIBS69R'
030800051202     c                   Parm                    Tibs69ds
030900051202     c                   Parm                    ds_cnaco
031000051202     c                   Parm                    ds_cnind
031100051202     c                   Parm                    ds_cnclp
031200051202     c                   Parm                    ds_fncls
031300051202     c                   Eval      wtibs69 = *On
031400051202     c                   If        o69err = *Blanks
031500051202     c                   Eval      v1dksc = acorag
031600051202     c                   Else
031700051202     c                   Eval      *In28 = *On
031800051202     c                   Eval      *In40 = *On
031900051202     c                   Eval      v1cmsg = msg(04)
032000051202     c                   Leavesr
032100051202     c                   EndIf
032200060216     c     noksc         tag
032300051202
032400051202      * Controllo se utente può gestire il cliente
032500051207      * se non richiamato
032600051207if  1c                   If        Not *In01
032700090630     c                   clear                   tntaa1ds
032800090630     c                   eval      itaa1caut='§utecli'
032900090630     c                   eval      itaa1ksc=v1cksc
033000090630     c                   clear                   kpjbu
033100090630     c                   movel     tntaa1ds      kpjbu
033200090703     c                   call      'TNTAA1C'
033300090630     c                   parm                    kpjba
033400090630     c                   movel     kpjbu         tntaa1ds
033500090630     c
033600090630    1c                   if        otaa1fabi='N'
033700051202     c                   Eval      *In28 = *On
033800051202     c                   Eval      *In40 = *On
033900051202     c                   Eval      v1cmsg = msg(05)
034000051202     c                   Leavesr
034100051207    2c                   EndIf
034200051207    1c                   EndIf
034300051202
034400051202     c                   EndSr
034500051202
034600051202      *------------------------------------------------------------------------*
034700051202      * CONTROLLO POTENZIALE
034800051202      *------------------------------------------------------------------------*
034900051202     c     Sr_Ctrpot     BegSr
035000051202
035100051202     c                   If        v1cpot = *Zeros
035200051202     c                   Eval      *In28 = *On
035300051202     c                   Eval      *In40 = *On
035400051202     c                   Eval      v1cmsg = msg(06)
035500051202     c                   Leavesr
035600051202     c                   EndIf
035700051202
035800060216      * se richiamato da immissione cliente non controllo
035900060216     c                   If        *In01 and
036000060216     c                             ta12gcli = '1' and ta12icli = '1'
036100060217      * imposto la ragione sociale
036200060217     c                   Eval      v1dpot = ta12rag
036300060216     c                   goto      nopot
036400060216     c                   endif
036500060216
036600051202     c     v1cpot        Chain     Tncpo01l
036700051202     c                   If        %Found(Tncpo01l) and cpoatb = *Blanks
036800060217     c                   Eval      v1dpot = cporag
036900051202     c                   Else
037000051202     c                   Eval      *In28 = *On
037100051202     c                   Eval      *In40 = *On
037200051202     c                   Eval      v1cmsg = msg(06)
037300051202     c                   Leavesr
037400051202     c                   EndIf
037500060216     c     nopot         tag
037600051202
037700051202      * Controllo se utente può gestire il potenziale
037800100331      * se non richiamato
037900100331if  1c                   If        Not *In01
038000090630     c                   clear                   tntaa1ds
038100090702     c                   eval      itaa1cpo =cpocpo
038200090630     c                   eval      itaa1caut='§UTEPOT'
038300090630     c                   clear                   kpjbu
038400090630     c                   movel     tntaa1ds      kpjbu
038500090703     c                   call      'TNTAA1C'
038600090630     c                   parm                    kpjba
038700090630     c                   movel     kpjbu         tntaa1ds
038800090630     c
038900090630     c                   if        otaa1fabi='N'
039000051202     c                   Eval      *In28 = *On
039100051202     c                   Eval      *In40 = *On
039200051202     c                   Eval      v1cmsg = msg(07)
039300051202     c                   Leavesr
039400051202     c                   EndIf
039500100331     c                   endif
039600051202
039700051202     c                   EndSr
039800051202
039900051202      *------------------------------------------------------------------------*
040000051202      * CONTROLLO TIPO CONTATTO
040100051202      *------------------------------------------------------------------------*
040200051202     c     Sr_Ctrtnt     BegSr
040300051202
040400051202      * Ricerca
040500051202     c                   Eval      wpos = %scan('?':v1ctnt)
040600051202     c                   If        wpos > 0
040700051202     c                   Eval      kcod = '1T'
040800051202     c                   Call      'TRTB15R'
040900051202     c                   Parm                    kcod
041000051202     c                   Parm                    ordina
041100051202     c                   Parm                    tiporich
041200051202     c                   Parm                    kkey
041300051202     c                   Parm                    comando
041400051202     c                   Eval      v1ctnt = kkey
041500051202     c                   Eval      *In41 = *On
041600051202     c                   Eval      *In90 = *On
041700051202     c                   EndIf
041800051202
041900051202      * Controllo
042000051202     c                   Clear                   v1dtnt
042100051202     c                   Clear                   ds1t
042200051202     c                   Eval      kcod = '1T'
042300051202     c                   Eval      kkey = v1ctnt
042400051202     c     kTabel        Chain     Tabel00f
042500051202     c                   If        Not %Found(Tabel00f) or
042600051202     c                             tblflg <> *Blanks
042700051202     c                   Eval      *In28 = *On
042800051202     c                   Eval      *In41 = *On
042900051202     c                   Eval      v1cmsg = msg(10)
043000051202     c                   Leavesr
043100051202     c                   EndIf
043200051202
043300051202     c                   Eval      ds1t = tbluni
043400051202     c                   Eval      v1dtnt = §1tdes
043500051202     c                   Clear                   sktnt1
043600090415     c                   clear                   skord1
043700051202     c                   Eval      sktnt1(1) = v1ctnt
043800090415     c                   eval      %subst(skord1(1):1:3) = %editc(§1tprg:'X')
043900090415     c                   eval      %subst(skord1(1):4:2) = sktnt1(1)
044000051202
044100051202     c                   EndSr
044200051201
044300051201      *------------------------------------------------------------------------*
044400051201      * PULISCO IL SUBFILE
044500051201      *------------------------------------------------------------------------*
044600051201     c     Sr_Pulsfl     BegSr
044700051201
044800051206     c                   Clear                   nrr1
044900051202     c                   Clear                   v2cksc
045000051202     c                   Clear                   v2cpot
045100051202     c                   Clear                   v2cnrv
045200051201     c                   Eval      *In20 = *Off
045300051201     c                   Eval      *In21 = *Off
045400051202     c                   Write     ta12c02
045500051201     c                   Eval      *In21 = *On
045600051201     c                   Eval      *In20 = *On
045700051201
045800051206     c                   Eval      recsf1 = 1
045900051201     c                   Eval      *In31 = *Off
046000051201
046100051201     c                   EndSr
046200051201
046300051201      *------------------------------------------------------------------------*
046400051201      * CARICO IL SUBFILE
046500051201      *------------------------------------------------------------------------*
046600051201     c     Sr_Carsfl     BegSr
046700051202
046800051202      * Imposto i dati del control
046900051202     c                   Eval      v2cksc = v1cksc
047000051202     c                   Eval      v2cpot = v1cpot
047100051202     c                   Eval      v2cnrv = v1cnrv
047200051202     c                   Eval      v2dksc = v1dksc
047300051202     c                   Eval      v2dpot = v1dpot
047400051202     c                   Eval      v2dnrv = v1dnrv
047500051206
047600051206     c                   Eval      *In42 = *Off
047700051201
047800051202      * Imposto la chiave per tfntc
047900051202     c                   Eval      kapl = v1capl
048000051202     c                   Clear                   knk2
048100051202     c   03              Movel     dutkci        knk1
048200051202     c   03              Move      v2cksc        knk1
048300051202     c   04              Move      v2cpot        knk1
048400091112     c   11              Movel     dutkci        knk1
048500091112     c   11              Move      v2cnrv        knk1
048600051202
048700051202      * Carico il subfile (ciclo per i tipi contatti caricati in sk)
048800051202     c                   Eval      xx = 1
048900090406     c                   Eval      zz = 1
049000090406for 1c                   For       zz by 1 to 99
049100090415     c                   if        skord1(zz) = *blanks
049200090406     c                   iter
049300051202    2c                   EndIf
049400090406      * recupero la sk della nota
049500090415     c                   eval      xx = %lookup(%subst(skord1(zz):4:2):sktnt1)
049600051202     c                   Clear                   riga
049700051202      * Cerco i dati del tipo contatto in tabella 1T
049800051202     c                   Clear                   ds1t
049900051202     c                   Eval      kcod = '1T'
050000051202     c                   Eval      kkey = sktnt1(xx)
050100051202     c     kTabel        Chain     Tabel00f
050200051202if  2c                   If        Not %Found(Tabel00f) or tblflg <> *Blanks
050300051202     c                   Iter
050400051202    2c                   EndIf
050500051202     c                   Eval      ds1t = tbluni
050600051202     c                   Eval      v2nota = %subst(tblkey:1:2) + ' - ' + §1tdes
050700051202     c                   Clear                   v2email
050800051205     c                   Clear                   v2cann
050900051205     c                   Clear                   v2nrrr
051000051206     c                   Clear                   v2csns
051100090406     c                   clear                   v2prg
051200051205     c                   Eval      v2cmod = 'S'
051300051202     c                   Eval      *In07 = *On
051400051207     c                   Eval      *In10 = *Off
051500051206     c                   Add       1             nrr1
051600051202     c                   Write     ta12s02
051700051202     c                   Eval      ktnt = sktnt1(xx)
051800051202     c     kTfntc        Setll     Tfntc01l
051900051202      * Trovo il contatto lo scrivo nel subfile
052000051202if  2c                   If        %Equal
052100051202do  3c                   Do        *Hival
052200051205     c     kTfntc        Reade     Tfntc01l
052300051202     c                   If        %Eof(Tfntc01l)
052400051202     c                   Leave
052500051202     c                   EndIf
052600051202     c                   Eval      v2nota = ntcrnt
052700051202     c                   Eval      v2email = §1teml
052800051205     c                   Eval      v2ctnt = ntctnt
052900051205     c                   Eval      v2cann = ntcflt
053000051205     c                   Eval      v2nrrr = ntcnrr
053100051206     c                   Eval      v2csns = ntcsns
053200090406     c                   eval      v2prg  = §1tprg
053300051205     c                   Clear                   v2cmod
053400051202     c                   Eval      *In07 = *Off
053500051202      * Proteggo nota se richiesto da tabella 1T
053600051207      * o se sola interrogazione
053700140530     c*//                If        §1tpro = 'S' or *In09
053800140530     c                   If        (§1tpro  =  'S'   and
053900140530     c                             (Not *in08        or
054000140530     c                             (§1TnPro = *blank and *in08)))
054100140530     c                                               or  *In09
054200051202     c                   Eval      *In07 = *On
054300051207     c   09              Eval      *In10 = *On
054400051205     c                   Eval      v2cmod = 'S'
054500051202     c                   EndIf
054600051206     c                   Add       1             nrr1
054700051202     c                   Add       1             riga
054800051202     c                   Write     ta12s02
054900051202    3c                   EndDo
055000051202      * Carico n righe vuote in base a quante ne ho già scritte
055100051207      * se non sono protette
055200051207      * o se non è sola interrogazione
055300051207if  3c                   If        riga < §1trig and
055400140603     c                             (§1tpro  <> 'S'    or
055500140603     c                             (§1TnPro <> *blank or Not *in08))
055600140603     c                                                or  *In09
055700140530     c*//                           §1tpro <> 'S' and Not *In09
055800051202for 2c                   For       riga = riga + 1 by 1 to §1trig
055900051202     c                   Clear                   v2nota
056000051202     c                   Eval      v2email = §1teml
056100051205     c                   Eval      v2ctnt = sktnt1(xx)
056200051205     c                   Clear                   v2cann
056300051205     c                   Clear                   v2nrrr
056400051205     c                   Clear                   v2cmod
056500090406     c                   eval      v2prg  = §1tprg
056600051206     c                   Eval      v2csns = 'S'
056700051202     c                   Eval      *In07 = *Off
056800051206     c                   Add       1             nrr1
056900051202     c                   Write     ta12s02
057000051202    2c                   EndFor
057100051202    3c                   EndIf
057200051202      * Non trovo il contatto scrivo righe vuote nel subfile
057300051202   x2c                   Else
057400051202      * Scrivo righe vuote solo se non è richiesta la protezione da tabella 1T
057500051207      * o se non è solo interrogazione
057600140530if  3c*//                If        §1tpro = 'S' or *In09
057700140530     c                   If        (§1tpro  =  'S'   and
057800140530     c                             (Not *in08        or
057900140530     c                             (§1TnPro = *blank and *in08)))
058000140530     c                                               or  *In09
058100051202     c                   Iter
058200051202    3c                   EndIf
058300051202     c                   Eval      yy = 1
058400051202for 3c                   For       yy by 1 to §1trig
058500051202     c                   Clear                   v2nota
058600051202     c                   Eval      v2email = §1teml
058700051205     c                   Eval      v2ctnt = sktnt1(xx)
058800051205     c                   Clear                   v2cann
058900051205     c                   Clear                   v2nrrr
059000051205     c                   Clear                   v2cmod
059100090406     c                   eval      v2prg  = §1tprg
059200051206     c                   Eval      v2csns = 'S'
059300051202     c                   Eval      *In07 = *Off
059400051206     c                   Add       1             nrr1
059500051202     c                   Write     ta12s02
059600051202    3c                   EndFor
059700051202    2c                   EndIf
059800051202    1c                   EndFor
059900090406
060000051201     c                   Eval      *In31 = *On
060100051201
060200051202     c                   EndSr
060300051202
060400051202      *------------------------------------------------------------------------*
060500051202      * EMETTO IL SUBFILE
060600051202      *------------------------------------------------------------------------*
060700051202     c     Sr_Emisfl     BegSr
060800051201
060900051202do  1c                   Do        *Hival
061000051205
061100051206     c  n28              Z-add     1             recsf1
061200051206
061300051202     c                   Write     ta12t01
061400051202     c                   Write     ta12z02
061500051202     c                   Exfmt     ta12c02
061600051201
061700051201     c                   Eval      *In28 = *Off
061800051202     c                   Clear                   v2cmsg
061900051201
062000051202      * F3=Fine
062100051202     c   kc              Goto      fine
062200051202
062300051201      * F12=Ritorno
062400051207     c                   If        *InKl
062500051207     c   01              Goto      fine
062600051207     c  n01              Goto      emid02
062700051207     c                   EndIf
062800051206
062900051206      * F10=Inserimento Multiplo E-Mail
063000051206     c                   If        *Inkj
063100051206     c                   ExSr      Sr_F10
063200051206     c                   Goto      carsfl
063300051206     c                   EndIf
063400051201
063500051201      * Controllo i dati immessi nel subfile
063600051202     c                   ExSr      Sr_Ctrsfl
063700051202     c   28              Iter
063800051202
063900051202      * F6=Conferma
064000051205     c                   If        *Inkf
064100051205     c                   ExSr      Sr_Conferma
064200051207     c                   If        *In01
064300051207     c                   Goto      fine
064400051207     c                   EndIf
064500051202     c                   Leave
064600051205     c                   EndIf
064700051205
064800051202    1c                   EndDo
064900051201
065000051201     c                   EndSr
065100051202
065200051202      *------------------------------------------------------------------------*
065300051202      * CONTROLLO I DATI DEL SUBFILE
065400051202      *------------------------------------------------------------------------*
065500051202     c     Sr_Ctrsfl     BegSr
065600051202
065700051206     c                   Clear                   nrr1
065800051202do  1c                   Do        *Hival
065900051206     c                   Add       1             nrr1
066000051206     c     nrr1          Chain     ta12s02                            32
066100051205      * Esco se fine sfl
066200051202     c                   If        *In32
066300051202     c                   Leave
066400051202     c                   EndIf
066500051206      * Non controllo se riga protetta
066600051206     c                   If        v2cmod = 'S'
066700051206     c                   Iter
066800051206     c                   EndIf
066900051202
067000051202     c                   setoff                                       42
067100051205
067200051205      * Se prima la nota c'era ora non posso cancellarla
067300051205if  2c                   If        v2nrrr <> *Zeros and v2nota = *Blanks
067400051205     c                   Eval      *In28 = *On
067500051205     c                   Eval      *In42 = *On
067600051205     c                   Eval      v2cmsg = msg(11)
067700051206     c                   Eval      recsf1 = nrr1
067800051205     c                   Update    ta12s02
067900051205     c                   Leave
068000051205    2c                   EndIf
068100051202
068200051202      * Controllo se indirizzo e-mail
068300051202if  2c                   If        v2email = 'S' and v2nota <> *Blanks
068400051202     c                   Clear                   dsemail
068500051202     c                   Eval      §emlindi = v2nota
068600051202     c                   Call      'XEMAIL'
068700051202     c                   Parm                    dsemail
068800051202      * Errore
068900051202if  3c                   If        §emlerro <> '0'
069000051202     c                   Eval      *In28 = *On
069100051202     c                   Eval      *In42 = *On
069200051202     c                   Eval      v2cmsg = §emlmsgo
069300051206     c                   Eval      recsf1 = nrr1
069400051205     c                   Update    ta12s02
069500051202     c                   Leave
069600051202   x3c                   else
069700051202     c                   Eval      v2nota = §emlindo
069800051202    3c                   EndIf
069900051202    2c                   EndIf
070000051202
070100051206     c                   Update    ta12s02
070200051202    1c                   EndDo
070300051202
070400051202     c                   EndSr
070500051202
070600051202      *------------------------------------------------------------------------*
070700051202      * CONFERMA
070800051202      *------------------------------------------------------------------------*
070900051202     c     Sr_Conferma   BegSr
071000051205
071100051206     c                   Clear                   nrr1
071200051205do  1c                   Do        *Hival
071300051206     c                   Add       1             nrr1
071400051206     c     nrr1          Chain     ta12s02                            32
071500051205      * Esco se fine sfl
071600051205     c                   If        *In32
071700051205     c                   Leave
071800051205     c                   EndIf
071900051205      * Torno a leggere se nota non modificabile
072000051205     c                   If        v2cmod = 'S'
072100051205     c                   Iter
072200051205     c                   EndIf
072300051205      * Torno a leggere se non immessa la nota
072400051205     c                   If        v2nota = *Blanks
072500051205     c                   Iter
072600051205     c                   EndIf
072700051205
072800051205      * C'è il nrr nel subfile contatto già esistente aggiorno/deleto
072900051205if  2c                   If        v2nrrr <> *Zeros
073000051205     c                   Eval      ntcnrr = v2nrrr
073100051205     c     ntcnrr        Chain     tfntc00f
073200051205if  3c                   If        %Found(tfntc00f)
073300051205     c                   Eval      ntcrnt = v2nota
073400051205     c                   Move      *date         ntcntr
073500051205     c                   Eval      ntcflt = v2cann
073600051206     c                   Eval      ntcsns = v2csns
073700051205      * Aggiorno
073800051205if  4c                   If        ntcflt = *Blanks
073900051205     c                   Update    tfntc0
074000051205      * Deleto
074100051205   x4c                   Else
074200051205     c                   Delete    tfntc0
074300051205    4c                   EndIf
074400051205    3c                   EndIf
074500051205
074600051205      * Non c'è il nrr sul subfile è rcd nuovo scrivo
074700051205   x2c                   Else
074800051205if  3c                   If        v2cann = *Blanks
074900090806     c                   Clear                   tfntc0
075000051205     c                   Eval      ntcapl = kapl
075100051205     c                   Eval      ntcnk1 = knk1
075200051205     c                   Eval      ntctnt = v2ctnt
075300051205     c                   Eval      ntcrnt = v2nota
075400051206     c                   Eval      ntcsns = v2csns
075500051205     c                   Move      *date         ntcntr
075600051205      * Scrivo il contatto
075700051205     c                   Write     tfntc0
075800051205    3c                   EndIf
075900051205    2c                   EndIf
076000051205
076100051205    1c                   EndDo
076200051202
076300051202     c                   EndSr
076400051206
076500051206      *------------------------------------------------------------------------*
076600051206      * TASTO FUNZIONE F10 - INSERIMENTO MULTIPLO MAIL
076700051206      *------------------------------------------------------------------------*
076800051206     c     Sr_F10        BegSr
076900051206
077000051206      * Pulisco il subfile
077100051206     c                   Clear                   nrr2
077200051206     c                   Clear                   v3cksc
077300051206     c                   Clear                   v3cpot
077400051206     c                   Clear                   v3cnrv
077500051206     c                   Eval      *In20 = *Off
077600051206     c                   Eval      *In21 = *Off
077700051206     c                   Write     ta12c03
077800051206     c                   Eval      *In21 = *On
077900051206     c                   Eval      *In20 = *On
078000051206
078100051206     c                   Eval      recsf2 = 1
078200051206     c                   Eval      *In31 = *Off
078300051206
078400051206      * Imposto i dati del control
078500051206     c                   Eval      v3cksc = v1cksc
078600051206     c                   Eval      v3cpot = v1cpot
078700051206     c                   Eval      v3cnrv = v1cnrv
078800051206     c                   Eval      v3dksc = v1dksc
078900051206     c                   Eval      v3dpot = v1dpot
079000051206     c                   Eval      v3dnrv = v1dnrv
079100051206     c                   Clear                   v3email
079200051206
079300051206      * Imposto la chiave per tfntc
079400051206     c                   Eval      kapl = v1capl
079500051206     c                   Clear                   knk2
079600051206     c   03              Movel     dutkci        knk1
079700051206     c   03              Move      v3cksc        knk1
079800051206     c   04              Move      v3cpot        knk1
079900091112     c   11              Movel     dutkci        knk1
080000091112     c   11              Move      v3cnrv        knk1
080100051206
080200051206      * Carico il subfile in base alla sk dei tipi nota che prevedono e-mail
080300060329      * e che sono gestibili dall'immissione multipla
080400051206     c                   Eval      xx = 1
080500090406     c                   Eval      zz = 1
080600090406for 1c                   For       zz by 1 to 99
080700090406     c                   if        skord(zz) = *blanks
080800090406     c                   iter
080900051206     c                   EndIf
081000090406      * recupero la sk della nota
081100090406     c                   eval      xx = %lookup(%subst(skord(zz):4:2):sktnt)
081200051206      * Solo le note e-mail
081300051206     c                   If        skceml(xx) = *Blanks
081400051206     c                   Iter
081500051206     c                   EndIf
081600060329      * Solo le gestibili
081700060329     c                   If        skmult(xx) <> *Blanks
081800060329     c                   Iter
081900060329     c                   EndIf
082000051206      * Se non esistono scrivo subfile
082100051206     c                   Eval      ktnt = sktnt(xx)
082200051206     c     kTfntc        Chain     Tfntc01l
082300051206     c                   If        %Found(Tfntc01l) and ntcflt <> 'A'
082400051206     c                   Iter
082500051206     c                   EndIf
082600051206     c                   Eval      v3nota = sktnt(xx) + ' - ' + skdtnt(xx)
082700051206     c                   Clear                   v3scelta
082800051206     c                   Eval      v3ctnt = sktnt(xx)
082900051206     c                   Add       1             nrr2
083000051206     c                   Write     ta12s03
083100051206    1c                   EndFor
083200051206     c                   Eval      *In31 = *On
083300051206
083400051206      * Se non ho caricato nessuna e-mail non emetto la videata
083500051206     c                   If        nrr2 = *Zeros
083600060329     c                   Eval      v2cmsg = 'già inserite tutte le e-mail gesti-
083700060329     c                             bili'
083800051206     c                   Eval      *In28 = *On
083900051206     c                   Goto      carsfl
084000051206     c                   EndIf
084100051206
084200051206      * Emetto il subfile
084300051206     c     emisfl2       Tag
084400051206do  1c                   Do        *Hival
084500051206
084600051206     c  n28              Z-add     1             recsf2
084700051206
084800051206     c                   Write     ta12t01
084900051206     c                   Write     ta12z03
085000051206     c                   Exfmt     ta12c03
085100051206
085200051206     c                   Eval      *In28 = *Off
085300051206     c                   Clear                   v3cmsg
085400051206
085500051206      * F3=Fine
085600051206     c   kc              Goto      fine
085700051206
085800051206      * F12=Ritorno
085900051206     c   kl              Goto      carsfl
086000051206
086100051206      * Controllo che sia inserita l'e-mail da creare
086200051206     c                   If        v3email = *Blanks
086300051206     c                   Eval      *In28 = *On
086400051206     c                   Eval      v3cmsg = msg(12)
086500051206     c                   Goto      emisfl2
086600051206     c                   EndIf
086700051206
086800051206      * Controllo che l'e-mail inserita sia valida
086900051206if  2c                   If        v3email <> *Blanks
087000051206     c                   Clear                   dsemail
087100051206     c                   Eval      §emlindi = v3email
087200051206     c                   Call      'XEMAIL'
087300051206     c                   Parm                    dsemail
087400051206if  3c                   If        §emlerro <> '0'
087500051206     c                   Eval      *In28 = *On
087600051206     c                   Eval      v3cmsg = §emlmsgo
087700051206     c                   Goto      emisfl2
087800051206   x3c                   Else
087900051206     c                   Eval      v3email = §emlindo
088000051206    3c                   EndIf
088100051206     c  nkf              Goto      emisfl2
088200051206    2c                   EndIf
088300051206      * F6=Conferma
088400051206if  2c                   If        *Inkf
088500051206     c                   Clear                   nrr2
088600051206do  3c                   Do        *Hival
088700051206     c                   Add       1             nrr2
088800051206     c     nrr2          Chain     ta12s03                            32
088900051206      * Esco se fine sfl
089000051206     c                   If        *In32
089100051206     c                   Leave
089200051206     c                   EndIf
089300051206      * Torno a leggere se non è stata scelta
089400051206     c                   If        v3scelta = *Blanks
089500051206     c                   Iter
089600051206     c                   EndIf
089700051206      * Scrivo le note
089800051206     c                   Clear                   tfntc
089900051206     c                   Eval      ntcapl = kapl
090000051206     c                   Eval      ntcnk1 = knk1
090100051206     c                   Eval      ntctnt = v3ctnt
090200051206     c                   Eval      ntcrnt = v3email
090300051206     c                   Eval      ntcsns = 'S'
090400051206     c                   Move      *date         ntcntr
090500051206      * Scrivo il contatto
090600051206     c                   Write     tfntc0
090700051206    3c                   EndDo
090800051206     c                   Leave
090900051206    2c                   EndIf
091000051206
091100051206    1c                   EndDo
091200051206
091300051206     c                   EndSr
091400051202
091500051201      *------------------------------------------------------------------------*
091600051201      * ROUTINE INIZIALE
091700051201      *------------------------------------------------------------------------*
091800051201     c     *Inzsr        BegSr
091900051201
092000051201     c     *Entry        Plist
092100051201     c                   Parm                    kpjba
092200051201     c                   Parm                    tnta12ds
092300051206
092400051206      * Controllo se richiamato e come
092500051206if  1c                   If        %Parms > 1
092600101019
092700051206if  2c                   If        ta12apl <> *Blanks
092800051201     c                   Eval      *In01 = *On
092900051206   x2c                   Else
093000051206     c                   Eval      *In01 = *Off
093100051206    2c                   EndIf
093200051207      * Chiamato per sola interrogazione
093300051207     c                   If        ta12ric = 'V'
093400051207     c                   Eval      *In09 = *On
093500051207     c                   Else
093600051207     c                   Eval      *In09 = *Off
093700051207     c                   EndIf
093800140530      * Chiamato dal *pgm Gestione del Credito
093900140530     c                   eval      *in08 = (TA12ric = 'C')
094000101019
094100101019      * da menù --> non richiamato
094200051206   x1c                   Else
094300051201     c                   Eval      *In01 = *Off
094400140530     c                   eval      *in08 = *off
094500051206    1c                   EndIf
094600051206
094700051201     c     *dtaara       define    §azute        azuteds
094800051201     c     *dtaara       define    §datiute      ddatiute
094900051201     c                   in(E)     *dtaara
095000051201     c                   If        %error  or rsut = *blanks
095100051201     c                   Clear                   tibs34ds
095200051201     c                   Call      'TIBS34R'
095300051201     c                   Parm                    tibs34ds
095400051201     c                   In        *dtaara
095500051201     c                   EndIf
095600051201
095700051201      * Controllo se sono in sede
095800051201     c                   Eval      *In12 = (simfel = *zeros)
095900051201
096000051201      * Carico tutti i contatti
096100051201     c                   Eval      kcod = '1T'
096200051201     c                   Clear                   xx
096300051201     c     kTabel01      Setll     Tabel00f
096400051201     c                   Do        *Hival
096500051201     c     kTabel01      Reade     Tabel00f
096600051201     c                   If        %Eof(Tabel00f)
096700051201     c                   Leave
096800051201     c                   EndIf
096900051201     c                   If        tblflg <> *Blanks
097000051201     c                   Iter
097100051201     c                   EndIf
097200051201     c                   Eval      ds1t = tbluni
097300051201     c                   If        §1trich <> 'C'
097400051201     c                   Iter
097500051201     c                   EndIf
097600051201     c                   Add       1             xx
097700051206     c                   Eval      ds1t = tbluni
097800051201     c                   Eval      sktnt(xx) = tblkey
097900051206     c                   Eval      skdtnt(xx) = §1tdes
098000051206     c                   Eval      skceml(xx) = §1teml
098100060329     c                   Eval      skmult(xx) = §1tmult
098200090406     c                   eval      %subst(skord(xx):1:3) = %editc(§1tprg:'X')
098300090406     c                   eval      %subst(skord(xx):4:2) = sktnt(xx)
098400051201     c                   EndDo
098500090406      * ordino la schiera
098600090406     c                   sorta     skord
098700051201
098800051201      * KLIST
098900051201     c     kTabel        klist
099000051201     c                   kfld                    kkut
099100051201     c                   kfld                    kcod
099200051201     c                   kfld                    kkey
099300051201
099400051201     c     kTabel01      klist
099500051201     c                   kfld                    kkut
099600051201     c                   kfld                    kcod
099700051202
099800051202     c     kTfntc        klist
099900051202     c                   kfld                    kapl
100000051202     c                   kfld                    knk1
100100051202     c                   kfld                    knk2
100200051202     c                   kfld                    ktnt
100300051201
100400051201     c                   EndSr
100500090701      *------------------------------------------------------------------------*
100600090701      * CONTROLLO SE UTENTE ABILITATO
100700090701      *------------------------------------------------------------------------*
100800090701     c     Sr_Ctraut     BegSr
100900090701     c                   clear                   tntaa1ds
101000090701     c   03              eval      itaa1caut='§utecli'
101100090701     c   04              eval      itaa1caut='§utepot'
101200091112     c   11              eval      itaa1caut='§utegtc'
101300090701     c                   clear                   kpjbu
101400090701     c                   movel     tntaa1ds      kpjbu
101500090703     c                   call      'TNTAA1C'
101600090701     c                   parm                    kpjba
101700090701     c                   movel     kpjbu         tntaa1ds
101800090701     c
101900090701    1c                   if        otaa1fabi='N'
102000090701     c                   Eval      *In02 = *On
102100090701     c                   Eval      *In28 = *On
102200090701     c                   Eval      v1cmsg = msg(03)
102300090701     c   03              eval      v1cmsg=%trim(v1cmsg)+' I CLIENTI   '
102400090701     c   04              eval      v1cmsg=%trim(v1cmsg)+' I POTENZIALI'
102500091112     c   11              eval      v1cmsg=%trim(v1cmsg)+' LE TRATTATIVE'
102600090701     c                   endif
102700090701     c
102800090701     c                   EndSr
102900090701
103000051201      *------------------------------------------------------------------------*
103100051201
103200051201** MSG  Lungh. 78                                                            *
103300051201Tipo applicazione errato                                                      01
103400051201Tipo applicazione non utlizzabile da utenti di SEDE                           02
103500090701UTENTE NON ABILITATO ALLA GESTIONE CONTATTI PER                               03
103600051202Cliente errato                                                                04
103700090630Cliente non in gestione all'utente                                            05
103800051202Potenziale errato                                                             06
103900090630Potenziale non in gestione all'utente                                         07
104000101019                                                                              08
104100101019                                                                              09
104200051202Contatto errato                                                               10
104300051205Manca riga contatto                                                           11
104400051206Manca e-mail                                                                  12
