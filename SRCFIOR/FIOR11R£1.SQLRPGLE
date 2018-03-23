000100071227     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200071227
000300071227      *------------------------------------------------------------------------*
000400071227      *         STAMPA ANAGRAFICA CLIENTI RITIRO - batch
000500071227      *------------------------------------------------------------------------*
000600000927
000700071227     fazorg01l  if   e           k disk
000800080104     ffnacr11l  if   e           k disk
000900080104     ffior11p   o    e             printer oflind(*in90)
001000071227
001100071227      *------------------------------------------------------------------------*
001200071227      *  RIEPILOGO INDICATORI
001300071227      *------------------------------------------------------------------------*
001400080108      *  15 - filiale utente attiva con GEO ORM
001500000927
001600071227      *------------------------------------------------------------------------*
001700071227      *   V A R I A B I L I
001800071227      *------------------------------------------------------------------------*
001900080104     d savacrcro       s                   like(acrcro)
002000071227     d stringasql      s           1100    varying
002100080104     d wdescgi         s                   like(pdcgi1)
002200071227
002300071227      *------------------------------------------------------------------------*
002400071227      *   S C H I E R E
002500071227      *------------------------------------------------------------------------*
002600071227
002700071227      *------------------------------------------------------------------------*
002800071227      *   D S   I N T E R N E / E S T E R N E
002900071227      *------------------------------------------------------------------------*
003000071227     d azuteds       e ds                  extname(azute00f)
003100071227     d ddatiute      e ds
003200080104     d ds_cnaco      e ds                  inz extname(cnaco00f)
003300080104     d ds_cnind      e ds                  inz extname(cnind00f)
003400080104     d ds_cnclp      e ds                  inz extname(cnclp00f)
003500080104     d ds_fncls      e ds                  inz extname(fncls00f)
003600071227     d fidg09ds      e ds
003700071227     d fior11ds      e ds
003800080104     d fnacrds       e ds                  extname(fnacr00f)
003900080104     d fnacr1ds      e ds                  extname(fnacr10f)
004000071227     d kpjba         e ds
004100071227     d tibs34ds      e ds
004200080104     d tibs69ds      e ds                  inz
004300071227
004400071227     d                sds
004500071227     d  vidpgm                 1     10
004600071227
004700071227      *------------------------------------------------------------------------*
004800071227      *   C O S T A N T I
004900071227      *------------------------------------------------------------------------*
005000071227
005100071227      *------------------------------------------------------------------------*
005200071227
005300080104      * stampo la prima pagina con i parametri scelti
005400071227     c                   exsr      sr_pripag
005500071227
005600071227      * preparo la stringa sql
005700071227     c                   exsr      sr_stringa
005800071227
005900080104      * leggo il file e stampo
006000080104     c                   exsr      sr_leggi
006100080104
006200080104      * fine
006300080104     c                   write     finest
006400080104
006500080104     c                   eval      *inlr = *on
006600071227
006700071227      *------------------------------------------------------------------------*
006800071227      * STAMPO LA PRIMA PAGINA
006900071227      *------------------------------------------------------------------------*
007000071227     c     sr_pripag     begsr
007100071227
007200071227      * Emissione Prima Pagina Con Parametri Elaborazione
007300080123     c                   select
007400071227      * selezione per codice cliente
007500080123     c                   when      r11ord = '1'
007600071227     c                   eval      pordin = 'Codice Cliente Ritiro'
007700080104     c                   if        r11craal = *zeros
007800071227     c                   clear                   pordi2
007900071227     c                   else
008000071227     c                   eval      pordi2 = 'dal: ' +
008100080104     c                             %editc(r11crada:'X') +
008200071227     c                             '  al: ' +
008300080104     c                             %editc(r11craal:'X')
008400071227     c                   endif
008500071227      * selezione per ragione sociale
008600080123     c                   when      r11ord = '2'
008700071227     c                   eval      pordin = 'Ragione Sociale'
008800080104     c                   if        r11rsr = *blanks
008900071227     c                   clear                   pordi2
009000071227     c                   else
009100080104     c                   eval      pordi2 = 'da: ' + r11rsr
009200071227     c                   endif
009300080123      * selezione per giro
009400080123     c                   when      r11ord = '3'
009500080123     c                   eval      pordin = 'Codice Giro'
009600080123     c                   endsl
009700071227      * filiale gestione cliente
009800071227     c     r11poa        chain     azorg01l
009900071227     c                   if        %found(azorg01l)
010000071227     c                   eval      pdescp = orgdes
010100071227     c                   else
010200080108     c                   eval      pdescp = 'Tutte'
010300071227     c                   endif
010400071227      * tipo anagrafica
010500071227     c                   select
010600071227     c                   when      r11tac = 'R'
010700071227     c                   eval      ptipoa = 'Ritiro'
010800071227     c                   when      r11tac = 'O'
010900071227     c                   eval      ptipoa = 'Ordinante'
011000071227     c                   other
011100080108     c                   eval      ptipoa = 'Tutte'
011200071227     c                   endsl
011300071227      * tipo frequenza ritiro
011400071227     c                   select
011500071227     c                   when      r11tcr = 'F'
011600071227     c                   eval      ptipor = 'Fisso'
011700071227     c                   when      r11tcr = 'R'
011800071227     c                   eval      ptipor = 'Ricorrente'
011900071227     c                   when      r11tcr = 'O'
012000081015     C                   eval      ptipor = 'No Internet'
012100071227     c                   when      r11tcr = 'M'
012200071227     C                   eval      ptipor = 'Mai'
012300071227     c                   other
012400080108     c                   eval      ptipor = 'Tutte'
012500071227     c                   endsl
012600071227      * codice piano dei conti
012700071227     c                   if        r11ksc = *zeros
012800071227     c                   eval      pdesksc = 'Tutti'
012900071227     c                   else
013000071227     c                   clear                   tibs69ds
013100071227     c                   clear                   ds_cnaco
013200071227     c                   clear                   ds_cnind
013300071227     c                   clear                   ds_cnclp
013400071227     c                   clear                   ds_fncls
013500080104     c                   eval      i69kac = r11ksc
013600071227     c                   call      'TIBS69R'
013700071227     c                   parm                    tibs69ds
013800071227     c                   parm                    ds_cnaco
013900071227     c                   parm                    ds_cnind
014000071227     c                   parm                    ds_cnclp
014100071227     c                   parm                    ds_fncls
014200071227     c                   eval      pdesksc = acorag
014300071227     c                   endif
014400071227      * solo clienti con giro
014500080108     c                   if        r11sol = *blanks
014600071227     c                   eval      psoloz = 'Tutti'
014700071227     c                   else
014800071227     c                   eval      psoloz = 'Solo Clienti con Giro Ritiro'
014900080108     c                   select
015000080108     c                   when      r11pocgi <> *zeros
015100080108     c                   eval      psoloz = %trim(psoloz) + ' ' +
015200080108     c                             %editc(r11pocgi:'X')
015300080108     c                   when      r11poa <> *zeros
015400080108     c                   eval      psoloz = %trim(psoloz) + ' ' +
015500080108     c                             %editc(r11poa:'X')
015600080108     c                   other
015700080108     c                   eval      psoloz = %trim(psoloz) + ' ' +
015800080108     c                             %editc(r11fil:'X')
015900080108     c                   endsl
016000071227     c                   endif
016100071227      * giro ritiro
016200071227     c                   if        r11cgi = *blanks
016300071227     c                   eval      pdescgi = 'Tutti'
016400071227     c                   else
016500071227     c                   clear                   fidg09ds
016600071227     c                   eval      d09iop0 = '001'
016700080108     c                   eval      d09ifgs = r11pocgi
016800071227     c                   eval      d09idat = *date
016900071227     c                   eval      d09icgi = r11cgi
017000071227     c                   eval      d09itug = 'R'
017100071227     c                   eval      kpjbu = fidg09ds
017200071227     c                   call      'FIDG09R'
017300071227     c                   parm                    kpjba
017400071227     c                   eval      fidg09ds = kpjbu
017500071227     c                   eval      pdescgi = d09odes
017600071227     c                   endif
017700071227
017800071227     c                   write     pripag
017900080107     c                   eval      *in90 = *on
018000000927
018100071227     c                   endsr
018200071227
018300071227      *------------------------------------------------------------------------*
018400071227      *  PREPARO LA STRINGA SQL
018500071227      *------------------------------------------------------------------------*
018600071227     c     sr_stringa    begsr
018700071227
018800071227     c                   clear                   stringasql
018900080104
019000071227      * se richiesto solo clienti con giro di ritiro o richiesto un giro in particolare
019100080123      * oppure ordinamento per giro
019200071227      * leggo FNACR00F + FNACR10F
019300071227     c                   if        r11sol <> *blanks or r11cgi <> *blanks
019400080123     c                             or r11ord = '3'
019500071227     c                   eval      stringasql =
019600071227     c                             'select fnacr00f.*, fnacr10f.* '           +
019700071227     c                             'from fnacr00f join fnacr10f on '          +
019800071227     c                             'acrcro=acr1cro'
019900071227     c                   else
020000071227      * negli altri casi leggo FNACR00F
020100071227     c                   eval      stringasql =
020200071227     c                             'select fnacr00f.* '                       +
020300071227     c                             'from fnacr00f '
020400071227     c                   endif
020500080104
020600080104      * seleziono solo i record non annullati
020700080104     c                   eval      stringasql =
020800080104     c                             %trim(stringasql) + ' '                    +
020900080104     c                             'where acratb = ' + ''''                   +
021000080104     c                             ' ' + ''''
021100071227
021200080104      * richiesto un range di clienti
021300080104     c                   if        r11crada <> *zeros
021400080104     c                   eval      stringasql =
021500080104     c                             %trim(stringasql) + ' '                    +
021600080107     c                             'and acrcro between '                      +
021700080104     c                              %editc(r11crada:'X') + ' and '            +
021800080104     c                              %editc(r11craal:'X') + ' '
021900080104     c                   endif
022000080104
022100080104      * richiesta una ragione sociale
022200080104     c                   if        r11rsr <> *blanks
022300080104     c                   eval      stringasql =
022400080104     c                             %trim(stringasql) + ' '                    +
022500080104     c                             'and acrrsr like '                         +
022600080104     c                             '''' + %trimr(r11rsr)                      +
022700080104     c                             %trimr('%')  + '''' + ' '
022800080104     c                   endif
022900071227
023000080104      * richiesta la filiale di gestione cliente
023100080104     c                   if        r11poa <> *zeros
023200080104     c                   eval      stringasql =
023300080104     c                             %trim(stringasql) + ' '                    +
023400080104     c                             'and acrpoa = '                            +
023500080104     c                              %editc(r11poa:'X') + ' '
023600080104     c                   endif
023700080104
023800080104      * richiesto tipo anagrafica
023900080104     c                   if        r11tac <> *blanks
024000080104     c                   eval      stringasql =
024100080104     c                             %trim(stringasql) + ' '                    +
024200080104     c                             'and acrtac = '                            +
024300080104     c                             '''' + r11tac + '''' + ' '
024400080104     c                   endif
024500080104
024600080104      * richiesto frequenza ritiro
024700080104     c                   if        r11tcr <> *blanks
024800080104     c                   eval      stringasql =
024900080104     c                             %trim(stringasql) + ' '                    +
025000080104     c                             'and acrtcr = '                            +
025100080104     c                             '''' + r11tcr + '''' + ' '
025200080104     c                   endif
025300080104
025400080104      * richiesto codice piano dei conti
025500080104     c                   if        r11ksc <> *zeros
025600080104     c                   eval      stringasql =
025700080104     c                             %trim(stringasql) + ' '                    +
025800080104     c                             'and acrksc = '                            +
025900080104     c                              %editc(r11ksc:'X') + ' '
026000080104     c                   endif
026100080108
026200080108      * richiesta solo anagrafiche con giro di ritiro
026300080108     c                   if        r11sol <> *blanks
026400080108     c                   eval      stringasql =
026500080108     c                             %trim(stringasql) + ' '                    +
026600080108     c                             'and acr1pocgi = '
026700080108     c                   select
026800080108     c                   when      r11pocgi <> *zeros
026900080108     c                   eval      stringasql =
027000080108     c                             %trim(stringasql)                          +
027100080108     c                              %editc(r11pocgi:'X') + ' '
027200080108     c                   when      r11poa <> *zeros
027300080108     c                   eval      stringasql =
027400080108     c                             %trim(stringasql)                          +
027500080108     c                              %editc(r11poa:'X') + ' '
027600080108     c                   other
027700080108     c                   eval      stringasql =
027800080108     c                             %trim(stringasql)                          +
027900080108     c                              %editc(r11fil:'X') + ' '
028000080108     c                   endsl
028100080108     c                   endif
028200080104
028300080107      * richiesta filiale giro di ritiro
028400080107     c                   if        r11pocgi <> *zeros
028500080107     c                   eval      stringasql =
028600080107     c                             %trim(stringasql) + ' '                    +
028700080107     c                             'and acr1pocgi = '                         +
028800080107     c                              %editc(r11pocgi:'X') + ' '
028900080107     c                   endif
029000080104      * richiesto giro di ritiro
029100080104     c                   if        r11cgi <> *blanks
029200080104     c                   eval      stringasql =
029300080104     c                             %trim(stringasql) + ' '                    +
029400080104     c                             'and acr1cgi = '                           +
029500080104     c                             '''' + r11cgi + '''' + ' '
029600080104     c                   endif
029700071227
029800080104      * ordinato per codice cliente ritiro
029900080104     c                   if        r11ord = '1'
030000071227     c                   eval      stringasql =
030100071227     c                             %trim(stringasql) + ' '                     +
030200080104     c                             'order by acrcro'
030300071227     c                   endif
030400080104
030500080104      * ordinato per codice cliente ritiro
030600080104     c                   if        r11ord = '2'
030700080104     c                   eval      stringasql =
030800080104     c                             %trim(stringasql) + ' '                     +
030900080104     c                             'order by acrrsr, acrcro'
031000080104     c                   endif
031100080123
031200080123      * ordinato per codice giro
031300080123     c                   if        r11ord = '3'
031400080123     c                   eval      stringasql =
031500080123     c                             %trim(stringasql) + ' '                     +
031600080123     c                             'order by acr1ain desc, acr1cgi, acr1cro'
031700080123     c                   endif
031800071227
031900071227     c                   endsr
032000080104
032100080104      *------------------------------------------------------------------------*
032200080104      *  LEGGO IL FILE
032300080104      *------------------------------------------------------------------------*
032400080104     c     sr_leggi      begsr
032500080104
032600080104     c/exec sql
032700080104     c+ prepare s1 from :stringasql
032800080104     c/end-exec
032900080104
033000080104     c/exec sql
033100080104     c+ declare a1 cursor for s1
033200080104     c/end-exec
033300080104
033400080104     c/exec sql
033500080104     c+ open a1
033600080104     c/end-exec
033700080104
033800080104     c                   do        *hival
033900080104
034000080104      * se ho preparato il join tutti e due i formati
034100080104     c                   if        r11sol <> *blanks or r11cgi <> *blanks
034200080104     c/exec sql
034300080104     c+ fetch next from a1 into :fnacrds, :fnacr1ds
034400080104     c/end-exec
034500080104      * se no solo 1 formato
034600080104     c                   else
034700080104     c/exec sql
034800080104     c+ fetch next from a1 into :fnacrds
034900080104     c/end-exec
035000080104     c                   endif
035100080104
035200080104      * errore o fine file esco
035300080104     c                   if        sqlcod < 0 or sqlcod = 100
035400080104     c                   leave
035500080104     c                   endif
035600080104
035700080104      * a rottura di cliente
035800080104     c                   if        savacrcro <> acrcro
035900080104
036000080104      * stampo il dettaglio
036100080104     c                   exsr      sr_stampa
036200080104
036300080104      * stampo i giri memorizzati sul cliente
036400080104     c                   exsr      sr_giri
036500080107
036600080107      * stampo riga vuota
036700080107     c                   write     vuoto
036800080104
036900080104     c                   eval      savacrcro = acrcro
037000080104     c                   endif
037100080104
037200080104     c                   enddo
037300080104
037400080104     c/exec sql
037500080104     c+ close a1
037600080104     c/end-exec
037700080104
037800080104     c                   endsr
037900080104
038000080104      *------------------------------------------------------------------------*
038100080104      *  STAMPO IL DETTAGLIO       ?
038200080104      *------------------------------------------------------------------------*
038300080104     c     sr_stampa     begsr
038400080104
038500080104      * stampo la testata
038600080104     c                   if        *in90
038700080104     c                   write     testa
038800080104     c                   eval      *in90 = *off
038900080104     c                   endif
039000080104
039100080104     c                   eval      pemail = acrmai
039200080104     c                   eval      prefer = acrper
039300080104
039400080104      * tariffa cliente
039500080104     c                   if        acrccc = 999
039600080104     c                   clear                   pctr
039700080104     c                   else
039800080104     c                   move      acrccc        pctr
039900080104     c                   endif
040000080104
040100080104      * orm commissionato
040200080104     c                   if        acrfcc = *blanks
040300080104     c                   eval      pcomm = 'SI'
040400080104     c                   else
040500080104     c                   eval      pcomm = 'NO'
040600080104     c                   endif
040700080104
040800080104     c                   write     dett
040900080104
041000080104     c                   endsr
041100080104
041200080104      *------------------------------------------------------------------------*
041300080104      *  STAMPO I GIRI
041400080104      *------------------------------------------------------------------------*
041500080104     c     sr_giri       begsr
041600080107
041700080107     c                   clear                   ppocgi1
041800080107     c                   clear                   ppocgi2
041900080107     c                   clear                   ppocgi3
042000080107     c                   clear                   pcgi1
042100080107     c                   clear                   pcgi2
042200080107     c                   clear                   pcgi3
042300080107     c                   clear                   pdcgi1
042400080107     c                   clear                   pdcgi2
042500080107     c                   clear                   pdcgi3
042600080104
042700080104     c     acrcro        setll     fnacr11l
042800080104     c                   do        *hival
042900080104     c     acrcro        reade     fnacr11l
043000080104
043100080107      * fine file
043200080104     c                   if        %eof(fnacr11l)
043300080104     c                   leave
043400080104     c                   endif
043500080104
043600080108     c                   select
043700080104      * solo la filiale del giro selezionato
043800080108     c                   when      r11pocgi <> *zeros and acr1pocgi <> r11pocgi
043900080104     c                   iter
044000080104      * solo la filiale di appartenenza
044100080108     c                   when      r11poa <> *zeros and acr1pocgi <> r11poa
044200080104     c                   iter
044300080108      * solo la filiale utente
044400080108     c                   when      acr1pocgi <> r11fil
044500080108     c                   iter
044600080108     c                   endsl
044700080107
044800080104      * imposto giro standard
044900080104     c                   if        acr1ain = '='
045000080104     c                   eval      ppocgi1 = acr1pocgi
045100080104     c                   eval      pcgi1 = acr1cgi
045200080104     c                   exsr      sr_decgiro
045300080104     c                   eval      pdcgi1 = wdescgi
045400080104     c                   endif
045500080104      * imposto giro oltre
045600080104     c                   if        acr1ain = '>'
045700080104     c                   eval      ppocgi2 = acr1pocgi
045800080104     c                   eval      pcgi2 = acr1cgi
045900080104     c                   exsr      sr_decgiro
046000080104     c                   eval      pdcgi2 = wdescgi
046100080104     c                   endif
046200080104      * imposto giro sotto
046300080104     c                   if        acr1ain = '<'
046400080104     c                   eval      ppocgi3 = acr1pocgi
046500080104     c                   eval      pcgi3 = acr1cgi
046600080104     c                   exsr      sr_decgiro
046700080104     c                   eval      pdcgi3 = wdescgi
046800080104     c                   endif
046900080104
047000080104     c                   enddo
047100080104
047200080107      * se almeno un giro impostato
047300080107     c                   if        pcgi1 <> *blanks or pcgi2 <> *blanks or
047400080107     c                             pcgi3 <> *blanks
047500080104      * stampo la testata
047600080104     c                   if        *in90
047700080104     c                   write     testa
047800080104     c                   eval      *in90 = *off
047900080104     c                   endif
048000080104
048100080104     c                   write     giro
048200080107     c                   endif
048300080104
048400080104     c                   endsr
048500080104
048600080104      *------------------------------------------------------------------------*
048700080104      *  DECODIFICO IL GIRO
048800080104      *------------------------------------------------------------------------*
048900080104     c     sr_decgiro    begsr
049000080104
049100080107     c                   clear                   wdescgi
049200080104     c                   clear                   fidg09ds
049300080104     c                   eval      d09iop0 = '001'
049400080104     c                   eval      d09ifgs = acr1pocgi
049500080104     c                   eval      d09idat = *date
049600080104     c                   eval      d09icgi = acr1cgi
049700080104     c                   eval      d09itug = 'R'
049800080104     c                   eval      kpjbu = fidg09ds
049900080104     c                   call      'FIDG09R'
050000080104     c                   parm                    kpjba
050100080104     c                   eval      fidg09ds = kpjbu
050200080104      * imposto la descrizione del giro
050300080104     c                   eval      wdescgi = d09odes
050400080104
050500080104     c                   endsr
050600071227
050700071227      *------------------------------------------------------------------------*
050800080104      *  OPERAZIONI INIZIALI       ?
050900071227      *------------------------------------------------------------------------*
051000071227     c     *inzsr        begsr
051100071227
051200071227     c     *entry        plist
051300071227     c                   parm                    kpjba
051400071227
051500071227     c                   eval      fior11ds = kpjbu
051600071227
051700071227     c     *dtaara       define    §azute        azuteds
051800071227     c     *dtaara       define    §datiute      ddatiute
051900071227     c                   in(e)     *dtaara
052000071227     c                   if        %error or rsut = *blanks
052100071227     c                   clear                   tibs34ds
052200071227     c                   call      'TIBS34R'
052300071227     c                   parm                    tibs34ds
052400071227     c                   in        *dtaara
052500071227     c                   endif
052600080108
052700080108      * imposto se filiale attiva con GEO ORM
052800080108     c                   eval      *in15 = (r11geo = 'S')
052900071227
053000071227     c                   endsr
