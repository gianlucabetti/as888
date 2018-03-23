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
004300131114      *
004400131114      * - Orari ritiro ed altro in FNACR00F
004500131114     d dACR01        e ds                  inz  qualified
004600071227
004700071227     d                sds
004800071227     d  vidpgm                 1     10
004900071227
005000071227      *------------------------------------------------------------------------*
005100071227      *   C O S T A N T I
005200071227      *------------------------------------------------------------------------*
005300131114
005400131114       // -?Costante per controllo "caratteri solo numerici"?
005500131114     d c_Digits        c                   const('0123456789')
005600071227
005700071227      *------------------------------------------------------------------------*
005800071227
005900080104      * stampo la prima pagina con i parametri scelti
006000071227     c                   exsr      sr_pripag
006100071227
006200071227      * preparo la stringa sql
006300071227     c                   exsr      sr_stringa
006400071227
006500080104      * leggo il file e stampo
006600080104     c                   exsr      sr_leggi
006700080104
006800080104      * fine
006900080104     c                   write     finest
007000080104
007100080104     c                   eval      *inlr = *on
007200071227
007300071227      *------------------------------------------------------------------------*
007400071227      * STAMPO LA PRIMA PAGINA
007500071227      *------------------------------------------------------------------------*
007600071227     c     sr_pripag     begsr
007700071227
007800071227      * Emissione Prima Pagina Con Parametri Elaborazione
007900080123     c                   select
008000071227      * selezione per codice cliente
008100080123     c                   when      r11ord = '1'
008200071227     c                   eval      pordin = 'Codice Cliente Ritiro'
008300080104     c                   if        r11craal = *zeros
008400071227     c                   clear                   pordi2
008500071227     c                   else
008600071227     c                   eval      pordi2 = 'dal: ' +
008700080104     c                             %editc(r11crada:'X') +
008800071227     c                             '  al: ' +
008900080104     c                             %editc(r11craal:'X')
009000071227     c                   endif
009100071227      * selezione per ragione sociale
009200080123     c                   when      r11ord = '2'
009300071227     c                   eval      pordin = 'Ragione Sociale'
009400080104     c                   if        r11rsr = *blanks
009500071227     c                   clear                   pordi2
009600071227     c                   else
009700080104     c                   eval      pordi2 = 'da: ' + r11rsr
009800071227     c                   endif
009900080123      * selezione per giro
010000080123     c                   when      r11ord = '3'
010100080123     c                   eval      pordin = 'Codice Giro'
010200080123     c                   endsl
010300071227      * filiale gestione cliente
010400071227     c     r11poa        chain     azorg01l
010500071227     c                   if        %found(azorg01l)
010600071227     c                   eval      pdescp = orgdes
010700071227     c                   else
010800080108     c                   eval      pdescp = 'Tutte'
010900071227     c                   endif
011000071227      * tipo anagrafica
011100071227     c                   select
011200071227     c                   when      r11tac = 'R'
011300071227     c                   eval      ptipoa = 'Ritiro'
011400071227     c                   when      r11tac = 'O'
011500071227     c                   eval      ptipoa = 'Ordinante'
011600071227     c                   other
011700080108     c                   eval      ptipoa = 'Tutte'
011800071227     c                   endsl
011900071227      * tipo frequenza ritiro
012000071227     c                   select
012100071227     c                   when      r11tcr = 'F'
012200071227     c                   eval      ptipor = 'Fisso'
012300071227     c                   when      r11tcr = 'R'
012400071227     c                   eval      ptipor = 'Ricorrente'
012500071227     c                   when      r11tcr = 'O'
012600081015     C                   eval      ptipor = 'No Internet'
012700071227     c                   when      r11tcr = 'M'
012800071227     C                   eval      ptipor = 'Mai'
012900071227     c                   other
013000080108     c                   eval      ptipor = 'Tutte'
013100071227     c                   endsl
013200071227      * codice piano dei conti
013300071227     c                   if        r11ksc = *zeros
013400071227     c                   eval      pdesksc = 'Tutti'
013500071227     c                   else
013600071227     c                   clear                   tibs69ds
013700071227     c                   clear                   ds_cnaco
013800071227     c                   clear                   ds_cnind
013900071227     c                   clear                   ds_cnclp
014000071227     c                   clear                   ds_fncls
014100080104     c                   eval      i69kac = r11ksc
014200071227     c                   call      'TIBS69R'
014300071227     c                   parm                    tibs69ds
014400071227     c                   parm                    ds_cnaco
014500071227     c                   parm                    ds_cnind
014600071227     c                   parm                    ds_cnclp
014700071227     c                   parm                    ds_fncls
014800071227     c                   eval      pdesksc = acorag
014900071227     c                   endif
015000071227      * solo clienti con giro
015100080108     c                   if        r11sol = *blanks
015200071227     c                   eval      psoloz = 'Tutti'
015300071227     c                   else
015400071227     c                   eval      psoloz = 'Solo Clienti con Giro Ritiro'
015500080108     c                   select
015600080108     c                   when      r11pocgi <> *zeros
015700080108     c                   eval      psoloz = %trim(psoloz) + ' ' +
015800080108     c                             %editc(r11pocgi:'X')
015900080108     c                   when      r11poa <> *zeros
016000080108     c                   eval      psoloz = %trim(psoloz) + ' ' +
016100080108     c                             %editc(r11poa:'X')
016200080108     c                   other
016300080108     c                   eval      psoloz = %trim(psoloz) + ' ' +
016400080108     c                             %editc(r11fil:'X')
016500080108     c                   endsl
016600071227     c                   endif
016700071227      * giro ritiro
016800071227     c                   if        r11cgi = *blanks
016900071227     c                   eval      pdescgi = 'Tutti'
017000071227     c                   else
017100071227     c                   clear                   fidg09ds
017200071227     c                   eval      d09iop0 = '001'
017300080108     c                   eval      d09ifgs = r11pocgi
017400071227     c                   eval      d09idat = *date
017500071227     c                   eval      d09icgi = r11cgi
017600071227     c                   eval      d09itug = 'R'
017700071227     c                   eval      kpjbu = fidg09ds
017800071227     c                   call      'FIDG09R'
017900071227     c                   parm                    kpjba
018000071227     c                   eval      fidg09ds = kpjbu
018100071227     c                   eval      pdescgi = d09odes
018200071227     c                   endif
018300071227
018400071227     c                   write     pripag
018500080107     c                   eval      *in90 = *on
018600000927
018700071227     c                   endsr
018800071227
018900071227      *------------------------------------------------------------------------*
019000071227      *  PREPARO LA STRINGA SQL
019100071227      *------------------------------------------------------------------------*
019200071227     c     sr_stringa    begsr
019300071227
019400071227     c                   clear                   stringasql
019500080104
019600071227      * se richiesto solo clienti con giro di ritiro o richiesto un giro in particolare
019700080123      * oppure ordinamento per giro
019800071227      * leggo FNACR00F + FNACR10F
019900071227     c                   if        r11sol <> *blanks or r11cgi <> *blanks
020000080123     c                             or r11ord = '3'
020100071227     c                   eval      stringasql =
020200071227     c                             'select fnacr00f.*, fnacr10f.* '           +
020300071227     c                             'from fnacr00f join fnacr10f on '          +
020400071227     c                             'acrcro=acr1cro'
020500071227     c                   else
020600071227      * negli altri casi leggo FNACR00F
020700071227     c                   eval      stringasql =
020800071227     c                             'select fnacr00f.* '                       +
020900071227     c                             'from fnacr00f '
021000071227     c                   endif
021100080104
021200080104      * seleziono solo i record non annullati
021300080104     c                   eval      stringasql =
021400080104     c                             %trim(stringasql) + ' '                    +
021500080104     c                             'where acratb = ' + ''''                   +
021600080104     c                             ' ' + ''''
021700071227
021800080104      * richiesto un range di clienti
021900080104     c                   if        r11crada <> *zeros
022000080104     c                   eval      stringasql =
022100080104     c                             %trim(stringasql) + ' '                    +
022200080107     c                             'and acrcro between '                      +
022300080104     c                              %editc(r11crada:'X') + ' and '            +
022400080104     c                              %editc(r11craal:'X') + ' '
022500080104     c                   endif
022600080104
022700080104      * richiesta una ragione sociale
022800080104     c                   if        r11rsr <> *blanks
022900080104     c                   eval      stringasql =
023000080104     c                             %trim(stringasql) + ' '                    +
023100080104     c                             'and acrrsr like '                         +
023200080104     c                             '''' + %trimr(r11rsr)                      +
023300080104     c                             %trimr('%')  + '''' + ' '
023400080104     c                   endif
023500071227
023600080104      * richiesta la filiale di gestione cliente
023700080104     c                   if        r11poa <> *zeros
023800080104     c                   eval      stringasql =
023900080104     c                             %trim(stringasql) + ' '                    +
024000080104     c                             'and acrpoa = '                            +
024100080104     c                              %editc(r11poa:'X') + ' '
024200080104     c                   endif
024300080104
024400080104      * richiesto tipo anagrafica
024500080104     c                   if        r11tac <> *blanks
024600080104     c                   eval      stringasql =
024700080104     c                             %trim(stringasql) + ' '                    +
024800080104     c                             'and acrtac = '                            +
024900080104     c                             '''' + r11tac + '''' + ' '
025000080104     c                   endif
025100080104
025200080104      * richiesto frequenza ritiro
025300080104     c                   if        r11tcr <> *blanks
025400080104     c                   eval      stringasql =
025500080104     c                             %trim(stringasql) + ' '                    +
025600080104     c                             'and acrtcr = '                            +
025700080104     c                             '''' + r11tcr + '''' + ' '
025800080104     c                   endif
025900080104
026000080104      * richiesto codice piano dei conti
026100080104     c                   if        r11ksc <> *zeros
026200080104     c                   eval      stringasql =
026300080104     c                             %trim(stringasql) + ' '                    +
026400080104     c                             'and acrksc = '                            +
026500080104     c                              %editc(r11ksc:'X') + ' '
026600080104     c                   endif
026700080108
026800080108      * richiesta solo anagrafiche con giro di ritiro
026900080108     c                   if        r11sol <> *blanks
027000080108     c                   eval      stringasql =
027100080108     c                             %trim(stringasql) + ' '                    +
027200080108     c                             'and acr1pocgi = '
027300080108     c                   select
027400080108     c                   when      r11pocgi <> *zeros
027500080108     c                   eval      stringasql =
027600080108     c                             %trim(stringasql)                          +
027700080108     c                              %editc(r11pocgi:'X') + ' '
027800080108     c                   when      r11poa <> *zeros
027900080108     c                   eval      stringasql =
028000080108     c                             %trim(stringasql)                          +
028100080108     c                              %editc(r11poa:'X') + ' '
028200080108     c                   other
028300080108     c                   eval      stringasql =
028400080108     c                             %trim(stringasql)                          +
028500080108     c                              %editc(r11fil:'X') + ' '
028600080108     c                   endsl
028700080108     c                   endif
028800080104
028900080107      * richiesta filiale giro di ritiro
029000080107     c                   if        r11pocgi <> *zeros
029100080107     c                   eval      stringasql =
029200080107     c                             %trim(stringasql) + ' '                    +
029300080107     c                             'and acr1pocgi = '                         +
029400080107     c                              %editc(r11pocgi:'X') + ' '
029500080107     c                   endif
029600080104      * richiesto giro di ritiro
029700080104     c                   if        r11cgi <> *blanks
029800080104     c                   eval      stringasql =
029900080104     c                             %trim(stringasql) + ' '                    +
030000080104     c                             'and acr1cgi = '                           +
030100080104     c                             '''' + r11cgi + '''' + ' '
030200080104     c                   endif
030300071227
030400080104      * ordinato per codice cliente ritiro
030500080104     c                   if        r11ord = '1'
030600071227     c                   eval      stringasql =
030700071227     c                             %trim(stringasql) + ' '                     +
030800080104     c                             'order by acrcro'
030900071227     c                   endif
031000080104
031100080104      * ordinato per codice cliente ritiro
031200080104     c                   if        r11ord = '2'
031300080104     c                   eval      stringasql =
031400080104     c                             %trim(stringasql) + ' '                     +
031500080104     c                             'order by acrrsr, acrcro'
031600080104     c                   endif
031700080123
031800080123      * ordinato per codice giro
031900080123     c                   if        r11ord = '3'
032000080123     c                   eval      stringasql =
032100080123     c                             %trim(stringasql) + ' '                     +
032200080123     c                             'order by acr1ain desc, acr1cgi, acr1cro'
032300080123     c                   endif
032400071227
032500071227     c                   endsr
032600080104
032700080104      *------------------------------------------------------------------------*
032800080104      *  LEGGO IL FILE
032900080104      *------------------------------------------------------------------------*
033000080104     c     sr_leggi      begsr
033100080104
033200080104     c/exec sql
033300080104     c+ prepare s1 from :stringasql
033400080104     c/end-exec
033500080104
033600080104     c/exec sql
033700080104     c+ declare a1 cursor for s1
033800080104     c/end-exec
033900080104
034000080104     c/exec sql
034100080104     c+ open a1
034200080104     c/end-exec
034300080104
034400080104     c                   do        *hival
034500080104
034600080104      * se ho preparato il join tutti e due i formati
034700080104     c                   if        r11sol <> *blanks or r11cgi <> *blanks
034800080104     c/exec sql
034900080104     c+ fetch next from a1 into :fnacrds, :fnacr1ds
035000080104     c/end-exec
035100080104      * se no solo 1 formato
035200080104     c                   else
035300080104     c/exec sql
035400080104     c+ fetch next from a1 into :fnacrds
035500080104     c/end-exec
035600080104     c                   endif
035700080104
035800080104      * errore o fine file esco
035900080104     c                   if        sqlcod < 0 or sqlcod = 100
036000080104     c                   leave
036100080104     c                   endif
036200080104
036300080104      * a rottura di cliente
036400080104     c                   if        savacrcro <> acrcro
036500080104
036600080104      * stampo il dettaglio
036700080104     c                   exsr      sr_stampa
036800080104
036900080104      * stampo i giri memorizzati sul cliente
037000080104     c                   exsr      sr_giri
037100080107
037200080107      * stampo riga vuota
037300080107     c                   write     vuoto
037400080104
037500080104     c                   eval      savacrcro = acrcro
037600080104     c                   endif
037700080104
037800080104     c                   enddo
037900080104
038000080104     c/exec sql
038100080104     c+ close a1
038200080104     c/end-exec
038300080104
038400080104     c                   endsr
038500080104
038600080104      *------------------------------------------------------------------------*
038700080104      *  STAMPO IL DETTAGLIO       ?
038800080104      *------------------------------------------------------------------------*
038900080104     c     sr_stampa     begsr
039000080104
039100080104      * stampo la testata
039200080104     c                   if        *in90
039300080104     c                   write     testa
039400080104     c                   eval      *in90 = *off
039500080104     c                   endif
039600080104
039700131114     c*//                eval      pemail = acrmai
039800131114     c                   eval      dACR01 = ACRmai
039900131114      * - Valorizzazione degli orari solo SE effettivamente numerici
040000131114     c                   if        %check( c_Digits : %subst(
040100131114     c                                     ACRmai : 1
040200131114     c                                            : %len(dACR01.你CRoa1) )
040300131114     c                                   ) = *zero
040400131114     c                   eval      pHMamI = %int( dACR01.你CRoa1 )
040500131114     c                   endif
040600131114     c                   if        %check( c_Digits : %subst(
040700131114     c                                     ACRmai : %len(dACR01.你CRoa1) + 1
040800131114     c                                            : %len(dACR01.你CRoa2) )
040900131114     c                                   ) = *zero
041000131114     c                   eval      pHMamF = %int( dACR01.你CRoa2 )
041100131114     c                   endif
041200131114     c                   if        %check( c_Digits : %subst(
041300131114     c                                     ACRmai : %len(dACR01.你CRoa1) +
041400131114     c                                              %len(dACR01.你CRoa2) + 1
041500131114     c                                            : %len(dACR01.你CRoa3) )
041600131114     c                                   ) = *zero
041700131114     c                   eval      pHMpmI = %int( dACR01.你CRoa3 )
041800131114     c                   endif
041900131114     c                   if        %check( c_Digits : %subst(
042000131114     c                                     ACRmai : %len(dACR01.你CRoa1) +
042100131114     c                                              %len(dACR01.你CRoa2) +
042200131114     c                                              %len(dACR01.你CRoa3) + 1
042300131114     c                                            : %len(dACR01.你CRoa4) )
042400131114     c                                   ) = *zero
042500131114     c                   eval      pHMpmF = %int( dACR01.你CRoa4 )
042600131114     c                   endif
042700131114
042800080104     c                   eval      prefer = acrper
042900080104
043000080104      * tariffa cliente
043100080104     c                   if        acrccc = 999
043200080104     c                   clear                   pctr
043300080104     c                   else
043400080104     c                   move      acrccc        pctr
043500080104     c                   endif
043600080104
043700080104      * orm commissionato
043800080104     c                   if        acrfcc = *blanks
043900080104     c                   eval      pcomm = 'SI'
044000080104     c                   else
044100080104     c                   eval      pcomm = 'NO'
044200080104     c                   endif
044300080104
044400080104     c                   write     dett
044500080104
044600080104     c                   endsr
044700080104
044800080104      *------------------------------------------------------------------------*
044900080104      *  STAMPO I GIRI
045000080104      *------------------------------------------------------------------------*
045100080104     c     sr_giri       begsr
045200080107
045300080107     c                   clear                   ppocgi1
045400080107     c                   clear                   ppocgi2
045500080107     c                   clear                   ppocgi3
045600080107     c                   clear                   pcgi1
045700080107     c                   clear                   pcgi2
045800080107     c                   clear                   pcgi3
045900080107     c                   clear                   pdcgi1
046000080107     c                   clear                   pdcgi2
046100080107     c                   clear                   pdcgi3
046200080104
046300080104     c     acrcro        setll     fnacr11l
046400080104     c                   do        *hival
046500080104     c     acrcro        reade     fnacr11l
046600080104
046700080107      * fine file
046800080104     c                   if        %eof(fnacr11l)
046900080104     c                   leave
047000080104     c                   endif
047100080104
047200080108     c                   select
047300080104      * solo la filiale del giro selezionato
047400080108     c                   when      r11pocgi <> *zeros and acr1pocgi <> r11pocgi
047500080104     c                   iter
047600080104      * solo la filiale di appartenenza
047700080108     c                   when      r11poa <> *zeros and acr1pocgi <> r11poa
047800080104     c                   iter
047900080108      * solo la filiale utente
048000080108     c                   when      acr1pocgi <> r11fil
048100080108     c                   iter
048200080108     c                   endsl
048300080107
048400080104      * imposto giro standard
048500080104     c                   if        acr1ain = '='
048600080104     c                   eval      ppocgi1 = acr1pocgi
048700080104     c                   eval      pcgi1 = acr1cgi
048800080104     c                   exsr      sr_decgiro
048900080104     c                   eval      pdcgi1 = wdescgi
049000080104     c                   endif
049100080104      * imposto giro oltre
049200080104     c                   if        acr1ain = '>'
049300080104     c                   eval      ppocgi2 = acr1pocgi
049400080104     c                   eval      pcgi2 = acr1cgi
049500080104     c                   exsr      sr_decgiro
049600080104     c                   eval      pdcgi2 = wdescgi
049700080104     c                   endif
049800080104      * imposto giro sotto
049900080104     c                   if        acr1ain = '<'
050000080104     c                   eval      ppocgi3 = acr1pocgi
050100080104     c                   eval      pcgi3 = acr1cgi
050200080104     c                   exsr      sr_decgiro
050300080104     c                   eval      pdcgi3 = wdescgi
050400080104     c                   endif
050500080104
050600080104     c                   enddo
050700080104
050800080107      * se almeno un giro impostato
050900080107     c                   if        pcgi1 <> *blanks or pcgi2 <> *blanks or
051000080107     c                             pcgi3 <> *blanks
051100080104      * stampo la testata
051200080104     c                   if        *in90
051300080104     c                   write     testa
051400080104     c                   eval      *in90 = *off
051500080104     c                   endif
051600080104
051700080104     c                   write     giro
051800080107     c                   endif
051900080104
052000080104     c                   endsr
052100080104
052200080104      *------------------------------------------------------------------------*
052300080104      *  DECODIFICO IL GIRO
052400080104      *------------------------------------------------------------------------*
052500080104     c     sr_decgiro    begsr
052600080104
052700080107     c                   clear                   wdescgi
052800080104     c                   clear                   fidg09ds
052900080104     c                   eval      d09iop0 = '001'
053000080104     c                   eval      d09ifgs = acr1pocgi
053100080104     c                   eval      d09idat = *date
053200080104     c                   eval      d09icgi = acr1cgi
053300080104     c                   eval      d09itug = 'R'
053400080104     c                   eval      kpjbu = fidg09ds
053500080104     c                   call      'FIDG09R'
053600080104     c                   parm                    kpjba
053700080104     c                   eval      fidg09ds = kpjbu
053800080104      * imposto la descrizione del giro
053900080104     c                   eval      wdescgi = d09odes
054000080104
054100080104     c                   endsr
054200071227
054300071227      *------------------------------------------------------------------------*
054400080104      *  OPERAZIONI INIZIALI       ?
054500071227      *------------------------------------------------------------------------*
054600071227     c     *inzsr        begsr
054700071227
054800071227     c     *entry        plist
054900071227     c                   parm                    kpjba
055000071227
055100071227     c                   eval      fior11ds = kpjbu
055200071227
055300071227     c     *dtaara       define    吧zute        azuteds
055400071227     c     *dtaara       define    吳atiute      ddatiute
055500071227     c                   in(e)     *dtaara
055600071227     c                   if        %error or rsut = *blanks
055700071227     c                   clear                   tibs34ds
055800071227     c                   call      'TIBS34R'
055900071227     c                   parm                    tibs34ds
056000071227     c                   in        *dtaara
056100071227     c                   endif
056200080108
056300080108      * imposto se filiale attiva con GEO ORM
056400080108     c                   eval      *in15 = (r11geo = 'S')
056500071227
056600071227     c                   endsr
