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
001500150910      *  16 - stampa filiale ritoro forzata
001600000927
001700071227      *------------------------------------------------------------------------*
001800071227      *   V A R I A B I L I
001900071227      *------------------------------------------------------------------------*
002000080104     d savacrcro       s                   like(acrcro)
002100071227     d stringasql      s           1100    varying
002200080104     d wdescgi         s                   like(pdcgi1)
002300071227
002400071227      *------------------------------------------------------------------------*
002500071227      *   S C H I E R E
002600071227      *------------------------------------------------------------------------*
002700071227
002800071227      *------------------------------------------------------------------------*
002900071227      *   D S   I N T E R N E / E S T E R N E
003000071227      *------------------------------------------------------------------------*
003100071227     d azuteds       e ds                  extname(azute00f)
003200071227     d ddatiute      e ds
003300080104     d ds_cnaco      e ds                  inz extname(cnaco00f)
003400080104     d ds_cnind      e ds                  inz extname(cnind00f)
003500080104     d ds_cnclp      e ds                  inz extname(cnclp00f)
003600080104     d ds_fncls      e ds                  inz extname(fncls00f)
003700071227     d fidg09ds      e ds
003800071227     d fior11ds      e ds
003900080104     d fnacrds       e ds                  extname(fnacr00f)
004000080104     d fnacr1ds      e ds                  extname(fnacr10f)
004100071227     d kpjba         e ds
004200071227     d tibs34ds      e ds
004300080104     d tibs69ds      e ds                  inz
004400131114      *
004500131114      * - Orari ritiro ed altro in FNACR00F
004600131114     d dACR01        e ds                  inz  qualified
004700071227
004800071227     d                sds
004900071227     d  vidpgm                 1     10
005000071227
005100071227      *------------------------------------------------------------------------*
005200071227      *   C O S T A N T I
005300071227      *------------------------------------------------------------------------*
005400131114
005500131114       // -?Costante per controllo "caratteri solo numerici"?
005600131114     d c_Digits        c                   const('0123456789')
005700071227
005800071227      *------------------------------------------------------------------------*
005900071227
006000080104      * stampo la prima pagina con i parametri scelti
006100071227     c                   exsr      sr_pripag
006200071227
006300071227      * preparo la stringa sql
006400071227     c                   exsr      sr_stringa
006500071227
006600080104      * leggo il file e stampo
006700080104     c                   exsr      sr_leggi
006800080104
006900080104      * fine
007000080104     c                   write     finest
007100080104
007200080104     c                   eval      *inlr = *on
007300071227
007400071227      *------------------------------------------------------------------------*
007500071227      * STAMPO LA PRIMA PAGINA
007600071227      *------------------------------------------------------------------------*
007700071227     c     sr_pripag     begsr
007800071227
007900071227      * Emissione Prima Pagina Con Parametri Elaborazione
008000080123     c                   select
008100071227      * selezione per codice cliente
008200080123     c                   when      r11ord = '1'
008300071227     c                   eval      pordin = 'Codice Cliente Ritiro'
008400080104     c                   if        r11craal = *zeros
008500071227     c                   clear                   pordi2
008600071227     c                   else
008700071227     c                   eval      pordi2 = 'dal: ' +
008800080104     c                             %editc(r11crada:'X') +
008900071227     c                             '  al: ' +
009000080104     c                             %editc(r11craal:'X')
009100071227     c                   endif
009200071227      * selezione per ragione sociale
009300080123     c                   when      r11ord = '2'
009400071227     c                   eval      pordin = 'Ragione Sociale'
009500080104     c                   if        r11rsr = *blanks
009600071227     c                   clear                   pordi2
009700071227     c                   else
009800080104     c                   eval      pordi2 = 'da: ' + r11rsr
009900071227     c                   endif
010000080123      * selezione per giro
010100080123     c                   when      r11ord = '3'
010200080123     c                   eval      pordin = 'Codice Giro'
010300080123     c                   endsl
010400071227      * filiale gestione cliente
010500071227     c     r11poa        chain     azorg01l
010600071227     c                   if        %found(azorg01l)
010700071227     c                   eval      pdescp = orgdes
010800071227     c                   else
010900080108     c                   eval      pdescp = 'Tutte'
011000071227     c                   endif
011100071227      * tipo anagrafica
011200071227     c                   select
011300071227     c                   when      r11tac = 'R'
011400071227     c                   eval      ptipoa = 'Ritiro'
011500071227     c                   when      r11tac = 'O'
011600071227     c                   eval      ptipoa = 'Ordinante'
011700071227     c                   other
011800080108     c                   eval      ptipoa = 'Tutte'
011900071227     c                   endsl
012000071227      * tipo frequenza ritiro
012100071227     c                   select
012200071227     c                   when      r11tcr = 'F'
012300071227     c                   eval      ptipor = 'Fisso'
012400071227     c                   when      r11tcr = 'R'
012500071227     c                   eval      ptipor = 'Ricorrente'
012600071227     c                   when      r11tcr = 'O'
012700081015     C                   eval      ptipor = 'No Internet'
012800071227     c                   when      r11tcr = 'M'
012900071227     C                   eval      ptipor = 'Mai'
013000071227     c                   other
013100080108     c                   eval      ptipor = 'Tutte'
013200071227     c                   endsl
013300071227      * codice piano dei conti
013400071227     c                   if        r11ksc = *zeros
013500071227     c                   eval      pdesksc = 'Tutti'
013600071227     c                   else
013700071227     c                   clear                   tibs69ds
013800071227     c                   clear                   ds_cnaco
013900071227     c                   clear                   ds_cnind
014000071227     c                   clear                   ds_cnclp
014100071227     c                   clear                   ds_fncls
014200080104     c                   eval      i69kac = r11ksc
014300071227     c                   call      'TIBS69R'
014400071227     c                   parm                    tibs69ds
014500071227     c                   parm                    ds_cnaco
014600071227     c                   parm                    ds_cnind
014700071227     c                   parm                    ds_cnclp
014800071227     c                   parm                    ds_fncls
014900071227     c                   eval      pdesksc = acorag
015000071227     c                   endif
015100071227      * solo clienti con giro
015200080108     c                   if        r11sol = *blanks
015300071227     c                   eval      psoloz = 'Tutti'
015400071227     c                   else
015500071227     c                   eval      psoloz = 'Solo Clienti con Giro Ritiro'
015600080108     c                   select
015700080108     c                   when      r11pocgi <> *zeros
015800080108     c                   eval      psoloz = %trim(psoloz) + ' ' +
015900080108     c                             %editc(r11pocgi:'X')
016000080108     c                   when      r11poa <> *zeros
016100080108     c                   eval      psoloz = %trim(psoloz) + ' ' +
016200080108     c                             %editc(r11poa:'X')
016300080108     c                   other
016400080108     c                   eval      psoloz = %trim(psoloz) + ' ' +
016500080108     c                             %editc(r11fil:'X')
016600080108     c                   endsl
016700071227     c                   endif
016800071227      * giro ritiro
016900071227     c                   if        r11cgi = *blanks
017000071227     c                   eval      pdescgi = 'Tutti'
017100071227     c                   else
017200071227     c                   clear                   fidg09ds
017300071227     c                   eval      d09iop0 = '001'
017400080108     c                   eval      d09ifgs = r11pocgi
017500071227     c                   eval      d09idat = *date
017600071227     c                   eval      d09icgi = r11cgi
017700071227     c                   eval      d09itug = 'R'
017800071227     c                   eval      kpjbu = fidg09ds
017900071227     c                   call      'FIDG09R'
018000071227     c                   parm                    kpjba
018100071227     c                   eval      fidg09ds = kpjbu
018200071227     c                   eval      pdescgi = d09odes
018300071227     c                   endif
018400071227
018500071227     c                   write     pripag
018600080107     c                   eval      *in90 = *on
018700000927
018800071227     c                   endsr
018900071227
019000071227      *------------------------------------------------------------------------*
019100071227      *  PREPARO LA STRINGA SQL
019200071227      *------------------------------------------------------------------------*
019300071227     c     sr_stringa    begsr
019400071227
019500071227     c                   clear                   stringasql
019600080104
019700071227      * se richiesto solo clienti con giro di ritiro o richiesto un giro in particolare
019800080123      * oppure ordinamento per giro
019900071227      * leggo FNACR00F + FNACR10F
020000071227     c                   if        r11sol <> *blanks or r11cgi <> *blanks
020100080123     c                             or r11ord = '3'
020200071227     c                   eval      stringasql =
020300071227     c                             'select fnacr00f.*, fnacr10f.* '           +
020400071227     c                             'from fnacr00f join fnacr10f on '          +
020500071227     c                             'acrcro=acr1cro'
020600071227     c                   else
020700071227      * negli altri casi leggo FNACR00F
020800071227     c                   eval      stringasql =
020900071227     c                             'select fnacr00f.* '                       +
021000071227     c                             'from fnacr00f '
021100071227     c                   endif
021200080104
021300080104      * seleziono solo i record non annullati
021400080104     c                   eval      stringasql =
021500080104     c                             %trim(stringasql) + ' '                    +
021600080104     c                             'where acratb = ' + ''''                   +
021700080104     c                             ' ' + ''''
021800071227
021900080104      * richiesto un range di clienti
022000080104     c                   if        r11crada <> *zeros
022100080104     c                   eval      stringasql =
022200080104     c                             %trim(stringasql) + ' '                    +
022300080107     c                             'and acrcro between '                      +
022400080104     c                              %editc(r11crada:'X') + ' and '            +
022500080104     c                              %editc(r11craal:'X') + ' '
022600080104     c                   endif
022700080104
022800080104      * richiesta una ragione sociale
022900080104     c                   if        r11rsr <> *blanks
023000080104     c                   eval      stringasql =
023100080104     c                             %trim(stringasql) + ' '                    +
023200080104     c                             'and acrrsr like '                         +
023300080104     c                             '''' + %trimr(r11rsr)                      +
023400080104     c                             %trimr('%')  + '''' + ' '
023500080104     c                   endif
023600071227
023700080104      * richiesta la filiale di gestione cliente
023800080104     c                   if        r11poa <> *zeros
023900080104     c                   eval      stringasql =
024000080104     c                             %trim(stringasql) + ' '                    +
024100150910     c                             'and substr(digits(ACRcro), 1, 3) = '      +
024200080104     c                              %editc(r11poa:'X') + ' '
024300080104     c                   endif
024400080104
024500080104      * richiesto tipo anagrafica
024600080104     c                   if        r11tac <> *blanks
024700080104     c                   eval      stringasql =
024800080104     c                             %trim(stringasql) + ' '                    +
024900080104     c                             'and acrtac = '                            +
025000080104     c                             '''' + r11tac + '''' + ' '
025100080104     c                   endif
025200080104
025300080104      * richiesto frequenza ritiro
025400080104     c                   if        r11tcr <> *blanks
025500080104     c                   eval      stringasql =
025600080104     c                             %trim(stringasql) + ' '                    +
025700080104     c                             'and acrtcr = '                            +
025800080104     c                             '''' + r11tcr + '''' + ' '
025900080104     c                   endif
026000080104
026100080104      * richiesto codice piano dei conti
026200080104     c                   if        r11ksc <> *zeros
026300080104     c                   eval      stringasql =
026400080104     c                             %trim(stringasql) + ' '                    +
026500080104     c                             'and acrksc = '                            +
026600080104     c                              %editc(r11ksc:'X') + ' '
026700080104     c                   endif
026800080108
026900080108      * richiesta solo anagrafiche con giro di ritiro
027000080108     c                   if        r11sol <> *blanks
027100080108     c                   eval      stringasql =
027200080108     c                             %trim(stringasql) + ' '                    +
027300080108     c                             'and acr1pocgi = '
027400080108     c                   select
027500080108     c                   when      r11pocgi <> *zeros
027600080108     c                   eval      stringasql =
027700080108     c                             %trim(stringasql)                          +
027800080108     c                              %editc(r11pocgi:'X') + ' '
027900080108     c                   when      r11poa <> *zeros
028000080108     c                   eval      stringasql =
028100080108     c                             %trim(stringasql)                          +
028200080108     c                              %editc(r11poa:'X') + ' '
028300080108     c                   other
028400080108     c                   eval      stringasql =
028500080108     c                             %trim(stringasql)                          +
028600080108     c                              %editc(r11fil:'X') + ' '
028700080108     c                   endsl
028800080108     c                   endif
028900080104
029000080107      * richiesta filiale giro di ritiro
029100080107     c                   if        r11pocgi <> *zeros
029200080107     c                   eval      stringasql =
029300080107     c                             %trim(stringasql) + ' '                    +
029400080107     c                             'and acr1pocgi = '                         +
029500080107     c                              %editc(r11pocgi:'X') + ' '
029600080107     c                   endif
029700080104      * richiesto giro di ritiro
029800080104     c                   if        r11cgi <> *blanks
029900080104     c                   eval      stringasql =
030000080104     c                             %trim(stringasql) + ' '                    +
030100080104     c                             'and acr1cgi = '                           +
030200080104     c                             '''' + r11cgi + '''' + ' '
030300080104     c                   endif
030400150910
030500150910      * richiesti solo i clienti con filiale ritiro forzata diversa
030600150910     c                   IF        R11fdiff <> *blanks
030700150910     c                   eval      stringasql =
030800150910     c                             %trim(stringasql) + ' '                    +
030900150910     c                             'and substr(digits(ACRcro), 1, 3) <> '     +
031000150910     c                             'ACRpoa '
031100150910     c                   ENDIF
031200071227
031300080104      * ordinato per codice cliente ritiro
031400080104     c                   if        r11ord = '1'
031500071227     c                   eval      stringasql =
031600071227     c                             %trim(stringasql) + ' '                     +
031700080104     c                             'order by acrcro'
031800071227     c                   endif
031900080104
032000080104      * ordinato per codice cliente ritiro
032100080104     c                   if        r11ord = '2'
032200080104     c                   eval      stringasql =
032300080104     c                             %trim(stringasql) + ' '                     +
032400080104     c                             'order by acrrsr, acrcro'
032500080104     c                   endif
032600080123
032700080123      * ordinato per codice giro
032800080123     c                   if        r11ord = '3'
032900080123     c                   eval      stringasql =
033000080123     c                             %trim(stringasql) + ' '                     +
033100080123     c                             'order by acr1ain desc, acr1cgi, acr1cro'
033200080123     c                   endif
033300071227
033400071227     c                   endsr
033500080104
033600080104      *------------------------------------------------------------------------*
033700080104      *  LEGGO IL FILE
033800080104      *------------------------------------------------------------------------*
033900080104     c     sr_leggi      begsr
034000080104
034100080104     c/exec sql
034200080104     c+ prepare s1 from :stringasql
034300080104     c/end-exec
034400080104
034500080104     c/exec sql
034600080104     c+ declare a1 cursor for s1
034700080104     c/end-exec
034800080104
034900080104     c/exec sql
035000080104     c+ open a1
035100080104     c/end-exec
035200080104
035300080104     c                   do        *hival
035400080104
035500080104      * se ho preparato il join tutti e due i formati
035600080104     c                   if        r11sol <> *blanks or r11cgi <> *blanks
035700080104     c/exec sql
035800080104     c+ fetch next from a1 into :fnacrds, :fnacr1ds
035900080104     c/end-exec
036000080104      * se no solo 1 formato
036100080104     c                   else
036200080104     c/exec sql
036300080104     c+ fetch next from a1 into :fnacrds
036400080104     c/end-exec
036500080104     c                   endif
036600080104
036700080104      * errore o fine file esco
036800080104     c                   if        sqlcod < 0 or sqlcod = 100
036900080104     c                   leave
037000080104     c                   endif
037100080104
037200080104      * a rottura di cliente
037300080104     c                   if        savacrcro <> acrcro
037400080104
037500080104      * stampo il dettaglio
037600080104     c                   exsr      sr_stampa
037700080104
037800080104      * stampo i giri memorizzati sul cliente
037900080104     c                   exsr      sr_giri
038000080107
038100080107      * stampo riga vuota
038200080107     c                   write     vuoto
038300080104
038400080104     c                   eval      savacrcro = acrcro
038500080104     c                   endif
038600080104
038700080104     c                   enddo
038800080104
038900080104     c/exec sql
039000080104     c+ close a1
039100080104     c/end-exec
039200080104
039300080104     c                   endsr
039400080104
039500080104      *------------------------------------------------------------------------*
039600080104      *  STAMPO IL DETTAGLIO       ?
039700080104      *------------------------------------------------------------------------*
039800080104     c     sr_stampa     begsr
039900080104
040000080104      * stampo la testata
040100080104     c                   if        *in90
040200080104     c                   write     testa
040300080104     c                   eval      *in90 = *off
040400080104     c                   endif
040500080104
040600131114     c                   eval      dACR01 = ACRmai
040700131114      * - Valorizzazione degli orari solo SE effettivamente numerici
040800131114     c                   if        %check( c_Digits : %subst(
040900131114     c                                     ACRmai : 1
041000131114     c                                            : %len(dACR01.你CRoa1) )
041100131114     c                                   ) = *zero
041200131114     c                   eval      pHMamI = %int( dACR01.你CRoa1 )
041300131114     c                   endif
041400131114     c                   if        %check( c_Digits : %subst(
041500131114     c                                     ACRmai : %len(dACR01.你CRoa1) + 1
041600131114     c                                            : %len(dACR01.你CRoa2) )
041700131114     c                                   ) = *zero
041800131114     c                   eval      pHMamF = %int( dACR01.你CRoa2 )
041900131114     c                   endif
042000131114     c                   if        %check( c_Digits : %subst(
042100131114     c                                     ACRmai : %len(dACR01.你CRoa1) +
042200131114     c                                              %len(dACR01.你CRoa2) + 1
042300131114     c                                            : %len(dACR01.你CRoa3) )
042400131114     c                                   ) = *zero
042500131114     c                   eval      pHMpmI = %int( dACR01.你CRoa3 )
042600131114     c                   endif
042700131114     c                   if        %check( c_Digits : %subst(
042800131114     c                                     ACRmai : %len(dACR01.你CRoa1) +
042900131114     c                                              %len(dACR01.你CRoa2) +
043000131114     c                                              %len(dACR01.你CRoa3) + 1
043100131114     c                                            : %len(dACR01.你CRoa4) )
043200131114     c                                   ) = *zero
043300131114     c                   eval      pHMpmF = %int( dACR01.你CRoa4 )
043400131114     c                   endif
043500131114
043600080104     c                   eval      prefer = acrper
043700080104
043800080104      * tariffa cliente
043900080104     c                   if        acrccc = 999
044000080104     c                   clear                   pctr
044100080104     c                   else
044200080104     c                   move      acrccc        pctr
044300080104     c                   endif
044400080104
044500080104      * orm commissionato
044600080104     c                   if        acrfcc = *blanks
044700080104     c                   eval      pcomm = 'SI'
044800080104     c                   else
044900080104     c                   eval      pcomm = 'NO'
045000080104     c                   endif
045100150910
045200150910     c                   eval      *in16 = *off
045300150910     c                   clear                   despoa
045400150910     c                   IF        ACRpoa <>
045500150910     c                             %dec(%subst(%editc(ACRcro:'X'):1:3):3:0)
045600150910      * decodifico filiale ritiro forzata
045700150910     c     ACRpoa        chain     AZORG01L
045800150910     c                   IF        %found(AZORG01L)
045900150910     c                   eval      despoa = ORGdes
046000150910     c                   ENDIF
046100150910     c                   eval      *in16 = *on
046200150910     c                   ENDIF
046300080104
046400080104     c                   write     dett
046500080104
046600080104     c                   endsr
046700080104
046800080104      *------------------------------------------------------------------------*
046900080104      *  STAMPO I GIRI
047000080104      *------------------------------------------------------------------------*
047100080104     c     sr_giri       begsr
047200080107
047300080107     c                   clear                   ppocgi1
047400080107     c                   clear                   ppocgi2
047500080107     c                   clear                   ppocgi3
047600080107     c                   clear                   pcgi1
047700080107     c                   clear                   pcgi2
047800080107     c                   clear                   pcgi3
047900080107     c                   clear                   pdcgi1
048000080107     c                   clear                   pdcgi2
048100080107     c                   clear                   pdcgi3
048200080104
048300080104     c     acrcro        setll     fnacr11l
048400080104     c                   do        *hival
048500080104     c     acrcro        reade     fnacr11l
048600080104
048700080107      * fine file
048800080104     c                   if        %eof(fnacr11l)
048900080104     c                   leave
049000080104     c                   endif
049100080104
049200080108     c                   select
049300080104      * solo la filiale del giro selezionato
049400080108     c                   when      r11pocgi <> *zeros and acr1pocgi <> r11pocgi
049500080104     c                   iter
049600080104      * solo la filiale di appartenenza
049700080108     c                   when      r11poa <> *zeros and acr1pocgi <> r11poa
049800080104     c                   iter
049900080108      * solo la filiale utente
050000080108     c                   when      acr1pocgi <> r11fil
050100080108     c                   iter
050200080108     c                   endsl
050300080107
050400080104      * imposto giro standard
050500080104     c                   if        acr1ain = '='
050600080104     c                   eval      ppocgi1 = acr1pocgi
050700080104     c                   eval      pcgi1 = acr1cgi
050800080104     c                   exsr      sr_decgiro
050900080104     c                   eval      pdcgi1 = wdescgi
051000080104     c                   endif
051100080104      * imposto giro oltre
051200080104     c                   if        acr1ain = '>'
051300080104     c                   eval      ppocgi2 = acr1pocgi
051400080104     c                   eval      pcgi2 = acr1cgi
051500080104     c                   exsr      sr_decgiro
051600080104     c                   eval      pdcgi2 = wdescgi
051700080104     c                   endif
051800080104      * imposto giro sotto
051900080104     c                   if        acr1ain = '<'
052000080104     c                   eval      ppocgi3 = acr1pocgi
052100080104     c                   eval      pcgi3 = acr1cgi
052200080104     c                   exsr      sr_decgiro
052300080104     c                   eval      pdcgi3 = wdescgi
052400080104     c                   endif
052500080104
052600080104     c                   enddo
052700080104
052800080107      * se almeno un giro impostato
052900080107     c                   if        pcgi1 <> *blanks or pcgi2 <> *blanks or
053000080107     c                             pcgi3 <> *blanks
053100080104      * stampo la testata
053200080104     c                   if        *in90
053300080104     c                   write     testa
053400080104     c                   eval      *in90 = *off
053500080104     c                   endif
053600080104
053700080104     c                   write     giro
053800080107     c                   endif
053900080104
054000080104     c                   endsr
054100080104
054200080104      *------------------------------------------------------------------------*
054300080104      *  DECODIFICO IL GIRO
054400080104      *------------------------------------------------------------------------*
054500080104     c     sr_decgiro    begsr
054600080104
054700080107     c                   clear                   wdescgi
054800080104     c                   clear                   fidg09ds
054900080104     c                   eval      d09iop0 = '001'
055000080104     c                   eval      d09ifgs = acr1pocgi
055100080104     c                   eval      d09idat = *date
055200080104     c                   eval      d09icgi = acr1cgi
055300080104     c                   eval      d09itug = 'R'
055400080104     c                   eval      kpjbu = fidg09ds
055500080104     c                   call      'FIDG09R'
055600080104     c                   parm                    kpjba
055700080104     c                   eval      fidg09ds = kpjbu
055800080104      * imposto la descrizione del giro
055900080104     c                   eval      wdescgi = d09odes
056000080104
056100080104     c                   endsr
056200071227
056300071227      *------------------------------------------------------------------------*
056400080104      *  OPERAZIONI INIZIALI       ?
056500071227      *------------------------------------------------------------------------*
056600071227     c     *inzsr        begsr
056700071227
056800071227     c     *entry        plist
056900071227     c                   parm                    kpjba
057000071227
057100071227     c                   eval      fior11ds = kpjbu
057200071227
057300071227     c     *dtaara       define    吧zute        azuteds
057400071227     c     *dtaara       define    吳atiute      ddatiute
057500071227     c                   in(e)     *dtaara
057600071227     c                   if        %error or rsut = *blanks
057700071227     c                   clear                   tibs34ds
057800071227     c                   call      'TIBS34R'
057900071227     c                   parm                    tibs34ds
058000071227     c                   in        *dtaara
058100071227     c                   endif
058200080108
058300080108      * imposto se filiale attiva con GEO ORM
058400080108     c                   eval      *in15 = (r11geo = 'S')
058500071227
058600071227     c                   endsr
