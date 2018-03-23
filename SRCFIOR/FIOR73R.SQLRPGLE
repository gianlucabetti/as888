000100070802     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200070802
000300070802      *------------------------------------------------------------------------*
000400070802      *                   ATTRIBUZIONE GIRO ORM        ?
000500070802      *------------------------------------------------------------------------*
000600070802
000700080208     fazcln01l  if   e           k disk
000800070803     fazorg01l  if   e           k disk
000900070803     ffnorm01l  if   e           k disk
001000070803     ffnorg01l  uf   e           k disk
001100070803     ffior73d   cf   e             workstn sfile(or73s01:nrr)
001200070802
001300070802      *------------------------------------------------------------------------*
001400070802      *  RIEPILOGO INDICATORI
001500070802      *------------------------------------------------------------------------*
001600070802      *  05 - sproteggo filiale in gestione
001700070802      *  06 - non abilito f18
001800070803      *  10 - ON  selezione Multipla
001900070803      *  10 - OFF selezione Singola
002000070803      *  20 - gestione subfile
002100070803      *  21 - gestione subfile
002200070802      *  28 - errore generico
002300070803      *  30 - comodo
002400070803      *  31 - fine file x subfile
002500071029      *  40 - PC nel subfile
002600070803      *  41 - PC filiale gestione
002700070803      *  42 - PC giro
002800070803      *  43 - PC località
002900080208      *  44 - Data ritiro
003000070803      *  90 - riemetto la videata
003100070802
003200070802      *------------------------------------------------------------------------*
003300070802      *   V A R I A B I L I
003400070802      *------------------------------------------------------------------------*
003500070802     d con_giro        s              1    inz
003600070904     d data_org        s              8  0 inz
003700070803     d dataeur         s               d   datfmt(*eur)
003800070803     d dataiso         s               d   datfmt(*iso)
003900080208     d ktfp            s                   like(clntfp)
004000080208     d ktfa            s                   like(clntfa)
004100080208     d kann            s                   like(clnann)
004200080208     d kmes            s                   like(clnmes)
004300070803     d nrr             s                   like(recsf)
004400070803     d nrr_x           s                   like(recsf)
004500070803     d sav_v2ccgin     s                   like(v2ccgin)
004600070803     d stringasql      s           1100    varying
004700070904     d test_simula     s              1    inz
004800071019     d wcgi            s                   like(orgcgi) inz
004900080208     d wdar            s              8  0
005000070806     d wdata           s              8  0
005100070803     d wnrr            s                   like(recsf)
005200070806     d wora            s              6  0
005300070903     d wzeta           s             19    inz(*all'Z')
005400070806     d w0140           s             14  0
005500070806     d $aggrcd         s              1    inz(*off)
005600070803     d $fatto          s              1    inz(*off)
005700070803     d $multi          s              1    inz(*off)
005800070802
005900070802      *------------------------------------------------------------------------*
006000070802      *   S C H I E R E
006100070802      *------------------------------------------------------------------------*
006200070802     d msg             s             78    dim(30) ctdata perrcd(1)
006300070802
006400070802      *------------------------------------------------------------------------*
006500070802      *   D S   I N T E R N E / E S T E R N E
006600070802      *------------------------------------------------------------------------*
006700080208     d clnmat          ds
006800080208     d  mat                    1     31    dim(31)
006900080208     d clnpom          ds
007000080208     d  pom                    1     31    dim(31)
007100080208
007200080208     d                 ds
007300080208     d  ds_giorno              1      2  0
007400080208     d  ds_mese                3      4  0
007500080208     d  ds_anno                5      8  0
007600080208     d  ds_data                1      8  0
007700080208
007800070803     d dsfior07        ds
007900070803     d  dsopoe                        3  0
008000070803     d  dsonor                        7  0
008100070803     d  dsonsr                        2  0
008200070803     d  dsonrv                        2  0
008300070803     d  dsoopz                        1
008400070803     d  dsofgs                        3  0
008500070806
008600070806     d                 ds
008700070806     d  v2ccor                 1     10  0
008800070906     d  v2ccor1                1      3  0
008900070906     d  v2ccor2                4      7  0
009000070806     d  v2ccor3                8     10  0
009100070806
009200070806     d                 ds
009300070806     d  v2ccra                 1     10  0
009400070906     d  v2ccra1                1      3  0
009500070906     d  v2ccra2                4      7  0
009600070806     d  v2ccra3                8     10  0
009700080208
009800080208     d wlbdat          ds                  inz
009900080208     d  g08dat                 1      8  0
010000080208     d  g08inv                 9     16  0
010100080208     d  g08err                17     17
010200080208     d  g08tgi                18     22  0
010300070806
010400070802     d azuteds       e ds                  extname(azute00f)
010500070802     d ddatiute      e ds
010600070803     d dfar          e ds
010700080208     d ddft          e ds
010800070803     d fidg08ds      e ds
010900070802     d fidg09ds      e ds
011000071026     d fior05ds      e ds
011100070904     d fior64ds      e ds
011200070802     d fnlv50ds      e ds
011300070802     d fnorgds       e ds                  extname(fnorg00f)
011400070802     d fnormds       e ds                  extname(fnorm00f)
011500070802     d kpjba         e ds
011600070802     d og147         e ds
011700070803     d tibs02ds      e ds
011800070802     d tibs34ds      e ds
011900131119     d fior73ds      e ds
012000070802
012100070802     d                sds
012200070803     d  vtcpgm                 1     10
012300070802
012400070802      *------------------------------------------------------------------------*
012500070802      *   C O S T A N T I
012600070802      *------------------------------------------------------------------------*
012700070802
012800070802      **********************************************************************
012900070802      * Constants
013000070802      *     MaxKey - è il max. num. di campi chiave permesso in questo pgm
013100070802      **********************************************************************
013200070802      * Indice di schiera dei campi chiave di ordinamento del SFL  (MAXkey)
013300070802     d MaxKey          c                   9
013400070802     d Ascendente      c                   1
013500070802     d Discendente     c                   2
013600070802     d Carattere       c                   6
013700070802     d Put             c                   1
013800070802     d EndPut          c                   2
013900070802     d Get             c                   3
014000070802     d Numerico        c                   8
014100070802      **********************************************************************
014200070802      * Campi utili:
014300070802      *     RRN1       - Numero relativo di record del Subfile
014400070802      *     SizeList   - Dimensione della lista
014500070802      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamento
014600070802      **********************************************************************
014700070802     d Rrn1            s              5I 0
014800070802     d NotUsed         s             16A
014900070802     d ReturnSize      s              9B 0
015000070802     d SizeList        s              9B 0
015100070802     d RrnLast         s              5I 0
015200070802     d WrkSort         s              1  0 inz(0)
015300070802     d WindSort        s              3I 0 inz(0)
015400070802      **********************************************************************
015500070802      * Data Structures
015600070802      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
015700070802      *     QLGSCB     - The sort request block for the QLGSORT API
015800070802      *     QLGSCB00   - The sort request block for the QLGSRTIO API
015900070802      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
016000070802      *     QUSEC      - Error structure for the QLGSORT API
016100070802      **********************************************************************
016200070802     d sflrcd          ds
016300070803     d  vs1lor
016400070803     d  vs1prr
016500070803     d  vs1inr
016600070803     d  vs1rsr
016700070803     d  vs1pkg
016800070803     d  vs1vlm
016900070803     d  vs1ncl
017000071019     d  vs1bnc
017100070803     d  vs1cgi
017200070803     d  vs1poe
017300070803     d  vs1nsr
017400070803     d  vs1nor
017500070803     d  vs1nrv
017600070803     d  vs1sce
017700070802     d  selected                      1A
017800070125
017900070125      /COPY QSYSINC/QRPGLESRC,QLGSORT
018000070125     d QLGKL                         16    DIM(MaxKey)
018100070125     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
018200070125     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
018300070125     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
018400070125     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
018500070125
018600070125      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
018700070125      /COPY QSYSINC/QRPGLESRC,QUSEC
018800070802
018900070802      *------------------------------------------------------------------------*
019000131119
019100131119       //?Se richiamato da assegnazione manuale ORM
019200131119     c                   IF        %parms > 1
019300131119       //?richiama una routine a parte
019400131119     c                   exsr      DaAssMan
019500131119     c                   goto      fine
019600131119     c                   ENDIF
019700070802
019800070903      * pulisco la videata
019900070903     c                   exsr      sr_puld01
020000070803      * gestione della prima videata --> parametri
020100070803     c     emid01        tag
020200070803      * emetto la videata
020300070803      *                  __________________
020400070803     c                   exfmt     or73d01
020500070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
020600070803      * spengo indicatori di comodo
020700070803     c                   eval      *in28 = *off
020800070803     c                   eval      *in42 = *off
020900070803     c                   eval      *in90 = *off
021000070803      * pulisco campo messaggi
021100070803     c                   clear                   v1cmsg
021200070803      * f3=fine
021300070803     c                   if        *inkc
021400070803     c                   goto      fine
021500070803     c                   endif
021600070803      * f7=interrogazione giri
021700070803     c                   if        *inkg
021800070803     c                   exsr      sr_f07
021900070803     c                   eval      v1ccgi = d09ocgi
022000070803     c                   eval      *in42 = *on
022100070803     c                   eval      *in90 = *on
022200070803     c                   goto      emid01
022300070803     c                   endif
022400070803      * f18=cambio filiale
022500070803     c                   if        *inks
022600070803     c                   eval      *in05 = *on
022700070803     c                   goto      emid01
022800070803     c                   endif
022900070803      * controllo dati immessi
023000070803     c                   exsr      sr_ctrd01
023100070803     c   28              goto      emid01
023200070803
023300070803     c     carica        tag
023400070803      * preparo la stringa SQL
023500070803     c                   exsr      sr_carica
023600070803      * preparo il subfile dettaglio ORM
023700070803     c                   exsr      sr_pulsfl
023800070803     c                   exsr      sr_carsfl
023900070803      * gestione del subfile
024000070803     c                   exsr      sr_ordsfl
024100070803     c                   exsr      sr_gessfl
024200071026     c                   if        $aggrcd = *on
024300070806     c                   goto      carica
024400070806     c                   endif
024500070802
024600070803     c     fine          tag
024700070802     c                   eval      *inlr = *on
024800070802
024900070803      *------------------------------------------------------------------------*
025000070803      *   Pulizia primo video --> parametri
025100070803      *------------------------------------------------------------------------*
025200070803     c     sr_puld01     begsr
025300070803
025400070803     c                   clear                   v1clord
025500070803     c                   clear                   v1clora
025600070803     c                   clear                   v1crsr
025700070803     c                   clear                   v1ccgi
025800070803      * pulisco anche il campo di posizionamento interrogazioni
025900070803      * dato che ho una routine unica per interrogare i giri
026000070803     c                   clear                   v2cposin
026100070803
026200070803     c                   endsr
026300070803
026400070802      *------------------------------------------------------------------------*
026500070802      *   Controlli primo video --> parametri
026600070802      *------------------------------------------------------------------------*
026700070803     c     sr_ctrd01     begsr
026800070802
026900070803     c                   eval      *in41 = *off
027000070803     c                   eval      *in42 = *off
027100070803     c                   eval      *in43 = *off
027200080208     c                   eval      *in44 = *off
027300070802
027400070802      * filiale gestione
027500070802     c                   clear                   og147
027600070802     c     v1cfgs        chain     azorg01l
027700070802     c                   if        not %found(azorg01l)
027800070803     c                   eval      v1cmsg = msg(01)
027900070802     c                   eval      *in28 = *on
028000070803     c                   eval      *in41 = *on
028100070802     c                   leavesr
028200070802     c                   endif
028300070803      * descrizione
028400070803     c                   eval      v1dfgs = orgdes
028500070803      * in gestione all'utente
028600070803     c                   clear                   fnlv50ds
028700070803     c                   eval      d50pru = knmus
028800070803     c                   eval      d50fgs = v1cfgs
028900070803     c                   call      'FNLV50R'
029000070803     c                   parm                    fnlv50ds
029100070803     c                   if        d50err <> *blanks
029200070803     c                   eval      v1cmsg = msg(02)
029300070803     c                   eval      *in28 = *on
029400070803     c                   eval      *in41 = *on
029500070803     c                   leavesr
029600070803     c                   endif
029700070803      * controllo se filiale attivata
029800070802     c                   eval      og147 = orgde7
029900070803     c                   if        §ogcgio = *blank
030000070803     c                   eval      v1cmsg = msg(03)
030100070803     c                   eval      *in28 = *on
030200070803     c                   eval      *in41 = *on
030300070802     c                   leavesr
030400070802     c                   endif
030500070926     c                   if        §ogddao = *blank or §ogddao = *all'0'
030600070926     c                   goto      fine
030700070926     c                   endif
030800070904      * data partenza procedura in organigramma
030900070904     c                   move      §ogddao       data_org
031000070904      * controllo se siamo in simulazione
031100070904     c                   if        *date < data_org
031200070904     c                   eval      test_simula = 'S'
031300070904     c                   else
031400070904     c                   clear                   test_simula
031500070904     c                   endif
031600070803
031700070802     c                   eval      *in05 = *off
031800070802
031900070802      * se imposto località 'DA' e località 'A' a blank la inserisco uguale
032000070802     c                   if        v1clord <> *blank and v1clora = *blank
032100070903     c                   eval      v1clora = %trimr(v1clord) + wzeta
032200070802     c                   endif
032300070802      * se imposto località 'DA' e località 'A' con solo iniziale metto nel
032400070802      * secondo byte la 'Z' x la selezione
032500070802     c                   if        v1clord <> *blank and v1clora <> *blank and
032600070802     c                             %subst(v1clora:2:1) = ' '
032700070903     c                   eval      v1clora = %trimr(v1clora) + wzeta
032800070802     c                   endif
032900070802      * vice versa località 'A' e località 'DA' a blank la inserisco uguale
033000070802     c                   if        v1clora <> *blank and v1clord = *blank
033100070802     c                   eval      v1clord = v1clora
033200070802     c                   endif
033300070802      * verifico che località 'DA' sia congruente con località 'A'
033400070802     c                   if        v1clord > v1clora
033500070803     c                   eval      v1cmsg = msg(05)
033600070803     c                   eval      *in28 = *on
033700070803     c                   eval      *in43 = *on
033800070802     c                   leavesr
033900070802     c                   endif
034000070802
034100070802      * giro
034200070802     c                   if        v1ccgi <> *blank and v1ccgi <> '9999999999'
034300070802     c                   eval      con_giro = 'S'
034400070802      * no giro
034500070802     c                   endif
034600070802      * senza giro
034700070802     c                   if        v1ccgi = *blanks
034800070802     c                   eval      con_giro = 'N'
034900070802     c                   endif
035000070802      * tutti
035100070802     c                   if        v1ccgi = *all'9'
035200070802     c                   clear                   con_giro
035300070802     c                   endif
035400080208
035500080208      * data ritiro obbligatoria
035600080208     c                   if        v1cdar = *zeros
035700080208     c                   eval      *in28 = *on
035800080208     c                   eval      *in44 = *on
035900080208     c                   eval      v1cmsg = msg(08)
036000080208     c                   leavesr
036100080208     c                   endif
036200080208
036300080208      * data ritiro valida
036400080208     c                   if        v1cdar > *zeros
036500080208     c                   clear                   wlbdat
036600080208     c                   z-add     v1cdar        g08dat
036700080208     c                   call      'XSRDA8'
036800080208     c                   parm                    wlbdat
036900080208     c                   if        g08err = '1'
037000080208     c                   eval      *in28 = *on
037100080208     c                   eval      *in44 = *on
037200080208     c                   eval      v1cmsg = msg(07)
037300080208     c                   leavesr
037400080208     c                   endif
037500080208     c                   z-add     g08dat        v1cdar
037600080208     c                   z-add     g08inv        wdar
037700080208     c                   endif
037800070802
037900070802     c                   endsr
038000070803
038100070803      *------------------------------------------------------------------------*
038200070803      *   Preparo la stringa SQL
038300070803      *------------------------------------------------------------------------*
038400070803     c     sr_carica     begsr
038500070803
038600070803     c                   clear                   stringasql
038700070803
038800070803      * preparo la stringa sql
038900070803     c                   eval      stringasql =
039000071029     c                             'select fnorg52j.* from '                  +
039100071029     c                             'fnorg52j '                                +
039200071029     c                             'where orgpor =  '                         +
039300071029     c                             %editc(v1cfgs:'X')                         +
039400071029     c                             ' and orgnsr = 0'
039500071029      * se simulazione solo orgndc = 0
039600071029     c                   if        test_simula = 'S'
039700071029     c                   eval      stringasql =
039800071029     c                             %trim(stringasql) + ' '                     +
039900071029     c                             'and orgndc = 0'
040000071029     c                   endif
040100071029      * se filiale già attiva solo ormndc = 0
040200071029     c                   if        test_simula <> 'S'
040300071029     c                   eval      stringasql =
040400071029     c                             %trim(stringasql) + ' '                     +
040500071029     c                             'and ormndc = 0'
040600071029     c                   endif
040700071029
040800070910      * parzializzazione per localita richiesto range a video
040900070910     c                   if        v1clord <> *blanks and v1clora <> *blanks
041000070910     c                   eval      stringasql =
041100070910     c                             %trim(stringasql) + ' '                     +
041200070910     c                             'and ormlor between '                       +
041300070910     c                             '''' + v1clord + ''''                       +
041400070910     c                             'and '                                      +
041500070910     c                             '''' + v1clora + ''''
041600070910     c                   endif
041700070910      * per nome mittente
041800070910     c                   if        v1crsr <> *blanks
041900070910     c                   eval      stringasql =
042000070910     c                             %trim(stringasql) + ' '                     +
042100070910     c                             'and ormrsr like ' + '''%'                  +
042200070910     c                             v1crsr
042300070910     c                   eval      stringasql =
042400070910     c                             %trim(stringasql) + '%'''
042500070910     c                   endif
042600070803      * se presente il giro seleziono solo quel giro
042700070803     c                   if        con_giro = 'S'
042800070803     c                   eval      stringasql =
042900070803     c                             %trim(stringasql) + ' '                     +
043000070803     c                             'and orgpocgi = '                           +
043100070803     c                              %editc(v1cfgs:'X') + ' '                   +
043200070803     c                             'and orgcgi = ' + '''' + v1ccgi + ''''
043300070803     c                   endif
043400070803      * solo gli orm senza giro
043500070803     c                   if        con_giro = 'N'
043600070803     c                   eval      stringasql =
043700070803     c                             %trim(stringasql) + ' '                     +
043800070803     c                             'and orgpocgi = 0 '                         +
043900070803     c                             'and orgcgi = ' + '''' + v1ccgi  + ''''
044000070803     c                   endif
044100070803
044200070803     C/EXEC SQL
044300070803     C+ PREPARE S1 FROM :stringasql
044400070803     C/END-EXEC
044500070803
044600070803     C/EXEC SQL
044700070803     C+ DECLARE A1 CURSOR FOR S1
044800070803     C/END-EXEC
044900070803
045000070803     C/EXEC SQL
045100070803     C+ OPEN A1
045200070803     C/END-EXEC
045300070803
045400070803     c                   endsr
045500070803
045600070803      *------------------------------------------------------------------------*
045700070803      *   Pulisco il subfile
045800070803      *------------------------------------------------------------------------*
045900070803     c     sr_pulsfl     begsr
046000070803
046100070803     c                   clear                   nrr
046200070803
046300070803     c                   eval      *in20 = *off
046400070803     c                   eval      *in21 = *off
046500070803      *                  __________________
046600070803     c                   write     or73c01
046700070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
046800070803     c                   eval      *in21 = *on
046900070803     c                   eval      *in20 = *on
047000070803
047100070803     c                   eval      recsf = 1
047200070803     c                   eval      *in31 = *off
047300070803     c                   eval      *in40 = *off
047400070803
047500070803     c                   endsr
047600070803
047700070803      *------------------------------------------------------------------------*
047800070803      *   Carico il subfile
047900070803      *------------------------------------------------------------------------*
048000070803     c     sr_carsfl     begsr
048100070803
048200070803     c                   do        *hival
048300070803
048400070803     C/EXEC SQL
048500071029     C+ FETCH NEXT FROM A1 INTO :fnorgds, :fnormds
048600070803     C/END-EXEC
048700070803
048800070803      * errore nel sql esco dal pgm
048900070803     c                   if        sqlcod < 0
049000070803     c                   eval      *inlr = *on
049100070803     c                   return
049200070803     c                   endif
049300070803      * fine file
049400070803     c                   if        sqlcod = 100
049500070803     c                   leave
049600070803     c                   endif
049700071029
049800070904      * controllo se l'ORM è assegnabile
049900070904     c                   exsr      check_orm
050000070904     c                   if        or64asso = 'N'
050100070904     c                   iter
050200070904     c                   endif
050300070803      * se arrivo qua il rcd è ok e devo caricare i dati
050400070803     c                   exsr      sr_wrtsfl
050500070803      * esco per raggiunta massima capacità sfl
050600070803     c                   if        nrr > 9990
050700070803     c                   leave
050800070803     c                   endif
050900070803
051000070803     c                   enddo
051100070803
051200070803     C/EXEC SQL
051300070803     C+ CLOSE A1
051400070803     C/END-EXEC
051500070803
051600070803     c                   eval      wnrr = nrr
051700070803     c                   eval      rrnlast = nrr
051800070803     c                   eval      *in31 = *on
051900070803
052000070803     c                   endsr
052100070803
052200070803      *------------------------------------------------------------------------*
052300070803      *   Scrivo la riga del subfile
052400070803      *------------------------------------------------------------------------*
052500070803     c     sr_wrtsfl     begsr
052600070803
052700070803      * imposto i campi
052800070803     c                   clear                   vs1sce
052900070803      * codice giro
053000070803     c                   eval      vs1cgi = orgcgi
053100070803      * chiave ORM
053200070803     c                   eval      vs1poe = ormpoe
053300070803     c                   eval      vs1nsr = ormnsr
053400070803     c                   eval      vs1nor = ormnor
053500070803     c                   eval      vs1nrv = ormnrv
053600070803      * mittente
053700070803     c                   eval      vs1rsr = ormrsr
053800070803     c                   eval      vs1inr = orminr
053900070803     c                   eval      vs1lor = ormlor
054000070803     c                   eval      vs1prr = ormprr
054100070803      * peso e volume da org
054200070803     c                   eval      vs1pkg = orgpkg
054300070803     c                   eval      vs1vlm = orgvlm
054400070803      * colli
054500071019     c                   z-add     ormncl        vs1ncl
054600071019      * bancali
054700071019     c                   z-add     ormbnc        vs1bnc
054800070803
054900070803      * scrivo il subfile
055000070803     c                   add       1             nrr
055100070803      * se primo rcd che scrivo accendo indicatore di PC
055200070803     c                   if        nrr = 1
055300070803     c                   eval      *in40 = *on
055400070803     c                   else
055500070803     c                   eval      *in40 = *off
055600070803     c                   endif
055700070803      *                  __________________
055800070803     c                   write     or73s01
055900070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
056000070803
056100070803     c                   endsr
056200070803
056300070803      *------------------------------------------------------------------------*
056400070803      *   Gestione del subfile
056500070803      *------------------------------------------------------------------------*
056600070803     c     sr_gessfl     begsr
056700070803
056800070806     c                   eval      $aggrcd = *off
056900070803     c                   eval      $fatto = *off
057000070803     c                   eval      $multi = *off
057100070803
057200070803     c                   do        *hival
057300070803      * emetto il piede del subfile
057400070803      *                  __________________
057500070803     c                   write     or73z01
057600070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
057700070803      * se non ho caricato niente emetto videata vuota
057800070803     c                   if        wnrr = *zeros
057900070803      *                  __________________
058000070803     c                   write     or73v01
058100070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
058200070803     c                   eval      *in20 = *off
058300070803     c                   else
058400070803     c                   eval      *in20 = *on
058500070803     c                   if        recsf > wnrr
058600070803     c                   eval      recsf = 1
058700070803     c                   endif
058800070803     c                   endif
058900070803      * emissione sfl
059000070803      *                  __________________
059100070803     c                   exfmt     or73c01
059200070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
059300070803     c                   eval      *in28 = *off
059400070803     c                   clear                   v1cmsg
059500070803
059600070803      * f3=fine
059700070803     c                   if        *inkc
059800070803     c                   goto      fine
059900070803     c                   endif
060000070803      * f12=ritorno
060100070803     c                   if        *inkl
060200070803     c                   goto      emid01
060300070803     c                   endif
060400070803      * f1=seleziona tutti
060500070803     c                   if        *inka
060600070803     c                   exsr      sr_f01
060700070803     c                   iter
060800070803     c                   endif
060900070803
061000070803      * controllo le selezioni fatte
061100070803     c                   if        wnrr > *zeros
061200070803     c                   exsr      sr_contrsfl
061300070803     c                   if        *in28
061400070803     c                   iter
061500070803     c                   endif
061600070806     c                   if        $aggrcd = *on
061700070806     c                   leave
061800070806     c                   endif
061900070803     c                   endif
062000070803
062100070803     c                   enddo
062200070803
062300070803     c                   endsr
062400070803
062500070803      *------------------------------------------------------------------------*
062600070803      *   Controllo le selezioni del subgile
062700070803      *------------------------------------------------------------------------*
062800070803     c     sr_contrsfl   begsr
062900070803
063000070803     c                   clear                   nrr
063100070803     c                   do        *hival
063200070803     c                   add       1             nrr
063300070803     c     nrr           chain     or73s01                            30
063400070803      * esco se fine sfl
063500070803     c                   if        *in30
063600070803     c                   leave
063700070803     c                   endif
063800070803
063900070803     c                   eval      *in10 = *off
064000070803
064100070803      * controllo le selezioni a video
064200070803     c                   select
064300071026
064400071026      * opzione 2 --> modifca ORM
064500071026     c                   when      vs1sce = '2'
064600071026     c                   exsr      sr_modorm
064700071026     c                   eval      recsf = nrr
064800071026     c                   clear                   vs1sce
064900071026     c                   eval      *in40 = *on
065000070803
065100070803      * opzione 5 --> visualizza ORM
065200070803     c                   when      vs1sce = '5'
065300070803     c                   exsr      sr_intorm
065400070803     c                   eval      recsf = nrr
065500070803     c                   clear                   vs1sce
065600070803     c                   eval      *in40 = *on
065700070803
065800070803      * opzione 1 --> attribuzione giro per singolo ORM
065900070803     c                   when      vs1sce = '1'
066000070803     c                   exsr      sr_singolo
066100070803     c                   eval      recsf = nrr
066200111109     c  n28              clear                   vs1sce
066300111109     c   28              eval      vs1sce = 'E'
066400070803     c                   eval      *in40 = *on
066500070803
066600070803      * opzione 9 --> attribuzione giro per più ORM
066700070803     c                   when      vs1sce = '9'
066800070803     c                   eval      *in10 = *on
066900070803     c                   exsr      sr_multiplo
067000111109     c  n28              clear                   vs1sce
067100111109     c   28              eval      vs1sce = 'E'
067200070803     c                   eval      *in40 = *on
067300111109
067400070803     c                   other
067500070803      * spengo l'indicatore di PC
067600070803     c                   eval      *in40 = *off
067700070803     c                   endsl
067800070803      *                  __________________
067900070803     c                   update    or73s01
068000070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
068100070803     c                   enddo
068200070803
068300070803     c                   endsr
068400071026
068500071026      *------------------------------------------------------------------------*
068600071026      *   Modifico l'ORM
068700071026      *------------------------------------------------------------------------*
068800071026     c     sr_modorm     begsr
068900071026
069000071026     c                   clear                   dsfior07
069100071026     c                   clear                   fior05ds
069200071026     c                   eval      dsopoe = vs1poe
069300071026     c                   eval      dsonor = vs1nor
069400071026     c                   eval      dsonsr = vs1nsr
069500071026     c                   eval      dsonrv = vs1nrv
069600071026     c                   eval      dsoopz = vs1sce
069700071026     c                   eval      dsofgs = v1cfgs
069800071026     c                   eval      kpjbu = dsfior07
069900071026     c                   clear                   fior05ds
070000071026     c                   call      'FIOR05R'
070100071026     c                   parm                    kpjba
070200071026     c                   parm                    fior05ds
070300071026     c                   eval      $aggrcd = *on
070400071026
070500071026     c                   endsr
070600070803
070700070803      *------------------------------------------------------------------------*
070800070803      *   Interrogo l'ORM
070900070803      *------------------------------------------------------------------------*
071000070803     c     sr_intorm     begsr
071100070803
071200070803     c                   clear                   dsfior07
071300070803     c                   eval      dsopoe = vs1poe
071400070803     c                   eval      dsonor = vs1nor
071500070803     c                   eval      dsonsr = vs1nsr
071600070803     c                   eval      dsonrv = vs1nrv
071700070803     c                   eval      dsoopz = vs1sce
071800070803     c                   eval      dsofgs = v1cfgs
071900070803     c                   eval      kpjbu = dsfior07
072000070803     c                   call      'FIOR07R'
072100070803     c                   parm                    kpjba
072200070803
072300070803     c                   endsr
072400070803
072500070803      *------------------------------------------------------------------------*
072600070803      *   Seleziona per attribuzione singolo ORM
072700070803      *------------------------------------------------------------------------*
072800070803     c     sr_singolo    begsr
072900070803
073000070803     c     korm          chain     fnorm01l
073100070803     c                   if        not %found(fnorm01l)
073200070803     c                   leavesr
073300070803     c                   endif
073400070803     c     korm          chain(n)  fnorg01l
073500070803     c                   if        not %found(fnorg01l)
073600070803     c                   leavesr
073700070803     c                   endif
073800070803      * imposto i campi per il video di dettaglio dati ORM
073900070803     c                   exsr      sr_datiorm
074000070803      * videata di dettaglio ORM
074100070803     c                   exsr      sr_visriga
074200070803
074300070803     c                   endsr
074400070803
074500070803      *------------------------------------------------------------------------*
074600070803      *   Seleziona per attribuzione più ORM
074700070803      *------------------------------------------------------------------------*
074800070803     c     sr_multiplo   begsr
074900070803
075000070803     c     korm          chain     fnorm01l
075100070803     c                   if        not %found(fnorm01l)
075200070803     c                   leavesr
075300070803     c                   endif
075400070803     c     korm          chain(n)  fnorg01l
075500070803     c                   if        not %found(fnorg01l)
075600070803     c                   leavesr
075700070803     c                   endif
075800070803      * imposto i campi per il video di dettaglio dati ORM
075900070803     c                   if        $multi = *off
076000070803     c                   exsr      sr_datiorm
076100070803      * videata di dettaglio ORM
076200070803     c                   exsr      sr_visriga
076300070803      * se "$fatto" ON ho aggiornato la prima riga multipla
076400070803      * quindi memorizzo i valori da aggiornare x eventuali righe con stessa
076500070803      * opzione e proseguo l'aggiornamento  senza più emettere il video per
076600070803      * queste ultime se le opzioni fossero mischiate le altre differenti
076700070803      * dalla multipla verranno elaborate normalmente nell'ambito dello stesso
076800070803      * ciclo di elaborazione. Le multiple verranno elaborate in maniera
076900070803      * occulta con il valore impostato inizialmente
077000070803     c                   if        $fatto = *on
077100070803     c                   eval      sav_v2ccgin = v2ccgin
077200070803     c                   eval      $multi = *on
077300070803     c                   endif
077400070803      * imposta il giro sulle ulteriori righe della scelta multipla
077500070803     c                   else
077600070803     c                   eval      v2ccgin = sav_v2ccgin
077700070803     c                   exsr      sr_aggiorna
077800070803     c                   endif
077900070803
078000070803     c                   endsr
078100070803
078200070803      *------------------------------------------------------------------------*
078300070803      *   Imposto i dati dell'ORM per la videata a dettaglio
078400070803      *------------------------------------------------------------------------*
078500070803     c     sr_datiorm    begsr
078600071019
078700071019     c                   clear                   v2dcgi
078800071019     c                   clear                   v2dcgin
078900070803
079000070803      * chiave ORM
079100070803     c                   eval      v2cpoe = ormpoe
079200070803     c                   eval      v2cnsr = ormnsr
079300070803     c                   eval      v2cnor = ormnor
079400070803     c                   eval      v2cnrv = ormnrv
079500070803      * ordinante
079600070803     c                   eval      v2ccor = ormcor
079700070803     c                   eval      v2crso = ormrso
079800070803      * tipo orm
079900070803     c                   clear                   v2dtor
080000070803     c                   clear                   tibs02ds
080100070803     c                   eval      t02mod = 'C'
080200070803     c                   eval      t02sif = knsif
080300070803     c                   eval      t02cod = 'TOR'
080400070803     c                   eval      t02ke1 = ormtor
080500070803     c                   call      'TIBS02R'
080600070803     c                   parm                    kpjba
080700070803     c                   parm                    tibs02ds
080800070803     c                   eval      v2dtor = t02uni
080900070803      * fase
081000070803     c                   eval      v2cfao =  ormfao
081100070803     c                   clear                   v2dfao
081200070803     c                   clear                   tibs02ds
081300070803     c                   eval      t02mod = 'C'
081400070803     c                   eval      t02sif = knsif
081500070806     c                   eval      t02cod = 'FAR'
081600070803     c                   movel(p)  ormfao        t02ke1
081700070803     c                   call      'TIBS02R'
081800070803     c                   parm                    kpjba
081900070803     c                   parm                    tibs02ds
082000070803     c                   eval      dfar = t02uni
082100070803     c                   eval      v2dfao = d§fardes
082200070803      * mittente
082300070803     c                   eval      v2ccra = ormcra
082400070803     c                   eval      v2crsr = ormrsr
082500070803      * indirizzo
082600070803     c                   eval      v2cinr = orminr
082700070803     c                   eval      v2clor = ormlor
082800070803     c                   eval      v2ccar = ormcar
082900070803     c                   eval      v2cprr = ormprr
083000070904     c                   eval      v2cnar = ormnar
083100070803      * note
083200070803     c                   eval      v2cno1 = ormno1
083300070803     c                   eval      v2cno2 = ormno2
083400070803      * colli/bancali
083500070803     c                   eval      v2cncl = ormncl
083600070803     c                   eval      v2cbnc = ormbnc
083700070803      * peso/volume da ORG
083800070803     c                   eval      v2cpkg = orgpkg
083900070803     c                   eval      v2cvlm = orgvlm
084000070803      * automezzo
084100070803     c                   select
084200070803     c                   when      ormblc <> *zeros
084300070803     c                   eval      v2caut = 'BIL'
084400070803     c                   when      ormatt <> *zeros
084500070803     c                   eval      v2caut = 'AUT'
084600070803     c                   when      ormmtc <> *zeros
084700070803     c                   eval      v2caut = 'MOT'
084800070803     c                   other
084900070803     c                   clear                   v2caut
085000070803     c                   endsl
085100070803      * sponda idraulica
085200070803     c                   if        ormspi = 'S'
085300070803     c                   eval      v2cspi = 'SI'
085400070803     c                   else
085500070803     c                   clear                   v2cspi
085600070803     c                   endif
085700070803      * data ritiro
085800070803     c     *iso          move      ormdar        dataiso
085900070803     c                   move      dataiso       dataeur
086000070803     c                   move      dataeur       v2cdar
086100070803      * ora
086200070803     c                   eval      v2corr = ormorr
086300070803      * priorità
086400070803     c                   z-add     ormsto        v2csto
086500070803      * natura merce
086600070803     c                   eval      v2cnam = ormnam
086700070803      * referente
086800070803     c                   eval      v2crer = ormrer
086900070803      * telefono
087000070803     c                   eval      v2cter = ormter
087100070803      * riferimento
087200070803     c                   eval      v2crfa = ormrfa
087300070803      * giro
087400070803     c                   eval      v2ccgi = orgcgi
087500071019     c                   if        v2ccgi <> *blanks
087600071019     c                   eval      wcgi = v2ccgi
087700071019     c                   exsr      ctrgiro
087800071019     c                   eval      v2dcgi = d09odes
087900071019     c                   endif
088000070803      * pulisco i dati di i/o
088100070803     c                   clear                   v2ccgin
088200070803     c                   clear                   v2dcgin
088300070803     c                   clear                   v2cposin
088400070803
088500070803     c                   endsr
088600070803
088700070803      *------------------------------------------------------------------------*
088800070803      *   Visualizzo la riga
088900070803      *------------------------------------------------------------------------*
089000070803     c     sr_visriga    begsr
089100070803
089200070803     c                   do        *hival
089300070803      *                  __________________
089400070803     c                   exfmt     or73d02
089500070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
089600070803     c                   eval      *in28 = *off
089700070806     c                   clear                   v2cmsg
089800070803      * f3=fine
089900070803     c                   if        *inkc
090000070803     c                   goto      fine
090100070803     c                   endif
090200070803      * f12=ritorno
090300070803     c                   if        *inkl
090400070803     c                   leave
090500070803     c                   endif
090600070803      * f7=interrogazione giri
090700070803     c                   if        *inkg
090800070803     c                   exsr      sr_f07
090900070803     c                   eval      v2ccgin = d09ocgi
091000070803     c                   iter
091100070803     c                   endif
091200070803      * f8=interrogazione autisti
091300070803     c                   if        *inkh
091400070803     c                   exsr      sr_f08
091500070803     c                   eval      v2ccgin = d08cgio
091600070803     c                   iter
091700070803     c                   endif
091800070803      * controllo
091900070803     c                   exsr      sr_ctrd02
092000070803     c                   if        *in28
092100070803     c                   iter
092200070803     c                   endif
092300070806      * f6=aggiorna
092400070803     c                   if        *inkf
092500070803     c                   exsr      sr_aggiorna
092600111111     c                   IF        *in28 and not *in10
092700111111     c                   iter
092800111109     c                   ENDIF
092900070803     c                   if        $fatto = *on
093000070803     c                   leave
093100070803     c                   endif
093200070803     c                   endif
093300070803
093400070803     c                   enddo
093500070803
093600070803     c                   endsr
093700070803
093800070803      *------------------------------------------------------------------------*
093900070803      *   Controlli video --> dettaglio ORM
094000070803      *------------------------------------------------------------------------*
094100070803     c     sr_ctrd02     begsr
094200070803
094300070803     c                   eval      *in28 = *off
094400070803      * controllo giro
094500070803     c                   if        v2ccgin <> *blanks
094600071019     c                   eval      wcgi = v2ccgin
094700070803     c                   exsr      ctrgiro
094800070803      * giro errato
094900070903     c                   if        d09oerr = '1' or d09oatb <> *blanks
095000070803     c                   eval      *in28 = *on
095100070806     c                   eval      v2cmsg = msg(04)
095200070803     c                   leavesr
095300070803     c                   endif
095400070803      * controllo filiale giro con filiale ritiro
095500070806     c                   if        d09ofgs <> v1cfgs
095600070806     c                   eval      *in28 = *on
095700070806     c                   eval      v2cmsg = msg(06)
095800070806     c                   leavesr
095900070806     c                   endif
096000071019     c                   eval      v2dcgin = d09odes
096100070806
096200070803     c                   endif
096300111111
096400111111      * l'orm, mentre sto mettendo il giro, può essere dirottato....in questo caso
096500111111      * la filiale ritiro non è più = alla filiale gestione quindi devo dare errore
096600111111      * lo faccio però solo se sono in selezione singola, se sono in selezione multipla
096700111111      * nella routine sr_aggiorna salto il rcd da aggiornare
096800111111      * chain di nuovo su FNORM perchè magari i dati, dal momento della selezione del rcd
096900111111      * al momento del F6 o del controllo su quanto immesso a video, possono essere cambiati
097000111111     c     korm          chain     fnorm01l
097100111111     c                   IF        ORMpor <> V1Cfgs
097200111111     c                   eval      *in28 = *on
097300111111     c                   eval      V2Cmsg = msg(09)
097400111111     c                   eval      %subst(V2Cmsg:42:3) = %editc(V1Cfgs:'X')
097500111111     c                   eval      $aggrcd = *on
097600111111     c                   leavesr
097700111111     c                   ENDIF
097800070803
097900070803     c                   endsr
098000070803
098100070803      *------------------------------------------------------------------------*
098200070803      *   Aggiorno i dati su FNORG x il giro
098300070803      *------------------------------------------------------------------------*
098400070803     c     sr_aggiorna   begsr
098500070803
098600070925     c                   if        v2ccgin <> *blanks
098700070803     c     korm          chain     fnorg01l
098800070803     c                   if        %found(fnorg01l)
098900111111     c                   IF        ORGpor <> V1Cfgs
099000111111      * non emetto il msg se selezione multipla
099100111111     c  n10              eval      *in28 = *on
099200111111     c  n10              eval      V1Cmsg = msg(09)
099300111111     c  n10              eval      %subst(V1Cmsg:42:3) = %editc(V1Cfgs:'X')
099400111111      * imposto come se avessi aggiornato se selezione multipla
099500111111     c   10              eval      $fatto = *on
099600111111     c   10              eval      $aggrcd = *on
099700111109     c                   unlock    fnorg01l
099800111109     c                   leavesr
099900111109     c                   ENDIF
100000070803     c                   eval      orgpocgi = v1cfgs
100100070803     c                   eval      orgcgi = v2ccgin
100200070803     c                   eval      orgtgi = 'M'
100300070806     c                   exsr      sr_dataora
100400070903     c                   eval      orgdtvtel = wdata
100500070903     c                   eval      orghvtel = wora
100600070803     c                   update    fnorg000
100700070803     c                   endif
100800070925     c                   endif
100900070803
101000070803     c                   eval      $fatto = *on
101100070806     c                   eval      $aggrcd = *on
101200070803
101300070803     c                   endsr
101400070904
101500070904      *------------------------------------------------------------------------*
101600070904      *  Controlla validità dell'ORM
101700070904      *------------------------------------------------------------------------*
101800070904     c     check_orm     begsr
101900070904
102000070904      * per controllare se ORM assegnabile richiamo pgm esterno
102100070904     c                   clear                   fior64ds
102200070904     c                   eval      or64poei = ormpoe
102300070904     c                   eval      or64nsri = ormnsr
102400070904     c                   eval      or64nori = ormnor
102500070904     c                   eval      or64nrvi = ormnrv
102600070904      * imposto la data
102700080208     c                   eval      or64dfvi = wdar
102800070904      * se sono in simulazione controllo la distinta su ORG
102900070904      * e imposto il flag di controllo ORMETI in simulazione
103000070904     c                   if        test_simula = 'S'
103100070904     c                   eval      or64ndci = 'G'
103200070904     c                   eval      or64etii = 'S'
103300070904     c                   else
103400070904      * se già attivo controllo la distinta su entrambi
103500070904      * e imposto il flag di controllo ORMETI solo per filiale attiva
103600070904     c                   eval      or64ndci = 'E'
103700070904     c                   clear                   or64etii
103800070904     c                   endif
103900070904      * controlla la data di ritiro
104000070904     c                   eval      or64dari = 'S'
104100070904     c                   call      'FIOR64R'
104200070904     c                   parm                    kpjba
104300070904     c                   parm                    fior64ds
104400070904
104500070904     c                   endsr
104600070802
104700070802      *------------------------------------------------------------------------*
104800070802      *   Controllo e decodifica giro
104900070802      *------------------------------------------------------------------------*
105000070802     c     ctrgiro       begsr
105100070802
105200070802     c                   clear                   fidg09ds
105300070802     c                   eval      d09iop0 = '001'
105400070803     c                   eval      d09ifgs = v1cfgs
105500070806     c                   exsr      sr_dataora
105600070806     c                   eval      d09idat = wdata
105700071019     c                   eval      d09icgi = wcgi
105800070802     c                   eval      d09itug = 'R'
105900070802     c                   eval      kpjbu = fidg09ds
106000070802     c                   call      'FIDG09R'
106100070802     c                   parm                    kpjba
106200070802     c                   eval      fidg09ds = kpjbu
106300070802
106400070802     c                   endsr
106500070803
106600070802      *------------------------------------------------------------------------*
106700070802      *   Seleziona tutti i record del sfl                         ?
106800070802      *------------------------------------------------------------------------*
106900070803     c     sr_f01        begsr
107000070802
107100070803     c                   if        wnrr > *zeros
107200070803     c                   clear                   nrr
107300070803     c                   do        *hival
107400070803     c                   add       1             nrr
107500070803     c     nrr           chain     or73s01                            30
107600070803      * esco se fine sfl
107700070803     c                   if        *in30
107800070803     c                   leave
107900070803     c                   endif
108000070803     c                   eval      vs1sce = '1'
108100070803     c                   update    or73s01
108200070803     c                   enddo
108300070802     c                   endif
108400070802
108500070802     c                   endsr
108600070802
108700070802      *------------------------------------------------------------------------*
108800070802      *   Gestione F07                                                ?
108900070802      *------------------------------------------------------------------------*
109000070803     c     sr_f07        begsr
109100070802
109200070802     c                   clear                   fidg09ds
109300070802     c                   eval      d09iop0 = 'P01'
109400070802     c                   eval      d09ifgs = v1cfgs
109500070806     c                   exsr      sr_dataora
109600070806     c                   eval      d09idat = wdata
109700070803     c                   if        v2cposin <> *blanks
109800070803     c                   eval      d09ides = v2cposin
109900070803     c                   endif
110000070802     c                   eval      d09itug = 'R'
110100070802     c                   eval      kpjbu = fidg09ds
110200070802     c                   call      'FIDG09R'
110300070802     c                   parm                    kpjba
110400070802     c                   eval      fidg09ds = kpjbu
110500070802
110600070802     c                   endsr
110700070803
110800070803      *------------------------------------------------------------------------*
110900070803      *   Gestione F08                                                ?
111000070803      *------------------------------------------------------------------------*
111100070803     c     sr_f08        begsr
111200070803
111300070803     c                   clear                   fidg08ds
111400070803     c                   eval      d08fgs = v1cfgs
111500070803     c                   eval      d08fgsgo = v1cfgs
111600070803     c                   eval      d08rsc = v2cposin
111700070903     c                   eval      d08tp = 'R'
111800070803     c                   eval      kpjbu = fidg08ds
111900070803     c                   call      'FIDG08R'
112000070803     c                   parm                    kpjba
112100070803     c                   eval      fidg08ds = kpjbu
112200070803
112300070803     c                   endsr
112400070802
112500070802      *------------------------------------------------------------------------*
112600070802      *   Riordina il sfl per località                                 ?
112700070802      *------------------------------------------------------------------------*
112800070803     c     sr_ordsfl     begsr
112900070802
113000070802      * totale records caricati nel SFL
113100070803     c                   eval      nrr = 0
113200070802
113300070802      * inizializza i campi chiave x l'ordinamento. C'è un campo in più non
113400070802      * presente nel subfile -- "Selected"?-- questo è aggiunto al record.
113500070802      * il campo è usato per selezionare i records dando un ordine a quelli
113600070802      * selezionati davanti ai non selezionati.
113700070802     c                   clear                   QLGSCB
113800070802     c                   clear                   QLGSCB00
113900070802
114000070803      * 1 campo chiave.   ordino x località
114100070803     c                   eval      QLGNBRK    = 1
114200070803      * localita a posizione 1
114300070802     c                   eval      QLGSP      = 1
114400070803     c                   eval      QLGSS      = %SIZE(vs1lor)
114500070802     c                   eval      QLGDT      = Carattere
114600070802     c                   eval      QLGSO      = Ascendente
114700070802     c                   eval      QLGKL(1)   = QLGSKL
114800070802
114900070802      * Load other sort parameters.
115000070802     c                   eval      QLGLB     = 80 + 16 * MaxKey
115100070802     c                   eval      QLGRL     = %SIZE(sflrcd) - 1
115200070802     c                   eval      QLGRT     = 8
115300070802     c                   eval      QLGOKL    = 80
115400070802     c                   eval      QLGLKE    = 16
115500070802     c                   eval      QLGLSS    = 290
115600070802
115700070802      * Initialize Sort I/O API fields.
115800070802     c                   eval      QLGRL00  = QLGRL
115900070802     c                   eval      QLGRC00  = 1
116000070802     c                   clear                   QUSEI
116100070802     c                   eval      QUSBPRV = %SIZE(QUSEC)
116200070802
116300070802      * First step - Initialize the sort routine.
116400070802     c                   call      'QLGSORT'
116500070802     c                   parm                    QLGSCB
116600070802     c                   parm                    NotUsed
116700070802     c                   parm                    NotUsed
116800070802     c                   parm                    SizeList
116900070802     c                   parm                    ReturnSize
117000070802     c                   parm                    QUSEC
117100070802
117200070802      * Next step - Write records to I/O routine.
117300070802     c                   eval      QLGRT00 = Put
117400070802
117500070803     c                   do        rrnlast       nrr_x
117600070803     c     nrr_x         chain     or73s01
117700070802
117800070802      * solo le righe con Selected = 'Y' sono riordinate,
117900070802      * quindi per fare un ordinamento di tutte le righe
118000070802      * metto 'Y' sempre.
118100070802     c                   eval      selected  = 'Y'
118200070802
118300070802     c                   clear                   QUSEI
118400070802     c                   eval      QUSBPRV = %SIZE(QUSEC)
118500070802
118600070802     c                   call      'QLGSRTIO'
118700070802     c                   parm                    QLGSCB00
118800070802     c                   parm                    SflRcd
118900070802     c                   parm                    NotUsed
119000070802     c                   parm                    SizeList
119100070802     c                   parm                    NotUsed
119200070802     c                   parm                    QUSEC
119300070802
119400070802     c                   enddo
119500070802
119600070802      * Next step - Signal end of input, clear subfile for reload.
119700070802     c                   eval      QLGRT00 = EndPut
119800070802     c                   clear                   QUSEI
119900070802     c                   eval      QUSBPRV = %SIZE(QUSEC)
120000070802
120100070802     c                   call      'QLGSRTIO'
120200070802     c                   parm                    QLGSCB00
120300070802     c                   parm                    SflRcd
120400070802     c                   parm                    NotUsed
120500070802     c                   parm                    SizeList
120600070802     c                   parm                    NotUsed
120700070802     c                   parm                    QUSEC
120800070803      * pulizia SFL    ?
120900070802     c                   eval      *in20 = *off
121000070802     c                   eval      *in21 = *off
121100070802      *                  __________________
121200070803     c                   write     or73c01
121300070802      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
121400070802     c                   eval      *in21 = *on
121500070802
121600070802      * Final step - Write the records back to the subfile.
121700070802     c                   eval      QLGRT00 = Get
121800070802
121900070803     c                   do        rrnlast       nrr_x
122000070803     c                   eval      nrr = nrr_x
122100070802     c                   clear                   QUSEI
122200070802     c                   eval      QUSBPRV = %SIZE(QUSEC)
122300070802     c                   call      'QLGSRTIO'
122400070802     c                   parm                    QLGSCB00
122500070802     c                   parm                    NotUsed
122600070802     c                   parm                    SflRcd
122700070802     c                   parm                    QLGRL00
122800070802     c                   parm                    NotUsed
122900070802     c                   parm                    QUSEC
123000070803
123100070803     c                   eval      nrr = nrr_x
123200070802      *                  __________________
123300070803     c                   write     or73s01
123400070802      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
123500070802     c                   enddo
123600070802
123700070802      *  All'uscita di queste chiamate a routine di sistema il SFL record
123800070802      *   si incrementa stranamente quindi lo reimposto correttamente
123900070802      *    prima di lasciare la routine.
124000070803     c                   eval      nrr = rrnlast
124100070802
124200070802     c                   endsr
124300070806
124400070806      *------------------------------------------------------------------------*
124500070806      *   Imposto data e ora del momento
124600070806      *------------------------------------------------------------------------*
124700070806     c     sr_dataora    begsr
124800070806
124900070806     c                   time                    w0140
125000070806      * ora
125100070806     c                   movel     w0140         wora
125200070806      * data
125300070806     c                   move      w0140         wdata
125400070806     c     *eur          move      wdata         dataeur
125500070806     c                   move      dataeur       dataiso
125600070806     c                   move      dataiso       wdata
125700070806
125800070806     c                   endsr
125900131119      /free
126000131119       //--------------------------------------------------------------
126100131119       //?Gestione assegnazione giro da assegnazione manuale
126200131119       //--------------------------------------------------------------
126300131119       BEGSR DaAssMan;
126400131119
126500131119       //?Aggancio l'ORM
126600131119         chain (IOR73poe:IOR73nsr:IOR73nor:IOR73nrv) FNORM01L;
126700131119         IF  not %found(FNORM01L);
126800131119           leavesr;
126900131119         ENDIF;
127000131119         chain(n) (IOR73poe:IOR73nsr:IOR73nor:IOR73nrv) FNORG01L;
127100131119         IF  not %found(FNORG01L);
127200131119           leavesr;
127300131119         ENDIF;
127400131119
127500131119         vs1poe = IOR73poe;
127600131119         vs1nsr = IOR73nsr;
127700131119         vs1nor = IOR73nor;
127800131119         vs1nrv = IOR73nrv;
127900131119
128000131119         exsr sr_datiorm;
128100131119         exsr sr_visriga;
128200131119
128300131119         IF  $aggrcd = *on;
128400131119           OOR73agg = *on;
128500131119         ELSE;
128600131119           OOR73agg = *off;
128700131119         ENDIF;
128800131119
128900131119       ENDSR;
129000131119      /end-free
129100070802
129200070802      *------------------------------------------------------------------------*
129300070802      *   Operazioni iniziali      ?
129400070802      *------------------------------------------------------------------------*
129500070802     c     *inzsr        begsr
129600070802
129700070802     c     *entry        plist
129800070802     c                   parm                    kpjba
129900131119     c                   parm                    fior73ds
130000070802
130100070802     c     *dtaara       define    §azute        azuteds
130200070802     c     *dtaara       define    §datiute      ddatiute
130300070802     c                   in(e)     *dtaara
130400070802     c                   if        %error or rsut = *blanks
130500070802     c                   clear                   tibs34ds
130600070802     c                   call      'TIBS34R'
130700070802     c                   parm                    tibs34ds
130800070802     c                   in        *dtaara
130900070802     c                   endif
131000070803
131100070803      * filiale gestione da filiale utente
131200070803     c                   eval      v1cfgs = dutpou
131300070803      * abilito f18
131400070803     c                   eval      *in06 = (dutlpo = '2' or dutlpo = *blanks)
131500080208
131600080208      * recupero i dati di default prima con la mia filiale
131700080208     c                   clear                   tibs02ds
131800080208     c                   clear                   ddft
131900080208     c                   movel     'C'           t02mod
132000080208     c                   movel     knsif         t02sif
132100080208     c                   movel     'DFT'         t02cod
132200080208     c                   movel(p)  v1cfgs        t02ke1
132300080208     c                   call      'TIBS02R'
132400080208     c                   parm                    kpjba
132500080208     c                   parm                    tibs02ds
132600080208     c                   if        t02err = *blanks
132700080208     c                   movel     t02uni        ddft
132800080208     c                   else
132900080208      * poi con 999 generico se non trovo con la mia filiale
133000080208     c                   clear                   tibs02ds
133100080208     c                   movel     'C'           t02mod
133200080208     c                   movel     knsif         t02sif
133300080208     c                   movel     'DFT'         t02cod
133400080208     c                   movel(p)  '999'         t02ke1
133500080208     c                   call      'TIBS02R'
133600080208     c                   parm                    kpjba
133700080208     c                   parm                    tibs02ds
133800080208     c                   if        t02err =  *blanks
133900080208     c                   movel     t02uni        ddft
134000080208     c                   endif
134100080208     c                   endif
134200080208
134300080208      * imposto la data ritiro a oggi
134400080208     c     *iso          move      *date         dataiso
134500080208     c                   move      dataiso       dataeur
134600080208     c                   move      dataeur       v1cdar
134700080208
134800080208      * se è già pomeriggio imposto la data di oggi + 1
134900080208     c                   exsr      sr_dataora
135000080208     c                   if        wora > d§dftora
135100080208     c                   do        *hival
135200080208     c                   adddur    1:*d          dataeur
135300080208     c                   move      dataeur       ds_data
135400080208     c                   clear                   ktfp
135500080208     c                   eval      ktfa = v1cfgs
135600080208     c                   eval      kann = ds_anno
135700080208     c                   eval      kmes = ds_mese
135800080208     c     kazcln        chain     azcln01l
135900080208     c                   if        not %found(azcln01l)
136000080208     c                   leave
136100080208     c                   endif
136200080208     c                   if        mat(ds_giorno) =  'F'
136300080208     c                             or pom(ds_giorno) =  'F'
136400080208     c                   iter
136500080208     c                   endif
136600080208     c                   leave
136700080208     c                   enddo
136800080208     c                   movel     ds_data       v1cdar
136900080208     c                   else
137000080208     c                   movel     dataeur       v1cdar
137100080208     c                   endif
137200080208
137300070803
137400070803     c     korm          klist
137500070803     c                   kfld                    vs1poe
137600070803     c                   kfld                    vs1nsr
137700070803     c                   kfld                    vs1nor
137800070803     c                   kfld                    vs1nrv
137900080208
138000080208     c     kazcln        klist
138100080208     c                   kfld                    ktfp
138200080208     c                   kfld                    ktfa
138300080208     c                   kfld                    kann
138400080208     c                   kfld                    kmes
138500070802
138600070802     c                   endsr
138700070802
138800070802      *------------------------------------------------------------------------*
138900070802** msg
139000070802Filiale errata                                                                 01
139100070802La Filiale non è in gestione all'utente                                        02
139200070803La Filiale non ha la procedura GIRI ORM attiva                                 03
139300070803Codice Giro non corretto                                                       04
139400070803Parzializzazione località errata inserire correttamente                        05
139500070806Filiale giro incongruente con filiale ritiro                                   06
139600080208Data ritiro errata                                                             07
139700080208Immettere la data ritiro                                                       08
139800111109L'ORM non è più in gestione alla filiale xxx                                   09
