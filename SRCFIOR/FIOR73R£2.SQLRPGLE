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
011900070802
012000070802     d                sds
012100070803     d  vtcpgm                 1     10
012200070802
012300070802      *------------------------------------------------------------------------*
012400070802      *   C O S T A N T I
012500070802      *------------------------------------------------------------------------*
012600070802
012700070802      **********************************************************************
012800070802      * Constants
012900070802      *     MaxKey - è il max. num. di campi chiave permesso in questo pgm
013000070802      **********************************************************************
013100070802      * Indice di schiera dei campi chiave di ordinamento del SFL  (MAXkey)
013200070802     d MaxKey          c                   9
013300070802     d Ascendente      c                   1
013400070802     d Discendente     c                   2
013500070802     d Carattere       c                   6
013600070802     d Put             c                   1
013700070802     d EndPut          c                   2
013800070802     d Get             c                   3
013900070802     d Numerico        c                   8
014000070802      **********************************************************************
014100070802      * Campi utili:
014200070802      *     RRN1       - Numero relativo di record del Subfile
014300070802      *     SizeList   - Dimensione della lista
014400070802      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamento
014500070802      **********************************************************************
014600070802     d Rrn1            s              5I 0
014700070802     d NotUsed         s             16A
014800070802     d ReturnSize      s              9B 0
014900070802     d SizeList        s              9B 0
015000070802     d RrnLast         s              5I 0
015100070802     d WrkSort         s              1  0 inz(0)
015200070802     d WindSort        s              3I 0 inz(0)
015300070802      **********************************************************************
015400070802      * Data Structures
015500070802      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
015600070802      *     QLGSCB     - The sort request block for the QLGSORT API
015700070802      *     QLGSCB00   - The sort request block for the QLGSRTIO API
015800070802      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
015900070802      *     QUSEC      - Error structure for the QLGSORT API
016000070802      **********************************************************************
016100070802     d sflrcd          ds
016200070803     d  vs1lor
016300070803     d  vs1prr
016400070803     d  vs1inr
016500070803     d  vs1rsr
016600070803     d  vs1pkg
016700070803     d  vs1vlm
016800070803     d  vs1ncl
016900071019     d  vs1bnc
017000070803     d  vs1cgi
017100070803     d  vs1poe
017200070803     d  vs1nsr
017300070803     d  vs1nor
017400070803     d  vs1nrv
017500070803     d  vs1sce
017600070802     d  selected                      1A
017700070125
017800070125      /COPY QSYSINC/QRPGLESRC,QLGSORT
017900070125     d QLGKL                         16    DIM(MaxKey)
018000070125     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
018100070125     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
018200070125     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
018300070125     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
018400070125
018500070125      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
018600070125      /COPY QSYSINC/QRPGLESRC,QUSEC
018700070802
018800070802      *------------------------------------------------------------------------*
018900070802
019000070903      * pulisco la videata
019100070903     c                   exsr      sr_puld01
019200070803      * gestione della prima videata --> parametri
019300070803     c     emid01        tag
019400070803      * emetto la videata
019500070803      *                  __________________
019600070803     c                   exfmt     or73d01
019700070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
019800070803      * spengo indicatori di comodo
019900070803     c                   eval      *in28 = *off
020000070803     c                   eval      *in42 = *off
020100070803     c                   eval      *in90 = *off
020200070803      * pulisco campo messaggi
020300070803     c                   clear                   v1cmsg
020400070803      * f3=fine
020500070803     c                   if        *inkc
020600070803     c                   goto      fine
020700070803     c                   endif
020800070803      * f7=interrogazione giri
020900070803     c                   if        *inkg
021000070803     c                   exsr      sr_f07
021100070803     c                   eval      v1ccgi = d09ocgi
021200070803     c                   eval      *in42 = *on
021300070803     c                   eval      *in90 = *on
021400070803     c                   goto      emid01
021500070803     c                   endif
021600070803      * f18=cambio filiale
021700070803     c                   if        *inks
021800070803     c                   eval      *in05 = *on
021900070803     c                   goto      emid01
022000070803     c                   endif
022100070803      * controllo dati immessi
022200070803     c                   exsr      sr_ctrd01
022300070803     c   28              goto      emid01
022400070803
022500070803     c     carica        tag
022600070803      * preparo la stringa SQL
022700070803     c                   exsr      sr_carica
022800070803      * preparo il subfile dettaglio ORM
022900070803     c                   exsr      sr_pulsfl
023000070803     c                   exsr      sr_carsfl
023100070803      * gestione del subfile
023200070803     c                   exsr      sr_ordsfl
023300070803     c                   exsr      sr_gessfl
023400071026     c                   if        $aggrcd = *on
023500070806     c                   goto      carica
023600070806     c                   endif
023700070802
023800070803     c     fine          tag
023900070802     c                   eval      *inlr = *on
024000070802
024100070803      *------------------------------------------------------------------------*
024200070803      *   Pulizia primo video --> parametri
024300070803      *------------------------------------------------------------------------*
024400070803     c     sr_puld01     begsr
024500070803
024600070803     c                   clear                   v1clord
024700070803     c                   clear                   v1clora
024800070803     c                   clear                   v1crsr
024900070803     c                   clear                   v1ccgi
025000070803      * pulisco anche il campo di posizionamento interrogazioni
025100070803      * dato che ho una routine unica per interrogare i giri
025200070803     c                   clear                   v2cposin
025300070803
025400070803     c                   endsr
025500070803
025600070802      *------------------------------------------------------------------------*
025700070802      *   Controlli primo video --> parametri
025800070802      *------------------------------------------------------------------------*
025900070803     c     sr_ctrd01     begsr
026000070802
026100070803     c                   eval      *in41 = *off
026200070803     c                   eval      *in42 = *off
026300070803     c                   eval      *in43 = *off
026400080208     c                   eval      *in44 = *off
026500070802
026600070802      * filiale gestione
026700070802     c                   clear                   og147
026800070802     c     v1cfgs        chain     azorg01l
026900070802     c                   if        not %found(azorg01l)
027000070803     c                   eval      v1cmsg = msg(01)
027100070802     c                   eval      *in28 = *on
027200070803     c                   eval      *in41 = *on
027300070802     c                   leavesr
027400070802     c                   endif
027500070803      * descrizione
027600070803     c                   eval      v1dfgs = orgdes
027700070803      * in gestione all'utente
027800070803     c                   clear                   fnlv50ds
027900070803     c                   eval      d50pru = knmus
028000070803     c                   eval      d50fgs = v1cfgs
028100070803     c                   call      'FNLV50R'
028200070803     c                   parm                    fnlv50ds
028300070803     c                   if        d50err <> *blanks
028400070803     c                   eval      v1cmsg = msg(02)
028500070803     c                   eval      *in28 = *on
028600070803     c                   eval      *in41 = *on
028700070803     c                   leavesr
028800070803     c                   endif
028900070803      * controllo se filiale attivata
029000070802     c                   eval      og147 = orgde7
029100070803     c                   if        §ogcgio = *blank
029200070803     c                   eval      v1cmsg = msg(03)
029300070803     c                   eval      *in28 = *on
029400070803     c                   eval      *in41 = *on
029500070802     c                   leavesr
029600070802     c                   endif
029700070926     c                   if        §ogddao = *blank or §ogddao = *all'0'
029800070926     c                   goto      fine
029900070926     c                   endif
030000070904      * data partenza procedura in organigramma
030100070904     c                   move      §ogddao       data_org
030200070904      * controllo se siamo in simulazione
030300070904     c                   if        *date < data_org
030400070904     c                   eval      test_simula = 'S'
030500070904     c                   else
030600070904     c                   clear                   test_simula
030700070904     c                   endif
030800070803
030900070802     c                   eval      *in05 = *off
031000070802
031100070802      * se imposto località 'DA' e località 'A' a blank la inserisco uguale
031200070802     c                   if        v1clord <> *blank and v1clora = *blank
031300070903     c                   eval      v1clora = %trimr(v1clord) + wzeta
031400070802     c                   endif
031500070802      * se imposto località 'DA' e località 'A' con solo iniziale metto nel
031600070802      * secondo byte la 'Z' x la selezione
031700070802     c                   if        v1clord <> *blank and v1clora <> *blank and
031800070802     c                             %subst(v1clora:2:1) = ' '
031900070903     c                   eval      v1clora = %trimr(v1clora) + wzeta
032000070802     c                   endif
032100070802      * vice versa località 'A' e località 'DA' a blank la inserisco uguale
032200070802     c                   if        v1clora <> *blank and v1clord = *blank
032300070802     c                   eval      v1clord = v1clora
032400070802     c                   endif
032500070802      * verifico che località 'DA' sia congruente con località 'A'
032600070802     c                   if        v1clord > v1clora
032700070803     c                   eval      v1cmsg = msg(05)
032800070803     c                   eval      *in28 = *on
032900070803     c                   eval      *in43 = *on
033000070802     c                   leavesr
033100070802     c                   endif
033200070802
033300070802      * giro
033400070802     c                   if        v1ccgi <> *blank and v1ccgi <> '9999999999'
033500070802     c                   eval      con_giro = 'S'
033600070802      * no giro
033700070802     c                   endif
033800070802      * senza giro
033900070802     c                   if        v1ccgi = *blanks
034000070802     c                   eval      con_giro = 'N'
034100070802     c                   endif
034200070802      * tutti
034300070802     c                   if        v1ccgi = *all'9'
034400070802     c                   clear                   con_giro
034500070802     c                   endif
034600080208
034700080208      * data ritiro obbligatoria
034800080208     c                   if        v1cdar = *zeros
034900080208     c                   eval      *in28 = *on
035000080208     c                   eval      *in44 = *on
035100080208     c                   eval      v1cmsg = msg(08)
035200080208     c                   leavesr
035300080208     c                   endif
035400080208
035500080208      * data ritiro valida
035600080208     c                   if        v1cdar > *zeros
035700080208     c                   clear                   wlbdat
035800080208     c                   z-add     v1cdar        g08dat
035900080208     c                   call      'XSRDA8'
036000080208     c                   parm                    wlbdat
036100080208     c                   if        g08err = '1'
036200080208     c                   eval      *in28 = *on
036300080208     c                   eval      *in44 = *on
036400080208     c                   eval      v1cmsg = msg(07)
036500080208     c                   leavesr
036600080208     c                   endif
036700080208     c                   z-add     g08dat        v1cdar
036800080208     c                   z-add     g08inv        wdar
036900080208     c                   endif
037000070802
037100070802     c                   endsr
037200070803
037300070803      *------------------------------------------------------------------------*
037400070803      *   Preparo la stringa SQL
037500070803      *------------------------------------------------------------------------*
037600070803     c     sr_carica     begsr
037700070803
037800070803     c                   clear                   stringasql
037900070803
038000070803      * preparo la stringa sql
038100070803     c                   eval      stringasql =
038200071029     c                             'select fnorg52j.* from '                  +
038300071029     c                             'fnorg52j '                                +
038400071029     c                             'where orgpor =  '                         +
038500071029     c                             %editc(v1cfgs:'X')                         +
038600071029     c                             ' and orgnsr = 0'
038700071029      * se simulazione solo orgndc = 0
038800071029     c                   if        test_simula = 'S'
038900071029     c                   eval      stringasql =
039000071029     c                             %trim(stringasql) + ' '                     +
039100071029     c                             'and orgndc = 0'
039200071029     c                   endif
039300071029      * se filiale già attiva solo ormndc = 0
039400071029     c                   if        test_simula <> 'S'
039500071029     c                   eval      stringasql =
039600071029     c                             %trim(stringasql) + ' '                     +
039700071029     c                             'and ormndc = 0'
039800071029     c                   endif
039900071029
040000070910      * parzializzazione per localita richiesto range a video
040100070910     c                   if        v1clord <> *blanks and v1clora <> *blanks
040200070910     c                   eval      stringasql =
040300070910     c                             %trim(stringasql) + ' '                     +
040400070910     c                             'and ormlor between '                       +
040500070910     c                             '''' + v1clord + ''''                       +
040600070910     c                             'and '                                      +
040700070910     c                             '''' + v1clora + ''''
040800070910     c                   endif
040900070910      * per nome mittente
041000070910     c                   if        v1crsr <> *blanks
041100070910     c                   eval      stringasql =
041200070910     c                             %trim(stringasql) + ' '                     +
041300070910     c                             'and ormrsr like ' + '''%'                  +
041400070910     c                             v1crsr
041500070910     c                   eval      stringasql =
041600070910     c                             %trim(stringasql) + '%'''
041700070910     c                   endif
041800070803      * se presente il giro seleziono solo quel giro
041900070803     c                   if        con_giro = 'S'
042000070803     c                   eval      stringasql =
042100070803     c                             %trim(stringasql) + ' '                     +
042200070803     c                             'and orgpocgi = '                           +
042300070803     c                              %editc(v1cfgs:'X') + ' '                   +
042400070803     c                             'and orgcgi = ' + '''' + v1ccgi + ''''
042500070803     c                   endif
042600070803      * solo gli orm senza giro
042700070803     c                   if        con_giro = 'N'
042800070803     c                   eval      stringasql =
042900070803     c                             %trim(stringasql) + ' '                     +
043000070803     c                             'and orgpocgi = 0 '                         +
043100070803     c                             'and orgcgi = ' + '''' + v1ccgi  + ''''
043200070803     c                   endif
043300070803
043400070803     C/EXEC SQL
043500070803     C+ PREPARE S1 FROM :stringasql
043600070803     C/END-EXEC
043700070803
043800070803     C/EXEC SQL
043900070803     C+ DECLARE A1 CURSOR FOR S1
044000070803     C/END-EXEC
044100070803
044200070803     C/EXEC SQL
044300070803     C+ OPEN A1
044400070803     C/END-EXEC
044500070803
044600070803     c                   endsr
044700070803
044800070803      *------------------------------------------------------------------------*
044900070803      *   Pulisco il subfile
045000070803      *------------------------------------------------------------------------*
045100070803     c     sr_pulsfl     begsr
045200070803
045300070803     c                   clear                   nrr
045400070803
045500070803     c                   eval      *in20 = *off
045600070803     c                   eval      *in21 = *off
045700070803      *                  __________________
045800070803     c                   write     or73c01
045900070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
046000070803     c                   eval      *in21 = *on
046100070803     c                   eval      *in20 = *on
046200070803
046300070803     c                   eval      recsf = 1
046400070803     c                   eval      *in31 = *off
046500070803     c                   eval      *in40 = *off
046600070803
046700070803     c                   endsr
046800070803
046900070803      *------------------------------------------------------------------------*
047000070803      *   Carico il subfile
047100070803      *------------------------------------------------------------------------*
047200070803     c     sr_carsfl     begsr
047300070803
047400070803     c                   do        *hival
047500070803
047600070803     C/EXEC SQL
047700071029     C+ FETCH NEXT FROM A1 INTO :fnorgds, :fnormds
047800070803     C/END-EXEC
047900070803
048000070803      * errore nel sql esco dal pgm
048100070803     c                   if        sqlcod < 0
048200070803     c                   eval      *inlr = *on
048300070803     c                   return
048400070803     c                   endif
048500070803      * fine file
048600070803     c                   if        sqlcod = 100
048700070803     c                   leave
048800070803     c                   endif
048900071029
049000070904      * controllo se l'ORM è assegnabile
049100070904     c                   exsr      check_orm
049200070904     c                   if        or64asso = 'N'
049300070904     c                   iter
049400070904     c                   endif
049500070803      * se arrivo qua il rcd è ok e devo caricare i dati
049600070803     c                   exsr      sr_wrtsfl
049700070803      * esco per raggiunta massima capacità sfl
049800070803     c                   if        nrr > 9990
049900070803     c                   leave
050000070803     c                   endif
050100070803
050200070803     c                   enddo
050300070803
050400070803     C/EXEC SQL
050500070803     C+ CLOSE A1
050600070803     C/END-EXEC
050700070803
050800070803     c                   eval      wnrr = nrr
050900070803     c                   eval      rrnlast = nrr
051000070803     c                   eval      *in31 = *on
051100070803
051200070803     c                   endsr
051300070803
051400070803      *------------------------------------------------------------------------*
051500070803      *   Scrivo la riga del subfile
051600070803      *------------------------------------------------------------------------*
051700070803     c     sr_wrtsfl     begsr
051800070803
051900070803      * imposto i campi
052000070803     c                   clear                   vs1sce
052100070803      * codice giro
052200070803     c                   eval      vs1cgi = orgcgi
052300070803      * chiave ORM
052400070803     c                   eval      vs1poe = ormpoe
052500070803     c                   eval      vs1nsr = ormnsr
052600070803     c                   eval      vs1nor = ormnor
052700070803     c                   eval      vs1nrv = ormnrv
052800070803      * mittente
052900070803     c                   eval      vs1rsr = ormrsr
053000070803     c                   eval      vs1inr = orminr
053100070803     c                   eval      vs1lor = ormlor
053200070803     c                   eval      vs1prr = ormprr
053300070803      * peso e volume da org
053400070803     c                   eval      vs1pkg = orgpkg
053500070803     c                   eval      vs1vlm = orgvlm
053600070803      * colli
053700071019     c                   z-add     ormncl        vs1ncl
053800071019      * bancali
053900071019     c                   z-add     ormbnc        vs1bnc
054000070803
054100070803      * scrivo il subfile
054200070803     c                   add       1             nrr
054300070803      * se primo rcd che scrivo accendo indicatore di PC
054400070803     c                   if        nrr = 1
054500070803     c                   eval      *in40 = *on
054600070803     c                   else
054700070803     c                   eval      *in40 = *off
054800070803     c                   endif
054900070803      *                  __________________
055000070803     c                   write     or73s01
055100070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
055200070803
055300070803     c                   endsr
055400070803
055500070803      *------------------------------------------------------------------------*
055600070803      *   Gestione del subfile
055700070803      *------------------------------------------------------------------------*
055800070803     c     sr_gessfl     begsr
055900070803
056000070806     c                   eval      $aggrcd = *off
056100070803     c                   eval      $fatto = *off
056200070803     c                   eval      $multi = *off
056300070803
056400070803     c                   do        *hival
056500070803      * emetto il piede del subfile
056600070803      *                  __________________
056700070803     c                   write     or73z01
056800070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
056900070803      * se non ho caricato niente emetto videata vuota
057000070803     c                   if        wnrr = *zeros
057100070803      *                  __________________
057200070803     c                   write     or73v01
057300070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
057400070803     c                   eval      *in20 = *off
057500070803     c                   else
057600070803     c                   eval      *in20 = *on
057700070803     c                   if        recsf > wnrr
057800070803     c                   eval      recsf = 1
057900070803     c                   endif
058000070803     c                   endif
058100070803      * emissione sfl
058200070803      *                  __________________
058300070803     c                   exfmt     or73c01
058400070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
058500070803     c                   eval      *in28 = *off
058600070803     c                   clear                   v1cmsg
058700070803
058800070803      * f3=fine
058900070803     c                   if        *inkc
059000070803     c                   goto      fine
059100070803     c                   endif
059200070803      * f12=ritorno
059300070803     c                   if        *inkl
059400070803     c                   goto      emid01
059500070803     c                   endif
059600070803      * f1=seleziona tutti
059700070803     c                   if        *inka
059800070803     c                   exsr      sr_f01
059900070803     c                   iter
060000070803     c                   endif
060100070803
060200070803      * controllo le selezioni fatte
060300070803     c                   if        wnrr > *zeros
060400070803     c                   exsr      sr_contrsfl
060500070803     c                   if        *in28
060600070803     c                   iter
060700070803     c                   endif
060800070806     c                   if        $aggrcd = *on
060900070806     c                   leave
061000070806     c                   endif
061100070803     c                   endif
061200070803
061300070803     c                   enddo
061400070803
061500070803     c                   endsr
061600070803
061700070803      *------------------------------------------------------------------------*
061800070803      *   Controllo le selezioni del subgile
061900070803      *------------------------------------------------------------------------*
062000070803     c     sr_contrsfl   begsr
062100070803
062200070803     c                   clear                   nrr
062300070803     c                   do        *hival
062400070803     c                   add       1             nrr
062500070803     c     nrr           chain     or73s01                            30
062600070803      * esco se fine sfl
062700070803     c                   if        *in30
062800070803     c                   leave
062900070803     c                   endif
063000070803
063100070803     c                   eval      *in10 = *off
063200070803
063300070803      * controllo le selezioni a video
063400070803     c                   select
063500071026
063600071026      * opzione 2 --> modifca ORM
063700071026     c                   when      vs1sce = '2'
063800071026     c                   exsr      sr_modorm
063900071026     c                   eval      recsf = nrr
064000071026     c                   clear                   vs1sce
064100071026     c                   eval      *in40 = *on
064200070803
064300070803      * opzione 5 --> visualizza ORM
064400070803     c                   when      vs1sce = '5'
064500070803     c                   exsr      sr_intorm
064600070803     c                   eval      recsf = nrr
064700070803     c                   clear                   vs1sce
064800070803     c                   eval      *in40 = *on
064900070803
065000070803      * opzione 1 --> attribuzione giro per singolo ORM
065100070803     c                   when      vs1sce = '1'
065200070803     c                   exsr      sr_singolo
065300070803     c                   eval      recsf = nrr
065400070803     c                   clear                   vs1sce
065500070803     c                   eval      *in40 = *on
065600070803
065700070803      * opzione 9 --> attribuzione giro per più ORM
065800070803     c                   when      vs1sce = '9'
065900070803     c                   eval      *in10 = *on
066000070803     c                   exsr      sr_multiplo
066100070803     c                   clear                   vs1sce
066200070803     c                   eval      *in40 = *on
066300070803
066400070803     c                   other
066500070803      * spengo l'indicatore di PC
066600070803     c                   eval      *in40 = *off
066700070803     c                   endsl
066800070803      *                  __________________
066900070803     c                   update    or73s01
067000070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
067100070803     c                   enddo
067200070803
067300070803     c                   endsr
067400071026
067500071026      *------------------------------------------------------------------------*
067600071026      *   Modifico l'ORM
067700071026      *------------------------------------------------------------------------*
067800071026     c     sr_modorm     begsr
067900071026
068000071026     c                   clear                   dsfior07
068100071026     c                   clear                   fior05ds
068200071026     c                   eval      dsopoe = vs1poe
068300071026     c                   eval      dsonor = vs1nor
068400071026     c                   eval      dsonsr = vs1nsr
068500071026     c                   eval      dsonrv = vs1nrv
068600071026     c                   eval      dsoopz = vs1sce
068700071026     c                   eval      dsofgs = v1cfgs
068800071026     c                   eval      kpjbu = dsfior07
068900071026     c                   clear                   fior05ds
069000071026     c                   call      'FIOR05R'
069100071026     c                   parm                    kpjba
069200071026     c                   parm                    fior05ds
069300071026     c                   eval      $aggrcd = *on
069400071026
069500071026     c                   endsr
069600070803
069700070803      *------------------------------------------------------------------------*
069800070803      *   Interrogo l'ORM
069900070803      *------------------------------------------------------------------------*
070000070803     c     sr_intorm     begsr
070100070803
070200070803     c                   clear                   dsfior07
070300070803     c                   eval      dsopoe = vs1poe
070400070803     c                   eval      dsonor = vs1nor
070500070803     c                   eval      dsonsr = vs1nsr
070600070803     c                   eval      dsonrv = vs1nrv
070700070803     c                   eval      dsoopz = vs1sce
070800070803     c                   eval      dsofgs = v1cfgs
070900070803     c                   eval      kpjbu = dsfior07
071000070803     c                   call      'FIOR07R'
071100070803     c                   parm                    kpjba
071200070803
071300070803     c                   endsr
071400070803
071500070803      *------------------------------------------------------------------------*
071600070803      *   Seleziona per attribuzione singolo ORM
071700070803      *------------------------------------------------------------------------*
071800070803     c     sr_singolo    begsr
071900070803
072000070803     c     korm          chain     fnorm01l
072100070803     c                   if        not %found(fnorm01l)
072200070803     c                   leavesr
072300070803     c                   endif
072400070803     c     korm          chain(n)  fnorg01l
072500070803     c                   if        not %found(fnorg01l)
072600070803     c                   leavesr
072700070803     c                   endif
072800070803      * imposto i campi per il video di dettaglio dati ORM
072900070803     c                   exsr      sr_datiorm
073000070803      * videata di dettaglio ORM
073100070803     c                   exsr      sr_visriga
073200070803
073300070803     c                   endsr
073400070803
073500070803      *------------------------------------------------------------------------*
073600070803      *   Seleziona per attribuzione più ORM
073700070803      *------------------------------------------------------------------------*
073800070803     c     sr_multiplo   begsr
073900070803
074000070803     c     korm          chain     fnorm01l
074100070803     c                   if        not %found(fnorm01l)
074200070803     c                   leavesr
074300070803     c                   endif
074400070803     c     korm          chain(n)  fnorg01l
074500070803     c                   if        not %found(fnorg01l)
074600070803     c                   leavesr
074700070803     c                   endif
074800070803      * imposto i campi per il video di dettaglio dati ORM
074900070803     c                   if        $multi = *off
075000070803     c                   exsr      sr_datiorm
075100070803      * videata di dettaglio ORM
075200070803     c                   exsr      sr_visriga
075300070803      * se "$fatto" ON ho aggiornato la prima riga multipla
075400070803      * quindi memorizzo i valori da aggiornare x eventuali righe con stessa
075500070803      * opzione e proseguo l'aggiornamento  senza più emettere il video per
075600070803      * queste ultime se le opzioni fossero mischiate le altre differenti
075700070803      * dalla multipla verranno elaborate normalmente nell'ambito dello stesso
075800070803      * ciclo di elaborazione. Le multiple verranno elaborate in maniera
075900070803      * occulta con il valore impostato inizialmente
076000070803     c                   if        $fatto = *on
076100070803     c                   eval      sav_v2ccgin = v2ccgin
076200070803     c                   eval      $multi = *on
076300070803     c                   endif
076400070803      * imposta il giro sulle ulteriori righe della scelta multipla
076500070803     c                   else
076600070803     c                   eval      v2ccgin = sav_v2ccgin
076700070803     c                   exsr      sr_aggiorna
076800070803     c                   endif
076900070803
077000070803     c                   endsr
077100070803
077200070803      *------------------------------------------------------------------------*
077300070803      *   Imposto i dati dell'ORM per la videata a dettaglio
077400070803      *------------------------------------------------------------------------*
077500070803     c     sr_datiorm    begsr
077600071019
077700071019     c                   clear                   v2dcgi
077800071019     c                   clear                   v2dcgin
077900070803
078000070803      * chiave ORM
078100070803     c                   eval      v2cpoe = ormpoe
078200070803     c                   eval      v2cnsr = ormnsr
078300070803     c                   eval      v2cnor = ormnor
078400070803     c                   eval      v2cnrv = ormnrv
078500070803      * ordinante
078600070803     c                   eval      v2ccor = ormcor
078700070803     c                   eval      v2crso = ormrso
078800070803      * tipo orm
078900070803     c                   clear                   v2dtor
079000070803     c                   clear                   tibs02ds
079100070803     c                   eval      t02mod = 'C'
079200070803     c                   eval      t02sif = knsif
079300070803     c                   eval      t02cod = 'TOR'
079400070803     c                   eval      t02ke1 = ormtor
079500070803     c                   call      'TIBS02R'
079600070803     c                   parm                    kpjba
079700070803     c                   parm                    tibs02ds
079800070803     c                   eval      v2dtor = t02uni
079900070803      * fase
080000070803     c                   eval      v2cfao =  ormfao
080100070803     c                   clear                   v2dfao
080200070803     c                   clear                   tibs02ds
080300070803     c                   eval      t02mod = 'C'
080400070803     c                   eval      t02sif = knsif
080500070806     c                   eval      t02cod = 'FAR'
080600070803     c                   movel(p)  ormfao        t02ke1
080700070803     c                   call      'TIBS02R'
080800070803     c                   parm                    kpjba
080900070803     c                   parm                    tibs02ds
081000070803     c                   eval      dfar = t02uni
081100070803     c                   eval      v2dfao = d§fardes
081200070803      * mittente
081300070803     c                   eval      v2ccra = ormcra
081400070803     c                   eval      v2crsr = ormrsr
081500070803      * indirizzo
081600070803     c                   eval      v2cinr = orminr
081700070803     c                   eval      v2clor = ormlor
081800070803     c                   eval      v2ccar = ormcar
081900070803     c                   eval      v2cprr = ormprr
082000070904     c                   eval      v2cnar = ormnar
082100070803      * note
082200070803     c                   eval      v2cno1 = ormno1
082300070803     c                   eval      v2cno2 = ormno2
082400070803      * colli/bancali
082500070803     c                   eval      v2cncl = ormncl
082600070803     c                   eval      v2cbnc = ormbnc
082700070803      * peso/volume da ORG
082800070803     c                   eval      v2cpkg = orgpkg
082900070803     c                   eval      v2cvlm = orgvlm
083000070803      * automezzo
083100070803     c                   select
083200070803     c                   when      ormblc <> *zeros
083300070803     c                   eval      v2caut = 'BIL'
083400070803     c                   when      ormatt <> *zeros
083500070803     c                   eval      v2caut = 'AUT'
083600070803     c                   when      ormmtc <> *zeros
083700070803     c                   eval      v2caut = 'MOT'
083800070803     c                   other
083900070803     c                   clear                   v2caut
084000070803     c                   endsl
084100070803      * sponda idraulica
084200070803     c                   if        ormspi = 'S'
084300070803     c                   eval      v2cspi = 'SI'
084400070803     c                   else
084500070803     c                   clear                   v2cspi
084600070803     c                   endif
084700070803      * data ritiro
084800070803     c     *iso          move      ormdar        dataiso
084900070803     c                   move      dataiso       dataeur
085000070803     c                   move      dataeur       v2cdar
085100070803      * ora
085200070803     c                   eval      v2corr = ormorr
085300070803      * priorità
085400070803     c                   z-add     ormsto        v2csto
085500070803      * natura merce
085600070803     c                   eval      v2cnam = ormnam
085700070803      * referente
085800070803     c                   eval      v2crer = ormrer
085900070803      * telefono
086000070803     c                   eval      v2cter = ormter
086100070803      * riferimento
086200070803     c                   eval      v2crfa = ormrfa
086300070803      * giro
086400070803     c                   eval      v2ccgi = orgcgi
086500071019     c                   if        v2ccgi <> *blanks
086600071019     c                   eval      wcgi = v2ccgi
086700071019     c                   exsr      ctrgiro
086800071019     c                   eval      v2dcgi = d09odes
086900071019     c                   endif
087000070803      * pulisco i dati di i/o
087100070803     c                   clear                   v2ccgin
087200070803     c                   clear                   v2dcgin
087300070803     c                   clear                   v2cposin
087400070803
087500070803     c                   endsr
087600070803
087700070803      *------------------------------------------------------------------------*
087800070803      *   Visualizzo la riga
087900070803      *------------------------------------------------------------------------*
088000070803     c     sr_visriga    begsr
088100070803
088200070803     c                   do        *hival
088300070803      *                  __________________
088400070803     c                   exfmt     or73d02
088500070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
088600070803     c                   eval      *in28 = *off
088700070806     c                   clear                   v2cmsg
088800070803      * f3=fine
088900070803     c                   if        *inkc
089000070803     c                   goto      fine
089100070803     c                   endif
089200070803      * f12=ritorno
089300070803     c                   if        *inkl
089400070803     c                   leave
089500070803     c                   endif
089600070803      * f7=interrogazione giri
089700070803     c                   if        *inkg
089800070803     c                   exsr      sr_f07
089900070803     c                   eval      v2ccgin = d09ocgi
090000070803     c                   iter
090100070803     c                   endif
090200070803      * f8=interrogazione autisti
090300070803     c                   if        *inkh
090400070803     c                   exsr      sr_f08
090500070803     c                   eval      v2ccgin = d08cgio
090600070803     c                   iter
090700070803     c                   endif
090800070803      * controllo
090900070803     c                   exsr      sr_ctrd02
091000070803     c                   if        *in28
091100070803     c                   iter
091200070803     c                   endif
091300070806      * f6=aggiorna
091400070803     c                   if        *inkf
091500070803     c                   exsr      sr_aggiorna
091600070803     c                   if        $fatto = *on
091700070803     c                   leave
091800070803     c                   endif
091900070803     c                   endif
092000070803
092100070803     c                   enddo
092200070803
092300070803     c                   endsr
092400070803
092500070803      *------------------------------------------------------------------------*
092600070803      *   Controlli video --> dettaglio ORM
092700070803      *------------------------------------------------------------------------*
092800070803     c     sr_ctrd02     begsr
092900070803
093000070803     c                   eval      *in28 = *off
093100070803      * controllo giro
093200070803     c                   if        v2ccgin <> *blanks
093300071019     c                   eval      wcgi = v2ccgin
093400070803     c                   exsr      ctrgiro
093500070803      * giro errato
093600070903     c                   if        d09oerr = '1' or d09oatb <> *blanks
093700070803     c                   eval      *in28 = *on
093800070806     c                   eval      v2cmsg = msg(04)
093900070803     c                   leavesr
094000070803     c                   endif
094100070803      * controllo filiale giro con filiale ritiro
094200070806     c                   if        d09ofgs <> v1cfgs
094300070806     c                   eval      *in28 = *on
094400070806     c                   eval      v2cmsg = msg(06)
094500070806     c                   leavesr
094600070806     c                   endif
094700071019     c                   eval      v2dcgin = d09odes
094800070806
094900070803     c                   endif
095000070803
095100070803     c                   endsr
095200070803
095300070803      *------------------------------------------------------------------------*
095400070803      *   Aggiorno i dati su FNORG x il giro
095500070803      *------------------------------------------------------------------------*
095600070803     c     sr_aggiorna   begsr
095700070803
095800070925     c                   if        v2ccgin <> *blanks
095900070803     c     korm          chain     fnorg01l
096000070803     c                   if        %found(fnorg01l)
096100070803     c                   eval      orgpocgi = v1cfgs
096200070803     c                   eval      orgcgi = v2ccgin
096300070803     c                   eval      orgtgi = 'M'
096400070806     c                   exsr      sr_dataora
096500070903     c                   eval      orgdtvtel = wdata
096600070903     c                   eval      orghvtel = wora
096700070803     c                   update    fnorg000
096800070803     c                   endif
096900070925     c                   endif
097000070803
097100070803     c                   eval      $fatto = *on
097200070806     c                   eval      $aggrcd = *on
097300070803
097400070803     c                   endsr
097500070904
097600070904      *------------------------------------------------------------------------*
097700070904      *  Controlla validità dell'ORM
097800070904      *------------------------------------------------------------------------*
097900070904     c     check_orm     begsr
098000070904
098100070904      * per controllare se ORM assegnabile richiamo pgm esterno
098200070904     c                   clear                   fior64ds
098300070904     c                   eval      or64poei = ormpoe
098400070904     c                   eval      or64nsri = ormnsr
098500070904     c                   eval      or64nori = ormnor
098600070904     c                   eval      or64nrvi = ormnrv
098700070904      * imposto la data
098800080208     c                   eval      or64dfvi = wdar
098900070904      * se sono in simulazione controllo la distinta su ORG
099000070904      * e imposto il flag di controllo ORMETI in simulazione
099100070904     c                   if        test_simula = 'S'
099200070904     c                   eval      or64ndci = 'G'
099300070904     c                   eval      or64etii = 'S'
099400070904     c                   else
099500070904      * se già attivo controllo la distinta su entrambi
099600070904      * e imposto il flag di controllo ORMETI solo per filiale attiva
099700070904     c                   eval      or64ndci = 'E'
099800070904     c                   clear                   or64etii
099900070904     c                   endif
100000070904      * controlla la data di ritiro
100100070904     c                   eval      or64dari = 'S'
100200070904     c                   call      'FIOR64R'
100300070904     c                   parm                    kpjba
100400070904     c                   parm                    fior64ds
100500070904
100600070904     c                   endsr
100700070802
100800070802      *------------------------------------------------------------------------*
100900070802      *   Controllo e decodifica giro
101000070802      *------------------------------------------------------------------------*
101100070802     c     ctrgiro       begsr
101200070802
101300070802     c                   clear                   fidg09ds
101400070802     c                   eval      d09iop0 = '001'
101500070803     c                   eval      d09ifgs = v1cfgs
101600070806     c                   exsr      sr_dataora
101700070806     c                   eval      d09idat = wdata
101800071019     c                   eval      d09icgi = wcgi
101900070802     c                   eval      d09itug = 'R'
102000070802     c                   eval      kpjbu = fidg09ds
102100070802     c                   call      'FIDG09R'
102200070802     c                   parm                    kpjba
102300070802     c                   eval      fidg09ds = kpjbu
102400070802
102500070802     c                   endsr
102600070803
102700070802      *------------------------------------------------------------------------*
102800070802      *   Seleziona tutti i record del sfl                         ?
102900070802      *------------------------------------------------------------------------*
103000070803     c     sr_f01        begsr
103100070802
103200070803     c                   if        wnrr > *zeros
103300070803     c                   clear                   nrr
103400070803     c                   do        *hival
103500070803     c                   add       1             nrr
103600070803     c     nrr           chain     or73s01                            30
103700070803      * esco se fine sfl
103800070803     c                   if        *in30
103900070803     c                   leave
104000070803     c                   endif
104100070803     c                   eval      vs1sce = '1'
104200070803     c                   update    or73s01
104300070803     c                   enddo
104400070802     c                   endif
104500070802
104600070802     c                   endsr
104700070802
104800070802      *------------------------------------------------------------------------*
104900070802      *   Gestione F07                                                ?
105000070802      *------------------------------------------------------------------------*
105100070803     c     sr_f07        begsr
105200070802
105300070802     c                   clear                   fidg09ds
105400070802     c                   eval      d09iop0 = 'P01'
105500070802     c                   eval      d09ifgs = v1cfgs
105600070806     c                   exsr      sr_dataora
105700070806     c                   eval      d09idat = wdata
105800070803     c                   if        v2cposin <> *blanks
105900070803     c                   eval      d09ides = v2cposin
106000070803     c                   endif
106100070802     c                   eval      d09itug = 'R'
106200070802     c                   eval      kpjbu = fidg09ds
106300070802     c                   call      'FIDG09R'
106400070802     c                   parm                    kpjba
106500070802     c                   eval      fidg09ds = kpjbu
106600070802
106700070802     c                   endsr
106800070803
106900070803      *------------------------------------------------------------------------*
107000070803      *   Gestione F08                                                ?
107100070803      *------------------------------------------------------------------------*
107200070803     c     sr_f08        begsr
107300070803
107400070803     c                   clear                   fidg08ds
107500070803     c                   eval      d08fgs = v1cfgs
107600070803     c                   eval      d08fgsgo = v1cfgs
107700070803     c                   eval      d08rsc = v2cposin
107800070903     c                   eval      d08tp = 'R'
107900070803     c                   eval      kpjbu = fidg08ds
108000070803     c                   call      'FIDG08R'
108100070803     c                   parm                    kpjba
108200070803     c                   eval      fidg08ds = kpjbu
108300070803
108400070803     c                   endsr
108500070802
108600070802      *------------------------------------------------------------------------*
108700070802      *   Riordina il sfl per località                                 ?
108800070802      *------------------------------------------------------------------------*
108900070803     c     sr_ordsfl     begsr
109000070802
109100070802      * totale records caricati nel SFL
109200070803     c                   eval      nrr = 0
109300070802
109400070802      * inizializza i campi chiave x l'ordinamento. C'è un campo in più non
109500070802      * presente nel subfile -- "Selected"?-- questo è aggiunto al record.
109600070802      * il campo è usato per selezionare i records dando un ordine a quelli
109700070802      * selezionati davanti ai non selezionati.
109800070802     c                   clear                   QLGSCB
109900070802     c                   clear                   QLGSCB00
110000070802
110100070803      * 1 campo chiave.   ordino x località
110200070803     c                   eval      QLGNBRK    = 1
110300070803      * localita a posizione 1
110400070802     c                   eval      QLGSP      = 1
110500070803     c                   eval      QLGSS      = %SIZE(vs1lor)
110600070802     c                   eval      QLGDT      = Carattere
110700070802     c                   eval      QLGSO      = Ascendente
110800070802     c                   eval      QLGKL(1)   = QLGSKL
110900070802
111000070802      * Load other sort parameters.
111100070802     c                   eval      QLGLB     = 80 + 16 * MaxKey
111200070802     c                   eval      QLGRL     = %SIZE(sflrcd) - 1
111300070802     c                   eval      QLGRT     = 8
111400070802     c                   eval      QLGOKL    = 80
111500070802     c                   eval      QLGLKE    = 16
111600070802     c                   eval      QLGLSS    = 290
111700070802
111800070802      * Initialize Sort I/O API fields.
111900070802     c                   eval      QLGRL00  = QLGRL
112000070802     c                   eval      QLGRC00  = 1
112100070802     c                   clear                   QUSEI
112200070802     c                   eval      QUSBPRV = %SIZE(QUSEC)
112300070802
112400070802      * First step - Initialize the sort routine.
112500070802     c                   call      'QLGSORT'
112600070802     c                   parm                    QLGSCB
112700070802     c                   parm                    NotUsed
112800070802     c                   parm                    NotUsed
112900070802     c                   parm                    SizeList
113000070802     c                   parm                    ReturnSize
113100070802     c                   parm                    QUSEC
113200070802
113300070802      * Next step - Write records to I/O routine.
113400070802     c                   eval      QLGRT00 = Put
113500070802
113600070803     c                   do        rrnlast       nrr_x
113700070803     c     nrr_x         chain     or73s01
113800070802
113900070802      * solo le righe con Selected = 'Y' sono riordinate,
114000070802      * quindi per fare un ordinamento di tutte le righe
114100070802      * metto 'Y' sempre.
114200070802     c                   eval      selected  = 'Y'
114300070802
114400070802     c                   clear                   QUSEI
114500070802     c                   eval      QUSBPRV = %SIZE(QUSEC)
114600070802
114700070802     c                   call      'QLGSRTIO'
114800070802     c                   parm                    QLGSCB00
114900070802     c                   parm                    SflRcd
115000070802     c                   parm                    NotUsed
115100070802     c                   parm                    SizeList
115200070802     c                   parm                    NotUsed
115300070802     c                   parm                    QUSEC
115400070802
115500070802     c                   enddo
115600070802
115700070802      * Next step - Signal end of input, clear subfile for reload.
115800070802     c                   eval      QLGRT00 = EndPut
115900070802     c                   clear                   QUSEI
116000070802     c                   eval      QUSBPRV = %SIZE(QUSEC)
116100070802
116200070802     c                   call      'QLGSRTIO'
116300070802     c                   parm                    QLGSCB00
116400070802     c                   parm                    SflRcd
116500070802     c                   parm                    NotUsed
116600070802     c                   parm                    SizeList
116700070802     c                   parm                    NotUsed
116800070802     c                   parm                    QUSEC
116900070803      * pulizia SFL    ?
117000070802     c                   eval      *in20 = *off
117100070802     c                   eval      *in21 = *off
117200070802      *                  __________________
117300070803     c                   write     or73c01
117400070802      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
117500070802     c                   eval      *in21 = *on
117600070802
117700070802      * Final step - Write the records back to the subfile.
117800070802     c                   eval      QLGRT00 = Get
117900070802
118000070803     c                   do        rrnlast       nrr_x
118100070803     c                   eval      nrr = nrr_x
118200070802     c                   clear                   QUSEI
118300070802     c                   eval      QUSBPRV = %SIZE(QUSEC)
118400070802     c                   call      'QLGSRTIO'
118500070802     c                   parm                    QLGSCB00
118600070802     c                   parm                    NotUsed
118700070802     c                   parm                    SflRcd
118800070802     c                   parm                    QLGRL00
118900070802     c                   parm                    NotUsed
119000070802     c                   parm                    QUSEC
119100070803
119200070803     c                   eval      nrr = nrr_x
119300070802      *                  __________________
119400070803     c                   write     or73s01
119500070802      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
119600070802     c                   enddo
119700070802
119800070802      *  All'uscita di queste chiamate a routine di sistema il SFL record
119900070802      *   si incrementa stranamente quindi lo reimposto correttamente
120000070802      *    prima di lasciare la routine.
120100070803     c                   eval      nrr = rrnlast
120200070802
120300070802     c                   endsr
120400070806
120500070806      *------------------------------------------------------------------------*
120600070806      *   Imposto data e ora del momento
120700070806      *------------------------------------------------------------------------*
120800070806     c     sr_dataora    begsr
120900070806
121000070806     c                   time                    w0140
121100070806      * ora
121200070806     c                   movel     w0140         wora
121300070806      * data
121400070806     c                   move      w0140         wdata
121500070806     c     *eur          move      wdata         dataeur
121600070806     c                   move      dataeur       dataiso
121700070806     c                   move      dataiso       wdata
121800070806
121900070806     c                   endsr
122000070802
122100070802      *------------------------------------------------------------------------*
122200070802      *   Operazioni iniziali      ?
122300070802      *------------------------------------------------------------------------*
122400070802     c     *inzsr        begsr
122500070802
122600070802     c     *entry        plist
122700070802     c                   parm                    kpjba
122800070802
122900070802     c     *dtaara       define    §azute        azuteds
123000070802     c     *dtaara       define    §datiute      ddatiute
123100070802     c                   in(e)     *dtaara
123200070802     c                   if        %error or rsut = *blanks
123300070802     c                   clear                   tibs34ds
123400070802     c                   call      'TIBS34R'
123500070802     c                   parm                    tibs34ds
123600070802     c                   in        *dtaara
123700070802     c                   endif
123800070803
123900070803      * filiale gestione da filiale utente
124000070803     c                   eval      v1cfgs = dutpou
124100070803      * abilito f18
124200070803     c                   eval      *in06 = (dutlpo = '2' or dutlpo = *blanks)
124300080208
124400080208      * recupero i dati di default prima con la mia filiale
124500080208     c                   clear                   tibs02ds
124600080208     c                   clear                   ddft
124700080208     c                   movel     'C'           t02mod
124800080208     c                   movel     knsif         t02sif
124900080208     c                   movel     'DFT'         t02cod
125000080208     c                   movel(p)  v1cfgs        t02ke1
125100080208     c                   call      'TIBS02R'
125200080208     c                   parm                    kpjba
125300080208     c                   parm                    tibs02ds
125400080208     c                   if        t02err = *blanks
125500080208     c                   movel     t02uni        ddft
125600080208     c                   else
125700080208      * poi con 999 generico se non trovo con la mia filiale
125800080208     c                   clear                   tibs02ds
125900080208     c                   movel     'C'           t02mod
126000080208     c                   movel     knsif         t02sif
126100080208     c                   movel     'DFT'         t02cod
126200080208     c                   movel(p)  '999'         t02ke1
126300080208     c                   call      'TIBS02R'
126400080208     c                   parm                    kpjba
126500080208     c                   parm                    tibs02ds
126600080208     c                   if        t02err =  *blanks
126700080208     c                   movel     t02uni        ddft
126800080208     c                   endif
126900080208     c                   endif
127000080208
127100080208      * imposto la data ritiro a oggi
127200080208     c     *iso          move      *date         dataiso
127300080208     c                   move      dataiso       dataeur
127400080208     c                   move      dataeur       v1cdar
127500080208
127600080208      * se è già pomeriggio imposto la data di oggi + 1
127700080208     c                   exsr      sr_dataora
127800080208     c                   if        wora > d§dftora
127900080208     c                   do        *hival
128000080208     c                   adddur    1:*d          dataeur
128100080208     c                   move      dataeur       ds_data
128200080208     c                   clear                   ktfp
128300080208     c                   eval      ktfa = v1cfgs
128400080208     c                   eval      kann = ds_anno
128500080208     c                   eval      kmes = ds_mese
128600080208     c     kazcln        chain     azcln01l
128700080208     c                   if        not %found(azcln01l)
128800080208     c                   leave
128900080208     c                   endif
129000080208     c                   if        mat(ds_giorno) =  'F'
129100080208     c                             or pom(ds_giorno) =  'F'
129200080208     c                   iter
129300080208     c                   endif
129400080208     c                   leave
129500080208     c                   enddo
129600080208     c                   movel     ds_data       v1cdar
129700080208     c                   else
129800080208     c                   movel     dataeur       v1cdar
129900080208     c                   endif
130000080208
130100070803
130200070803     c     korm          klist
130300070803     c                   kfld                    vs1poe
130400070803     c                   kfld                    vs1nsr
130500070803     c                   kfld                    vs1nor
130600070803     c                   kfld                    vs1nrv
130700080208
130800080208     c     kazcln        klist
130900080208     c                   kfld                    ktfp
131000080208     c                   kfld                    ktfa
131100080208     c                   kfld                    kann
131200080208     c                   kfld                    kmes
131300070802
131400070802     c                   endsr
131500070802
131600070802      *------------------------------------------------------------------------*
131700070802** msg
131800070802Filiale errata                                                                 01
131900070802La Filiale non è in gestione all'utente                                        02
132000070803La Filiale non ha la procedura GIRI ORM attiva                                 03
132100070803Codice Giro non corretto                                                       04
132200070803Parzializzazione località errata inserire correttamente                        05
132300070806Filiale giro incongruente con filiale ritiro                                   06
132400080208Data ritiro errata                                                             07
132500080208Immettere la data ritiro                                                       08
