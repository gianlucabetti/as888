000100070802     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200070802
000300070802      *------------------------------------------------------------------------*
000400070802      *                   ATTRIBUZIONE GIRO ORM        ?
000500070802      *------------------------------------------------------------------------*
000600070802
000700070803     fazorg01l  if   e           k disk
000800070803     ffnorm01l  if   e           k disk
000900070803     ffnorg01l  uf   e           k disk
001000070803     ffior73d   cf   e             workstn sfile(or73s01:nrr)
001100070802
001200070802      *------------------------------------------------------------------------*
001300070802      *  RIEPILOGO INDICATORI
001400070802      *------------------------------------------------------------------------*
001500070802      *  05 - sproteggo filiale in gestione
001600070802      *  06 - non abilito f18
001700070803      *  10 - ON  selezione Multipla
001800070803      *  10 - OFF selezione Singola
001900070803      *  20 - gestione subfile
002000070803      *  21 - gestione subfile
002100070802      *  28 - errore generico
002200070803      *  30 - comodo
002300070803      *  31 - fine file x subfile
002400071029      *  40 - PC nel subfile
002500070803      *  41 - PC filiale gestione
002600070803      *  42 - PC giro
002700070803      *  43 - PC località
002800070803      *  90 - riemetto la videata
002900070802
003000070802      *------------------------------------------------------------------------*
003100070802      *   V A R I A B I L I
003200070802      *------------------------------------------------------------------------*
003300070802     d con_giro        s              1    inz
003400070904     d data_org        s              8  0 inz
003500070803     d dataeur         s               d   datfmt(*eur)
003600070803     d dataiso         s               d   datfmt(*iso)
003700070803     d nrr             s                   like(recsf)
003800070803     d nrr_x           s                   like(recsf)
003900070803     d sav_v2ccgin     s                   like(v2ccgin)
004000070803     d stringasql      s           1100    varying
004100070904     d test_simula     s              1    inz
004200071019     d wcgi            s                   like(orgcgi) inz
004300070806     d wdata           s              8  0
004400070803     d wnrr            s                   like(recsf)
004500070806     d wora            s              6  0
004600070903     d wzeta           s             19    inz(*all'Z')
004700070806     d w0140           s             14  0
004800070806     d $aggrcd         s              1    inz(*off)
004900070803     d $fatto          s              1    inz(*off)
005000070803     d $multi          s              1    inz(*off)
005100070802
005200070802      *------------------------------------------------------------------------*
005300070802      *   S C H I E R E
005400070802      *------------------------------------------------------------------------*
005500070802     d msg             s             78    dim(30) ctdata perrcd(1)
005600070802
005700070802      *------------------------------------------------------------------------*
005800070802      *   D S   I N T E R N E / E S T E R N E
005900070802      *------------------------------------------------------------------------*
006000070803     d dsfior07        ds
006100070803     d  dsopoe                        3  0
006200070803     d  dsonor                        7  0
006300070803     d  dsonsr                        2  0
006400070803     d  dsonrv                        2  0
006500070803     d  dsoopz                        1
006600070803     d  dsofgs                        3  0
006700070806
006800070806     d                 ds
006900070806     d  v2ccor                 1     10  0
007000070906     d  v2ccor1                1      3  0
007100070906     d  v2ccor2                4      7  0
007200070806     d  v2ccor3                8     10  0
007300070806
007400070806     d                 ds
007500070806     d  v2ccra                 1     10  0
007600070906     d  v2ccra1                1      3  0
007700070906     d  v2ccra2                4      7  0
007800070806     d  v2ccra3                8     10  0
007900070806
008000070802     d azuteds       e ds                  extname(azute00f)
008100070802     d ddatiute      e ds
008200070803     d dfar          e ds
008300070803     d fidg08ds      e ds
008400070802     d fidg09ds      e ds
008500071026     d fior05ds      e ds
008600070904     d fior64ds      e ds
008700070802     d fnlv50ds      e ds
008800070802     d fnorgds       e ds                  extname(fnorg00f)
008900070802     d fnormds       e ds                  extname(fnorm00f)
009000070802     d kpjba         e ds
009100070802     d og147         e ds
009200070803     d tibs02ds      e ds
009300070802     d tibs34ds      e ds
009400070802
009500070802     d                sds
009600070803     d  vtcpgm                 1     10
009700070802
009800070802      *------------------------------------------------------------------------*
009900070802      *   C O S T A N T I
010000070802      *------------------------------------------------------------------------*
010100070802
010200070802      **********************************************************************
010300070802      * Constants
010400070802      *     MaxKey - è il max. num. di campi chiave permesso in questo pgm
010500070802      **********************************************************************
010600070802      * Indice di schiera dei campi chiave di ordinamento del SFL  (MAXkey)
010700070802     d MaxKey          c                   9
010800070802     d Ascendente      c                   1
010900070802     d Discendente     c                   2
011000070802     d Carattere       c                   6
011100070802     d Put             c                   1
011200070802     d EndPut          c                   2
011300070802     d Get             c                   3
011400070802     d Numerico        c                   8
011500070802      **********************************************************************
011600070802      * Campi utili:
011700070802      *     RRN1       - Numero relativo di record del Subfile
011800070802      *     SizeList   - Dimensione della lista
011900070802      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamento
012000070802      **********************************************************************
012100070802     d Rrn1            s              5I 0
012200070802     d NotUsed         s             16A
012300070802     d ReturnSize      s              9B 0
012400070802     d SizeList        s              9B 0
012500070802     d RrnLast         s              5I 0
012600070802     d WrkSort         s              1  0 inz(0)
012700070802     d WindSort        s              3I 0 inz(0)
012800070802      **********************************************************************
012900070802      * Data Structures
013000070802      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
013100070802      *     QLGSCB     - The sort request block for the QLGSORT API
013200070802      *     QLGSCB00   - The sort request block for the QLGSRTIO API
013300070802      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
013400070802      *     QUSEC      - Error structure for the QLGSORT API
013500070802      **********************************************************************
013600070802     d sflrcd          ds
013700070803     d  vs1lor
013800070803     d  vs1prr
013900070803     d  vs1inr
014000070803     d  vs1rsr
014100070803     d  vs1pkg
014200070803     d  vs1vlm
014300070803     d  vs1ncl
014400071019     d  vs1bnc
014500070803     d  vs1cgi
014600070803     d  vs1poe
014700070803     d  vs1nsr
014800070803     d  vs1nor
014900070803     d  vs1nrv
015000070803     d  vs1sce
015100070802     d  selected                      1A
015200070125
015300070125      /COPY QSYSINC/QRPGLESRC,QLGSORT
015400070125     d QLGKL                         16    DIM(MaxKey)
015500070125     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
015600070125     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
015700070125     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
015800070125     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
015900070125
016000070125      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
016100070125      /COPY QSYSINC/QRPGLESRC,QUSEC
016200070802
016300070802      *------------------------------------------------------------------------*
016400070802
016500070903      * pulisco la videata
016600070903     c                   exsr      sr_puld01
016700070803      * gestione della prima videata --> parametri
016800070803     c     emid01        tag
016900070803      * emetto la videata
017000070803      *                  __________________
017100070803     c                   exfmt     or73d01
017200070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
017300070803      * spengo indicatori di comodo
017400070803     c                   eval      *in28 = *off
017500070803     c                   eval      *in42 = *off
017600070803     c                   eval      *in90 = *off
017700070803      * pulisco campo messaggi
017800070803     c                   clear                   v1cmsg
017900070803      * f3=fine
018000070803     c                   if        *inkc
018100070803     c                   goto      fine
018200070803     c                   endif
018300070803      * f7=interrogazione giri
018400070803     c                   if        *inkg
018500070803     c                   exsr      sr_f07
018600070803     c                   eval      v1ccgi = d09ocgi
018700070803     c                   eval      *in42 = *on
018800070803     c                   eval      *in90 = *on
018900070803     c                   goto      emid01
019000070803     c                   endif
019100070803      * f18=cambio filiale
019200070803     c                   if        *inks
019300070803     c                   eval      *in05 = *on
019400070803     c                   goto      emid01
019500070803     c                   endif
019600070803      * controllo dati immessi
019700070803     c                   exsr      sr_ctrd01
019800070803     c   28              goto      emid01
019900070803
020000070803     c     carica        tag
020100070803      * preparo la stringa SQL
020200070803     c                   exsr      sr_carica
020300070803      * preparo il subfile dettaglio ORM
020400070803     c                   exsr      sr_pulsfl
020500070803     c                   exsr      sr_carsfl
020600070803      * gestione del subfile
020700070803     c                   exsr      sr_ordsfl
020800070803     c                   exsr      sr_gessfl
020900071026     c                   if        $aggrcd = *on
021000070806     c                   goto      carica
021100070806     c                   endif
021200070802
021300070803     c     fine          tag
021400070802     c                   eval      *inlr = *on
021500070802
021600070803      *------------------------------------------------------------------------*
021700070803      *   Pulizia primo video --> parametri
021800070803      *------------------------------------------------------------------------*
021900070803     c     sr_puld01     begsr
022000070803
022100070803     c                   clear                   v1clord
022200070803     c                   clear                   v1clora
022300070803     c                   clear                   v1crsr
022400070803     c                   clear                   v1ccgi
022500070803      * pulisco anche il campo di posizionamento interrogazioni
022600070803      * dato che ho una routine unica per interrogare i giri
022700070803     c                   clear                   v2cposin
022800070803
022900070803     c                   endsr
023000070803
023100070802      *------------------------------------------------------------------------*
023200070802      *   Controlli primo video --> parametri
023300070802      *------------------------------------------------------------------------*
023400070803     c     sr_ctrd01     begsr
023500070802
023600070803     c                   eval      *in41 = *off
023700070803     c                   eval      *in42 = *off
023800070803     c                   eval      *in43 = *off
023900070802
024000070802      * filiale gestione
024100070802     c                   clear                   og147
024200070802     c     v1cfgs        chain     azorg01l
024300070802     c                   if        not %found(azorg01l)
024400070803     c                   eval      v1cmsg = msg(01)
024500070802     c                   eval      *in28 = *on
024600070803     c                   eval      *in41 = *on
024700070802     c                   leavesr
024800070802     c                   endif
024900070803      * descrizione
025000070803     c                   eval      v1dfgs = orgdes
025100070803      * in gestione all'utente
025200070803     c                   clear                   fnlv50ds
025300070803     c                   eval      d50pru = knmus
025400070803     c                   eval      d50fgs = v1cfgs
025500070803     c                   call      'FNLV50R'
025600070803     c                   parm                    fnlv50ds
025700070803     c                   if        d50err <> *blanks
025800070803     c                   eval      v1cmsg = msg(02)
025900070803     c                   eval      *in28 = *on
026000070803     c                   eval      *in41 = *on
026100070803     c                   leavesr
026200070803     c                   endif
026300070803      * controllo se filiale attivata
026400070802     c                   eval      og147 = orgde7
026500070803     c                   if        §ogcgio = *blank
026600070803     c                   eval      v1cmsg = msg(03)
026700070803     c                   eval      *in28 = *on
026800070803     c                   eval      *in41 = *on
026900070802     c                   leavesr
027000070802     c                   endif
027100070926     c                   if        §ogddao = *blank or §ogddao = *all'0'
027200070926     c                   goto      fine
027300070926     c                   endif
027400070904      * data partenza procedura in organigramma
027500070904     c                   move      §ogddao       data_org
027600070904      * controllo se siamo in simulazione
027700070904     c                   if        *date < data_org
027800070904     c                   eval      test_simula = 'S'
027900070904     c                   else
028000070904     c                   clear                   test_simula
028100070904     c                   endif
028200070803
028300070802     c                   eval      *in05 = *off
028400070802
028500070802      * se imposto località 'DA' e località 'A' a blank la inserisco uguale
028600070802     c                   if        v1clord <> *blank and v1clora = *blank
028700070903     c                   eval      v1clora = %trimr(v1clord) + wzeta
028800070802     c                   endif
028900070802      * se imposto località 'DA' e località 'A' con solo iniziale metto nel
029000070802      * secondo byte la 'Z' x la selezione
029100070802     c                   if        v1clord <> *blank and v1clora <> *blank and
029200070802     c                             %subst(v1clora:2:1) = ' '
029300070903     c                   eval      v1clora = %trimr(v1clora) + wzeta
029400070802     c                   endif
029500070802      * vice versa località 'A' e località 'DA' a blank la inserisco uguale
029600070802     c                   if        v1clora <> *blank and v1clord = *blank
029700070802     c                   eval      v1clord = v1clora
029800070802     c                   endif
029900070802      * verifico che località 'DA' sia congruente con località 'A'
030000070802     c                   if        v1clord > v1clora
030100070803     c                   eval      v1cmsg = msg(05)
030200070803     c                   eval      *in28 = *on
030300070803     c                   eval      *in43 = *on
030400070802     c                   leavesr
030500070802     c                   endif
030600070802
030700070802      * giro
030800070802     c                   if        v1ccgi <> *blank and v1ccgi <> '9999999999'
030900070802     c                   eval      con_giro = 'S'
031000070802      * no giro
031100070802     c                   endif
031200070802      * senza giro
031300070802     c                   if        v1ccgi = *blanks
031400070802     c                   eval      con_giro = 'N'
031500070802     c                   endif
031600070802      * tutti
031700070802     c                   if        v1ccgi = *all'9'
031800070802     c                   clear                   con_giro
031900070802     c                   endif
032000070802
032100070802     c                   endsr
032200070803
032300070803      *------------------------------------------------------------------------*
032400070803      *   Preparo la stringa SQL
032500070803      *------------------------------------------------------------------------*
032600070803     c     sr_carica     begsr
032700070803
032800070803     c                   clear                   stringasql
032900070803
033000070803      * preparo la stringa sql
033100070803     c                   eval      stringasql =
033200071029     c                             'select fnorg52j.* from '                  +
033300071029     c                             'fnorg52j '                                +
033400071029     c                             'where orgpor =  '                         +
033500071029     c                             %editc(v1cfgs:'X')                         +
033600071029     c                             ' and orgnsr = 0'
033700071029      * se simulazione solo orgndc = 0
033800071029     c                   if        test_simula = 'S'
033900071029     c                   eval      stringasql =
034000071029     c                             %trim(stringasql) + ' '                     +
034100071029     c                             'and orgndc = 0'
034200071029     c                   endif
034300071029      * se filiale già attiva solo ormndc = 0
034400071029     c                   if        test_simula <> 'S'
034500071029     c                   eval      stringasql =
034600071029     c                             %trim(stringasql) + ' '                     +
034700071029     c                             'and ormndc = 0'
034800071029     c                   endif
034900071029
035000070910      * parzializzazione per localita richiesto range a video
035100070910     c                   if        v1clord <> *blanks and v1clora <> *blanks
035200070910     c                   eval      stringasql =
035300070910     c                             %trim(stringasql) + ' '                     +
035400070910     c                             'and ormlor between '                       +
035500070910     c                             '''' + v1clord + ''''                       +
035600070910     c                             'and '                                      +
035700070910     c                             '''' + v1clora + ''''
035800070910     c                   endif
035900070910      * per nome mittente
036000070910     c                   if        v1crsr <> *blanks
036100070910     c                   eval      stringasql =
036200070910     c                             %trim(stringasql) + ' '                     +
036300070910     c                             'and ormrsr like ' + '''%'                  +
036400070910     c                             v1crsr
036500070910     c                   eval      stringasql =
036600070910     c                             %trim(stringasql) + '%'''
036700070910     c                   endif
036800070803      * se presente il giro seleziono solo quel giro
036900070803     c                   if        con_giro = 'S'
037000070803     c                   eval      stringasql =
037100070803     c                             %trim(stringasql) + ' '                     +
037200070803     c                             'and orgpocgi = '                           +
037300070803     c                              %editc(v1cfgs:'X') + ' '                   +
037400070803     c                             'and orgcgi = ' + '''' + v1ccgi + ''''
037500070803     c                   endif
037600070803      * solo gli orm senza giro
037700070803     c                   if        con_giro = 'N'
037800070803     c                   eval      stringasql =
037900070803     c                             %trim(stringasql) + ' '                     +
038000070803     c                             'and orgpocgi = 0 '                         +
038100070803     c                             'and orgcgi = ' + '''' + v1ccgi  + ''''
038200070803     c                   endif
038300070803
038400070803     C/EXEC SQL
038500070803     C+ PREPARE S1 FROM :stringasql
038600070803     C/END-EXEC
038700070803
038800070803     C/EXEC SQL
038900070803     C+ DECLARE A1 CURSOR FOR S1
039000070803     C/END-EXEC
039100070803
039200070803     C/EXEC SQL
039300070803     C+ OPEN A1
039400070803     C/END-EXEC
039500070803
039600070803     c                   endsr
039700070803
039800070803      *------------------------------------------------------------------------*
039900070803      *   Pulisco il subfile
040000070803      *------------------------------------------------------------------------*
040100070803     c     sr_pulsfl     begsr
040200070803
040300070803     c                   clear                   nrr
040400070803
040500070803     c                   eval      *in20 = *off
040600070803     c                   eval      *in21 = *off
040700070803      *                  __________________
040800070803     c                   write     or73c01
040900070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
041000070803     c                   eval      *in21 = *on
041100070803     c                   eval      *in20 = *on
041200070803
041300070803     c                   eval      recsf = 1
041400070803     c                   eval      *in31 = *off
041500070803     c                   eval      *in40 = *off
041600070803
041700070803     c                   endsr
041800070803
041900070803      *------------------------------------------------------------------------*
042000070803      *   Carico il subfile
042100070803      *------------------------------------------------------------------------*
042200070803     c     sr_carsfl     begsr
042300070803
042400070803     c                   do        *hival
042500070803
042600070803     C/EXEC SQL
042700071029     C+ FETCH NEXT FROM A1 INTO :fnorgds, :fnormds
042800070803     C/END-EXEC
042900070803
043000070803      * errore nel sql esco dal pgm
043100070803     c                   if        sqlcod < 0
043200070803     c                   eval      *inlr = *on
043300070803     c                   return
043400070803     c                   endif
043500070803      * fine file
043600070803     c                   if        sqlcod = 100
043700070803     c                   leave
043800070803     c                   endif
043900071029
044000070904      * controllo se l'ORM è assegnabile
044100070904     c                   exsr      check_orm
044200070904     c                   if        or64asso = 'N'
044300070904     c                   iter
044400070904     c                   endif
044500070803      * se arrivo qua il rcd è ok e devo caricare i dati
044600070803     c                   exsr      sr_wrtsfl
044700070803      * esco per raggiunta massima capacità sfl
044800070803     c                   if        nrr > 9990
044900070803     c                   leave
045000070803     c                   endif
045100070803
045200070803     c                   enddo
045300070803
045400070803     C/EXEC SQL
045500070803     C+ CLOSE A1
045600070803     C/END-EXEC
045700070803
045800070803     c                   eval      wnrr = nrr
045900070803     c                   eval      rrnlast = nrr
046000070803     c                   eval      *in31 = *on
046100070803
046200070803     c                   endsr
046300070803
046400070803      *------------------------------------------------------------------------*
046500070803      *   Scrivo la riga del subfile
046600070803      *------------------------------------------------------------------------*
046700070803     c     sr_wrtsfl     begsr
046800070803
046900070803      * imposto i campi
047000070803     c                   clear                   vs1sce
047100070803      * codice giro
047200070803     c                   eval      vs1cgi = orgcgi
047300070803      * chiave ORM
047400070803     c                   eval      vs1poe = ormpoe
047500070803     c                   eval      vs1nsr = ormnsr
047600070803     c                   eval      vs1nor = ormnor
047700070803     c                   eval      vs1nrv = ormnrv
047800070803      * mittente
047900070803     c                   eval      vs1rsr = ormrsr
048000070803     c                   eval      vs1inr = orminr
048100070803     c                   eval      vs1lor = ormlor
048200070803     c                   eval      vs1prr = ormprr
048300070803      * peso e volume da org
048400070803     c                   eval      vs1pkg = orgpkg
048500070803     c                   eval      vs1vlm = orgvlm
048600070803      * colli
048700071019     c                   z-add     ormncl        vs1ncl
048800071019      * bancali
048900071019     c                   z-add     ormbnc        vs1bnc
049000070803
049100070803      * scrivo il subfile
049200070803     c                   add       1             nrr
049300070803      * se primo rcd che scrivo accendo indicatore di PC
049400070803     c                   if        nrr = 1
049500070803     c                   eval      *in40 = *on
049600070803     c                   else
049700070803     c                   eval      *in40 = *off
049800070803     c                   endif
049900070803      *                  __________________
050000070803     c                   write     or73s01
050100070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
050200070803
050300070803     c                   endsr
050400070803
050500070803      *------------------------------------------------------------------------*
050600070803      *   Gestione del subfile
050700070803      *------------------------------------------------------------------------*
050800070803     c     sr_gessfl     begsr
050900070803
051000070806     c                   eval      $aggrcd = *off
051100070803     c                   eval      $fatto = *off
051200070803     c                   eval      $multi = *off
051300070803
051400070803     c                   do        *hival
051500070803      * emetto il piede del subfile
051600070803      *                  __________________
051700070803     c                   write     or73z01
051800070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
051900070803      * se non ho caricato niente emetto videata vuota
052000070803     c                   if        wnrr = *zeros
052100070803      *                  __________________
052200070803     c                   write     or73v01
052300070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
052400070803     c                   eval      *in20 = *off
052500070803     c                   else
052600070803     c                   eval      *in20 = *on
052700070803     c                   if        recsf > wnrr
052800070803     c                   eval      recsf = 1
052900070803     c                   endif
053000070803     c                   endif
053100070803      * emissione sfl
053200070803      *                  __________________
053300070803     c                   exfmt     or73c01
053400070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
053500070803     c                   eval      *in28 = *off
053600070803     c                   clear                   v1cmsg
053700070803
053800070803      * f3=fine
053900070803     c                   if        *inkc
054000070803     c                   goto      fine
054100070803     c                   endif
054200070803      * f12=ritorno
054300070803     c                   if        *inkl
054400070803     c                   goto      emid01
054500070803     c                   endif
054600070803      * f1=seleziona tutti
054700070803     c                   if        *inka
054800070803     c                   exsr      sr_f01
054900070803     c                   iter
055000070803     c                   endif
055100070803
055200070803      * controllo le selezioni fatte
055300070803     c                   if        wnrr > *zeros
055400070803     c                   exsr      sr_contrsfl
055500070803     c                   if        *in28
055600070803     c                   iter
055700070803     c                   endif
055800070806     c                   if        $aggrcd = *on
055900070806     c                   leave
056000070806     c                   endif
056100070803     c                   endif
056200070803
056300070803     c                   enddo
056400070803
056500070803     c                   endsr
056600070803
056700070803      *------------------------------------------------------------------------*
056800070803      *   Controllo le selezioni del subgile
056900070803      *------------------------------------------------------------------------*
057000070803     c     sr_contrsfl   begsr
057100070803
057200070803     c                   clear                   nrr
057300070803     c                   do        *hival
057400070803     c                   add       1             nrr
057500070803     c     nrr           chain     or73s01                            30
057600070803      * esco se fine sfl
057700070803     c                   if        *in30
057800070803     c                   leave
057900070803     c                   endif
058000070803
058100070803     c                   eval      *in10 = *off
058200070803
058300070803      * controllo le selezioni a video
058400070803     c                   select
058500071026
058600071026      * opzione 2 --> modifca ORM
058700071026     c                   when      vs1sce = '2'
058800071026     c                   exsr      sr_modorm
058900071026     c                   eval      recsf = nrr
059000071026     c                   clear                   vs1sce
059100071026     c                   eval      *in40 = *on
059200070803
059300070803      * opzione 5 --> visualizza ORM
059400070803     c                   when      vs1sce = '5'
059500070803     c                   exsr      sr_intorm
059600070803     c                   eval      recsf = nrr
059700070803     c                   clear                   vs1sce
059800070803     c                   eval      *in40 = *on
059900070803
060000070803      * opzione 1 --> attribuzione giro per singolo ORM
060100070803     c                   when      vs1sce = '1'
060200070803     c                   exsr      sr_singolo
060300070803     c                   eval      recsf = nrr
060400070803     c                   clear                   vs1sce
060500070803     c                   eval      *in40 = *on
060600070803
060700070803      * opzione 9 --> attribuzione giro per più ORM
060800070803     c                   when      vs1sce = '9'
060900070803     c                   eval      *in10 = *on
061000070803     c                   exsr      sr_multiplo
061100070803     c                   clear                   vs1sce
061200070803     c                   eval      *in40 = *on
061300070803
061400070803     c                   other
061500070803      * spengo l'indicatore di PC
061600070803     c                   eval      *in40 = *off
061700070803     c                   endsl
061800070803      *                  __________________
061900070803     c                   update    or73s01
062000070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
062100070803     c                   enddo
062200070803
062300070803     c                   endsr
062400071026
062500071026      *------------------------------------------------------------------------*
062600071026      *   Modifico l'ORM
062700071026      *------------------------------------------------------------------------*
062800071026     c     sr_modorm     begsr
062900071026
063000071026     c                   clear                   dsfior07
063100071026     c                   clear                   fior05ds
063200071026     c                   eval      dsopoe = vs1poe
063300071026     c                   eval      dsonor = vs1nor
063400071026     c                   eval      dsonsr = vs1nsr
063500071026     c                   eval      dsonrv = vs1nrv
063600071026     c                   eval      dsoopz = vs1sce
063700071026     c                   eval      dsofgs = v1cfgs
063800071026     c                   eval      kpjbu = dsfior07
063900071026     c                   clear                   fior05ds
064000071026     c                   call      'FIOR05R'
064100071026     c                   parm                    kpjba
064200071026     c                   parm                    fior05ds
064300071026     c                   eval      $aggrcd = *on
064400071026
064500071026     c                   endsr
064600070803
064700070803      *------------------------------------------------------------------------*
064800070803      *   Interrogo l'ORM
064900070803      *------------------------------------------------------------------------*
065000070803     c     sr_intorm     begsr
065100070803
065200070803     c                   clear                   dsfior07
065300070803     c                   eval      dsopoe = vs1poe
065400070803     c                   eval      dsonor = vs1nor
065500070803     c                   eval      dsonsr = vs1nsr
065600070803     c                   eval      dsonrv = vs1nrv
065700070803     c                   eval      dsoopz = vs1sce
065800070803     c                   eval      dsofgs = v1cfgs
065900070803     c                   eval      kpjbu = dsfior07
066000070803     c                   call      'FIOR07R'
066100070803     c                   parm                    kpjba
066200070803
066300070803     c                   endsr
066400070803
066500070803      *------------------------------------------------------------------------*
066600070803      *   Seleziona per attribuzione singolo ORM
066700070803      *------------------------------------------------------------------------*
066800070803     c     sr_singolo    begsr
066900070803
067000070803     c     korm          chain     fnorm01l
067100070803     c                   if        not %found(fnorm01l)
067200070803     c                   leavesr
067300070803     c                   endif
067400070803     c     korm          chain(n)  fnorg01l
067500070803     c                   if        not %found(fnorg01l)
067600070803     c                   leavesr
067700070803     c                   endif
067800070803      * imposto i campi per il video di dettaglio dati ORM
067900070803     c                   exsr      sr_datiorm
068000070803      * videata di dettaglio ORM
068100070803     c                   exsr      sr_visriga
068200070803
068300070803     c                   endsr
068400070803
068500070803      *------------------------------------------------------------------------*
068600070803      *   Seleziona per attribuzione più ORM
068700070803      *------------------------------------------------------------------------*
068800070803     c     sr_multiplo   begsr
068900070803
069000070803     c     korm          chain     fnorm01l
069100070803     c                   if        not %found(fnorm01l)
069200070803     c                   leavesr
069300070803     c                   endif
069400070803     c     korm          chain(n)  fnorg01l
069500070803     c                   if        not %found(fnorg01l)
069600070803     c                   leavesr
069700070803     c                   endif
069800070803      * imposto i campi per il video di dettaglio dati ORM
069900070803     c                   if        $multi = *off
070000070803     c                   exsr      sr_datiorm
070100070803      * videata di dettaglio ORM
070200070803     c                   exsr      sr_visriga
070300070803      * se "$fatto" ON ho aggiornato la prima riga multipla
070400070803      * quindi memorizzo i valori da aggiornare x eventuali righe con stessa
070500070803      * opzione e proseguo l'aggiornamento  senza più emettere il video per
070600070803      * queste ultime se le opzioni fossero mischiate le altre differenti
070700070803      * dalla multipla verranno elaborate normalmente nell'ambito dello stesso
070800070803      * ciclo di elaborazione. Le multiple verranno elaborate in maniera
070900070803      * occulta con il valore impostato inizialmente
071000070803     c                   if        $fatto = *on
071100070803     c                   eval      sav_v2ccgin = v2ccgin
071200070803     c                   eval      $multi = *on
071300070803     c                   endif
071400070803      * imposta il giro sulle ulteriori righe della scelta multipla
071500070803     c                   else
071600070803     c                   eval      v2ccgin = sav_v2ccgin
071700070803     c                   exsr      sr_aggiorna
071800070803     c                   endif
071900070803
072000070803     c                   endsr
072100070803
072200070803      *------------------------------------------------------------------------*
072300070803      *   Imposto i dati dell'ORM per la videata a dettaglio
072400070803      *------------------------------------------------------------------------*
072500070803     c     sr_datiorm    begsr
072600071019
072700071019     c                   clear                   v2dcgi
072800071019     c                   clear                   v2dcgin
072900070803
073000070803      * chiave ORM
073100070803     c                   eval      v2cpoe = ormpoe
073200070803     c                   eval      v2cnsr = ormnsr
073300070803     c                   eval      v2cnor = ormnor
073400070803     c                   eval      v2cnrv = ormnrv
073500070803      * ordinante
073600070803     c                   eval      v2ccor = ormcor
073700070803     c                   eval      v2crso = ormrso
073800070803      * tipo orm
073900070803     c                   clear                   v2dtor
074000070803     c                   clear                   tibs02ds
074100070803     c                   eval      t02mod = 'C'
074200070803     c                   eval      t02sif = knsif
074300070803     c                   eval      t02cod = 'TOR'
074400070803     c                   eval      t02ke1 = ormtor
074500070803     c                   call      'TIBS02R'
074600070803     c                   parm                    kpjba
074700070803     c                   parm                    tibs02ds
074800070803     c                   eval      v2dtor = t02uni
074900070803      * fase
075000070803     c                   eval      v2cfao =  ormfao
075100070803     c                   clear                   v2dfao
075200070803     c                   clear                   tibs02ds
075300070803     c                   eval      t02mod = 'C'
075400070803     c                   eval      t02sif = knsif
075500070806     c                   eval      t02cod = 'FAR'
075600070803     c                   movel(p)  ormfao        t02ke1
075700070803     c                   call      'TIBS02R'
075800070803     c                   parm                    kpjba
075900070803     c                   parm                    tibs02ds
076000070803     c                   eval      dfar = t02uni
076100070803     c                   eval      v2dfao = d§fardes
076200070803      * mittente
076300070803     c                   eval      v2ccra = ormcra
076400070803     c                   eval      v2crsr = ormrsr
076500070803      * indirizzo
076600070803     c                   eval      v2cinr = orminr
076700070803     c                   eval      v2clor = ormlor
076800070803     c                   eval      v2ccar = ormcar
076900070803     c                   eval      v2cprr = ormprr
077000070904     c                   eval      v2cnar = ormnar
077100070803      * note
077200070803     c                   eval      v2cno1 = ormno1
077300070803     c                   eval      v2cno2 = ormno2
077400070803      * colli/bancali
077500070803     c                   eval      v2cncl = ormncl
077600070803     c                   eval      v2cbnc = ormbnc
077700070803      * peso/volume da ORG
077800070803     c                   eval      v2cpkg = orgpkg
077900070803     c                   eval      v2cvlm = orgvlm
078000070803      * automezzo
078100070803     c                   select
078200070803     c                   when      ormblc <> *zeros
078300070803     c                   eval      v2caut = 'BIL'
078400070803     c                   when      ormatt <> *zeros
078500070803     c                   eval      v2caut = 'AUT'
078600070803     c                   when      ormmtc <> *zeros
078700070803     c                   eval      v2caut = 'MOT'
078800070803     c                   other
078900070803     c                   clear                   v2caut
079000070803     c                   endsl
079100070803      * sponda idraulica
079200070803     c                   if        ormspi = 'S'
079300070803     c                   eval      v2cspi = 'SI'
079400070803     c                   else
079500070803     c                   clear                   v2cspi
079600070803     c                   endif
079700070803      * data ritiro
079800070803     c     *iso          move      ormdar        dataiso
079900070803     c                   move      dataiso       dataeur
080000070803     c                   move      dataeur       v2cdar
080100070803      * ora
080200070803     c                   eval      v2corr = ormorr
080300070803      * priorità
080400070803     c                   z-add     ormsto        v2csto
080500070803      * natura merce
080600070803     c                   eval      v2cnam = ormnam
080700070803      * referente
080800070803     c                   eval      v2crer = ormrer
080900070803      * telefono
081000070803     c                   eval      v2cter = ormter
081100070803      * riferimento
081200070803     c                   eval      v2crfa = ormrfa
081300070803      * giro
081400070803     c                   eval      v2ccgi = orgcgi
081500071019     c                   if        v2ccgi <> *blanks
081600071019     c                   eval      wcgi = v2ccgi
081700071019     c                   exsr      ctrgiro
081800071019     c                   eval      v2dcgi = d09odes
081900071019     c                   endif
082000070803      * pulisco i dati di i/o
082100070803     c                   clear                   v2ccgin
082200070803     c                   clear                   v2dcgin
082300070803     c                   clear                   v2cposin
082400070803
082500070803     c                   endsr
082600070803
082700070803      *------------------------------------------------------------------------*
082800070803      *   Visualizzo la riga
082900070803      *------------------------------------------------------------------------*
083000070803     c     sr_visriga    begsr
083100070803
083200070803     c                   do        *hival
083300070803      *                  __________________
083400070803     c                   exfmt     or73d02
083500070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
083600070803     c                   eval      *in28 = *off
083700070806     c                   clear                   v2cmsg
083800070803      * f3=fine
083900070803     c                   if        *inkc
084000070803     c                   goto      fine
084100070803     c                   endif
084200070803      * f12=ritorno
084300070803     c                   if        *inkl
084400070803     c                   leave
084500070803     c                   endif
084600070803      * f7=interrogazione giri
084700070803     c                   if        *inkg
084800070803     c                   exsr      sr_f07
084900070803     c                   eval      v2ccgin = d09ocgi
085000070803     c                   iter
085100070803     c                   endif
085200070803      * f8=interrogazione autisti
085300070803     c                   if        *inkh
085400070803     c                   exsr      sr_f08
085500070803     c                   eval      v2ccgin = d08cgio
085600070803     c                   iter
085700070803     c                   endif
085800070803      * controllo
085900070803     c                   exsr      sr_ctrd02
086000070803     c                   if        *in28
086100070803     c                   iter
086200070803     c                   endif
086300070806      * f6=aggiorna
086400070803     c                   if        *inkf
086500070803     c                   exsr      sr_aggiorna
086600070803     c                   if        $fatto = *on
086700070803     c                   leave
086800070803     c                   endif
086900070803     c                   endif
087000070803
087100070803     c                   enddo
087200070803
087300070803     c                   endsr
087400070803
087500070803      *------------------------------------------------------------------------*
087600070803      *   Controlli video --> dettaglio ORM
087700070803      *------------------------------------------------------------------------*
087800070803     c     sr_ctrd02     begsr
087900070803
088000070803     c                   eval      *in28 = *off
088100070803      * controllo giro
088200070803     c                   if        v2ccgin <> *blanks
088300071019     c                   eval      wcgi = v2ccgin
088400070803     c                   exsr      ctrgiro
088500070803      * giro errato
088600070903     c                   if        d09oerr = '1' or d09oatb <> *blanks
088700070803     c                   eval      *in28 = *on
088800070806     c                   eval      v2cmsg = msg(04)
088900070803     c                   leavesr
089000070803     c                   endif
089100070803      * controllo filiale giro con filiale ritiro
089200070806     c                   if        d09ofgs <> v1cfgs
089300070806     c                   eval      *in28 = *on
089400070806     c                   eval      v2cmsg = msg(06)
089500070806     c                   leavesr
089600070806     c                   endif
089700071019     c                   eval      v2dcgin = d09odes
089800070806
089900070803     c                   endif
090000070803
090100070803     c                   endsr
090200070803
090300070803      *------------------------------------------------------------------------*
090400070803      *   Aggiorno i dati su FNORG x il giro
090500070803      *------------------------------------------------------------------------*
090600070803     c     sr_aggiorna   begsr
090700070803
090800070925     c                   if        v2ccgin <> *blanks
090900070803     c     korm          chain     fnorg01l
091000070803     c                   if        %found(fnorg01l)
091100070803     c                   eval      orgpocgi = v1cfgs
091200070803     c                   eval      orgcgi = v2ccgin
091300070803     c                   eval      orgtgi = 'M'
091400070806     c                   exsr      sr_dataora
091500070903     c                   eval      orgdtvtel = wdata
091600070903     c                   eval      orghvtel = wora
091700070803     c                   update    fnorg000
091800070803     c                   endif
091900070925     c                   endif
092000070803
092100070803     c                   eval      $fatto = *on
092200070806     c                   eval      $aggrcd = *on
092300070803
092400070803     c                   endsr
092500070904
092600070904      *------------------------------------------------------------------------*
092700070904      *  Controlla validità dell'ORM
092800070904      *------------------------------------------------------------------------*
092900070904     c     check_orm     begsr
093000070904
093100070904      * per controllare se ORM assegnabile richiamo pgm esterno
093200070904     c                   clear                   fior64ds
093300070904     c                   eval      or64poei = ormpoe
093400070904     c                   eval      or64nsri = ormnsr
093500070904     c                   eval      or64nori = ormnor
093600070904     c                   eval      or64nrvi = ormnrv
093700070904      * imposto la data
093800070904     c                   eval      or64dfvi = *date
093900070904      * se sono in simulazione controllo la distinta su ORG
094000070904      * e imposto il flag di controllo ORMETI in simulazione
094100070904     c                   if        test_simula = 'S'
094200070904     c                   eval      or64ndci = 'G'
094300070904     c                   eval      or64etii = 'S'
094400070904     c                   else
094500070904      * se già attivo controllo la distinta su entrambi
094600070904      * e imposto il flag di controllo ORMETI solo per filiale attiva
094700070904     c                   eval      or64ndci = 'E'
094800070904     c                   clear                   or64etii
094900070904     c                   endif
095000070904      * controlla la data di ritiro
095100070904     c                   eval      or64dari = 'S'
095200070904     c                   call      'FIOR64R'
095300070904     c                   parm                    kpjba
095400070904     c                   parm                    fior64ds
095500070904
095600070904     c                   endsr
095700070802
095800070802      *------------------------------------------------------------------------*
095900070802      *   Controllo e decodifica giro
096000070802      *------------------------------------------------------------------------*
096100070802     c     ctrgiro       begsr
096200070802
096300070802     c                   clear                   fidg09ds
096400070802     c                   eval      d09iop0 = '001'
096500070803     c                   eval      d09ifgs = v1cfgs
096600070806     c                   exsr      sr_dataora
096700070806     c                   eval      d09idat = wdata
096800071019     c                   eval      d09icgi = wcgi
096900070802     c                   eval      d09itug = 'R'
097000070802     c                   eval      kpjbu = fidg09ds
097100070802     c                   call      'FIDG09R'
097200070802     c                   parm                    kpjba
097300070802     c                   eval      fidg09ds = kpjbu
097400070802
097500070802     c                   endsr
097600070803
097700070802      *------------------------------------------------------------------------*
097800070802      *   Seleziona tutti i record del sfl                         ?
097900070802      *------------------------------------------------------------------------*
098000070803     c     sr_f01        begsr
098100070802
098200070803     c                   if        wnrr > *zeros
098300070803     c                   clear                   nrr
098400070803     c                   do        *hival
098500070803     c                   add       1             nrr
098600070803     c     nrr           chain     or73s01                            30
098700070803      * esco se fine sfl
098800070803     c                   if        *in30
098900070803     c                   leave
099000070803     c                   endif
099100070803     c                   eval      vs1sce = '1'
099200070803     c                   update    or73s01
099300070803     c                   enddo
099400070802     c                   endif
099500070802
099600070802     c                   endsr
099700070802
099800070802      *------------------------------------------------------------------------*
099900070802      *   Gestione F07                                                ?
100000070802      *------------------------------------------------------------------------*
100100070803     c     sr_f07        begsr
100200070802
100300070802     c                   clear                   fidg09ds
100400070802     c                   eval      d09iop0 = 'P01'
100500070802     c                   eval      d09ifgs = v1cfgs
100600070806     c                   exsr      sr_dataora
100700070806     c                   eval      d09idat = wdata
100800070803     c                   if        v2cposin <> *blanks
100900070803     c                   eval      d09ides = v2cposin
101000070803     c                   endif
101100070802     c                   eval      d09itug = 'R'
101200070802     c                   eval      kpjbu = fidg09ds
101300070802     c                   call      'FIDG09R'
101400070802     c                   parm                    kpjba
101500070802     c                   eval      fidg09ds = kpjbu
101600070802
101700070802     c                   endsr
101800070803
101900070803      *------------------------------------------------------------------------*
102000070803      *   Gestione F08                                                ?
102100070803      *------------------------------------------------------------------------*
102200070803     c     sr_f08        begsr
102300070803
102400070803     c                   clear                   fidg08ds
102500070803     c                   eval      d08fgs = v1cfgs
102600070803     c                   eval      d08fgsgo = v1cfgs
102700070803     c                   eval      d08rsc = v2cposin
102800070903     c                   eval      d08tp = 'R'
102900070803     c                   eval      kpjbu = fidg08ds
103000070803     c                   call      'FIDG08R'
103100070803     c                   parm                    kpjba
103200070803     c                   eval      fidg08ds = kpjbu
103300070803
103400070803     c                   endsr
103500070802
103600070802      *------------------------------------------------------------------------*
103700070802      *   Riordina il sfl per località                                 ?
103800070802      *------------------------------------------------------------------------*
103900070803     c     sr_ordsfl     begsr
104000070802
104100070802      * totale records caricati nel SFL
104200070803     c                   eval      nrr = 0
104300070802
104400070802      * inizializza i campi chiave x l'ordinamento. C'è un campo in più non
104500070802      * presente nel subfile -- "Selected"?-- questo è aggiunto al record.
104600070802      * il campo è usato per selezionare i records dando un ordine a quelli
104700070802      * selezionati davanti ai non selezionati.
104800070802     c                   clear                   QLGSCB
104900070802     c                   clear                   QLGSCB00
105000070802
105100070803      * 1 campo chiave.   ordino x località
105200070803     c                   eval      QLGNBRK    = 1
105300070803      * localita a posizione 1
105400070802     c                   eval      QLGSP      = 1
105500070803     c                   eval      QLGSS      = %SIZE(vs1lor)
105600070802     c                   eval      QLGDT      = Carattere
105700070802     c                   eval      QLGSO      = Ascendente
105800070802     c                   eval      QLGKL(1)   = QLGSKL
105900070802
106000070802      * Load other sort parameters.
106100070802     c                   eval      QLGLB     = 80 + 16 * MaxKey
106200070802     c                   eval      QLGRL     = %SIZE(sflrcd) - 1
106300070802     c                   eval      QLGRT     = 8
106400070802     c                   eval      QLGOKL    = 80
106500070802     c                   eval      QLGLKE    = 16
106600070802     c                   eval      QLGLSS    = 290
106700070802
106800070802      * Initialize Sort I/O API fields.
106900070802     c                   eval      QLGRL00  = QLGRL
107000070802     c                   eval      QLGRC00  = 1
107100070802     c                   clear                   QUSEI
107200070802     c                   eval      QUSBPRV = %SIZE(QUSEC)
107300070802
107400070802      * First step - Initialize the sort routine.
107500070802     c                   call      'QLGSORT'
107600070802     c                   parm                    QLGSCB
107700070802     c                   parm                    NotUsed
107800070802     c                   parm                    NotUsed
107900070802     c                   parm                    SizeList
108000070802     c                   parm                    ReturnSize
108100070802     c                   parm                    QUSEC
108200070802
108300070802      * Next step - Write records to I/O routine.
108400070802     c                   eval      QLGRT00 = Put
108500070802
108600070803     c                   do        rrnlast       nrr_x
108700070803     c     nrr_x         chain     or73s01
108800070802
108900070802      * solo le righe con Selected = 'Y' sono riordinate,
109000070802      * quindi per fare un ordinamento di tutte le righe
109100070802      * metto 'Y' sempre.
109200070802     c                   eval      selected  = 'Y'
109300070802
109400070802     c                   clear                   QUSEI
109500070802     c                   eval      QUSBPRV = %SIZE(QUSEC)
109600070802
109700070802     c                   call      'QLGSRTIO'
109800070802     c                   parm                    QLGSCB00
109900070802     c                   parm                    SflRcd
110000070802     c                   parm                    NotUsed
110100070802     c                   parm                    SizeList
110200070802     c                   parm                    NotUsed
110300070802     c                   parm                    QUSEC
110400070802
110500070802     c                   enddo
110600070802
110700070802      * Next step - Signal end of input, clear subfile for reload.
110800070802     c                   eval      QLGRT00 = EndPut
110900070802     c                   clear                   QUSEI
111000070802     c                   eval      QUSBPRV = %SIZE(QUSEC)
111100070802
111200070802     c                   call      'QLGSRTIO'
111300070802     c                   parm                    QLGSCB00
111400070802     c                   parm                    SflRcd
111500070802     c                   parm                    NotUsed
111600070802     c                   parm                    SizeList
111700070802     c                   parm                    NotUsed
111800070802     c                   parm                    QUSEC
111900070803      * pulizia SFL    ?
112000070802     c                   eval      *in20 = *off
112100070802     c                   eval      *in21 = *off
112200070802      *                  __________________
112300070803     c                   write     or73c01
112400070802      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
112500070802     c                   eval      *in21 = *on
112600070802
112700070802      * Final step - Write the records back to the subfile.
112800070802     c                   eval      QLGRT00 = Get
112900070802
113000070803     c                   do        rrnlast       nrr_x
113100070803     c                   eval      nrr = nrr_x
113200070802     c                   clear                   QUSEI
113300070802     c                   eval      QUSBPRV = %SIZE(QUSEC)
113400070802     c                   call      'QLGSRTIO'
113500070802     c                   parm                    QLGSCB00
113600070802     c                   parm                    NotUsed
113700070802     c                   parm                    SflRcd
113800070802     c                   parm                    QLGRL00
113900070802     c                   parm                    NotUsed
114000070802     c                   parm                    QUSEC
114100070803
114200070803     c                   eval      nrr = nrr_x
114300070802      *                  __________________
114400070803     c                   write     or73s01
114500070802      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
114600070802     c                   enddo
114700070802
114800070802      *  All'uscita di queste chiamate a routine di sistema il SFL record
114900070802      *   si incrementa stranamente quindi lo reimposto correttamente
115000070802      *    prima di lasciare la routine.
115100070803     c                   eval      nrr = rrnlast
115200070802
115300070802     c                   endsr
115400070806
115500070806      *------------------------------------------------------------------------*
115600070806      *   Imposto data e ora del momento
115700070806      *------------------------------------------------------------------------*
115800070806     c     sr_dataora    begsr
115900070806
116000070806     c                   time                    w0140
116100070806      * ora
116200070806     c                   movel     w0140         wora
116300070806      * data
116400070806     c                   move      w0140         wdata
116500070806     c     *eur          move      wdata         dataeur
116600070806     c                   move      dataeur       dataiso
116700070806     c                   move      dataiso       wdata
116800070806
116900070806     c                   endsr
117000070802
117100070802      *------------------------------------------------------------------------*
117200070802      *   Operazioni iniziali      ?
117300070802      *------------------------------------------------------------------------*
117400070802     c     *inzsr        begsr
117500070802
117600070802     c     *entry        plist
117700070802     c                   parm                    kpjba
117800070802
117900070802     c     *dtaara       define    §azute        azuteds
118000070802     c     *dtaara       define    §datiute      ddatiute
118100070802     c                   in(e)     *dtaara
118200070802     c                   if        %error or rsut = *blanks
118300070802     c                   clear                   tibs34ds
118400070802     c                   call      'TIBS34R'
118500070802     c                   parm                    tibs34ds
118600070802     c                   in        *dtaara
118700070802     c                   endif
118800070803
118900070803      * filiale gestione da filiale utente
119000070803     c                   eval      v1cfgs = dutpou
119100070803      * abilito f18
119200070803     c                   eval      *in06 = (dutlpo = '2' or dutlpo = *blanks)
119300070803
119400070803     c     korm          klist
119500070803     c                   kfld                    vs1poe
119600070803     c                   kfld                    vs1nsr
119700070803     c                   kfld                    vs1nor
119800070803     c                   kfld                    vs1nrv
119900070802
120000070802     c                   endsr
120100070802
120200070802      *------------------------------------------------------------------------*
120300070802** msg
120400070802Filiale errata                                                                 01
120500070802La Filiale non è in gestione all'utente                                        02
120600070803La Filiale non ha la procedura GIRI ORM attiva                                 03
120700070803Codice Giro non corretto                                                       04
120800070803Parzializzazione località errata inserire correttamente                        05
120900070806Filiale giro incongruente con filiale ritiro                                   06
