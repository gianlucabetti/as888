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
004800081104     d yy              s              3  0
004900071019     d wcgi            s                   like(orgcgi) inz
005000080208     d wdar            s              8  0
005100070806     d wdata           s              8  0
005200070803     d wnrr            s                   like(recsf)
005300070806     d wora            s              6  0
005400070903     d wzeta           s             19    inz(*all'Z')
005500070806     d w0140           s             14  0
005600070806     d $aggrcd         s              1    inz(*off)
005700070803     d $fatto          s              1    inz(*off)
005800070803     d $multi          s              1    inz(*off)
005900070802
006000070802      *------------------------------------------------------------------------*
006100070802      *   S C H I E R E
006200070802      *------------------------------------------------------------------------*
006300070802     d msg             s             78    dim(30) ctdata perrcd(1)
006400081104     d skpodpd         s              3  0 dim(250)                             p.o. DPD
006500070802
006600070802      *------------------------------------------------------------------------*
006700070802      *   D S   I N T E R N E / E S T E R N E
006800070802      *------------------------------------------------------------------------*
006900080208     d clnmat          ds
007000080208     d  mat                    1     31    dim(31)
007100080208     d clnpom          ds
007200080208     d  pom                    1     31    dim(31)
007300080208
007400080208     d                 ds
007500080208     d  ds_giorno              1      2  0
007600080208     d  ds_mese                3      4  0
007700080208     d  ds_anno                5      8  0
007800080208     d  ds_data                1      8  0
007900080208
008000070803     d dsfior07        ds
008100070803     d  dsopoe                        3  0
008200070803     d  dsonor                        7  0
008300070803     d  dsonsr                        2  0
008400070803     d  dsonrv                        2  0
008500070803     d  dsoopz                        1
008600070803     d  dsofgs                        3  0
008700070806
008800070806     d                 ds
008900070806     d  v2ccor                 1     10  0
009000070906     d  v2ccor1                1      3  0
009100070906     d  v2ccor2                4      7  0
009200070806     d  v2ccor3                8     10  0
009300070806
009400070806     d                 ds
009500070806     d  v2ccra                 1     10  0
009600070906     d  v2ccra1                1      3  0
009700070906     d  v2ccra2                4      7  0
009800070806     d  v2ccra3                8     10  0
009900080208
010000080208     d wlbdat          ds                  inz
010100080208     d  g08dat                 1      8  0
010200080208     d  g08inv                 9     16  0
010300080208     d  g08err                17     17
010400080208     d  g08tgi                18     22  0
010500070806
010600070802     d azuteds       e ds                  extname(azute00f)
010700070802     d ddatiute      e ds
010800070803     d dfar          e ds
010900080208     d ddft          e ds
011000070803     d fidg08ds      e ds
011100070802     d fidg09ds      e ds
011200071026     d fior05ds      e ds
011300070904     d fior64ds      e ds
011400070802     d fnlv50ds      e ds
011500070802     d fnorgds       e ds                  extname(fnorg00f)
011600070802     d fnormds       e ds                  extname(fnorm00f)
011700070802     d kpjba         e ds
011800081104     d og143         e ds
011900070802     d og147         e ds
012000070803     d tibs02ds      e ds
012100070802     d tibs34ds      e ds
012200070802
012300070802     d                sds
012400070803     d  vtcpgm                 1     10
012500070802
012600070802      *------------------------------------------------------------------------*
012700070802      *   C O S T A N T I
012800070802      *------------------------------------------------------------------------*
012900070802
013000070802      **********************************************************************
013100070802      * Constants
013200070802      *     MaxKey - è il max. num. di campi chiave permesso in questo pgm
013300070802      **********************************************************************
013400070802      * Indice di schiera dei campi chiave di ordinamento del SFL  (MAXkey)
013500070802     d MaxKey          c                   9
013600070802     d Ascendente      c                   1
013700070802     d Discendente     c                   2
013800070802     d Carattere       c                   6
013900070802     d Put             c                   1
014000070802     d EndPut          c                   2
014100070802     d Get             c                   3
014200070802     d Numerico        c                   8
014300070802      **********************************************************************
014400070802      * Campi utili:
014500070802      *     RRN1       - Numero relativo di record del Subfile
014600070802      *     SizeList   - Dimensione della lista
014700070802      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamento
014800070802      **********************************************************************
014900070802     d Rrn1            s              5I 0
015000070802     d NotUsed         s             16A
015100070802     d ReturnSize      s              9B 0
015200070802     d SizeList        s              9B 0
015300070802     d RrnLast         s              5I 0
015400070802     d WrkSort         s              1  0 inz(0)
015500070802     d WindSort        s              3I 0 inz(0)
015600070802      **********************************************************************
015700070802      * Data Structures
015800070802      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
015900070802      *     QLGSCB     - The sort request block for the QLGSORT API
016000070802      *     QLGSCB00   - The sort request block for the QLGSRTIO API
016100070802      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
016200070802      *     QUSEC      - Error structure for the QLGSORT API
016300070802      **********************************************************************
016400070802     d sflrcd          ds
016500070803     d  vs1lor
016600070803     d  vs1prr
016700070803     d  vs1inr
016800070803     d  vs1rsr
016900070803     d  vs1pkg
017000070803     d  vs1vlm
017100070803     d  vs1ncl
017200071019     d  vs1bnc
017300070803     d  vs1cgi
017400070803     d  vs1poe
017500070803     d  vs1nsr
017600070803     d  vs1nor
017700070803     d  vs1nrv
017800081104     d  vs1tco
017900070803     d  vs1sce
018000070802     d  selected                      1A
018100070125
018200070125      /COPY QSYSINC/QRPGLESRC,QLGSORT
018300070125     d QLGKL                         16    DIM(MaxKey)
018400070125     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
018500070125     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
018600070125     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
018700070125     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
018800070125
018900070125      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
019000070125      /COPY QSYSINC/QRPGLESRC,QUSEC
019100070802
019200070802      *------------------------------------------------------------------------*
019300070802
019400070903      * pulisco la videata
019500070903     c                   exsr      sr_puld01
019600070803      * gestione della prima videata --> parametri
019700070803     c     emid01        tag
019800070803      * emetto la videata
019900070803      *                  __________________
020000070803     c                   exfmt     or73d01
020100070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
020200070803      * spengo indicatori di comodo
020300070803     c                   eval      *in28 = *off
020400070803     c                   eval      *in42 = *off
020500070803     c                   eval      *in90 = *off
020600070803      * pulisco campo messaggi
020700070803     c                   clear                   v1cmsg
020800070803      * f3=fine
020900070803     c                   if        *inkc
021000070803     c                   goto      fine
021100070803     c                   endif
021200070803      * f7=interrogazione giri
021300070803     c                   if        *inkg
021400070803     c                   exsr      sr_f07
021500070803     c                   eval      v1ccgi = d09ocgi
021600070803     c                   eval      *in42 = *on
021700070803     c                   eval      *in90 = *on
021800070803     c                   goto      emid01
021900070803     c                   endif
022000070803      * f18=cambio filiale
022100070803     c                   if        *inks
022200070803     c                   eval      *in05 = *on
022300070803     c                   goto      emid01
022400070803     c                   endif
022500070803      * controllo dati immessi
022600070803     c                   exsr      sr_ctrd01
022700070803     c   28              goto      emid01
022800070803
022900070803     c     carica        tag
023000070803      * preparo la stringa SQL
023100070803     c                   exsr      sr_carica
023200070803      * preparo il subfile dettaglio ORM
023300070803     c                   exsr      sr_pulsfl
023400070803     c                   exsr      sr_carsfl
023500070803      * gestione del subfile
023600070803     c                   exsr      sr_ordsfl
023700070803     c                   exsr      sr_gessfl
023800071026     c                   if        $aggrcd = *on
023900070806     c                   goto      carica
024000070806     c                   endif
024100070802
024200070803     c     fine          tag
024300070802     c                   eval      *inlr = *on
024400070802
024500070803      *------------------------------------------------------------------------*
024600070803      *   Pulizia primo video --> parametri
024700070803      *------------------------------------------------------------------------*
024800070803     c     sr_puld01     begsr
024900070803
025000070803     c                   clear                   v1clord
025100070803     c                   clear                   v1clora
025200070803     c                   clear                   v1crsr
025300070803     c                   clear                   v1ccgi
025400070803      * pulisco anche il campo di posizionamento interrogazioni
025500070803      * dato che ho una routine unica per interrogare i giri
025600070803     c                   clear                   v2cposin
025700070803
025800070803     c                   endsr
025900070803
026000070802      *------------------------------------------------------------------------*
026100070802      *   Controlli primo video --> parametri
026200070802      *------------------------------------------------------------------------*
026300070803     c     sr_ctrd01     begsr
026400070802
026500070803     c                   eval      *in41 = *off
026600070803     c                   eval      *in42 = *off
026700070803     c                   eval      *in43 = *off
026800080208     c                   eval      *in44 = *off
026900070802
027000070802      * filiale gestione
027100070802     c                   clear                   og147
027200070802     c     v1cfgs        chain     azorg01l
027300070802     c                   if        not %found(azorg01l)
027400070803     c                   eval      v1cmsg = msg(01)
027500070802     c                   eval      *in28 = *on
027600070803     c                   eval      *in41 = *on
027700070802     c                   leavesr
027800070802     c                   endif
027900070803      * descrizione
028000070803     c                   eval      v1dfgs = orgdes
028100070803      * in gestione all'utente
028200070803     c                   clear                   fnlv50ds
028300070803     c                   eval      d50pru = knmus
028400070803     c                   eval      d50fgs = v1cfgs
028500070803     c                   call      'FNLV50R'
028600070803     c                   parm                    fnlv50ds
028700070803     c                   if        d50err <> *blanks
028800070803     c                   eval      v1cmsg = msg(02)
028900070803     c                   eval      *in28 = *on
029000070803     c                   eval      *in41 = *on
029100070803     c                   leavesr
029200070803     c                   endif
029300070803      * controllo se filiale attivata
029400070802     c                   eval      og147 = orgde7
029500070803     c                   if        §ogcgio = *blank
029600070803     c                   eval      v1cmsg = msg(03)
029700070803     c                   eval      *in28 = *on
029800070803     c                   eval      *in41 = *on
029900070802     c                   leavesr
030000070802     c                   endif
030100070926     c                   if        §ogddao = *blank or §ogddao = *all'0'
030200070926     c                   goto      fine
030300070926     c                   endif
030400070904      * data partenza procedura in organigramma
030500070904     c                   move      §ogddao       data_org
030600070904      * controllo se siamo in simulazione
030700070904     c                   if        *date < data_org
030800070904     c                   eval      test_simula = 'S'
030900070904     c                   else
031000070904     c                   clear                   test_simula
031100070904     c                   endif
031200070803
031300070802     c                   eval      *in05 = *off
031400070802
031500070802      * se imposto località 'DA' e località 'A' a blank la inserisco uguale
031600070802     c                   if        v1clord <> *blank and v1clora = *blank
031700070903     c                   eval      v1clora = %trimr(v1clord) + wzeta
031800070802     c                   endif
031900070802      * se imposto località 'DA' e località 'A' con solo iniziale metto nel
032000070802      * secondo byte la 'Z' x la selezione
032100070802     c                   if        v1clord <> *blank and v1clora <> *blank and
032200070802     c                             %subst(v1clora:2:1) = ' '
032300070903     c                   eval      v1clora = %trimr(v1clora) + wzeta
032400070802     c                   endif
032500070802      * vice versa località 'A' e località 'DA' a blank la inserisco uguale
032600070802     c                   if        v1clora <> *blank and v1clord = *blank
032700070802     c                   eval      v1clord = v1clora
032800070802     c                   endif
032900070802      * verifico che località 'DA' sia congruente con località 'A'
033000070802     c                   if        v1clord > v1clora
033100070803     c                   eval      v1cmsg = msg(05)
033200070803     c                   eval      *in28 = *on
033300070803     c                   eval      *in43 = *on
033400070802     c                   leavesr
033500070802     c                   endif
033600070802
033700070802      * giro
033800070802     c                   if        v1ccgi <> *blank and v1ccgi <> '9999999999'
033900070802     c                   eval      con_giro = 'S'
034000070802      * no giro
034100070802     c                   endif
034200070802      * senza giro
034300070802     c                   if        v1ccgi = *blanks
034400070802     c                   eval      con_giro = 'N'
034500070802     c                   endif
034600070802      * tutti
034700070802     c                   if        v1ccgi = *all'9'
034800070802     c                   clear                   con_giro
034900070802     c                   endif
035000080208
035100080208      * data ritiro obbligatoria
035200080208     c                   if        v1cdar = *zeros
035300080208     c                   eval      *in28 = *on
035400080208     c                   eval      *in44 = *on
035500080208     c                   eval      v1cmsg = msg(08)
035600080208     c                   leavesr
035700080208     c                   endif
035800080208
035900080208      * data ritiro valida
036000080208     c                   if        v1cdar > *zeros
036100080208     c                   clear                   wlbdat
036200080208     c                   z-add     v1cdar        g08dat
036300080208     c                   call      'XSRDA8'
036400080208     c                   parm                    wlbdat
036500080208     c                   if        g08err = '1'
036600080208     c                   eval      *in28 = *on
036700080208     c                   eval      *in44 = *on
036800080208     c                   eval      v1cmsg = msg(07)
036900080208     c                   leavesr
037000080208     c                   endif
037100080208     c                   z-add     g08dat        v1cdar
037200080208     c                   z-add     g08inv        wdar
037300080208     c                   endif
037400070802
037500070802     c                   endsr
037600070803
037700070803      *------------------------------------------------------------------------*
037800070803      *   Preparo la stringa SQL
037900070803      *------------------------------------------------------------------------*
038000070803     c     sr_carica     begsr
038100070803
038200070803     c                   clear                   stringasql
038300070803
038400070803      * preparo la stringa sql
038500070803     c                   eval      stringasql =
038600071029     c                             'select fnorg52j.* from '                  +
038700071029     c                             'fnorg52j '                                +
038800071029     c                             'where orgpor =  '                         +
038900071029     c                             %editc(v1cfgs:'X')                         +
039000071029     c                             ' and orgnsr = 0'
039100071029      * se simulazione solo orgndc = 0
039200071029     c                   if        test_simula = 'S'
039300071029     c                   eval      stringasql =
039400071029     c                             %trim(stringasql) + ' '                     +
039500071029     c                             'and orgndc = 0'
039600071029     c                   endif
039700071029      * se filiale già attiva solo ormndc = 0
039800071029     c                   if        test_simula <> 'S'
039900071029     c                   eval      stringasql =
040000071029     c                             %trim(stringasql) + ' '                     +
040100071029     c                             'and ormndc = 0'
040200071029     c                   endif
040300071029
040400070910      * parzializzazione per localita richiesto range a video
040500070910     c                   if        v1clord <> *blanks and v1clora <> *blanks
040600070910     c                   eval      stringasql =
040700070910     c                             %trim(stringasql) + ' '                     +
040800070910     c                             'and ormlor between '                       +
040900070910     c                             '''' + v1clord + ''''                       +
041000070910     c                             'and '                                      +
041100070910     c                             '''' + v1clora + ''''
041200070910     c                   endif
041300070910      * per nome mittente
041400070910     c                   if        v1crsr <> *blanks
041500070910     c                   eval      stringasql =
041600070910     c                             %trim(stringasql) + ' '                     +
041700070910     c                             'and ormrsr like ' + '''%'                  +
041800070910     c                             v1crsr
041900070910     c                   eval      stringasql =
042000070910     c                             %trim(stringasql) + '%'''
042100070910     c                   endif
042200070803      * se presente il giro seleziono solo quel giro
042300070803     c                   if        con_giro = 'S'
042400070803     c                   eval      stringasql =
042500070803     c                             %trim(stringasql) + ' '                     +
042600070803     c                             'and orgpocgi = '                           +
042700070803     c                              %editc(v1cfgs:'X') + ' '                   +
042800070803     c                             'and orgcgi = ' + '''' + v1ccgi + ''''
042900070803     c                   endif
043000070803      * solo gli orm senza giro
043100070803     c                   if        con_giro = 'N'
043200070803     c                   eval      stringasql =
043300070803     c                             %trim(stringasql) + ' '                     +
043400070803     c                             'and orgpocgi = 0 '                         +
043500070803     c                             'and orgcgi = ' + '''' + v1ccgi  + ''''
043600070803     c                   endif
043700070803
043800070803     C/EXEC SQL
043900070803     C+ PREPARE S1 FROM :stringasql
044000070803     C/END-EXEC
044100070803
044200070803     C/EXEC SQL
044300070803     C+ DECLARE A1 CURSOR FOR S1
044400070803     C/END-EXEC
044500070803
044600070803     C/EXEC SQL
044700070803     C+ OPEN A1
044800070803     C/END-EXEC
044900070803
045000070803     c                   endsr
045100070803
045200070803      *------------------------------------------------------------------------*
045300070803      *   Pulisco il subfile
045400070803      *------------------------------------------------------------------------*
045500070803     c     sr_pulsfl     begsr
045600070803
045700070803     c                   clear                   nrr
045800070803
045900070803     c                   eval      *in20 = *off
046000070803     c                   eval      *in21 = *off
046100070803      *                  __________________
046200070803     c                   write     or73c01
046300070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
046400070803     c                   eval      *in21 = *on
046500070803     c                   eval      *in20 = *on
046600070803
046700070803     c                   eval      recsf = 1
046800070803     c                   eval      *in31 = *off
046900070803     c                   eval      *in40 = *off
047000070803
047100070803     c                   endsr
047200070803
047300070803      *------------------------------------------------------------------------*
047400070803      *   Carico il subfile
047500070803      *------------------------------------------------------------------------*
047600070803     c     sr_carsfl     begsr
047700070803
047800070803     c                   do        *hival
047900070803
048000070803     C/EXEC SQL
048100071029     C+ FETCH NEXT FROM A1 INTO :fnorgds, :fnormds
048200070803     C/END-EXEC
048300070803
048400070803      * errore nel sql esco dal pgm
048500070803     c                   if        sqlcod < 0
048600070803     c                   eval      *inlr = *on
048700070803     c                   return
048800070803     c                   endif
048900070803      * fine file
049000070803     c                   if        sqlcod = 100
049100070803     c                   leave
049200070803     c                   endif
049300071029
049400070904      * controllo se l'ORM è assegnabile
049500070904     c                   exsr      check_orm
049600070904     c                   if        or64asso = 'N'
049700070904     c                   iter
049800070904     c                   endif
049900070803      * se arrivo qua il rcd è ok e devo caricare i dati
050000070803     c                   exsr      sr_wrtsfl
050100070803      * esco per raggiunta massima capacità sfl
050200070803     c                   if        nrr > 9990
050300070803     c                   leave
050400070803     c                   endif
050500070803
050600070803     c                   enddo
050700070803
050800070803     C/EXEC SQL
050900070803     C+ CLOSE A1
051000070803     C/END-EXEC
051100070803
051200070803     c                   eval      wnrr = nrr
051300070803     c                   eval      rrnlast = nrr
051400070803     c                   eval      *in31 = *on
051500070803
051600070803     c                   endsr
051700070803
051800070803      *------------------------------------------------------------------------*
051900070803      *   Scrivo la riga del subfile
052000070803      *------------------------------------------------------------------------*
052100070803     c     sr_wrtsfl     begsr
052200070803
052300070803      * imposto i campi
052400070803     c                   clear                   vs1sce
052500070803      * codice giro
052600070803     c                   eval      vs1cgi = orgcgi
052700070803      * chiave ORM
052800070803     c                   eval      vs1poe = ormpoe
052900070803     c                   eval      vs1nsr = ormnsr
053000070803     c                   eval      vs1nor = ormnor
053100070803     c                   eval      vs1nrv = ormnrv
053200081104      * tipo comunicazione ORM
053300081104     c                   eval      vs1tco = ormtco
053400070803      * mittente
053500070803     c                   eval      vs1rsr = ormrsr
053600070803     c                   eval      vs1inr = orminr
053700070803     c                   eval      vs1lor = ormlor
053800070803     c                   eval      vs1prr = ormprr
053900070803      * peso e volume da org
054000070803     c                   eval      vs1pkg = orgpkg
054100070803     c                   eval      vs1vlm = orgvlm
054200070803      * colli
054300071019     c                   z-add     ormncl        vs1ncl
054400071019      * bancali
054500071019     c                   z-add     ormbnc        vs1bnc
054600070803
054700070803      * scrivo il subfile
054800070803     c                   add       1             nrr
054900070803      * se primo rcd che scrivo accendo indicatore di PC
055000070803     c                   if        nrr = 1
055100070803     c                   eval      *in40 = *on
055200070803     c                   else
055300070803     c                   eval      *in40 = *off
055400070803     c                   endif
055500070803      *                  __________________
055600070803     c                   write     or73s01
055700070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
055800070803
055900070803     c                   endsr
056000070803
056100070803      *------------------------------------------------------------------------*
056200070803      *   Gestione del subfile
056300070803      *------------------------------------------------------------------------*
056400070803     c     sr_gessfl     begsr
056500070803
056600070806     c                   eval      $aggrcd = *off
056700070803     c                   eval      $fatto = *off
056800070803     c                   eval      $multi = *off
056900070803
057000070803     c                   do        *hival
057100070803      * emetto il piede del subfile
057200070803      *                  __________________
057300070803     c                   write     or73z01
057400070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
057500070803      * se non ho caricato niente emetto videata vuota
057600070803     c                   if        wnrr = *zeros
057700070803      *                  __________________
057800070803     c                   write     or73v01
057900070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
058000070803     c                   eval      *in20 = *off
058100070803     c                   else
058200070803     c                   eval      *in20 = *on
058300070803     c                   if        recsf > wnrr
058400070803     c                   eval      recsf = 1
058500070803     c                   endif
058600070803     c                   endif
058700070803      * emissione sfl
058800070803      *                  __________________
058900070803     c                   exfmt     or73c01
059000070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
059100070803     c                   eval      *in28 = *off
059200070803     c                   clear                   v1cmsg
059300070803
059400070803      * f3=fine
059500070803     c                   if        *inkc
059600070803     c                   goto      fine
059700070803     c                   endif
059800070803      * f12=ritorno
059900070803     c                   if        *inkl
060000070803     c                   goto      emid01
060100070803     c                   endif
060200070803      * f1=seleziona tutti
060300070803     c                   if        *inka
060400070803     c                   exsr      sr_f01
060500070803     c                   iter
060600070803     c                   endif
060700070803
060800070803      * controllo le selezioni fatte
060900070803     c                   if        wnrr > *zeros
061000070803     c                   exsr      sr_contrsfl
061100070803     c                   if        *in28
061200070803     c                   iter
061300070803     c                   endif
061400070806     c                   if        $aggrcd = *on
061500070806     c                   leave
061600070806     c                   endif
061700070803     c                   endif
061800070803
061900070803     c                   enddo
062000070803
062100070803     c                   endsr
062200070803
062300070803      *------------------------------------------------------------------------*
062400070803      *   Controllo le selezioni del subgile
062500070803      *------------------------------------------------------------------------*
062600070803     c     sr_contrsfl   begsr
062700070803
062800070803     c                   clear                   nrr
062900070803     c                   do        *hival
063000070803     c                   add       1             nrr
063100070803     c     nrr           chain     or73s01                            30
063200070803      * esco se fine sfl
063300070803     c                   if        *in30
063400070803     c                   leave
063500070803     c                   endif
063600070803
063700070803     c                   eval      *in10 = *off
063800070803
063900070803      * controllo le selezioni a video
064000070803     c                   select
064100071026
064200071026      * opzione 2 --> modifca ORM
064300071026     c                   when      vs1sce = '2'
064400081104      * se ORM Import DPD automatico non posso manutenzionare
064500081104      * solo EDP
064600081104if  2c                   if        %subst(knmus:1:3) <> 'EDP' and
064700081104     c                             %lookup(vs1poe:skpodpd) > *zeros and
064800081104     c                             vs1tco = 'F'
064900081104     c                   eval      *in28 = *on
065000081104     c                   eval      v1cmsg = msg(09)
065100081104e   2c                   else
065200071026     c                   exsr      sr_modorm
065300081104     c                   endif
065400071026     c                   eval      recsf = nrr
065500071026     c                   clear                   vs1sce
065600071026     c                   eval      *in40 = *on
065700070803
065800070803      * opzione 5 --> visualizza ORM
065900070803     c                   when      vs1sce = '5'
066000070803     c                   exsr      sr_intorm
066100070803     c                   eval      recsf = nrr
066200070803     c                   clear                   vs1sce
066300070803     c                   eval      *in40 = *on
066400070803
066500070803      * opzione 1 --> attribuzione giro per singolo ORM
066600070803     c                   when      vs1sce = '1'
066700070803     c                   exsr      sr_singolo
066800070803     c                   eval      recsf = nrr
066900070803     c                   clear                   vs1sce
067000070803     c                   eval      *in40 = *on
067100070803
067200070803      * opzione 9 --> attribuzione giro per più ORM
067300070803     c                   when      vs1sce = '9'
067400070803     c                   eval      *in10 = *on
067500070803     c                   exsr      sr_multiplo
067600070803     c                   clear                   vs1sce
067700070803     c                   eval      *in40 = *on
067800070803
067900070803     c                   other
068000070803      * spengo l'indicatore di PC
068100070803     c                   eval      *in40 = *off
068200070803     c                   endsl
068300070803      *                  __________________
068400070803     c                   update    or73s01
068500070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
068600070803     c                   enddo
068700070803
068800070803     c                   endsr
068900071026
069000071026      *------------------------------------------------------------------------*
069100071026      *   Modifico l'ORM
069200071026      *------------------------------------------------------------------------*
069300071026     c     sr_modorm     begsr
069400071026
069500071026     c                   clear                   dsfior07
069600071026     c                   clear                   fior05ds
069700071026     c                   eval      dsopoe = vs1poe
069800071026     c                   eval      dsonor = vs1nor
069900071026     c                   eval      dsonsr = vs1nsr
070000071026     c                   eval      dsonrv = vs1nrv
070100071026     c                   eval      dsoopz = vs1sce
070200071026     c                   eval      dsofgs = v1cfgs
070300071026     c                   eval      kpjbu = dsfior07
070400071026     c                   clear                   fior05ds
070500071026     c                   call      'FIOR05R'
070600071026     c                   parm                    kpjba
070700071026     c                   parm                    fior05ds
070800071026     c                   eval      $aggrcd = *on
070900071026
071000071026     c                   endsr
071100070803
071200070803      *------------------------------------------------------------------------*
071300070803      *   Interrogo l'ORM
071400070803      *------------------------------------------------------------------------*
071500070803     c     sr_intorm     begsr
071600070803
071700070803     c                   clear                   dsfior07
071800070803     c                   eval      dsopoe = vs1poe
071900070803     c                   eval      dsonor = vs1nor
072000070803     c                   eval      dsonsr = vs1nsr
072100070803     c                   eval      dsonrv = vs1nrv
072200070803     c                   eval      dsoopz = vs1sce
072300070803     c                   eval      dsofgs = v1cfgs
072400070803     c                   eval      kpjbu = dsfior07
072500070803     c                   call      'FIOR07R'
072600070803     c                   parm                    kpjba
072700070803
072800070803     c                   endsr
072900070803
073000070803      *------------------------------------------------------------------------*
073100070803      *   Seleziona per attribuzione singolo ORM
073200070803      *------------------------------------------------------------------------*
073300070803     c     sr_singolo    begsr
073400070803
073500070803     c     korm          chain     fnorm01l
073600070803     c                   if        not %found(fnorm01l)
073700070803     c                   leavesr
073800070803     c                   endif
073900070803     c     korm          chain(n)  fnorg01l
074000070803     c                   if        not %found(fnorg01l)
074100070803     c                   leavesr
074200070803     c                   endif
074300070803      * imposto i campi per il video di dettaglio dati ORM
074400070803     c                   exsr      sr_datiorm
074500070803      * videata di dettaglio ORM
074600070803     c                   exsr      sr_visriga
074700070803
074800070803     c                   endsr
074900070803
075000070803      *------------------------------------------------------------------------*
075100070803      *   Seleziona per attribuzione più ORM
075200070803      *------------------------------------------------------------------------*
075300070803     c     sr_multiplo   begsr
075400070803
075500070803     c     korm          chain     fnorm01l
075600070803     c                   if        not %found(fnorm01l)
075700070803     c                   leavesr
075800070803     c                   endif
075900070803     c     korm          chain(n)  fnorg01l
076000070803     c                   if        not %found(fnorg01l)
076100070803     c                   leavesr
076200070803     c                   endif
076300070803      * imposto i campi per il video di dettaglio dati ORM
076400070803     c                   if        $multi = *off
076500070803     c                   exsr      sr_datiorm
076600070803      * videata di dettaglio ORM
076700070803     c                   exsr      sr_visriga
076800070803      * se "$fatto" ON ho aggiornato la prima riga multipla
076900070803      * quindi memorizzo i valori da aggiornare x eventuali righe con stessa
077000070803      * opzione e proseguo l'aggiornamento  senza più emettere il video per
077100070803      * queste ultime se le opzioni fossero mischiate le altre differenti
077200070803      * dalla multipla verranno elaborate normalmente nell'ambito dello stesso
077300070803      * ciclo di elaborazione. Le multiple verranno elaborate in maniera
077400070803      * occulta con il valore impostato inizialmente
077500070803     c                   if        $fatto = *on
077600070803     c                   eval      sav_v2ccgin = v2ccgin
077700070803     c                   eval      $multi = *on
077800070803     c                   endif
077900070803      * imposta il giro sulle ulteriori righe della scelta multipla
078000070803     c                   else
078100070803     c                   eval      v2ccgin = sav_v2ccgin
078200070803     c                   exsr      sr_aggiorna
078300070803     c                   endif
078400070803
078500070803     c                   endsr
078600070803
078700070803      *------------------------------------------------------------------------*
078800070803      *   Imposto i dati dell'ORM per la videata a dettaglio
078900070803      *------------------------------------------------------------------------*
079000070803     c     sr_datiorm    begsr
079100071019
079200071019     c                   clear                   v2dcgi
079300071019     c                   clear                   v2dcgin
079400070803
079500070803      * chiave ORM
079600070803     c                   eval      v2cpoe = ormpoe
079700070803     c                   eval      v2cnsr = ormnsr
079800070803     c                   eval      v2cnor = ormnor
079900070803     c                   eval      v2cnrv = ormnrv
080000070803      * ordinante
080100070803     c                   eval      v2ccor = ormcor
080200070803     c                   eval      v2crso = ormrso
080300070803      * tipo orm
080400070803     c                   clear                   v2dtor
080500070803     c                   clear                   tibs02ds
080600070803     c                   eval      t02mod = 'C'
080700070803     c                   eval      t02sif = knsif
080800070803     c                   eval      t02cod = 'TOR'
080900070803     c                   eval      t02ke1 = ormtor
081000070803     c                   call      'TIBS02R'
081100070803     c                   parm                    kpjba
081200070803     c                   parm                    tibs02ds
081300070803     c                   eval      v2dtor = t02uni
081400070803      * fase
081500070803     c                   eval      v2cfao =  ormfao
081600070803     c                   clear                   v2dfao
081700070803     c                   clear                   tibs02ds
081800070803     c                   eval      t02mod = 'C'
081900070803     c                   eval      t02sif = knsif
082000070806     c                   eval      t02cod = 'FAR'
082100070803     c                   movel(p)  ormfao        t02ke1
082200070803     c                   call      'TIBS02R'
082300070803     c                   parm                    kpjba
082400070803     c                   parm                    tibs02ds
082500070803     c                   eval      dfar = t02uni
082600070803     c                   eval      v2dfao = d§fardes
082700070803      * mittente
082800070803     c                   eval      v2ccra = ormcra
082900070803     c                   eval      v2crsr = ormrsr
083000070803      * indirizzo
083100070803     c                   eval      v2cinr = orminr
083200070803     c                   eval      v2clor = ormlor
083300070803     c                   eval      v2ccar = ormcar
083400070803     c                   eval      v2cprr = ormprr
083500070904     c                   eval      v2cnar = ormnar
083600070803      * note
083700070803     c                   eval      v2cno1 = ormno1
083800070803     c                   eval      v2cno2 = ormno2
083900070803      * colli/bancali
084000070803     c                   eval      v2cncl = ormncl
084100070803     c                   eval      v2cbnc = ormbnc
084200070803      * peso/volume da ORG
084300070803     c                   eval      v2cpkg = orgpkg
084400070803     c                   eval      v2cvlm = orgvlm
084500070803      * automezzo
084600070803     c                   select
084700070803     c                   when      ormblc <> *zeros
084800070803     c                   eval      v2caut = 'BIL'
084900070803     c                   when      ormatt <> *zeros
085000070803     c                   eval      v2caut = 'AUT'
085100070803     c                   when      ormmtc <> *zeros
085200070803     c                   eval      v2caut = 'MOT'
085300070803     c                   other
085400070803     c                   clear                   v2caut
085500070803     c                   endsl
085600070803      * sponda idraulica
085700070803     c                   if        ormspi = 'S'
085800070803     c                   eval      v2cspi = 'SI'
085900070803     c                   else
086000070803     c                   clear                   v2cspi
086100070803     c                   endif
086200070803      * data ritiro
086300070803     c     *iso          move      ormdar        dataiso
086400070803     c                   move      dataiso       dataeur
086500070803     c                   move      dataeur       v2cdar
086600070803      * ora
086700070803     c                   eval      v2corr = ormorr
086800070803      * priorità
086900070803     c                   z-add     ormsto        v2csto
087000070803      * natura merce
087100070803     c                   eval      v2cnam = ormnam
087200070803      * referente
087300070803     c                   eval      v2crer = ormrer
087400070803      * telefono
087500070803     c                   eval      v2cter = ormter
087600070803      * riferimento
087700070803     c                   eval      v2crfa = ormrfa
087800070803      * giro
087900070803     c                   eval      v2ccgi = orgcgi
088000071019     c                   if        v2ccgi <> *blanks
088100071019     c                   eval      wcgi = v2ccgi
088200071019     c                   exsr      ctrgiro
088300071019     c                   eval      v2dcgi = d09odes
088400071019     c                   endif
088500070803      * pulisco i dati di i/o
088600070803     c                   clear                   v2ccgin
088700070803     c                   clear                   v2dcgin
088800070803     c                   clear                   v2cposin
088900070803
089000070803     c                   endsr
089100070803
089200070803      *------------------------------------------------------------------------*
089300070803      *   Visualizzo la riga
089400070803      *------------------------------------------------------------------------*
089500070803     c     sr_visriga    begsr
089600070803
089700070803     c                   do        *hival
089800070803      *                  __________________
089900070803     c                   exfmt     or73d02
090000070803      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
090100070803     c                   eval      *in28 = *off
090200070806     c                   clear                   v2cmsg
090300070803      * f3=fine
090400070803     c                   if        *inkc
090500070803     c                   goto      fine
090600070803     c                   endif
090700070803      * f12=ritorno
090800070803     c                   if        *inkl
090900070803     c                   leave
091000070803     c                   endif
091100070803      * f7=interrogazione giri
091200070803     c                   if        *inkg
091300070803     c                   exsr      sr_f07
091400070803     c                   eval      v2ccgin = d09ocgi
091500070803     c                   iter
091600070803     c                   endif
091700070803      * f8=interrogazione autisti
091800070803     c                   if        *inkh
091900070803     c                   exsr      sr_f08
092000070803     c                   eval      v2ccgin = d08cgio
092100070803     c                   iter
092200070803     c                   endif
092300070803      * controllo
092400070803     c                   exsr      sr_ctrd02
092500070803     c                   if        *in28
092600070803     c                   iter
092700070803     c                   endif
092800070806      * f6=aggiorna
092900070803     c                   if        *inkf
093000070803     c                   exsr      sr_aggiorna
093100070803     c                   if        $fatto = *on
093200070803     c                   leave
093300070803     c                   endif
093400070803     c                   endif
093500070803
093600070803     c                   enddo
093700070803
093800070803     c                   endsr
093900070803
094000070803      *------------------------------------------------------------------------*
094100070803      *   Controlli video --> dettaglio ORM
094200070803      *------------------------------------------------------------------------*
094300070803     c     sr_ctrd02     begsr
094400070803
094500070803     c                   eval      *in28 = *off
094600070803      * controllo giro
094700070803     c                   if        v2ccgin <> *blanks
094800071019     c                   eval      wcgi = v2ccgin
094900070803     c                   exsr      ctrgiro
095000070803      * giro errato
095100070903     c                   if        d09oerr = '1' or d09oatb <> *blanks
095200070803     c                   eval      *in28 = *on
095300070806     c                   eval      v2cmsg = msg(04)
095400070803     c                   leavesr
095500070803     c                   endif
095600070803      * controllo filiale giro con filiale ritiro
095700070806     c                   if        d09ofgs <> v1cfgs
095800070806     c                   eval      *in28 = *on
095900070806     c                   eval      v2cmsg = msg(06)
096000070806     c                   leavesr
096100070806     c                   endif
096200071019     c                   eval      v2dcgin = d09odes
096300070806
096400070803     c                   endif
096500070803
096600070803     c                   endsr
096700070803
096800070803      *------------------------------------------------------------------------*
096900070803      *   Aggiorno i dati su FNORG x il giro
097000070803      *------------------------------------------------------------------------*
097100070803     c     sr_aggiorna   begsr
097200070803
097300070925     c                   if        v2ccgin <> *blanks
097400070803     c     korm          chain     fnorg01l
097500070803     c                   if        %found(fnorg01l)
097600070803     c                   eval      orgpocgi = v1cfgs
097700070803     c                   eval      orgcgi = v2ccgin
097800070803     c                   eval      orgtgi = 'M'
097900070806     c                   exsr      sr_dataora
098000070903     c                   eval      orgdtvtel = wdata
098100070903     c                   eval      orghvtel = wora
098200070803     c                   update    fnorg000
098300070803     c                   endif
098400070925     c                   endif
098500070803
098600070803     c                   eval      $fatto = *on
098700070806     c                   eval      $aggrcd = *on
098800070803
098900070803     c                   endsr
099000070904
099100070904      *------------------------------------------------------------------------*
099200070904      *  Controlla validità dell'ORM
099300070904      *------------------------------------------------------------------------*
099400070904     c     check_orm     begsr
099500070904
099600070904      * per controllare se ORM assegnabile richiamo pgm esterno
099700070904     c                   clear                   fior64ds
099800070904     c                   eval      or64poei = ormpoe
099900070904     c                   eval      or64nsri = ormnsr
100000070904     c                   eval      or64nori = ormnor
100100070904     c                   eval      or64nrvi = ormnrv
100200070904      * imposto la data
100300080208     c                   eval      or64dfvi = wdar
100400070904      * se sono in simulazione controllo la distinta su ORG
100500070904      * e imposto il flag di controllo ORMETI in simulazione
100600070904     c                   if        test_simula = 'S'
100700070904     c                   eval      or64ndci = 'G'
100800070904     c                   eval      or64etii = 'S'
100900070904     c                   else
101000070904      * se già attivo controllo la distinta su entrambi
101100070904      * e imposto il flag di controllo ORMETI solo per filiale attiva
101200070904     c                   eval      or64ndci = 'E'
101300070904     c                   clear                   or64etii
101400070904     c                   endif
101500070904      * controlla la data di ritiro
101600070904     c                   eval      or64dari = 'S'
101700070904     c                   call      'FIOR64R'
101800070904     c                   parm                    kpjba
101900070904     c                   parm                    fior64ds
102000070904
102100070904     c                   endsr
102200070802
102300070802      *------------------------------------------------------------------------*
102400070802      *   Controllo e decodifica giro
102500070802      *------------------------------------------------------------------------*
102600070802     c     ctrgiro       begsr
102700070802
102800070802     c                   clear                   fidg09ds
102900070802     c                   eval      d09iop0 = '001'
103000070803     c                   eval      d09ifgs = v1cfgs
103100070806     c                   exsr      sr_dataora
103200070806     c                   eval      d09idat = wdata
103300071019     c                   eval      d09icgi = wcgi
103400070802     c                   eval      d09itug = 'R'
103500070802     c                   eval      kpjbu = fidg09ds
103600070802     c                   call      'FIDG09R'
103700070802     c                   parm                    kpjba
103800070802     c                   eval      fidg09ds = kpjbu
103900070802
104000070802     c                   endsr
104100070803
104200070802      *------------------------------------------------------------------------*
104300070802      *   Seleziona tutti i record del sfl                         ?
104400070802      *------------------------------------------------------------------------*
104500070803     c     sr_f01        begsr
104600070802
104700070803     c                   if        wnrr > *zeros
104800070803     c                   clear                   nrr
104900070803     c                   do        *hival
105000070803     c                   add       1             nrr
105100070803     c     nrr           chain     or73s01                            30
105200070803      * esco se fine sfl
105300070803     c                   if        *in30
105400070803     c                   leave
105500070803     c                   endif
105600070803     c                   eval      vs1sce = '1'
105700070803     c                   update    or73s01
105800070803     c                   enddo
105900070802     c                   endif
106000070802
106100070802     c                   endsr
106200070802
106300070802      *------------------------------------------------------------------------*
106400070802      *   Gestione F07                                                ?
106500070802      *------------------------------------------------------------------------*
106600070803     c     sr_f07        begsr
106700070802
106800070802     c                   clear                   fidg09ds
106900070802     c                   eval      d09iop0 = 'P01'
107000070802     c                   eval      d09ifgs = v1cfgs
107100070806     c                   exsr      sr_dataora
107200070806     c                   eval      d09idat = wdata
107300070803     c                   if        v2cposin <> *blanks
107400070803     c                   eval      d09ides = v2cposin
107500070803     c                   endif
107600070802     c                   eval      d09itug = 'R'
107700070802     c                   eval      kpjbu = fidg09ds
107800070802     c                   call      'FIDG09R'
107900070802     c                   parm                    kpjba
108000070802     c                   eval      fidg09ds = kpjbu
108100070802
108200070802     c                   endsr
108300070803
108400070803      *------------------------------------------------------------------------*
108500070803      *   Gestione F08                                                ?
108600070803      *------------------------------------------------------------------------*
108700070803     c     sr_f08        begsr
108800070803
108900070803     c                   clear                   fidg08ds
109000070803     c                   eval      d08fgs = v1cfgs
109100070803     c                   eval      d08fgsgo = v1cfgs
109200070803     c                   eval      d08rsc = v2cposin
109300070903     c                   eval      d08tp = 'R'
109400070803     c                   eval      kpjbu = fidg08ds
109500070803     c                   call      'FIDG08R'
109600070803     c                   parm                    kpjba
109700070803     c                   eval      fidg08ds = kpjbu
109800070803
109900070803     c                   endsr
110000070802
110100070802      *------------------------------------------------------------------------*
110200070802      *   Riordina il sfl per località                                 ?
110300070802      *------------------------------------------------------------------------*
110400070803     c     sr_ordsfl     begsr
110500070802
110600070802      * totale records caricati nel SFL
110700070803     c                   eval      nrr = 0
110800070802
110900070802      * inizializza i campi chiave x l'ordinamento. C'è un campo in più non
111000070802      * presente nel subfile -- "Selected"?-- questo è aggiunto al record.
111100070802      * il campo è usato per selezionare i records dando un ordine a quelli
111200070802      * selezionati davanti ai non selezionati.
111300070802     c                   clear                   QLGSCB
111400070802     c                   clear                   QLGSCB00
111500070802
111600070803      * 1 campo chiave.   ordino x località
111700070803     c                   eval      QLGNBRK    = 1
111800070803      * localita a posizione 1
111900070802     c                   eval      QLGSP      = 1
112000070803     c                   eval      QLGSS      = %SIZE(vs1lor)
112100070802     c                   eval      QLGDT      = Carattere
112200070802     c                   eval      QLGSO      = Ascendente
112300070802     c                   eval      QLGKL(1)   = QLGSKL
112400070802
112500070802      * Load other sort parameters.
112600070802     c                   eval      QLGLB     = 80 + 16 * MaxKey
112700070802     c                   eval      QLGRL     = %SIZE(sflrcd) - 1
112800070802     c                   eval      QLGRT     = 8
112900070802     c                   eval      QLGOKL    = 80
113000070802     c                   eval      QLGLKE    = 16
113100070802     c                   eval      QLGLSS    = 290
113200070802
113300070802      * Initialize Sort I/O API fields.
113400070802     c                   eval      QLGRL00  = QLGRL
113500070802     c                   eval      QLGRC00  = 1
113600070802     c                   clear                   QUSEI
113700070802     c                   eval      QUSBPRV = %SIZE(QUSEC)
113800070802
113900070802      * First step - Initialize the sort routine.
114000070802     c                   call      'QLGSORT'
114100070802     c                   parm                    QLGSCB
114200070802     c                   parm                    NotUsed
114300070802     c                   parm                    NotUsed
114400070802     c                   parm                    SizeList
114500070802     c                   parm                    ReturnSize
114600070802     c                   parm                    QUSEC
114700070802
114800070802      * Next step - Write records to I/O routine.
114900070802     c                   eval      QLGRT00 = Put
115000070802
115100070803     c                   do        rrnlast       nrr_x
115200070803     c     nrr_x         chain     or73s01
115300070802
115400070802      * solo le righe con Selected = 'Y' sono riordinate,
115500070802      * quindi per fare un ordinamento di tutte le righe
115600070802      * metto 'Y' sempre.
115700070802     c                   eval      selected  = 'Y'
115800070802
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
116900070802
117000070802     c                   enddo
117100070802
117200070802      * Next step - Signal end of input, clear subfile for reload.
117300070802     c                   eval      QLGRT00 = EndPut
117400070802     c                   clear                   QUSEI
117500070802     c                   eval      QUSBPRV = %SIZE(QUSEC)
117600070802
117700070802     c                   call      'QLGSRTIO'
117800070802     c                   parm                    QLGSCB00
117900070802     c                   parm                    SflRcd
118000070802     c                   parm                    NotUsed
118100070802     c                   parm                    SizeList
118200070802     c                   parm                    NotUsed
118300070802     c                   parm                    QUSEC
118400070803      * pulizia SFL    ?
118500070802     c                   eval      *in20 = *off
118600070802     c                   eval      *in21 = *off
118700070802      *                  __________________
118800070803     c                   write     or73c01
118900070802      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
119000070802     c                   eval      *in21 = *on
119100070802
119200070802      * Final step - Write the records back to the subfile.
119300070802     c                   eval      QLGRT00 = Get
119400070802
119500070803     c                   do        rrnlast       nrr_x
119600070803     c                   eval      nrr = nrr_x
119700070802     c                   clear                   QUSEI
119800070802     c                   eval      QUSBPRV = %SIZE(QUSEC)
119900070802     c                   call      'QLGSRTIO'
120000070802     c                   parm                    QLGSCB00
120100070802     c                   parm                    NotUsed
120200070802     c                   parm                    SflRcd
120300070802     c                   parm                    QLGRL00
120400070802     c                   parm                    NotUsed
120500070802     c                   parm                    QUSEC
120600070803
120700070803     c                   eval      nrr = nrr_x
120800070802      *                  __________________
120900070803     c                   write     or73s01
121000070802      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
121100070802     c                   enddo
121200070802
121300070802      *  All'uscita di queste chiamate a routine di sistema il SFL record
121400070802      *   si incrementa stranamente quindi lo reimposto correttamente
121500070802      *    prima di lasciare la routine.
121600070803     c                   eval      nrr = rrnlast
121700070802
121800070802     c                   endsr
121900070806
122000070806      *------------------------------------------------------------------------*
122100070806      *   Imposto data e ora del momento
122200070806      *------------------------------------------------------------------------*
122300070806     c     sr_dataora    begsr
122400070806
122500070806     c                   time                    w0140
122600070806      * ora
122700070806     c                   movel     w0140         wora
122800070806      * data
122900070806     c                   move      w0140         wdata
123000070806     c     *eur          move      wdata         dataeur
123100070806     c                   move      dataeur       dataiso
123200070806     c                   move      dataiso       wdata
123300070806
123400070806     c                   endsr
123500070802
123600070802      *------------------------------------------------------------------------*
123700070802      *   Operazioni iniziali      ?
123800070802      *------------------------------------------------------------------------*
123900070802     c     *inzsr        begsr
124000070802
124100070802     c     *entry        plist
124200070802     c                   parm                    kpjba
124300070802
124400070802     c     *dtaara       define    §azute        azuteds
124500070802     c     *dtaara       define    §datiute      ddatiute
124600070802     c                   in(e)     *dtaara
124700070802     c                   if        %error or rsut = *blanks
124800070802     c                   clear                   tibs34ds
124900070802     c                   call      'TIBS34R'
125000070802     c                   parm                    tibs34ds
125100070802     c                   in        *dtaara
125200070802     c                   endif
125300070803
125400070803      * filiale gestione da filiale utente
125500070803     c                   eval      v1cfgs = dutpou
125600070803      * abilito f18
125700070803     c                   eval      *in06 = (dutlpo = '2' or dutlpo = *blanks)
125800080208
125900080208      * recupero i dati di default prima con la mia filiale
126000080208     c                   clear                   tibs02ds
126100080208     c                   clear                   ddft
126200080208     c                   movel     'C'           t02mod
126300080208     c                   movel     knsif         t02sif
126400080208     c                   movel     'DFT'         t02cod
126500080208     c                   movel(p)  v1cfgs        t02ke1
126600080208     c                   call      'TIBS02R'
126700080208     c                   parm                    kpjba
126800080208     c                   parm                    tibs02ds
126900080208     c                   if        t02err = *blanks
127000080208     c                   movel     t02uni        ddft
127100080208     c                   else
127200080208      * poi con 999 generico se non trovo con la mia filiale
127300080208     c                   clear                   tibs02ds
127400080208     c                   movel     'C'           t02mod
127500080208     c                   movel     knsif         t02sif
127600080208     c                   movel     'DFT'         t02cod
127700080208     c                   movel(p)  '999'         t02ke1
127800080208     c                   call      'TIBS02R'
127900080208     c                   parm                    kpjba
128000080208     c                   parm                    tibs02ds
128100080208     c                   if        t02err =  *blanks
128200080208     c                   movel     t02uni        ddft
128300080208     c                   endif
128400080208     c                   endif
128500080208
128600080208      * imposto la data ritiro a oggi
128700080208     c     *iso          move      *date         dataiso
128800080208     c                   move      dataiso       dataeur
128900080208     c                   move      dataeur       v1cdar
129000080208
129100080208      * se è già pomeriggio imposto la data di oggi + 1
129200080208     c                   exsr      sr_dataora
129300080208     c                   if        wora > d§dftora
129400080208     c                   do        *hival
129500080208     c                   adddur    1:*d          dataeur
129600080208     c                   move      dataeur       ds_data
129700080208     c                   clear                   ktfp
129800080208     c                   eval      ktfa = v1cfgs
129900080208     c                   eval      kann = ds_anno
130000080208     c                   eval      kmes = ds_mese
130100080208     c     kazcln        chain     azcln01l
130200080208     c                   if        not %found(azcln01l)
130300080208     c                   leave
130400080208     c                   endif
130500080208     c                   if        mat(ds_giorno) =  'F'
130600080208     c                             or pom(ds_giorno) =  'F'
130700080208     c                   iter
130800080208     c                   endif
130900080208     c                   leave
131000080208     c                   enddo
131100080208     c                   movel     ds_data       v1cdar
131200080208     c                   else
131300080208     c                   movel     dataeur       v1cdar
131400080208     c                   endif
131500080208
131600081104      * cairco sk dei p.o. DPD
131700081104     c                   clear                   yy
131800081104     c     *loval        setll     azorg01l
131900081104     c                   do        *hival
132000081104     c                   read      azorg01l
132100081104     c                   if        %eof(azorg01l)
132200081104     c                   leave
132300081104     c                   endif
132400081104     c                   if        orgfva <> *blanks  or
132500081104     c                             (orgfag <> 'F' and orgfag <> 'A')
132600081104     c                   iter
132700081104     c                   endif
132800081104     c                   eval      og143 = orgde3
132900081104     c                   if        §ogntw = 'DPD'
133000081104     c                   add       1             yy
133100081104     c                   eval      skpodpd(yy) = orgfil
133200081104     c                   endif
133300081104     c                   enddo
133400070803
133500070803     c     korm          klist
133600070803     c                   kfld                    vs1poe
133700070803     c                   kfld                    vs1nsr
133800070803     c                   kfld                    vs1nor
133900070803     c                   kfld                    vs1nrv
134000080208
134100080208     c     kazcln        klist
134200080208     c                   kfld                    ktfp
134300080208     c                   kfld                    ktfa
134400080208     c                   kfld                    kann
134500080208     c                   kfld                    kmes
134600070802
134700070802     c                   endsr
134800070802
134900070802      *------------------------------------------------------------------------*
135000070802** msg
135100070802Filiale errata                                                                 01
135200070802La Filiale non è in gestione all'utente                                        02
135300070803La Filiale non ha la procedura GIRI ORM attiva                                 03
135400070803Codice Giro non corretto                                                       04
135500070803Parzializzazione località errata inserire correttamente                        05
135600070806Filiale giro incongruente con filiale ritiro                                   06
135700080208Data ritiro errata                                                             07
135800080208Immettere la data ritiro                                                       08
135900081104Non è possibile manutenzionare ORM import DPD automatico                       09
