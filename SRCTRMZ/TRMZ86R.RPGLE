000100100204     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300090604     Ftrmz86d   CF   E             WORKSTN
000400070605     Faitra04l  if   e           k disk
000500050623     Fazorg01l  if   e           k disk    rename(azorg:azorg1)
000600090604     Ftrmz860f  uf a e           k disk    usropn
000700090604     Ftrmz861f  uf a e           k disk    usropn
000800050801     ftabel00f  if   e           k disk
000900090618     ftntbe01l  if   e           k disk
001000020304     dkpjba          e ds
001100030630     d Tibs36Ds      e ds
001200090618     d datu          e ds
001300090618     d giorni          s              5  0
001400050623     d primo           s              1
001500050623     d excar           s                   like(orgcar)
001600070608     d coman           s             99
001700050802     d luncmd          s             15  5
001800090604     d limprova        s               d   datfmt(*iso)
001900090604     d dacciso         s               d   datfmt(*iso)
002000090604     d ddisiso         s               d   datfmt(*iso)
002100070605     d dataiso         s               d   datfmt(*iso)
002200070605     d dataeur         s               d   datfmt(*eur)
002300130129     D mot             S              1    DIM(9)
002400070605     D WLBDA8          DS
002500070605     D  G02DAT                 1      8  0
002600070605     D  G02INV                 9     16  0
002700070605     D  G02ERR                17     17
002800070605     D  G02TGI                18     22  0
002900080502      *
003000070608     d file            s             10
003100070608     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
003200070608     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
003300070608     D cmd3            S              1    DIM(99) CTDATA PERRCD(99)
003400070608     D cmd4            S              1    DIM(99) CTDATA PERRCD(99)
003500070608     D cmd5            S              1    DIM(99) CTDATA PERRCD(99)
003600070608     D cmd6            S              1    DIM(99) CTDATA PERRCD(99)
003700090604     D cmd7            S              1    DIM(99) CTDATA PERRCD(99)
003800000000     C*---------------------------------------------------------------*
003900020304      *
004000031105     c                   do        *hival
004100031105     c                   exfmt     video1
004200031105     c                   clear                   $msg
004300031105     c   kc              leave
004400031105     c                   exsr      controv1
004500031105     c   96              iter
004600050623     c                   if        *inkf
004700050802      *clear file
004800091124     c                   move      'X'           lanciato          1
004900090604     c                   close     trmz861f
005000050802     c                   movea     cmd1          coman
005100050802     c                   exsr      comandi
005200090604     c                   open      trmz861f
005300050802
005400090604     c                   close     trmz860f
005500090604     c                   movea     cmd7          coman
005600090604     c                   exsr      comandi
005700090604     c                   open      trmz860f
005800090604
005900050623     c                   exsr      elabora
006000050805     c                   leave
006100050623     c                   endif
006200050805     c                   enddo
006300090604      * elaboro x generazione riepilogo
006400091124     c                   if        lanciato <> *blank
006500090604     c                   exsr      riepilogo
006600091124     c                   endif
006700090604      * chiudo file prima di comandi finali
006800090604     c                   close     trmz860f
006900090604     c                   close     trmz861f
007000070608      *verifica se esiste file in libreria di wrk per utente
007100070608     c                   movea     cmd4          coman
007200090604     c                   exsr      comandi
007300070608      *se trova il file esegue copy file altrimenti lo crea
007400070608     c                   if        *in34
007500070608     c                   movea     cmd5          coman
007600090604     c                   exsr      comandi
007700070608     c                   else
007800070608     c                   movea     cmd6          coman
007900090604     c                   exsr      comandi
008000070608     c                   endif
008100050802      *elimina file in qtemp x la filiale
008200051017     c                   movea     cmd3          coman
008300051017     c                   exsr      comandi
008400050802
008500020308     c                   seton                                        lr
008600990113     c*-------------------------------------------------
008700090604     c     elabora       begsr
008800990113     c*-------------------------------------------------
008900090604     c     *loval        setll     aitra04l
009000981029     c                   do        *hival
009100090604     c                   read      aitra04l
009200090604     c                   if        %eof(aitra04l)
009300090604     c                   leave
009400090604     c                   endif
009500130129     c                   setoff                                       99
009600120918      *ATTENZIONE
009700130129      *QUESTO CONTROLLO È INSERITO PER VERIFICARE SE IL DISACCREDITAMENTO DELL'AUTISTA
009800130129      *È AVVENUTO PER MOTIVI TECNICI TIPO RIFACIMENTO CONTRATTI DI MASSA OPPURE CAMBIO
009900130129      *DI SOCIETÀ OPERATIVE IN QUESTO CASO CI SI SALVA LA DATA DI ACCREDITAMENTO IN MODO
010000130129      *DA CONSIDERARE IL RECORD SCADUTO COME INGRESSO DELL'AUTISTA PER POTER CALCOLARE
010100130129      * CORRETTAMENTE IL PERIODO DI PROVA DI 90 GIORNI
010200130129      * PER EFFETTUARE QUESTO RAGIONAMENTO SI UTILIZZA IL CAMPO DEL TURNOVER VERIFICANDO CHE
010300130129      * IL MOTIVO INSERITO SIA UN MOTIVO TECNICO DI DISACCREDITAMENTO
010400130129     c                   if        tradfi > 0
010500130129     c                   if        tratur <> *blank
010600130129     c                   z-add     1             tec
010700130129     c     tratur        lookup    mot(tec)                               99
010800130129     c                   endif
010900130129     c                   if        *in99
011000120511     c                   movel     traaut        extraaut         25
011100130129     c                   movel     trataa        extrataa         10
011200120511     c                   movel     tradin        extradin          8 0
011300040212     c                   iter
011400130129     c                   endif
011500040212     c                   endif
011600130129      * esclude annullati e corrispondenti e scorta
011700130129      * se disaccreditato l'anno precedente
011800130129     c                   if        tradfi > 0 and tradfi < inianno  or
011900130131     c                             traann <> *blank
012000130131     c*                            tradin > vdatalg
012100090604     c                   iter
012200090604     c                   endif
012300090604      * reperisce area
012400090604     c     trafil        chain     azorg01l
012500130129      * verifica se prima di questo rekord ne è stato letto uno con un disaccreditamento tecnico
012600130129      * nel caso imposta come data accreditamento la data precedente
012700130129     c                   if        traaut = extraaut and
012800130129     c                             trataa = extrataa and
012900130129     c                             extradin > 0
013000120511     c                   z-add     extradin      tradin
013100120423     c                   endif
013200050801     c                   move      orgcar        traarea
013300130129      * scrive W.F
013400090604     c                   write     trmz8610
013500050623     c                   enddo
013600020304
013700020304     c                   endsr
013800981029      *
013900050802     c*-------------------------------------------------
014000090604     c     riepilogo     begsr
014100050802     c*-------------------------------------------------
014200090604     c     *loval        setll     trmz861f
014300070530     c                   do        *hival
014400090604     c                   read      trmz861f                               98
014500070530     c   98              leave
014600100304      *
014700070530     c                   if        primo = *blank
014800070530     c                   move      traarea       excar
014900070530     c                   move      'X'           primo
015000070530     c                   endif
015100090604      * a rottura scrive rekord
015200090604     c                   if        traarea <> excar
015300090604      * reperisce descrizione area
015400090604     c                   clear                   t86aread
015500090604     c                   movel     excar         kkey
015600090604     c                   move      '05'          kcod
015700090604     c     ktab          chain     tabel00f
015800090604     c                   if        %found(tabel00f)
015900090604     c                   movel     tbluni        t86aread
016000090604     c                   endif
016100090605     c                   movel     excar         t86area
016200090604     c                   write     trmz8600
016300090604      *prepara nuovo rekord
016400090604     c                   clear                   trmz8600
016500090604     c                   move      traarea       excar
016600090604     c                   endif
016700090604      *
016800090604     c                   exsr      imbuca
016900090604     c                   enddo
017000090921      * reperisce descrizione area
017100090921     c                   clear                   t86aread
017200090921     c                   movel     excar         kkey
017300090921     c                   move      '05'          kcod
017400090921     c     ktab          chain     tabel00f
017500090921     c                   if        %found(tabel00f)
017600090921     c                   movel     tbluni        t86aread
017700090921     c                   endif
017800090921     c                   movel     excar         t86area
017900090921     c                   write     trmz8600
018000070530      *
018100070530     c                   endsr
018200090604     c*-------------------------------------------------
018300090604     c     imbuca        begsr
018400090604     c*-------------------------------------------------
018500090604      *accreditati
018600090604     c                   move      tradin        dacciso
018700090604     c                   if        tradfi = 0
018800090604      * in prova
018900091014     c                   if        dacciso >= limprova
019000100304      *autisti
019100100304     c                   if        trasco = *blank
019200090604     c                   add       1             t86paccp
019300100304     c                   endif
019400100304      * mezzi accreditati in prova
019500100304     c                   if        traris = ' ' and tracor = ' '
019600100310     c                             and tratmp <> ' '
019700100304     c                   add       1             t86accinp
019800100304     c                   endif
019900090604      * non in prova
020000090604     c                   else
020100100304      *autisti
020200100304     c                   if        trasco = *blank
020300090604     c                   add       1             t86pacc
020400100304     c                   endif
020500090604      * conformi
020600100309     c*                  if        tratmp = 'S' and
020700100309     c*                            traris = ' ' and tracor = ' '
020800100309     c*                  add       1             t86conf
020900100309     c*                  endif
021000100309      * conformi
021100100311     c                   if        tratmp = 'S' and trapub = 'S' or
021200110704     c                             tratmp = 'S' and trapub = 'X' or
021300110704     c                             tratmp = 'S' and trapub = 'V'
021400100309     c                   add       1             t86conf
021500100309     c                   endif
021600090923      * verifico la pubblicità solo su queste categorie
021700100310     c                   if        tratmp = 'S' and
021800091020     c                             trapub = 'N'and (trafur = 'T' or
021900091019     c                             trafur = 'F' or trafur = 'C'
022000091019     c                             or trafur = 'R')
022100090604     c                   add       1             t86nopub
022200090923     c                   endif
022300100311     c                   if        tratmp = 'N'
022400100311     c*                            tratmp = ' ' and
022500100311     c*                            traris = ' ' and tracor =  ' '
022600090604     c                   add       1             t86nconf
022700091019     c                   endif
022800091016     c                   endif
022900090604     c*disaccreditati
023000090604     c                   else
023100090618     c                   move      tradfi        ddisiso
023200090618      * nell'anno precedente non considero il turnover
023300090923      * e nemmeno se è un mezzo di scorta
023400090923     c                   if        tradfi < inianno or trasco = 'S'
023500090604     c                   leavesr
023600090604     c                   endif
023700090618      * valuto dal motivo di turnover se devo conteggiarlo
023800090618     c                   move      'ATU'         TBECOD
023900090618     c                   movel(p)  tratur        TBEKE1
024000090618     c     ktbe          chain     tntbe01l
024100090618     c                   if        %found(tntbe01l)
024200090618     c                   movel     tbeuni        datu
024300090618     c                   if        §atusnt <> 'S'
024400090618     c                   leavesr
024500090618     c                   endif
024600091021     c*calcolo differenza fra data disaccreditamento e data accreditamento
024700090618     c                   eval      giorni = %diff(ddisiso : dacciso : *days)
024800090618     c                   if        giorni > §atugg
024900090604     c                   exsr      imbucaTO
025000090618     c                   endif
025100090618     c                   endif
025200090604     c
025300090604     c                   endif
025400090604     c                   endsr
025500090604     c*-------------------------------------------------
025600090604     c     imbucaTO      begsr
025700090604     c*-------------------------------------------------
025800090604     c                   extrct    ddisiso:*m    mesedis           2 0
025900090604      * imbuca motivo se turnover nel mese corrente
026000090604     c                   if        mesedis = mesecorrente
026100090618     c                   add       1             T86turm
026200100304      * incasella i t-o nei vari motivi x il mese corrente
026300100304     c                   select
026400100304     c                   when      tratur = '1'
026500100304     c                   add       1             T86FCA
026600100304     c                   when      tratur = '2'
026700100304     c                   add       1             T86tri
026800100304     c                   when      tratur = '3'
026900100304     c                   add       1             T86naf
027000100304     c                   when      tratur = '4'
027100100304     c                   add       1             T86dnr
027200100304     c                   when      tratur = '5'
027300100304     c                   add       1             T86soaut
027400100304     c                   when      tratur = '6'
027500100304     c                   add       1             T86var
027600100304      * i motivi 7/8/9 non sono di turnover e possono essere sostituiti con eventuali
027700100304      * nuovi elementi di tabella ATU che invece prevedono il conteggio T-Over
027800100304     c                   when      tratur = '7'
027900100304     c                   add       1             T86xx1
028000100304     c                   when      tratur = '8'
028100100304     c                   add       1             T86xx2
028200100304     c                   when      tratur = '9'
028300100304     c                   add       1             T86xx3
028400100304     c                   when      tratur = 'S'
028500100304     c                   add       1             T86xx4
028600100304     c                   endsl
028700100225     c                   else
028800100304      * imbuca motivo se turnover nel progressivo anno e specifici dettagli
028900100225     c                   add       1             T86PRGTU
029000090604     c                   select
029100090604     c                   when      tratur = '1'
029200100304     c                   add       1             T86FCAp
029300090604     c                   when      tratur = '2'
029400100304     c                   add       1             T86trip
029500090604     c                   when      tratur = '3'
029600100304     c                   add       1             T86nafp
029700090604     c                   when      tratur = '4'
029800100304     c                   add       1             T86dnrp
029900090604     c                   when      tratur = '5'
030000100304     c                   add       1             T86soautp
030100090604     c                   when      tratur = '6'
030200100304     c                   add       1             T86varp
030300100204      * i motivi 7/8/9 non sono di turnover e possono essere sostituiti con eventuali
030400100204      * nuovi elementi di tabella ATU che invece prevedono il conteggio T-Over
030500100204     c                   when      tratur = '7'
030600100304     c                   add       1             T86xx1p
030700100204     c                   when      tratur = '8'
030800100304     c                   add       1             T86xx2p
030900100204     c                   when      tratur = '9'
031000100304     c                   add       1             T86xx3p
031100090604     c                   when      tratur = 'S'
031200100304     c                   add       1             T86xx4p
031300090604     c                   endsl
031400100304     c                   endif
031500090604     c                   endsr
031600031105     c*-------------------------------------------------
031700031105     c     controv1      begsr
031800031105     c*-------------------------------------------------
031900090604     c                   setoff                                       964241
032000051121
032100090604     c                   if        vdatal = 0
032200090604     C                   seton                                        4196
032300090604     c                   goto      endcontr
032400090604     C                   endif
032500090604     c                   if        vdatal > 0
032600090604     C                   MOVE      Vdatal        G02DAT
032700090604     C                   MOVEL     *BLANK        G02ERR
032800090604     C                   CALL      'XSRDA8'
032900090604     C                   PARM                    WLBDA8
033000090604     C     G02ERR        IFEQ      '1'
033100090604     C                   SETON                                        4295
033200090604     c                   goto      endcontr
033300090604     C                   endif
033400090604     c                   move      g02dat        vdatal
033500090604     c                   move      g02inv        vdatalg           8 0
033600090604     c                   move      g02inv        inianno           8 0
033700090604     c                   move      0101          inianno
033800090604     c                   move      g02inv        dataiso
033900090604     c                   extrct    dataiso:*m    mesecorrente      2 0
034000091014     c     dataiso       subdur    90:*d         dataiso
034100091014     c                   extrct    dataiso:*d    giorno            2 0
034200091014     c                   if        giorno > 15
034300091014     c     dataiso       adddur    1:*m          dataiso
034400091014     c                   endif
034500091014     c                   sub       1             giorno
034600091014     c     dataiso       subdur    giorno:*d     limprova
034700090604     c
034800090604     c                   endif
034900130128     c                   if        vdatalg < ultgmesp
035000130128     c                   seton                                        4295
035100130128     c                   endif
035200051122     c     endcontr      endsr
035300070608     c*-------------------------------------------------
035400090604     c     comandi       begsr
035500070608     c*-------------------------------------------------
035600070608      *
035700070608     C                   z-add     99            luncmd
035800070608     C                   CALL      'QCMDEXC'                            34
035900070608     C                   parm                    coman
036000070608     C                   parm                    luncmd
036100070608      *
036200070608     c                   endsr
036300011026     c*-------------------------------------------------
036400011026     c     *inzsr        begsr
036500011026     c*-------------------------------------------------
036600020304     c     *entry        plist
036700020304     c                   parm                    kpjba
036800050801
036900050801     c     ktab          klist
037000050801     c                   kfld                    kkut              1 0
037100050801     c                   kfld                    kcod              2
037200050801     c                   kfld                    kkey              8
037300050801     c                   z-add     1             kkut
037400050801     c                   move      '05'          kcod
037500050801     c
037600090618     c     ktbe          klist
037700090618     c                   kfld                    tbecod
037800090618     c                   kfld                    tbeke1
037900050801
038000020218     C                   time                    w0120            14 0
038100020218     C                   move      w0120         wdat              8 0
038200130128     C                   movel     01            wdat
038300130128     C                   move      wdat          dataeur
038400130128     c     dataeur       subdur    1:*d          dataiso
038500130128     c                   move      dataiso       ultgmesp          8 0
038600130128     c                   move      dataiso       dataeur
038700130128     c                   move      dataeur       vdatal
038800030630      *profilo di filiale estraggo solo quelli relativi al P.O.
038900030630     C                   CLEAR                   Tibs36Ds
039000030630     C                   EVAL      I36ute = knmus
039100030630     C                   CALL      'TIBS36R'
039200030630     C                   PARM                    tibs36ds
039300130129      *carica motivi tecnici di disaccreditamento
039400130129     c                   z-add     0             tec               1 0
039500130129     c     'ATU'         setll     tntbe01l
039600130129     c                   do        *hival
039700130129     c     'ATU'         reade     tntbe01l
039800130129     c                   if        %eof(tntbe01l)
039900130129     c                   leave
040000130129     c                   endif
040100130129     c                   movel     tbeuni        datu
040200130129     c                   if        §atutec = 'S'
040300130129     c                   add       1             tec
040400130129     c                   movel     tbeke1        mot(tec)
040500130129     c                   endif
040600130129     c                   enddo
040700051017      * x filiale duplica file di work in qtemp
040800090604     c                   close     trmz861f
040900051017     c                   movea     cmd2          coman
041000051017     c                   exsr      comandi
041100090604     c                   open      trmz861f
041200080502      *
041300011026     c                   endsr
041400050802      *---------------------------------------------------------
041500050802**  CMD1
041600090604CLRPFM    FILE(trmz861F)
041700050802**  CMD2
041800090604CRTDUPOBJ OBJ(trmz861F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
041900050802**  CMD3
042000090604DLTF FILE(QTEMP/trmz861F)
042100070608**  CMD4
042200090604CHKOBJ  OBJ(WRKQUERY/trmz860f) OBJTYPE(*FILE)
042300070608**  CMD5
042400090604CRTDUPOBJ OBJ(trmz860f) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(trmz860f) DATA(*YES)
042500070608**  CMD6
042600090604CPYF FROMFILE(trmz860f) TOFILE(WRKQUERY/trmz860f) MBROPT(*REPLACE)
042700090604**  CMD7
042800090604CLRPFM    FILE(trmz860F)
