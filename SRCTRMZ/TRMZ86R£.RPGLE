000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200100204     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400090604     Ftrmz86d   CF   E             WORKSTN
000500070605     Faitra04l  if   e           k disk
000600050623     Fazorg01l  if   e           k disk    rename(azorg:azorg1)
000700090604     Ftrmz860f  uf a e           k disk    usropn
000800090604     Ftrmz861f  uf a e           k disk    usropn
000900050801     ftabel00f  if   e           k disk
001000090618     ftntbe01l  if   e           k disk
001100020304     dkpjba          e ds
001200030630     d Tibs36Ds      e ds
001300090618     d datu          e ds
001400090618     d giorni          s              5  0
001500050623     d primo           s              1
001600050623     d excar           s                   like(orgcar)
001700070608     d coman           s             99
001800050802     d luncmd          s             15  5
001900090604     d limprova        s               d   datfmt(*iso)
002000090604     d dacciso         s               d   datfmt(*iso)
002100090604     d ddisiso         s               d   datfmt(*iso)
002200070605     d dataiso         s               d   datfmt(*iso)
002300070605     d dataeur         s               d   datfmt(*eur)
002301130129     D mot             S              1    DIM(9)
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
009401130129     c                   setoff                                       99
010500120918      *ATTENZIONE
010600130129      *QUESTO CONTROLLO È INSERITO PER VERIFICARE SE IL DISACCREDITAMENTO DELL'AUTISTA
010700130129      *È AVVENUTO PER MOTIVI TECNICI TIPO RIFACIMENTO CONTRATTI DI MASSA OPPURE CAMBIO
010800130129      *DI SOCIETÀ OPERATIVE IN QUESTO CASO CI SI SALVA LA DATA DI ACCREDITAMENTO IN MODO
010900130129      *DA CONSIDERARE IL RECORD SCADUTO COME INGRESSO DELL'AUTISTA PER POTER CALCOLARE
011000130129      * CORRETTAMENTE IL PERIODO DI PROVA DI 90 GIORNI
011100130129      * PER EFFETTUARE QUESTO RAGIONAMENTO SI UTILIZZA IL CAMPO DEL TURNOVER VERIFICANDO CHE
011101130129      * IL MOTIVO INSERITO SIA UN MOTIVO TECNICO DI DISACCREDITAMENTO
011102130129     c                   if        tradfi > 0
011103130129     c                   if        tratur <> *blank
011104130129     c                   z-add     1             tec
011105130129     c     tratur        lookup    mot(tec)                               99
011106130129     c                   endif
011200130129     c                   if        *in99
011300120511     c                   movel     traaut        extraaut         25
011301130129     c                   movel     trataa        extrataa         10
011400120511     c                   movel     tradin        extradin          8 0
011600040212     c                   iter
011601130129     c                   endif
011700040212     c                   endif
011701130129      * esclude annullati e corrispondenti e scorta
011702130129      * se disaccreditato l'anno precedente
011800130129     c                   if        tradfi > 0 and tradfi < inianno  or
011801130131     c                             traann <> *blank
011802130131     c*                            tradin > vdatalg
011900090604     c                   iter
012000090604     c                   endif
012100090604      * reperisce area
012200090604     c     trafil        chain     azorg01l
012201130129      * verifica se prima di questo rekord ne è stato letto uno con un disaccreditamento tecnico
012202130129      * nel caso imposta come data accreditamento la data precedente
012500130129     c                   if        traaut = extraaut and
012501130129     c                             trataa = extrataa and
012502130129     c                             extradin > 0
012600120511     c                   z-add     extradin      tradin
012700120423     c                   endif
012800050801     c                   move      orgcar        traarea
012801130129      * scrive W.F
012900090604     c                   write     trmz8610
013000050623     c                   enddo
013100020304
013200020304     c                   endsr
013300981029      *
013400050802     c*-------------------------------------------------
013500090604     c     riepilogo     begsr
013600050802     c*-------------------------------------------------
013700090604     c     *loval        setll     trmz861f
013800070530     c                   do        *hival
013900090604     c                   read      trmz861f                               98
014000070530     c   98              leave
014100100304      *
014200070530     c                   if        primo = *blank
014300070530     c                   move      traarea       excar
014400070530     c                   move      'X'           primo
014500070530     c                   endif
014600090604      * a rottura scrive rekord
014700090604     c                   if        traarea <> excar
014800090604      * reperisce descrizione area
014900090604     c                   clear                   t86aread
015000090604     c                   movel     excar         kkey
015100090604     c                   move      '05'          kcod
015200090604     c     ktab          chain     tabel00f
015300090604     c                   if        %found(tabel00f)
015400090604     c                   movel     tbluni        t86aread
015500090604     c                   endif
015600090605     c                   movel     excar         t86area
015700090604     c                   write     trmz8600
015800090604      *prepara nuovo rekord
015900090604     c                   clear                   trmz8600
016000090604     c                   move      traarea       excar
016100090604     c                   endif
016200090604      *
016300090604     c                   exsr      imbuca
016400090604     c                   enddo
016500090921      * reperisce descrizione area
016600090921     c                   clear                   t86aread
016700090921     c                   movel     excar         kkey
016800090921     c                   move      '05'          kcod
016900090921     c     ktab          chain     tabel00f
017000090921     c                   if        %found(tabel00f)
017100090921     c                   movel     tbluni        t86aread
017200090921     c                   endif
017300090921     c                   movel     excar         t86area
017400090921     c                   write     trmz8600
017500070530      *
017600070530     c                   endsr
017700090604     c*-------------------------------------------------
017800090604     c     imbuca        begsr
017900090604     c*-------------------------------------------------
018000090604      *accreditati
018100090604     c                   move      tradin        dacciso
018200090604     c                   if        tradfi = 0
018300090604      * in prova
018400091014     c                   if        dacciso >= limprova
018500100304      *autisti
018600100304     c                   if        trasco = *blank
018700090604     c                   add       1             t86paccp
018800100304     c                   endif
018900100304      * mezzi accreditati in prova
019000100304     c                   if        traris = ' ' and tracor = ' '
019100100310     c                             and tratmp <> ' '
019200100304     c                   add       1             t86accinp
019300100304     c                   endif
019400090604      * non in prova
019500090604     c                   else
019600100304      *autisti
019700100304     c                   if        trasco = *blank
019800090604     c                   add       1             t86pacc
019900100304     c                   endif
020000090604      * conformi
020100100309     c*                  if        tratmp = 'S' and
020200100309     c*                            traris = ' ' and tracor = ' '
020300100309     c*                  add       1             t86conf
020400100309     c*                  endif
020500100309      * conformi
020600100311     c                   if        tratmp = 'S' and trapub = 'S' or
020700110704     c                             tratmp = 'S' and trapub = 'X' or
020800110704     c                             tratmp = 'S' and trapub = 'V'
020900100309     c                   add       1             t86conf
021000100309     c                   endif
021100090923      * verifico la pubblicità solo su queste categorie
021200100310     c                   if        tratmp = 'S' and
021300091020     c                             trapub = 'N'and (trafur = 'T' or
021400091019     c                             trafur = 'F' or trafur = 'C'
021500091019     c                             or trafur = 'R')
021600090604     c                   add       1             t86nopub
021700090923     c                   endif
021800100311     c                   if        tratmp = 'N'
021900100311     c*                            tratmp = ' ' and
022000100311     c*                            traris = ' ' and tracor =  ' '
022100090604     c                   add       1             t86nconf
022200091019     c                   endif
022300091016     c                   endif
022400090604     c*disaccreditati
022500090604     c                   else
022600090618     c                   move      tradfi        ddisiso
022700090618      * nell'anno precedente non considero il turnover
022800090923      * e nemmeno se è un mezzo di scorta
022900090923     c                   if        tradfi < inianno or trasco = 'S'
023000090604     c                   leavesr
023100090604     c                   endif
023200090618      * valuto dal motivo di turnover se devo conteggiarlo
023300090618     c                   move      'ATU'         TBECOD
023400090618     c                   movel(p)  tratur        TBEKE1
023500090618     c     ktbe          chain     tntbe01l
023600090618     c                   if        %found(tntbe01l)
023700090618     c                   movel     tbeuni        datu
023800090618     c                   if        §atusnt <> 'S'
023900090618     c                   leavesr
024000090618     c                   endif
024100091021     c*calcolo differenza fra data disaccreditamento e data accreditamento
024200090618     c                   eval      giorni = %diff(ddisiso : dacciso : *days)
024300090618     c                   if        giorni > §atugg
024400090604     c                   exsr      imbucaTO
024500090618     c                   endif
024600090618     c                   endif
024700090604     c
024800090604     c                   endif
024900090604     c                   endsr
025000090604     c*-------------------------------------------------
025100090604     c     imbucaTO      begsr
025200090604     c*-------------------------------------------------
025300090604     c                   extrct    ddisiso:*m    mesedis           2 0
025400090604      * imbuca motivo se turnover nel mese corrente
025500090604     c                   if        mesedis = mesecorrente
025600090618     c                   add       1             T86turm
025700100304      * incasella i t-o nei vari motivi x il mese corrente
025800100304     c                   select
025900100304     c                   when      tratur = '1'
026000100304     c                   add       1             T86FCA
026100100304     c                   when      tratur = '2'
026200100304     c                   add       1             T86tri
026300100304     c                   when      tratur = '3'
026400100304     c                   add       1             T86naf
026500100304     c                   when      tratur = '4'
026600100304     c                   add       1             T86dnr
026700100304     c                   when      tratur = '5'
026800100304     c                   add       1             T86soaut
026900100304     c                   when      tratur = '6'
027000100304     c                   add       1             T86var
027100100304      * i motivi 7/8/9 non sono di turnover e possono essere sostituiti con eventuali
027200100304      * nuovi elementi di tabella ATU che invece prevedono il conteggio T-Over
027300100304     c                   when      tratur = '7'
027400100304     c                   add       1             T86xx1
027500100304     c                   when      tratur = '8'
027600100304     c                   add       1             T86xx2
027700100304     c                   when      tratur = '9'
027800100304     c                   add       1             T86xx3
027900100304     c                   when      tratur = 'S'
028000100304     c                   add       1             T86xx4
028100100304     c                   endsl
028200100225     c                   else
028300100304      * imbuca motivo se turnover nel progressivo anno e specifici dettagli
028400100225     c                   add       1             T86PRGTU
028500090604     c                   select
028600090604     c                   when      tratur = '1'
028700100304     c                   add       1             T86FCAp
028800090604     c                   when      tratur = '2'
028900100304     c                   add       1             T86trip
029000090604     c                   when      tratur = '3'
029100100304     c                   add       1             T86nafp
029200090604     c                   when      tratur = '4'
029300100304     c                   add       1             T86dnrp
029400090604     c                   when      tratur = '5'
029500100304     c                   add       1             T86soautp
029600090604     c                   when      tratur = '6'
029700100304     c                   add       1             T86varp
029800100204      * i motivi 7/8/9 non sono di turnover e possono essere sostituiti con eventuali
029900100204      * nuovi elementi di tabella ATU che invece prevedono il conteggio T-Over
030000100204     c                   when      tratur = '7'
030100100304     c                   add       1             T86xx1p
030200100204     c                   when      tratur = '8'
030300100304     c                   add       1             T86xx2p
030400100204     c                   when      tratur = '9'
030500100304     c                   add       1             T86xx3p
030600090604     c                   when      tratur = 'S'
030700100304     c                   add       1             T86xx4p
030800090604     c                   endsl
030900100304     c                   endif
031000090604     c                   endsr
031100031105     c*-------------------------------------------------
031200031105     c     controv1      begsr
031300031105     c*-------------------------------------------------
031400090604     c                   setoff                                       964241
031500051121
031600090604     c                   if        vdatal = 0
031700090604     C                   seton                                        4196
031800090604     c                   goto      endcontr
031900090604     C                   endif
032000090604     c                   if        vdatal > 0
032100090604     C                   MOVE      Vdatal        G02DAT
032200090604     C                   MOVEL     *BLANK        G02ERR
032300090604     C                   CALL      'XSRDA8'
032400090604     C                   PARM                    WLBDA8
032500090604     C     G02ERR        IFEQ      '1'
032600090604     C                   SETON                                        4295
032700090604     c                   goto      endcontr
032800090604     C                   endif
032900090604     c                   move      g02dat        vdatal
033000090604     c                   move      g02inv        vdatalg           8 0
033100090604     c                   move      g02inv        inianno           8 0
033200090604     c                   move      0101          inianno
033300090604     c                   move      g02inv        dataiso
033400090604     c                   extrct    dataiso:*m    mesecorrente      2 0
033500091014     c     dataiso       subdur    90:*d         dataiso
033600091014     c                   extrct    dataiso:*d    giorno            2 0
033700091014     c                   if        giorno > 15
033800091014     c     dataiso       adddur    1:*m          dataiso
033900091014     c                   endif
034000091014     c                   sub       1             giorno
034100091014     c     dataiso       subdur    giorno:*d     limprova
034200090604     c
034300090604     c                   endif
034301130128     c                   if        vdatalg < ultgmesp
034302130128     c                   seton                                        4295
034303130128     c                   endif
034400051122     c     endcontr      endsr
034500070608     c*-------------------------------------------------
034600090604     c     comandi       begsr
034700070608     c*-------------------------------------------------
034800070608      *
034900070608     C                   z-add     99            luncmd
035000070608     C                   CALL      'QCMDEXC'                            34
035100070608     C                   parm                    coman
035200070608     C                   parm                    luncmd
035300070608      *
035400070608     c                   endsr
035500011026     c*-------------------------------------------------
035600011026     c     *inzsr        begsr
035700011026     c*-------------------------------------------------
035800020304     c     *entry        plist
035900020304     c                   parm                    kpjba
036000050801
036100050801     c     ktab          klist
036200050801     c                   kfld                    kkut              1 0
036300050801     c                   kfld                    kcod              2
036400050801     c                   kfld                    kkey              8
036500050801     c                   z-add     1             kkut
036600050801     c                   move      '05'          kcod
036700050801     c
036800090618     c     ktbe          klist
036900090618     c                   kfld                    tbecod
037000090618     c                   kfld                    tbeke1
037100050801
037200020218     C                   time                    w0120            14 0
037300020218     C                   move      w0120         wdat              8 0
037301130128     C                   movel     01            wdat
037302130128     C                   move      wdat          dataeur
037303130128     c     dataeur       subdur    1:*d          dataiso
037304130128     c                   move      dataiso       ultgmesp          8 0
037305130128     c                   move      dataiso       dataeur
037306130128     c                   move      dataeur       vdatal
037400030630      *profilo di filiale estraggo solo quelli relativi al P.O.
037500030630     C                   CLEAR                   Tibs36Ds
037600030630     C                   EVAL      I36ute = knmus
037700030630     C                   CALL      'TIBS36R'
037800030630     C                   PARM                    tibs36ds
037900130129      *carica motivi tecnici di disaccreditamento
037901130129     c                   z-add     0             tec               1 0
037902130129     c     'ATU'         setll     tntbe01l
037903130129     c                   do        *hival
037904130129     c     'ATU'         reade     tntbe01l
037905130129     c                   if        %eof(tntbe01l)
037906130129     c                   leave
037907130129     c                   endif
037908130129     c                   movel     tbeuni        datu
037909130129     c                   if        §atutec = 'S'
037910130129     c                   add       1             tec
037911130129     c                   movel     tbeke1        mot(tec)
037912130129     c                   endif
037914130129     c                   enddo
038000051017      * x filiale duplica file di work in qtemp
038100090604     c                   close     trmz861f
038200051017     c                   movea     cmd2          coman
038300051017     c                   exsr      comandi
038400090604     c                   open      trmz861f
038500080502      *
038600011026     c                   endsr
038700050802      *---------------------------------------------------------
038800050802**  CMD1
038900090604CLRPFM    FILE(trmz861F)
039000050802**  CMD2
039100090604CRTDUPOBJ OBJ(trmz861F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
039200050802**  CMD3
039300090604DLTF FILE(QTEMP/trmz861F)
039400070608**  CMD4
039500090604CHKOBJ  OBJ(WRKQUERY/trmz860f) OBJTYPE(*FILE)
039600070608**  CMD5
039700090604CRTDUPOBJ OBJ(trmz860f) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(trmz860f) DATA(*YES)
039800070608**  CMD6
039900090604CPYF FROMFILE(trmz860f) TOFILE(WRKQUERY/trmz860f) MBROPT(*REPLACE)
040000090604**  CMD7
040100090604CLRPFM    FILE(trmz860F)
