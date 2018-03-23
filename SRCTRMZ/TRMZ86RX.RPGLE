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
009500090604      * esclude annullati e corrispondenti e scorta
009600090604      * se disaccreditato l'anno precedente
009700091020     c                   if        traann <> *blank or
009800091019     c                             tradin > vdatalg
009900040212     c                   iter
010000040212     c                   endif
010100090604     c                   if        tradfi > 0 and tradfi < inianno
010200090604     c                   iter
010300090604     c                   endif
010400090604      * reperisce area
010500090604     c     trafil        chain     azorg01l
010600050801      * scrive W.F
010700050801     c                   move      orgcar        traarea
010800090604     c                   write     trmz8610
010900050623     c                   enddo
011000020304
011100020304     c                   endsr
011200981029      *
011300050802     c*-------------------------------------------------
011400090604     c     riepilogo     begsr
011500050802     c*-------------------------------------------------
011600090604     c     *loval        setll     trmz861f
011700070530     c                   do        *hival
011800090604     c                   read      trmz861f                               98
011900070530     c   98              leave
012000100304      *
012100070530     c                   if        primo = *blank
012200070530     c                   move      traarea       excar
012300070530     c                   move      'X'           primo
012400070530     c                   endif
012500090604      * a rottura scrive rekord
012600090604     c                   if        traarea <> excar
012700090604      * reperisce descrizione area
012800090604     c                   clear                   t86aread
012900090604     c                   movel     excar         kkey
013000090604     c                   move      '05'          kcod
013100090604     c     ktab          chain     tabel00f
013200090604     c                   if        %found(tabel00f)
013300090604     c                   movel     tbluni        t86aread
013400090604     c                   endif
013500090605     c                   movel     excar         t86area
013600090604     c                   write     trmz8600
013700090604      *prepara nuovo rekord
013800090604     c                   clear                   trmz8600
013900090604     c                   move      traarea       excar
014000090604     c                   endif
014100090604      *
014200090604     c                   exsr      imbuca
014300090604     c                   enddo
014400090921      * reperisce descrizione area
014500090921     c                   clear                   t86aread
014600090921     c                   movel     excar         kkey
014700090921     c                   move      '05'          kcod
014800090921     c     ktab          chain     tabel00f
014900090921     c                   if        %found(tabel00f)
015000090921     c                   movel     tbluni        t86aread
015100090921     c                   endif
015200090921     c                   movel     excar         t86area
015300090921     c                   write     trmz8600
015400070530      *
015500070530     c                   endsr
015600090604     c*-------------------------------------------------
015700090604     c     imbuca        begsr
015800090604     c*-------------------------------------------------
015900090604      *accreditati
016000090604     c                   move      tradin        dacciso
016100090604     c                   if        tradfi = 0
016200090604      * in prova
016300091014     c                   if        dacciso >= limprova
016400100304      *autisti
016500100304     c                   if        trasco = *blank
016600090604     c                   add       1             t86paccp
016700100304     c                   endif
016800100304      * mezzi accreditati in prova
016900100304     c                   if        traris = ' ' and tracor = ' '
017000100310     c                             and tratmp <> ' '
017100100304     c                   add       1             t86accinp
017200100304     c                   endif
017300090604      * non in prova
017400090604     c                   else
017500100304      *autisti
017600100304     c                   if        trasco = *blank
017700090604     c                   add       1             t86pacc
017800100304     c                   endif
017900090604      * conformi
018000100309     c*                  if        tratmp = 'S' and
018100100309     c*                            traris = ' ' and tracor = ' '
018200100309     c*                  add       1             t86conf
018300100309     c*                  endif
018400100309      * conformi
018500100311     c                   if        tratmp = 'S' and trapub = 'S' or
018600110704     c                             tratmp = 'S' and trapub = 'X' or
018700110704     c                             tratmp = 'S' and trapub = 'V'
018800100309     c                   add       1             t86conf
018900100309     c                   endif
019000090923      * verifico la pubblicità solo su queste categorie
019100100310     c                   if        tratmp = 'S' and
019200091020     c                             trapub = 'N'and (trafur = 'T' or
019300091019     c                             trafur = 'F' or trafur = 'C'
019400091019     c                             or trafur = 'R')
019500090604     c                   add       1             t86nopub
019600090923     c                   endif
019700100311     c                   if        tratmp = 'N'
019800100311     c*                            tratmp = ' ' and
019900100311     c*                            traris = ' ' and tracor =  ' '
020000090604     c                   add       1             t86nconf
020100091019     c                   endif
020200091016     c                   endif
020300090604     c*disaccreditati
020400090604     c                   else
020500090618     c                   move      tradfi        ddisiso
020600090618      * nell'anno precedente non considero il turnover
020700090923      * e nemmeno se è un mezzo di scorta
020800090923     c                   if        tradfi < inianno or trasco = 'S'
020900090604     c                   leavesr
021000090604     c                   endif
021100090618      * valuto dal motivo di turnover se devo conteggiarlo
021200090618     c                   move      'ATU'         TBECOD
021300090618     c                   movel(p)  tratur        TBEKE1
021400090618     c     ktbe          chain     tntbe01l
021500090618     c                   if        %found(tntbe01l)
021600090618     c                   movel     tbeuni        datu
021700090618     c                   if        §atusnt <> 'S'
021800090618     c                   leavesr
021900090618     c                   endif
022000091021     c*calcolo differenza fra data disaccreditamento e data accreditamento
022100090618     c                   eval      giorni = %diff(ddisiso : dacciso : *days)
022200090618     c                   if        giorni > §atugg
022300090604     c                   exsr      imbucaTO
022400090618     c                   endif
022500090618     c                   endif
022600090604     c
022700090604     c                   endif
022800090604     c                   endsr
022900090604     c*-------------------------------------------------
023000090604     c     imbucaTO      begsr
023100090604     c*-------------------------------------------------
023200090604     c                   extrct    ddisiso:*m    mesedis           2 0
023300090604      * imbuca motivo se turnover nel mese corrente
023400090604     c                   if        mesedis = mesecorrente
023500090618     c                   add       1             T86turm
023600100304      * incasella i t-o nei vari motivi x il mese corrente
023700100304     c                   select
023800100304     c                   when      tratur = '1'
023900100304     c                   add       1             T86FCA
024000100304     c                   when      tratur = '2'
024100100304     c                   add       1             T86tri
024200100304     c                   when      tratur = '3'
024300100304     c                   add       1             T86naf
024400100304     c                   when      tratur = '4'
024500100304     c                   add       1             T86dnr
024600100304     c                   when      tratur = '5'
024700100304     c                   add       1             T86soaut
024800100304     c                   when      tratur = '6'
024900100304     c                   add       1             T86var
025000100304      * i motivi 7/8/9 non sono di turnover e possono essere sostituiti con eventuali
025100100304      * nuovi elementi di tabella ATU che invece prevedono il conteggio T-Over
025200100304     c                   when      tratur = '7'
025300100304     c                   add       1             T86xx1
025400100304     c                   when      tratur = '8'
025500100304     c                   add       1             T86xx2
025600100304     c                   when      tratur = '9'
025700100304     c                   add       1             T86xx3
025800100304     c                   when      tratur = 'S'
025900100304     c                   add       1             T86xx4
026000100304     c                   endsl
026100100225     c                   else
026200100304      * imbuca motivo se turnover nel progressivo anno e specifici dettagli
026300100225     c                   add       1             T86PRGTU
026400090604     c                   select
026500090604     c                   when      tratur = '1'
026600100304     c                   add       1             T86FCAp
026700090604     c                   when      tratur = '2'
026800100304     c                   add       1             T86trip
026900090604     c                   when      tratur = '3'
027000100304     c                   add       1             T86nafp
027100090604     c                   when      tratur = '4'
027200100304     c                   add       1             T86dnrp
027300090604     c                   when      tratur = '5'
027400100304     c                   add       1             T86soautp
027500090604     c                   when      tratur = '6'
027600100304     c                   add       1             T86varp
027700100204      * i motivi 7/8/9 non sono di turnover e possono essere sostituiti con eventuali
027800100204      * nuovi elementi di tabella ATU che invece prevedono il conteggio T-Over
027900100204     c                   when      tratur = '7'
028000100304     c                   add       1             T86xx1p
028100100204     c                   when      tratur = '8'
028200100304     c                   add       1             T86xx2p
028300100204     c                   when      tratur = '9'
028400100304     c                   add       1             T86xx3p
028500090604     c                   when      tratur = 'S'
028600100304     c                   add       1             T86xx4p
028700090604     c                   endsl
028800100304     c                   endif
028900090604     c                   endsr
029000031105     c*-------------------------------------------------
029100031105     c     controv1      begsr
029200031105     c*-------------------------------------------------
029300090604     c                   setoff                                       964241
029400051121
029500090604     c                   if        vdatal = 0
029600090604     C                   seton                                        4196
029700090604     c                   goto      endcontr
029800090604     C                   endif
029900090604     c                   if        vdatal > 0
030000090604     C                   MOVE      Vdatal        G02DAT
030100090604     C                   MOVEL     *BLANK        G02ERR
030200090604     C                   CALL      'XSRDA8'
030300090604     C                   PARM                    WLBDA8
030400090604     C     G02ERR        IFEQ      '1'
030500090604     C                   SETON                                        4295
030600090604     c                   goto      endcontr
030700090604     C                   endif
030800090604     c                   move      g02dat        vdatal
030900090604     c                   move      g02inv        vdatalg           8 0
031000090604     c                   move      g02inv        inianno           8 0
031100090604     c                   move      0101          inianno
031200090604     c                   move      g02inv        dataiso
031300090604     c                   extrct    dataiso:*m    mesecorrente      2 0
031400091014     c     dataiso       subdur    90:*d         dataiso
031500091014     c                   extrct    dataiso:*d    giorno            2 0
031600091014     c                   if        giorno > 15
031700091014     c     dataiso       adddur    1:*m          dataiso
031800091014     c                   endif
031900091014     c                   sub       1             giorno
032000091014     c     dataiso       subdur    giorno:*d     limprova
032100090604     c
032200090604     c                   endif
032300051122     c     endcontr      endsr
032400070608     c*-------------------------------------------------
032500090604     c     comandi       begsr
032600070608     c*-------------------------------------------------
032700070608      *
032800070608     C                   z-add     99            luncmd
032900070608     C                   CALL      'QCMDEXC'                            34
033000070608     C                   parm                    coman
033100070608     C                   parm                    luncmd
033200070608      *
033300070608     c                   endsr
033400011026     c*-------------------------------------------------
033500011026     c     *inzsr        begsr
033600011026     c*-------------------------------------------------
033700020304     c     *entry        plist
033800020304     c                   parm                    kpjba
033900050801
034000050801     c     ktab          klist
034100050801     c                   kfld                    kkut              1 0
034200050801     c                   kfld                    kcod              2
034300050801     c                   kfld                    kkey              8
034400050801     c                   z-add     1             kkut
034500050801     c                   move      '05'          kcod
034600050801     c
034700090618     c     ktbe          klist
034800090618     c                   kfld                    tbecod
034900090618     c                   kfld                    tbeke1
035000050801
035100020218     C                   time                    w0120            14 0
035200020218     C                   move      w0120         wdat              8 0
035300030630      *profilo di filiale estraggo solo quelli relativi al P.O.
035400030630     C                   CLEAR                   Tibs36Ds
035500030630     C                   EVAL      I36ute = knmus
035600030630     C                   CALL      'TIBS36R'
035700030630     C                   PARM                    tibs36ds
035800080502      *
035900051017      * x filiale duplica file di work in qtemp
036000090604     c                   close     trmz861f
036100051017     c                   movea     cmd2          coman
036200051017     c                   exsr      comandi
036300090604     c                   open      trmz861f
036400080502      *
036500011026     c                   endsr
036600050802      *---------------------------------------------------------
036700050802**  CMD1
036800090604CLRPFM    FILE(trmz861F)
036900050802**  CMD2
037000090604CRTDUPOBJ OBJ(trmz861F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
037100050802**  CMD3
037200090604DLTF FILE(QTEMP/trmz861F)
037300070608**  CMD4
037400090604CHKOBJ  OBJ(WRKQUERY/trmz860f) OBJTYPE(*FILE)
037500070608**  CMD5
037600090604CRTDUPOBJ OBJ(trmz860f) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(trmz860f) DATA(*YES)
037700070608**  CMD6
037800090604CPYF FROMFILE(trmz860f) TOFILE(WRKQUERY/trmz860f) MBROPT(*REPLACE)
037900090604**  CMD7
038000090604CLRPFM    FILE(trmz860F)
