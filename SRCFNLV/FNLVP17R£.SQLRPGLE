000100140613     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200140613     h dftactgrp(*NO) actgrp(*caller) bnddir('UBBNDDIR')
000300000000     F*----------------------------------------------------------------
000400140310     Fazorg01l  iF   E           K DISK
000500140310     FfidsT01l  iF   E           K DISK    usropn
000600140310     f                                     extfile(filedsT)
000700120330     Ffiapd01l  iF   E           K DISK
000800120329     Ffnorm01l  iF   E           K DISK
000900120329     Ftabel00f  iF   E           K DISK
001000120329     Ftntbe01l  iF   E           K DISK
001100140310     Ffiplg02l  iF   E           K DISK
001200140307     Fwfnlvp17f o  a E           K DISK    usropn
001300140312     Ffnlvp17p  O    e             PRINTER oflind(*in01) usropn
001400030314     d*
001500000000     D*----------------------------------------------------------------
001600140613     D/COPY GAITRASRC/SRCPROTOPR,UBUTISQL
001700140613     D/COPY GAITRASRC/SRCPROTOPI,UBUTISQL
001800140613     D WrkSqlCmd       S           1024
001900140312     d azuteds       e ds                  extname(azute00f)
002000140312     d ddatiute      e ds
002100140312     d tibs34ds      e ds                  inz
002200030314     D KPJBA         E DS
002300120329     D ds2a          E DS
002400120329     D dcmr          E DS
002500120402     D tibs55ds      E DS
002600140307     D fnlvp16ds     E DS
002700140307     D fiprdcusds    E DS
002800140307     D wfiplgds      E DS                  extname(wfiplg0f)
002900140307     D wfiplgorads   E DS                  extname(wfiplgoraf)
003000140310     D wfnlvp17ds    E DS                  extname(wfnlvp17f)
003100140310     c* campi di lavoro
003200140310     d filedsT         s             21
003300140319     d v1primac        s                   like(p17primac)
003400140319     d v1dopoc         s                   like(p17dopoc)
003500140319     d v1oraokc        s                   like(p17oraokc)
003600140319     d v1oraokr        s                   like(p17oraokr)
003700140310     d DaTAISO         S               D   DATFMT(*ISO)
003800140310     d DaTAEUR         S               D   DATFMT(*EUR)
003900140319     d v1area          s                   like(orgcar)
004000140319     d v1dis           s                   like(orgfl3)
004100140319     d f16i            s                   like(f16po)
004200140319     d savfgs          s                   like(comfgs)
004300140320     d savdisd         s                   like(p17disd)
004400140320     d savcard         s                   like(p17card)
004500140312     d Record_OK       s              1
004600140310     d tabok           s              1
004700140311     d savtabok        s              1
004800140310     d*
004900140327     d v1accura        s              1  0
005000140327     d v1esi           s              1  0
005100140327     d v1geo           s              1  0
005200140310     d nora            s              5  0
005300140310     d* distinta
005400140320     D dsdis           ds                  inz
005500140310     d comaut                         7  0
005600140310     d comfgs                         3  0
005700140310     d comndc                         6  0
005800140320     d comora          s              6  0
005900140320     d comtime         s             14  0
006000140320     d AMGUDT          s              8  0
006100140310     D distinta        S             16  0
006200140328     d v1geoc          s              5  0
006300140328     d v1geor          s              5  0
006400140320     D vtesic          S              7  0
006500140320     D vtprimac        S              7  0
006600140320     D vtoraokc        S              7  0
006700140320     D vtdopoc         S              7  0
006800140320     D vtesir          S              7  0
006900140320     D vtoraokr        S              7  0
007000140320     D taut            S              5  0
007100140320     d cprimac         s              5  0
007200140320     d coraokc         s              5  0
007300140324     d coraokr         s              5  0
007400140320     d vpprimac        s              3  0
007500140320     d vporaokc        s              3  0
007600140320     d vporaokr        s              3  0
007700140320     d tpprimac        s              3  0
007800140320     d tporaokc        s              3  0
007900140320     d tporaokr        s              3  0
008000140320     d tra             s            132
008100140320     d tra1            s            132
008200140320     d da              s              8
008300140320     d a               s              8
008400140310     d savdistinta     s                   like(distinta)
008500140310     d saviddoc        s                   like(wrkiddoc)
008600140310     d savtipo         s                   like(wrktipappl)
008700140310     d savdatora       s                   like(wrkdata)
008800140312     d savdataesi      s                   like(wrkdata)
008900140310     d comwrkora       s               t   timfmt(*iso)
009000140310     d comcmc          s              3
009100140310     d*
009200140310     d orasti          s              4
009300140310     d comorasti       s               t   timfmt(*iso)
009400140310     d*
009500140310     d minsco          s              5  0
009600140310     d piuminsco       s              5  0
009700140310     d* data ora ok ad uscire
009800140310     d oracus          s              6
009900140310     d datacus         s              8
010000140310     d* confronto i 6 minuti tra un esito e l'altro
010100140310     d ora             s               t   timfmt(*iso)
010200140310     d data            s              8  0
010300140310     d savora          s               t   timfmt(*iso)
010400140310     d savdata         s              8  0
010500140310     d minuti          s              5  0
010600140310     d*
010700120402     d lib001ala       s                   like(o50ala)
010800131203     D*-----------
010900120618     D  MSGDS          S            100
011000120618     D  LENGH          S             15  5
011100030314     c*****************************************************************
011200140613      /FREE
011300140613            SQL_DropAlias('FIDSTXXF':'QTEMP');
011400140613            if SQL_CreaAlias('FIDSTXXF':'QTEMP':'FIDST00F':
011500140613                              lib001ala :' ':piOutEsito) <> *zeros;
011600140613            return;
011700140613            endif;
011800140613      /END-FREE
011900140320     c                   movel     *ALL'-'       TRA
012000140320     c                   movel     *ALL'-'       TRA1
012100140320     c                   movel     f16dti        da
012200140320     c                   movel     f16dtf        a
012300030310     C/EXEC SQL
012400140515     C+ DELETE FROM qtemp/wfiplg0f
012500120329     C/END-EXEC
012600140307     C/EXEC SQL
012700140515     C+ DELETE FROM qtemp/wfiplgoraf
012800140307     C/END-EXEC
012900140613     c*
013000140613     C/EXEC SQL
013100140613      *------------------------------------------------
013200140613      * dati senza errori di data esitazione CONSEGNE
013300140613      *------------------------------------------------
013400140613     C+ INSERT INTO qtemp/wfiplg0f
013500140613     C+ SELECT PLGTIPORCD, PLGTIPAPPL,
013600120329     C+ PLGPRFC, PLGIDDISP, PLGDATORA, PLGCODAUT, PLGFGS, PLGNDC,
013700140403     C+ PLGIDDOC, PLGDATI, PLGERR, PLGMSG, SUBSTR(PLGDATI, 204, 8),
013800140403     C+ SUBSTR(PLGDATI, 212, 6), 'D'
013900140613     C+ FROM FIdstxxF join fiplg00f on
014000140613     C+ dstfgs = plgfgs and
014100140613     C+ dstnfv = plgndc and
014200140613     C+ dstpdr = plgcodaut and
014300140613     C+ 'CET' = plgtiporcd
014400140613     C+ WHERE dsttipa = 'A' and
014500140613     C+ dstfgs between :f16i and :f16po and
014600140613     C+ dstdfv between :da and :a and
014700140613     C+ dstpdr between :f16cda and :f16ca and dstfcf = 'S' and
014800140616     C+ dstpda in ('C', 'E') and dstatb = ' ' and
014900121115     C+ PLGTIPAPPL = 'C' and plgerr = ' ' and
015000121115     C+ substr(plgdati, 218, 1) = ' '
015100121127     C+ and plgMSG not like '%DATA < o > di 10 GG%'
015200120329     C/END-EXEC
015300140613      *------------------------------------------------
015400140613      * dati senza errori di data esitazione RITIRI
015500140613      *------------------------------------------------
015600120329     C/EXEC SQL
015700140515     C+ INSERT INTO qtemp/wfiplg0f  SELECT PLGTIPORCD, PLGTIPAPPL,
015800120329     C+ PLGPRFC, PLGIDDISP, PLGDATORA, PLGCODAUT, PLGFGS, PLGNDC,
015900120329     C+ PLGIDDOC, PLGDATI, PLGERR, PLGMSG, SUBSTR(PLGDATI, 24, 8),
016000121205     C+ SUBSTR(PLGDATI, 32, 6), 'D'
016100140613     C+ FROM FIdstxxF join fiplg00f on
016200140613     C+ dstfgs = plgfgs and
016300140613     C+ dstnfv = plgndc and
016400140613     C+ dstpdr = plgcodaut and
016500140613     C+ 'RES' = plgtiporcd
016600140613     C+ WHERE dsttipa = 'A' and
016700140613     C+ dstfgs between :f16i and :f16po and
016800140613     C+ dstdfv between :da and :a and
016900140613     C+ dstpdr between :f16cda and :f16ca and dstfcf = 'S' and
017000140616     C+ dstpda in ('O', 'E') and dstatb = ' ' and
017100140613     C+ PLGTIPAPPL = 'R' and plgerr = ' ' and
017200140307     C+ substr(plgdati, 185, 1) = ' '
017300140307     C+ and substr(plgiddoc, 1, 8)  <> '00000000'
017400121127     C+ and plgMSG not like '%DATA < o > di 10 GG%'
017500120329     C/END-EXEC
017600140310      *------------------------------------------------
017700140613      * POI prende i dati con errori di data esitazione consegne
017800140310      *------------------------------------------------
017900121127     C/EXEC SQL
018000140515     C+ INSERT INTO qtemp/wfiplg0f  SELECT PLGTIPORCD, PLGTIPAPPL,
018100121127     C+ PLGPRFC, PLGIDDISP, PLGDATORA, PLGCODAUT, PLGFGS, PLGNDC,
018200121127     C+ PLGIDDOC, PLGDATI, PLGERR, PLGMSG, substr(plgdatora, 1, 8),
018300121205     C+ SUBSTR(PLGDATI, 37, 6), 'D'
018400140613     C+ FROM fidstxxf join FIPLG00F on
018500140613     C+ dstfgs = plgfgs and
018600140613     C+ dstnfv = plgndc and
018700140613     C+ dstpdr = plgcodaut and
018800140613     C+ 'CET' = plgtiporcd
018900140613     C+ WHERE dsttipa = 'A' and
019000140613     C+ dstfgs between :f16i and :f16po and
019100140613     C+ dstdfv between :da and :a and
019200140613     C+ dstpdr between :f16cda and :f16ca and dstfcf = 'S' and
019300140616     C+ dstpda in ('C', 'E') and dstatb = ' ' and
019400121127     C+ PLGTIPAPPL = 'C' and plgerr = ' ' and
019500121127     C+ substr(plgdati, 218, 1) = ' '
019600121127     C+ and plgMSG like '%DATA < o > di 10 GG%'
019700121127     C/END-EXEC
019800140613      *------------------------------------------------
019900140613      * POI prende i dati con errori di data esitazione ritiri
020000140613      *------------------------------------------------
020100121127     C/EXEC SQL
020200140515     C+ INSERT INTO qtemp/wfiplg0f  SELECT PLGTIPORCD, PLGTIPAPPL,
020300121127     C+ PLGPRFC, PLGIDDISP, PLGDATORA, PLGCODAUT, PLGFGS, PLGNDC,
020400121127     C+ PLGIDDOC, PLGDATI, PLGERR, PLGMSG, substr(plgdatora, 1, 8),
020500121205     C+ SUBSTR(PLGDATI, 32, 6), 'D'
020600140613     C+ FROM fidstxxf join FIPLG00F on
020700140613     C+ dstfgs = plgfgs and
020800140613     C+ dstnfv = plgndc and
020900140613     C+ dstpdr = plgcodaut and
021000140613     C+ 'RES' = plgtiporcd
021100140613     C+ WHERE dsttipa = 'A' and
021200140613     C+ dstfgs between :f16i and :f16po and
021300140613     C+ dstdfv between :da and :a and
021400140613     C+ dstpdr between :f16cda and :f16ca and dstfcf = 'S' and
021500140616     C+ dstpda in ('O', 'E') and dstatb = ' ' and
021600140613     C+ substr(plgiddoc, 1, 8)  <> '00000000'
021700121127     C+ and PLGTIPAPPL = 'R' and plgerr = ' ' and
021800121127     C+ substr(plgdati, 185, 1) = ' '
021900121127     C+ and plgMSG like '%DATA < o > di 10 GG%'
022000121127     C/END-EXEC
022100140613      *------------------------------------------------
022200140613      * dati senza errori di data esitazione ORA STIMATA CONSEGNE
022300140613      *------------------------------------------------
022400140613     C/EXEC SQL
022500140613     C+ INSERT INTO qtemp/wfiplgoraf  SELECT PLGTIPORCD, PLGTIPAPPL,
022600140613     C+ PLGPRFC, PLGIDDISP, PLGDATORA, PLGCODAUT, PLGFGS, PLGNDC,
022700140613     C+ PLGIDDOC, PLGDATI, PLGERR, PLGMSG, SUBSTR(PLGDATI, 26, 8),
022800140613     C+ SUBSTR(PLGDATI, 34, 6)
022900140613     C+ FROM fidstxxf join FIPLG00F on
023000140613     C+ dstfgs = plgfgs and
023100140613     C+ dstnfv = plgndc and
023200140613     C+ dstpdr = plgcodaut and
023300140613     C+ 'COR' = plgtiporcd
023400140613     C+ WHERE dsttipa = 'A' and
023500140613     C+ dstfgs between :f16i and :f16po and
023600140613     C+ dstdfv between :da and :a and
023700140613     C+ dstpdr between :f16cda and :f16ca and dstfcf = 'S' and
023800140616     C+ dstpda in ('C', 'E') and dstatb = ' ' and
023900140613     C+ PLGTIPAPPL = 'C' and plgerr = ' ' and
024000140613     C+ substr(plgdati, 218, 1) = ' '
024100140613     C+ and plgMSG not like '%DATA < o > di 10 GG%'
024200140613     C/END-EXEC
024300140613      *------------------------------------------------
024400140613      * dati senza errori di data esitazione ORA STIMATA RITIRI
024500140613      *------------------------------------------------
024600140613     C/EXEC SQL
024700140613     C+ INSERT INTO qtemp/wfiplgoraf  SELECT PLGTIPORCD, PLGTIPAPPL,
024800140613     C+ PLGPRFC, PLGIDDISP, PLGDATORA, PLGCODAUT, PLGFGS, PLGNDC,
024900140613     C+ PLGIDDOC, PLGDATI, PLGERR, PLGMSG, SUBSTR(PLGDATI, 26, 8),
025000140613     C+ SUBSTR(PLGDATI, 34, 6)
025100140613     C+ FROM fidstxxf join FIPLG00F on
025200140613     C+ dstfgs = plgfgs and
025300140613     C+ dstnfv = plgndc and
025400140613     C+ dstpdr = plgcodaut and
025500140613     C+ 'ROR' = plgtiporcd
025600140613     C+ WHERE dsttipa = 'A' and
025700140613     C+ dstfgs between :f16i and :f16po and
025800140613     C+ dstdfv between :da and :a and
025900140613     C+ dstpdr between :f16cda and :f16ca and dstfcf = 'S' and
026000140616     C+ dstpda in ('O', 'E') and dstatb = ' ' and
026100140613     C+ PLGTIPAPPL = 'R' and plgerr = ' ' and
026200140613     C+ substr(plgdati, 185, 1) = ' '
026300140613     C+ and substr(plgiddoc, 1, 8)  <> '00000000'
026400140613     C+ and plgMSG not like '%DATA < o > di 10 GG%'
026500140613     C/END-EXEC
026600140613      *------------------------------------------------
026700140613      * POI prende i dati con errori di data esitazione ORA STIMATA CONSEGNE
026800140613      *------------------------------------------------
026900140307     C/EXEC SQL
027000140515     C+ INSERT INTO qtemp/wfiplgoraf  SELECT PLGTIPORCD, PLGTIPAPPL,
027100140307     C+ PLGPRFC, PLGIDDISP, PLGDATORA, PLGCODAUT, PLGFGS, PLGNDC,
027200140307     C+ PLGIDDOC, PLGDATI, PLGERR, PLGMSG, substr(plgdatora, 1, 8),
027300140613     C+ SUBSTR(PLGDATI, 34, 6)
027400140613     C+ FROM fidstxxf join FIPLG00F on
027500140613     C+ dstfgs = plgfgs and
027600140613     C+ dstnfv = plgndc and
027700140613     C+ dstpdr = plgcodaut and
027800140613     C+ 'COR' = plgtiporcd
027900140613     C+ WHERE dsttipa = 'A' and
028000140613     C+ dstfgs between :f16i and :f16po and
028100140613     C+ dstdfv between :da and :a and
028200140613     C+ dstpdr between :f16cda and :f16ca and dstfcf = 'S' and
028300140616     C+ dstpda in ('C', 'E') and dstatb = ' ' and
028400140307     C+ PLGTIPAPPL = 'C' and plgerr = ' ' and
028500140307     C+ substr(plgdati, 218, 1) = ' '
028600140307     C+ and plgMSG like '%DATA < o > di 10 GG%'
028700140307     C/END-EXEC
028800140613      *------------------------------------------------
028900140613      * POI prende i dati con errori di data esitazione ORA STIMATA RITIRI
029000140613      *------------------------------------------------
029100140307     C/EXEC SQL
029200140515     C+ INSERT INTO qtemp/wfiplgoraf  SELECT PLGTIPORCD, PLGTIPAPPL,
029300140307     C+ PLGPRFC, PLGIDDISP, PLGDATORA, PLGCODAUT, PLGFGS, PLGNDC,
029400140307     C+ PLGIDDOC, PLGDATI, PLGERR, PLGMSG, substr(plgdatora, 1, 8),
029500140307     C+ SUBSTR(PLGDATI, 34, 6)
029600140613     C+ FROM fidstxxf join FIPLG00F on
029700140613     C+ dstfgs = plgfgs and
029800140613     C+ dstnfv = plgndc and
029900140613     C+ dstpdr = plgcodaut and
030000140613     C+ 'ROR' = plgtiporcd
030100140613     C+ WHERE dsttipa = 'A' and
030200140613     C+ dstfgs between :f16i and :f16po and
030300140613     C+ dstdfv between :da and :a and
030400140613     C+ dstpdr between :f16cda and :f16ca and dstfcf = 'S' and
030500140616     C+ dstpda in ('O', 'E') and dstatb = ' ' and
030600140613     C+ PLGTIPAPPL = 'R' and plgerr = ' ' and
030700140307     C+ substr(plgdati, 185, 1) = ' '
030800140307     C+ and substr(plgiddoc, 1, 8)  <> '00000000'
030900140307     C+ and plgMSG like '%DATA < o > di 10 GG%'
031000140307     C/END-EXEC
031100121127
031200120329     C/EXEC SQL
031300140307     C+ DECLARE FIPLG CURSOR FOR SELECT DISTINCT * FROM wfiplg0f ORDER BY
031400140307     C+ wrkFGS, wrkNDC, wrkCODAUT, WRKIDDOC, WRKDATA, WRKORA
031500120329     C/END-EXEC
031600030314
031700030314     C/EXEC SQL
031800120329     C+ OPEN fiplg
031900030314     C/END-EXEC
032000030325     C                   DOU       sqlcod <> 0
032100030314     C/EXEC SQL
032200120330     C+ FETCH NEXT FROM fiplg  INTO :wfiplgds
032300030314     C/END-EXEC
032400030314
032500030314     C                   SELECT
032600030314     C                   WHEN      SQLCod = 100
032700140312     c                   exsr      srdis
032800120605     c                   leave
032900030314     C                   WHEN      SQLCod < 0
033000030314     C                   EVAL      *INH1 = *ON
033100030314     C                   OTHER
033200140307     c* rottura di distinta aut
033300140310     c                   eval      comfgs = wrkfgs
033400140310     c                   eval      comaut = wrkcodaut
033500140310     c                   eval      comndc = wrkndc
033600140310     c                   movel     dsdis         distinta
033700140307     c                   if        distinta <> savdistinta
033800140307     c                   exsr      srdis
033900140312     c                   else
034000140307     C*
034100130128     c                   if        record_OK = 'S'
034200030314     c                   exsr      leggi
034300130128     c                   endIF
034400140312     c                   endIF
034500030314     C                   ENDSL
034600030314
034700030314     C                   ENDDO
034800030314     C/EXEC SQL
034900120329     C+ CLOSE fiplg
035000030314     C/END-EXEC
035100030310
035200140310     c                   close     wfnlvp17f                            99
035300140310     c                   close     fnlvp17p                             99
035400140613      /FREE
035500140613            SQL_DropAlias('FIDSTXXF':'QTEMP');
035600140613      /END-FREE
035700030314     C                   SETON                                        LR
035800140307     C*--------------------------------------------------------
035900140320     c* a rottura di distinta
036000140307     C     srdis         begsr
036100140307     C*--------------------------------------------------------
036200140312     c                   exsr      srrottura
036300140310     c* se presenti esiti
036400140310     c                   if        v1esic <> 0 or v1esir <> 0
036500140320     c* calcolo le percentuali. l'indicatore 04 serve per stampare la
036600140320     c* riga di dettaglio, se le percentuali calcolate di seguito non
036700140320     c* corrispondono ai parametri scelti, verrà spento nella subroutine
036800140320     c                   seton                                        04
036900140319     c                   exsr      srperc
037000140310     c                   if        f16stafil = 'S'
037100140310     c                   exsr      srstampa
037200140310     c                   else
037300140310     c                   exsr      srfile
037400140310     c                   end
037500140310     c                   end
037600140320     c* stampo il totale filiale
037700140320     c                   if        f16stafil = 'S'
037800140321     c                   if        (comfgs <> savfgs and savfgs <> 0 ) or
037900140321     c                             (sqlcod = 100 and savfgs <> 0)
038000140320     c                   seton                                        03
038100140320     c                   exsr      srperct
038200140320     c                   exsr      srstampa
038300140320     c                   end
038400140320     c* fine file, stampo fine stampa
038500140320     c                   if        sqlcod = 100
038600140320     c                   seton                                        02
038700140320     c                   exsr      srstampa
038800140320     c                   end
038900140320     c                   end
039000140320     c* fine file esco
039100140320     c                   if        sqlcod = 100
039200140320     c                   leavesr
039300140320     c                   end
039400140320     c* rottura di distinta: azzero i campi di totale
039500140328     c                   clear                   v1geoc
039600140328     c                   clear                   v1geor
039700140307     c                   clear                   v1esic
039800140307     c                   clear                   v1esir
039900140307     c                   clear                   v1primac
040000140307     c                   clear                   v1dopoc
040100140307     c                   clear                   v1oraokc
040200140307     c                   clear                   v1oraokr
040300140307     c                   clear                   datacus
040400140307     c                   clear                   oracus
040500140307     c                   eval      record_OK = 'S'
040600140307     c                   eval      savdistinta = distinta
040700140307     c* parcel/messaggeria oppure test si/no
040800140310     c                   clear                   v1test
040900140307     C     Kdst          chain     fiDST01l
041000140307     c                   if        %Found(fiDST01l)
041100140307     c                   If        f16prcMES='P' and dstTPM <> *blank
041200140307     c                              or
041300140307     c                             f16prcMES='M' and dstTPM <> 'M'
041400140307     c                              or
041500140307     c                             f16test='N' and %subst(dstflr:1:1) <> ' '
041600140307      *
041700140307     c                   eval      record_OK = 'N'
041800140312     c                   clear                   v1esi
041900140312     c                   clear                   v1accura
042000140327     c                   clear                   v1geo
042100140307     c                   leavesr
042200140325     c                   end
042300140310     c                   if        %subst(dstflr:1:1) <> ' '
042400140310     c                   eval      v1test = 'T'
042500140310     c                   end
042600140310     c                   move      dstfgs        v1fgs
042700140310     c                   move      dstnfv        v1ndc
042800140310     c                   move      dstdfv        dataiso
042900140310     c                   move      dataiso       dataeur
043000140310     c                   move      dataeur       v1ddc
043100140515     c                   else
043200140515     c                   eval      record_OK = 'N'
043300140515     c                   clear                   v1esi
043400140515     c                   clear                   v1accura
043500140515     c                   clear                   v1geo
043600140515     c                   leavesr
043700140310     c                   End
043800140515     c* reperisco la data e ora fase CUS, se non la trovo continuo
043900140515     c* perchè potrebbe essere una distinta di solo ritiri
044000140307     c     kplg          setgt     fiplg02l
044100140307     c     kplg          readpe    fiplg02l
044200140515     c                   if        %eof(fiplg02l)
044300140515     c*                  eval      record_OK = 'N'
044400140515     c*                  clear                   v1esi
044500140515     c*                  clear                   v1accura
044600140515     c*                  clear                   v1geo
044700140515     c*                  leavesr
044800140312     c                   else
044900140307     c                   movel     plgdati       fiprdcusds
045000140307     c                   if        plgerr = ' ' and
045100140307     c                             plgMSG ='DATA < o > di 10 GG'
045200140307     c                   eval      datacus = %subst(plgdatora: 1: 8)
045300140307     c                   else
045400140310     c                   movel     §CUSDTORIN    datacus
045500140307     c                   end
045600140310     c                   move      §CUSDTORIN    oracus
045700140307     c                   endIF
045800140307     c* reperisco anagrafica aut
045900140307     c     kapd          chain     fiapd01l
046000140307     c                   if        not %found(fiapd01l)
046100140310     c                   clear                   v1rsf
046200140310     c                   else
046300140310     c                   movel     apdrsf        v1rsf
046400140310     c                   movel     apdpdr        v1codaut
046500140307     c                   endIF
046600140320     c* ROTTURA DI FILIALE
046700140319     c                   if        comfgs <> savfgs
046800140320     c                   seton                                        01
046900140310     c     wrkfgs        chain     azorg01l
047000140310     c                   if        not %found(fiapd01l)
047100140310     c                   clear                   v1des
047200140310     c                   else
047300140310     c                   movel     orgdes        v1des
047400140310     c                   endIF
047500140319     c                   eval      v1area =orgcar
047600140319     c                   eval      v1dis =orgfl3
047700140320     c* distretto
047800140319     c                   eval      tblcod = '17'
047900140319     c                   eval      tblkey = orgfl3
048000140319     c     ktab          chain     tabel00f
048100140319     c                   if        not %found(tabel00f) or tblflg <> *blanks
048200140325     c                   eval      savdisd = *all'*'
048300140319     c                   else
048400140320     c                   eval      savdisd = tbluni
048500140319     c                   endif
048600140319      * area
048700140319     c                   eval      tblcod = '05'
048800140319     c                   movel     orgcar        tblkey
048900140319     c     ktab          chain     tabel00f
049000140319     c                   if        not %found(tabel00f) or tblflg <> *blanks
049100140325     c                   eval      savcard = *all'*'
049200140319     c                   else
049300140320     c                   eval      savcard = tbluni
049400140319     c                   endif
049500140320     c                   clear                   vtesic
049600140320     c                   clear                   vtprimac
049700140320     c                   clear                   vtoraokc
049800140320     c                   clear                   vtdopoc
049900140320     c                   clear                   vtesir
050000140320     c                   clear                   vtoraokr
050100140320     c                   clear                   taut
050200140319     c                   endIF
050300140319     c                   eval      savfgs = comfgs
050400140307     c*
050500140319     c                   endsr
050600030314     C*--------------------------------------------------------
050700140307     C     leggi         begsr
050800030314     C*--------------------------------------------------------
050900140310     c                   if        wrkiddoc = saviddoc
051000140307     c                   exsr      srstesso
051100140307     c                   else
051200140307     c                   exsr      srrottura
051300140307     c                   end
051400140307     c                   endsr
051500120330     C*--------------------------------------------------------
051600140320     c* a rottura di documento
051700140307     C     srrottura     BEGSR
051800120330     C*--------------------------------------------------------
051900140307     c* se il documento prima è stato conteggiato
052000140307     c                   if        v1esi <> 0
052100140320     c* consegna
052200140307     c                   if        savtipo = 'C'
052300140312     c                   add       1             v1esic
052400140328     c                   if        v1geo <> 0
052500140328     c                   add       1             v1geoc
052600140328     c                   end
052700140312     c                   if        v1accura <> 0
052800140312     c                   add       1             v1oraokc
052900140312     c                   end
053000140307     c                   else
053100140320     c* ritiro
053200140328     c                   add       1             v1esir
053300140328     c                   if        v1geo <> 0
053400140328     c                   add       1             v1geor
053500140328     c                   end
053600140312     c                   if        v1accura <> 0
053700140312     c                   add       1             v1oraokr
053800140312     c                   end
053900140307     c                   end
054000140328     c                   clear                   v1geo
054100140328     c                   clear                   v1esi
054200140326     c                   clear                   v1accura
054300140320     c* verifico l'ora stimata solo per consegna
054400140312     c                   if        savdataesi = datacus
054500140319     c                             and savtipo = 'C'
054600140312     c                   if        orasti <> ' '
054700140310     c                   add       1             v1primac
054800140312     c                   end
054900140310     c                   add       nora          v1dopoc
055000140307     c                   end
055100140319     c                   end
055200140320     c* per fine file esco
055300140320     c                   if        sqlcod = 100
055400140320     c                   leavesr
055500140320     c                   end
055600140307     c* cerco ora stimata del nuovo documento
055700140307     c                   exsr      srcorror
055800140327     c* controllo coordinate GEO
055900140326     c                   if        %subst(wrkdati: 220: 16) <> ' ' and
056000140326     c                             %subst(wrkdati: 220: 16) <>
056100140327     c                             '0000000000000000'
056200140327     c                   z-add     1             v1geo
056300140326     c                   end
056400140307     c* considero il record solo se codice mancata consegna <> da non fatta
056500140320     c* quindi leggo le tabelle relative al tipo documento
056600140310     c                   if        wrktipappl = 'C'
056700140307     c                   exsr      srtab2a
056800140307     c                   else
056900140307     c                   exsr      srtabCMR
057000140307     c                   end
057100140327     c* se da considerare forzo 1 flag del conteggio, per l'accuratezza
057200140327     c* tengo buono il 1° CET/RES valido
057300140307     c                   if        tabok = 'S'
057400140312     c                   z-add     1             v1esi
057500140307     c                   exsr      sraccura
057600140310     c                   eval      savdataesi = wrkdata
057700140310     c                   eval      savdatora = wrkdatora
057800140307     c                   end
057900140320     c*
058000140312     c                   eval      savtipo = wrktipappl
058100140312     c                   eval      saviddoc = wrkiddoc
058200140311     c                   eval      savtabok = tabok
058300140307     c                   endsr
058400140307     C*--------------------------------------------------------
058500140320     c* stesso documento
058600140307     C     srstesso      BEGSR
058700140307     C*--------------------------------------------------------
058800140327     c* controllo coordinate GEO, devo tenere buone le coordinate dell'ultimo
058900140327     c* esito
059000140326     c                   if        %subst(wrkdati: 220: 16) <> ' ' and
059100140326     c                             %subst(wrkdati: 220: 16) <>
059200140327     c                             '0000000000000000'
059300140327     c                   z-add     1             v1geo
059400140328     c                   else
059500140328     c                   clear                   v1geo
059600140326     c                   end
059700140326     c*
059800140310     c                   if        wrktipappl = 'C'
059900140307     c                   exsr      srtab2a
060000140307     c                   else
060100140307     c                   exsr      srtabCMR
060200140307     c                   end
060300140320      * x stesso doc.: se record arriva in meno di sei minuti buono l'ultimo
060400140310     c                   movel     wrkdatora     data
060500140310     c                   move      wrkdatora     comora
060600140310     c                   movel     comora        ora
060700140310     c                   movel     savdatora     savdata
060800140310     c                   move      savdatora     comora
060900140310     c                   movel     comora        savora
061000140310     c     ora           subdur    savora        minuti:*mn
061100140327     c* se esito non è da considerare pulisco i flag di conteggiato
061200140327     c                   if        minuti < 6 and data = savdata and
061300140327     c                             tabok = 'N' and
061400140311     c                             savtabok = 'S'
061500140312     c                   clear                   v1esi
061600140312     c                   clear                   v1accura
061700140327     c                   clear                   v1geo
061800140311     c                   endif
061900140327     c* esito da considerare perchè oltre i 6 e non ho ancora conteggiato
062000140327     c* verifico l'esito forzando 1 nel flag del conteggio verificandone
062100140327     c* l'accuratezza
062200140327     c* BUONO IL 1° CET/RES VALIDO
062300140312     c                   if        tabok = 'S' and v1esi = 0
062400140312     c                   z-add     1             v1esi
062500140307     c* verifico se orario stimato è accurato
062600140307     c                   exsr      sraccura
062700140310     c                   eval      savdatora = wrkdatora
062800140312     c                   eval      savdataesi = wrkdata
062900140312     c                   end
063000140311     c                   eval      savtabok = tabok
063100140307     c                   endsr
063200140307     C*--------------------------------------------------------
063300140307     C     srcorror      BEGSR
063400140307     C*--------------------------------------------------------
063500140307     c                   clear                   orasti
063600140307     c                   clear                   nora
063700140415     c* reperisco la data e ora fase CUS se non ancora rilevata
063800140415     c                   if        distinta <> savdistinta
063900140415     c     kplg          setgt     fiplg02l
064000140415     c     kplg          readpe    fiplg02l
064100140515     c                   if        %eof(fiplg02l)
064200140515     c                   clear                   datacus
064300140515     c                   clear                   oracus
064400140415     c                   else
064500140415     c                   movel     plgdati       fiprdcusds
064600140415     c                   if        plgerr = ' ' and
064700140415     c                             plgMSG ='DATA < o > di 10 GG'
064800140415     c                   eval      datacus = %subst(plgdatora: 1: 8)
064900140415     c                   else
065000140415     c                   movel     §CUSDTORIN    datacus
065100140415     c                   end
065200140415     c                   move      §CUSDTORIN    oracus
065300140415     c                   endIF
065400140415     c                   endIF
065500140320     c* orasti = ora stimata del documento
065600140320     c* per le consegne sarà l'utima ora inserita prima dell'ok ad uscire
065700140320     c* per i ritiri è la prima ora stimata inserita
065800140310     c* nora   = nr. dei record con ora stimata dopo l'ok ad uscire
065900140320     c*
066000140320     c* CONSEGNA
066100140307     c                   if        wrktipappl = 'C'
066200140310     c* oraora = ora di esitazione del record COR
066300140307     C/EXEC SQL
066400140310     C+ SELECT substr(oradati, 41, 4) INTO :orasti FROM wfiplgoraf WHERE
066500140307     C+ ORAora = ( SELECT max(oraora) FROM wfiplgoraf WHERE ORATIPORCD =
066600140307     C+ 'COR' and ORACODAUT = :wrkCODAUT and ORAFGS = :wrkFGS and ORANDC
066700140311     C+ = :wrkNDC and ORADATA = :datacus and oraora <= :oracus and
066800140307     C+ ORAIDDOC = :wrkIDDOC ) and ORATIPORCD = 'COR' and ORACODAUT =
066900140307     C+ :wrkCODAUT and ORAFGS = :wrkFGS and ORANDC = :wrkNDC and ORADATA
067000140311     C+ = :datacus and oraora <= :oracus and ORAIDDOC = :wrkIDDOC
067100140307     C/END-EXEC
067200140307     C/EXEC SQL
067300140307     C+ SELECT count(*)   INTO :nora FROM wfiplgoraf WHERE ORATIPORCD
067400140307     C+ = 'COR' and ORACODAUT = :wrkCODAUT and ORAFGS = :wrkFGS and
067500140310     C+ ORANDC = :wrkNDC and ORADATA = :datacus and oraora > :oracus and
067600140307     C+ ORAIDDOC = :wrkIDDOC
067700140307     C/END-EXEC
067800140307     c                   else
067900140320     c*RITIRO
068000140320     c*
068100140320     c* oraora = ora esitaz. del 1° record ROR
068200140307     C/EXEC SQL
068300140310     C+ SELECT substr(oradati, 41, 4) INTO :orasti FROM wfiplgoraf WHERE
068400140320     C+ ORAora = ( SELECT min(oraora) FROM wfiplgoraf WHERE ORATIPORCD =
068500140307     C+ 'ROR' and ORACODAUT = :wrkCODAUT and ORAFGS = :wrkFGS and ORANDC
068600140515     C* = :wrkNDC and ORADATA = :datacus and
068700140515     C+ = :wrkNDC and ORAIDDOC = :wrkIDDOC ) and ORATIPORCD = 'ROR' and
068800140515     C+ ORACODAUT = :wrkCODAUT and ORAFGS = :wrkFGS and ORANDC = :wrkNDC
068900140515     C* and ORADATA = :datacus and ORAIDDOC = :wrkIDDOC
069000140515     C+ and ORAIDDOC = :wrkIDDOC
069100140307     C/END-EXEC
069200140307     c                   end
069300140320     c                   clear                   sqlcod
069400140307     c                   endsr
069500140310     C*--------------------------------------------------------
069600140310     C     sraccura      BEGSR
069700140310     C*--------------------------------------------------------
069800140310     c                   clear                   v1accura
069900140515     c* solo per consegna verifico che l'esito sia congruente con la data
070000140515     c* del cus
070100140515     c                   if        WRKTIPAPPL ='C' and datacus <> wrkdata
070200140515     c                   leavesr
070300140515     c                   end
070400140515     c                   if        orasti <> ' '
070500140310     c                   clear                   comora
070600140320     c                   movel     orasti        comora
070700140310     c                   movel     comora        comorasti
070800140320     c                   movel     wrkora        comora
070900140310     c                   movel     comora        comwrkora
071000140312     c     comwrkora     subdur    comorasti     minsco:*mn
071100140310     c                   if        minsco < 0
071200140312     c     minsco        mult      -1            piuminsco
071300140310     c                   end
071400140310     c                   if        minsco >=0 and minsco <= f16scop
071500140327     c                   z-add     1             v1accura
071600140310     c                   end
071700140310     c                   if        minsco < 0 and piuminsco <= f16scom
071800140327     c                   z-add     1             v1accura
071900140310     c                   end
072000140310     c                   end
072100140310     c                   endsr
072200140307     C*--------------------------------------------------------
072300140310     C     srtab2a       BEGSR
072400140307     C*--------------------------------------------------------
072500140310     c                   eval      tabok = 'S'
072600140310     c                   if        %subst(wrkdati:26:3) <> ' '
072700140310     c                   eval      comcmc = %subst(wrkdati:26:3)
072800140310     c                   movel     comcmc        tblkey
072900140319     c                   eval      tblcod = '2A'
073000140310     c     ktab          chain     tabel00f
073100140310     c                   if        %found(tabel00f) and tblflg = ' '
073200140310     c                   movel(p)  tbluni        ds2a
073300140310     c                   movel     §2antc        tabok
073400140310     c                   end
073500140310     c                   end
073600030314     C                   ENDSR
073700140310     C*--------------------------------------------------------
073800140310     C     srtabcmr      BEGSR
073900140310     C*--------------------------------------------------------
074000140310     c                   eval      tabok = 'S'
074100140310     c                   if        %subst(wrkdati:83:3) <> ' '
074200140310     c                   eval      comcmc = %subst(wrkdati:83:3)
074300140310     c                   movel     comcmc        tbeke1
074400140320     c                   eval      tbecod = 'CMR'
074500140310     c     ktbe          chain     tntbe01l
074600140310     c                   if        %found(tntbe01l) and tbeatb = ' '
074700140310     c                   movel(p)  tbeuni        dcmr
074800140310     c                   if        D§CMRREA = 'S'
074900140310     c                   eval      tabok = 'N'
075000140310     c                   end
075100140310     c                   end
075200140310     c                   end
075300140310     C                   ENDSR
075400140310     C*--------------------------------------------------------
075500140310     C     srstampa      BEGSR
075600140310     C*--------------------------------------------------------
075700140310     c* testa
075800140312     c                   if        *in01
075900140310     c                   write     testa
076000140310     c                   write     tesdis
076100140312     c                   setoff                                       01
076200140310     c                   end
076300140320     c     vpprimac      comp      0                                  1010
076400140320     c     vporaokc      comp      0                                  1212
076500140320     c     vporaokr      comp      0                                  1313
076600140320     c     tpprimac      comp      0                                  2020
076700140320     c     tporaokc      comp      0                                  2222
076800140320     c     tporaokr      comp      0                                  2323
076900140320     c* dettaglio
077000140320     c   04              write     DET
077100140320     c* totali
077200140320     c   03              write     totali
077300140320     c* fine stampa
077400140320     c   02              write     fine
077500140320     c*
077600140320     c                   setoff                                       030402
077700140310     C                   ENDSR
077800140319     C*--------------------------------------------------------
077900140319     C     srperc        BEGSR
078000140319     C*--------------------------------------------------------
078100140320     c                   clear                   cprimac
078200140320     c                   clear                   coraokc
078300140324     c                   clear                   coraokr
078400140320     c                   clear                   vpprimac
078500140320     c                   clear                   vporaokc
078600140320     c                   clear                   vporaokr
078700140319     c* CONSEGNE
078800140319     c                   if         v1esic <> 0
078900140319     c* l'istruzione seguente mi serve per determinare quante spedizioni
079000140319     c* non hanno l'ora stimata prima dell'ok. utilizzo lo stesso campo
079100140320     c* perchè poi lo devo riporto nel file (se richiesto)
079200140320     c     v1esic        sub       v1primac      cprimac
079300140320     c* % ore NON inserite prima dell'ok ad uscire
079400140320     c                   if        cprimac <> 0
079500140320     c                   eval(h)   vpprimac = cprimac * 100/v1esic
079600140319     c                   end
079700140319     c* l'istruzione seguente mi serve per determinare quante spedizioni
079800140319     c* sono fuori range per accuratezza. utilizzo lo stesso campo
079900140320     c* perchè poi lo devo riporto nel file (se richiesto)
080000140320     c     v1esic        sub       v1oraokc      coraokc
080100140320     c* % delle ore fuori range
080200140320     c                   if        coraokc <> 0
080300140320     c                   eval(h)   vporaokc = coraokc* 100/v1esic
080400140319     c                   end
080500140319     c                   end
080600140320     c* se la % è < del parametro scelto, non stampo
080700140320     c                   if        vpprimac < f16primac or
080800140320     c                             vporaokc < f16oraokc
080900140320     c                   setoff                                       04
081000140320     c                   leavesr
081100140320     c                   end
081200140320     c* totalizzo x filiale
081300140320     c                   add       v1esic        vtesic
081400140320     c                   add       v1primac      vtprimac
081500140320     c                   add       v1oraokc      vtoraokc
081600140320     c                   add       v1dopoc       vtdopoc
081700140320     c                   add       v1esir        vtesir
081800140320     c                   add       v1oraokr      vtoraokr
081900140320     c                   add       1             taut
082000140319     c* RITIRI
082100140319     c                   if         v1esir <> 0
082200140319     c* l'istruzione seguente mi serve per determinare quanti orm
082300140319     c* sono fuori range per accuratezza. utilizzo lo stesso campo
082400140320     c* perchè poi lo devo riporto nel file (se richiesto)
082500140324     c     v1esir        sub       v1oraokr      coraokr
082600140320     c* % delle ore fuori range
082700140324     c                   if        coraokr <> 0
082800140324     c                   eval(h)   vporaokr = coraokr* 100/v1esir
082900140319     c                   end
083000140319     c                   end
083100140319     C                   ENDSR
083200140320     C*--------------------------------------------------------
083300140320     C     srperct       BEGSR
083400140320     C*--------------------------------------------------------
083500140320     c                   clear                   tpprimac
083600140320     c                   clear                   tporaokc
083700140320     c                   clear                   tporaokr
083800140320     c* CONSEGNE
083900140320     c                   if         vtesic <> 0
084000140320     c* l'istruzione seguente mi serve per determinare quante spedizioni
084100140320     c* non hanno l'ora stimata prima dell'ok. utilizzo lo stesso campo
084200140320     c* perchè poi lo devo riporto nel file (se richiesto)
084300140320     c     vtesic        sub       vtprimac      vtprimac
084400140320     c* % delle NON indicate
084500140320     c                   if        vtprimac <> 0
084600140320     c                   eval(h)   tpprimac = vtprimac * 100/vtesic
084700140320     c                   end
084800140320     c* l'istruzione seguente mi serve per determinare quante spedizioni
084900140320     c* sono fuori range per accuratezza. utilizzo lo stesso campo
085000140320     c* perchè poi lo devo riporto nel file (se richiesto)
085100140320     c     vtesic        sub       vtoraokc      vtoraokc
085200140320     c* % delle fuori range
085300140320     c                   if        vtoraokc <> 0
085400140320     c                   eval(h)   tporaokc = vtoraokc* 100/vtesic
085500140320     c                   end
085600140320     c                   end
085700140320     c* RITIRI
085800140320     c                   if         vtesir <> 0
085900140320     c* l'istruzione seguente mi serve per determinare quanti orm
086000140320     c* sono fuori range per accuratezza. utilizzo lo stesso campo
086100140320     c* perchè poi lo devo riporto nel file (se richiesto)
086200140320     c     vtesir        sub       vtoraokr      vtoraokr
086300140320     c* % delle fuori range
086400140320     c                   if        vtoraokr <> 0
086500140320     c                   eval(h)   tporaokr = vtoraokr* 100/vtesir
086600140320     c                   end
086700140320     c                   end
086800140320     C                   ENDSR
086900120329     C*---------------------------------------------------------------*
087000120329     C     *inzsr        BEGSR
087100120329     C*---------------------------------------------------------------*
087200120329     C     *ENTRY        PLIST
087300120329     C                   PARM                    KPJBA
087400131128      *
087500140310     c                   movel     kpjbu         fnlvp16ds
087600140312     c                   if        f16po = 999
087700140312     c                   eval      f16i = 1
087800140312     c                   else
087900140312     c                   eval      f16i = f16po
088000140312     c                   end
088100140312     c     *dtaara       define    §azute        azuteds
088200140312     c     *dtaara       define    §datiute      ddatiute
088300140312     c                   in(e)     *dtaara
088400140312     c                   if        %error  or rsut = *blanks
088500140312     c                   clear                   tibs34ds
088600140312     c                   call      'TIBS34R'
088700140312     c                   parm                    tibs34ds
088800140312     c                   in        *dtaara
088900140312     c                   endif
089000130128      *
089100120329     C     Kapd          KLIST
089200120329     C                   KFLD                    apdtip
089300120329     C                   KFLD                    wrkcodaut
089400130128      *
089500140310     C     Ktab          KLIST
089600140310     C                   KFLD                    TBLKUT
089700140310     C                   KFLD                    tblcod
089800140310     C                   KFLD                    tblkey
089900140310     C     Ktbe          KLIST
090000140310     C                   KFLD                    TBecod
090100140310     C                   KFLD                    tbeke1
090200140310      *
090300140310     C     Kdst          KLIST
090400140310     C                   KFLD                    dsTnpg
090500140310     C                   KFLD                    wrkNDC
090600140310     C                   KFLD                    wrkFGS
090700140310      *
090800140310     C     Kplg          KLIST
090900140310     C                   KFLD                    PLGTIPORCD
091000140310     C                   KFLD                    wrkFGS
091100140310     C                   KFLD                    wrkNDC
091200140310     C                   KFLD                    wrkCODAUT
091300140310     c*
091400140310     c                   eval      PLGTIPORCD = 'CUS'
091500140320     c                   z-add     1             tblkut
091600140310     c                   eval      dstnpg = 4
091700120330     c                   eval      apdtip = 'A'
091800140320     c                   time                    comtime
091900140310     c                   move      udate         dataeur
092000140310     c                   move      dataeur       dataiso
092100140320     C                   move      dataiso       AMGUDT
092200140310     C                   MOVE      dataeur       WOGGI
092300120402     C                   CLEAR                   TIBS55ds
092400120402     C                   MOVEL     'L'           I50TLA
092500140320     C                   MOVEL     '001'         I50PPO
092600120402     C                   MOVEL     '001'         I50APO
092700120402     C                   CALL      'TIBS55R'
092800120402     C                   PARM                    TIBS55ds
092900120402     c                   eval      lib001ala = o50ala
093000130128     C* FIDST00F
093100130128     c                   EVAL      filedsT=%trim(lib001ala)+'/'+'FIDST01L'
093200130128     c                   open      fidsT01l
093300140310     c* otput su file
093400140310     c                   if        f16stafil = 'F'
093500140310     c                   exsr      CLEAR_WFile
093600140310     c                   open      wfnlvp17f
093700140310     c                   else
093800140310     c* output su Stampa
093900140310     c                   open      fnlvp17p
094000140312     c                   seton                                        01
094100120618     c                   end
094200140310     c*
094300120329     C                   ENDSR
094400120618     C*---------------------------------------------------------------*
094500140310     C     srfile        BEGSR
094600120618     C*---------------------------------------------------------------*
094700140310     c                   clear                   wfnlvp17ds
094800120618      **
094900140325     c                   eval       p17test= v1test
095000140325     c                   eval       p17disd= savdisd
095100140320     c                   eval       p17card= savcard
095200140311     c                   eval       P17fgs = v1fgs
095300140319     c                   eval       P17area= v1area
095400140319     c                   eval       P17dis= v1dis
095500140310     c                   eval       P17des = v1des
095600140311     c                   eval       P17NDC = v1NDC
095700140310     c                   eval       P17ddc = v1ddc
095800140311     c                   eval       P17pdc = v1codaut
095900140310     c                   eval       P17rsf = v1rsf
096000140310     c                   eval       P17esic= v1esic
096100140328     c                   eval       P17geoc= v1geoc
096200140328     c                   eval       P17geor= v1geor
096300140310     c                   eval       P17esir= v1esir
096400140310     c                   eval       P17primac= v1primac
096500140310     c                   eval       P17dopoc= v1dopoc
096600140310     c                   eval       P17oraokc= v1oraokc
096700140312     c                   eval       P17oraokr= v1oraokr
096800140320     c                   eval       Pp7utente= knmus
096900140320     c                   eval       Pp7time = comtime
097000140320     c                   eval       Pp7scom = f16scom
097100140320     c                   eval       Pp7scop = f16scop
097200140320     c                   eval       Pp7primac = f16primac
097300140320     c                   eval       Pp7oraokc = f16oraokc
097400140310     c                   write     wfnlvp17
097500120618      **
097600120618     C                   ENDSR
097700120618     C*---------------------------------------------------------------*
097800120618     C     CLEAR_WFile   BEGSR
097900120618     C*---------------------------------------------------------------*
098000120618      * Pulisce il file
098100120618     C                   clear                   MSGDS
098200140310     C                   eval      MSGDS = 'CLRPFM FILE(WFNLVP17F)'
098300120618     C                   Z-ADD     100           LENGH
098400120618     C                   CALL      'QCMDEXC'
098500120618     C                   PARM                    MSGDS
098600120618     C                   PARM                    LENGH
098700120618     C                   ENDSR
098800120618     C*---------------------------------------------------------------*
