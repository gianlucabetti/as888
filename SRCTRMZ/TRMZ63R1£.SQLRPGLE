000100150309     H DEBUG DECEDIT('0,') DATEDIT(*YMD.)
000200000000     H*--------------------------------------------------------------*
000300150311      * IL Pgm GENERA sempre 4 RECORDS di work:
000400150311      * (1)       Accreditati della WEEK          + valori
000500150311      * (2)    DISAccreditati della WEEK          + valori
000600150311      * (3)       Accreditati PROGRESSIVO PERIODO + valori
000700150311      * (4)    DISAccreditati PROGRESSIVO PERIODO + valori
000800150311      ***
000900150311      *  se NON riceve il range di DATE    (x sottomissione skedulata)
001000150311      *   vuole esaminare:
001100150311      *   i dati della WEEK scorsa rispetto all'UDATE e
001200150311      *   i dati da inizio Anno alla WEEK scorsa x il PROGRESSIVO PERIODO.
001300150311      *
001400150312      *  se riceve il range di date f l-al.(richiamato a Video x RANGE particolare)
001500150311      *   Con la data "AL" ricava la Settimana di riferimento dei dati della WEEK
001600150311      *   ed il range di date Dal-Al servono per i dati di PROGRESSIVO PERIODO.
001700150311     H*--------------------------------------------------------------*
001800150310     Ftrmz630f  uf a e           k disk    usropn
001900150310     FTrmz63p   O    E             PRINTER OFLIND(*IN01)
002000150311      *---------------------------
002100020304     dkpjba          e ds
002200030630     d Tibs36Ds      e ds
002300150309     D TIBS34DS      E DS                                                       *Profili utente
002400150309     D DDATIUTE      E DS                                                       *Dati utente
002500150309     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
002600150311      *--
002700150310     d TRMZ63DS      e ds
002800150311      *--
002900150311     D aitraDS       E DS                  extname(AITRA00F)
003000150311      *--
003100150309     d COM08           s              8  0
003200150311      *--
003300070605     D WLBDA8          DS
003400070605     D  G02DAT                 1      8  0
003500070605     D  G02INV                 9     16  0
003600070605     D  G02ERR                17     17
003700070605     D  G02TGI                18     22  0
003800080502      *
003900090519     d coman           s             99
004000090519     d luncmd          s             15  5
004100090519     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
004200090519     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
004300090519     D cmd3            S              1    DIM(99) CTDATA PERRCD(99)
004400090519     D cmd4            S              1    DIM(99) CTDATA PERRCD(99)
004500090519     D cmd5            S              1    DIM(99) CTDATA PERRCD(99)
004600150311      **--------------------
004700150311     D DATECNV0F     E DS
004800150311      **--------------------
004900150311      **
005000150311     D da_lunedi_scorso...
005100150311     D                 S               D
005200150311     D a_domenica_scorsa...
005300150311     D                 S               D
005400150311     D oggi            S               D
005500150311     d DataISO         s               d   datfmt(*iso)
005600150311     d DataEUR         s               d   datfmt(*eur)
005700150311      **
005800150311     D Anno_week       S              4  0
005900150311     D Data_08         S              8  0
006000150311      **
006100150311     D Data_INI        S              8  0
006200150311     D Data_FIN        S              8  0
006300150311     D Data_INI_wk     S              8  0
006400150311     D Data_FIN_wk     S              8  0
006500150311      **
006600150311     D Numero          S              7  0
006700150311      *---------------------------------------------------------------*
006800150311      *    calcola i 4 casi e li scrive in QTEMP sul File
006900050623     c                   exsr      elabora
007000150311     c* stampa
007100150311     c                   if        i63tipoela ='S'
007200150311     c                   exsr      srstampa
007300150311     c                   end
007400150311      *
007500090519      *verifica se esiste file in libreria wrkquery se c'è lo elimina
007600150310     c                   if        i63tipoela ='F'
007700090519     c                   movea     cmd4          coman
007800090519     c                   exsr      comandi
007900090519      *esegue crtdupobj da qtemp a wrkquery
008000090519     c                   movea     cmd5          coman
008100090519     c                   exsr      comandi
008200150309     c                   end
008300150311      *
008400090519      *elimina file in qtemp x la filiale
008500150310     c                   close     trmz630f
008600090519     c                   movea     cmd3          coman
008700090519     c                   exsr      comandi
008800150311      *
008900020308     c                   seton                                        lr
009000050802     c*-------------------------------------------------
009100090518     c     elabora       begsr
009200050802     c*-------------------------------------------------
009300150311      *  1-accreditato    WEEK
009400150311      *  2-disaccreditato WEEK
009500150311     c                   exsr      X_WEEK
009600150311      *
009700150311      *  3-accreditato    PROGRESSIVO
009800150311      *  4-disaccreditato PROGRESSIVO
009900150311     c                   exsr      X_PROGR
010000150311      *
010100150311     c                   endsr
010200150311     c*-------------------------------------------------
010300150311     c     X_WEEK        begsr
010400150311     c*-------------------------------------------------
010500150311      ** prima la WEEK degli acccreditati
010600150311     c                   clear                   trmz6300
010700150311      *
010800150311     c                   eval      T63ACCDIS ='ACC'
010900150311     c                   eval      T63WEKPRG ='WEK'
011000150311     c                   eval      T63DAL   = data_INI_wk
011100150311     c                   eval      T63AL    = data_FIN_wk
011200150311      *
011300150311      *    calcola AAAAWW delle 2 date di riferimento
011400150311     c                   clear                   numero
011500150311     c     *iso          move      data_INI_wk   dataiso
011600150311     C/EXEC SQL
011700150311     C+ SELECT DC_YOY_ISO*100 +DC_WOY_ISO  INTO :numero
011800150311     C+ FROM DATECNV0F WHERE DC_DATE = :dataiso
011900150311     C/END-EXEC
012000150311     c                   if        sqlcod =0
012100150311     c                   eval      T63DAANWW = numero
012200150311     c                   eval      T63AANWW  = numero
012300150311     c                   end
012400150311      *
012500150311      * accreditati AUTONOMI
012600150311     c                   clear                   numero
012700150311     C/EXEC SQL
012800150311     C+ SELECT count(*)  INTO  :numero
012900150311     C+ FROM aitra00f WHERE TRADFI =0 and
013000150311     C+ TRADIN between :DATA_INI_wk and :DATA_FIN_wk
013100150313     C+ and TRASCO = ' ' AND traann = ' '
013200160113     C+ and tradipa ='A'
013300150311     C/END-EXEC
013400150311     c                   if        sqlcod =0
013500150311     c                   eval      T63AUTON = numero
013600150311     c                   end
013700150311      *
013800150311      * accreditati DIPENDENTI
013900150311     c                   clear                   numero
014000150311     C/EXEC SQL
014100150311     C+ SELECT count(*) INTO  :numero
014200150311     C+ FROM aitra00f WHERE TRADFI =0 and
014300150311     C+ TRADIN between :DATA_INI_wk and :DATA_FIN_wk
014400150313     C+ and TRASCO = ' ' AND traann = ' '
014500160113     C+ and tradipa ='D'
014600150311     C/END-EXEC
014700150311     c                   if        sqlcod =0
014800150311     c                   eval      T63DIPEN = numero
014900150311     c                   end
015000150311      *
015100150311      *  totale Autisti accreditati
015200150311     c                   eval      T63NRAUT = T63DIPEN + T63AUTON
015300150311      *
015400150311      * accreditati ITALIANI
015500150311     c                   clear                   numero
015600150311     C/EXEC SQL
015700150311     C+ SELECT count(*) INTO  :numero
015800150311     C+ FROM aitra00f WHERE TRADFI =0 and
015900150311     C+ TRADIN between :DATA_INI_wk and :DATA_FIN_wk
016000150313     C+ and TRASCO = ' ' AND traann = ' '
016100160113     C+ and traitaest ='I'
016200150311     C/END-EXEC
016300150311     c                   if        sqlcod =0
016400150311     c                   eval      T63ITALIA= numero
016500150311     c                   end
016600150311      *
016700150311      * accreditati ESTERI
016800150311     c                   clear                   numero
016900150311     C/EXEC SQL
017000150311     C+ SELECT count(*) INTO  :numero
017100150311     C+ FROM aitra00f WHERE TRADFI =0 and
017200150311     C+ TRADIN between :DATA_INI_wk and :DATA_FIN_wk
017300150313     C+ and TRASCO = ' ' AND traann = ' '
017400160113     C+ and traitaest ='E'
017500150311     C/END-EXEC
017600150311     c                   if        sqlcod =0
017700150311     c                   eval      T63ESTERO= numero
017800150311     c                   end
017900150311      *
018000150311      * accreditati NON definiti
018100150311     c                   clear                   numero
018200150311     C/EXEC SQL
018300150311     C+ SELECT count(*) INTO  :numero
018400150311     C+ FROM aitra00f WHERE TRADFI =0 and
018500150311     C+ TRADIN between :DATA_INI_wk and :DATA_FIN_wk
018600150313     C+ and TRASCO = ' ' AND traann = ' '
018700160113     C+ and traitaest not in ('I', 'E')
018800150311     C/END-EXEC
018900150311     c                   if        sqlcod =0
019000150311     c                   eval      T63NOITES= numero
019100150311     c                   end
019200150311      *
019300150311      * accreditati DIPENDENTI FULL TIME
019400150311     c                   clear                   numero
019500150311     C/EXEC SQL
019600150311     C+ SELECT count(*) INTO  :numero
019700150311     C+ FROM aitra00f WHERE TRADFI =0 and
019800150311     C+ TRADIN between :DATA_INI_wk and :DATA_FIN_wk
019900150313     C+ and TRASCO = ' ' AND traann = ' '
020000160113     C+ and tradipa ='D'
020100160113     C+ and (traorel >=40 OR  traorel = 0)
020200150311     C/END-EXEC
020300150311     c                   if        sqlcod =0
020400150311     c                   eval      T63FULLT = numero
020500150311     c                   end
020600150311      *
020700150311      * accreditati DIPENDENTI PART TIME
020800150311     c                   clear                   numero
020900150311     C/EXEC SQL
021000150311     C+ SELECT count(*) INTO  :numero
021100150311     C+ FROM aitra00f WHERE TRADFI =0 and
021200150311     C+ TRADIN between :DATA_INI_wk and :DATA_FIN_wk
021300150313     C+ and TRASCO = ' ' AND traann = ' '
021400160113     C+ and tradipa ='D'
021500160113     C+ and traorel < 40  and traorel > 0
021600150311     C/END-EXEC
021700150311     c                   if        sqlcod =0
021800150311     c                   eval      T63PARTT = numero
021900150311     c                   end
022000150311      *    scrive rec.(1)
022100150310     c                   write     trmz6300
022200150311      *    -------------------
022300150311      *
022400150311      ** poi la WEEK dei disacccreditati
022500150311     c                   eval      T63ACCDIS ='DIS'
022600150311      *
022700150311      * DISaccreditati AUTONOMI
022800150311     c                   clear                   numero
022900150311     c                   clear                   T63AUTON
023000150311     C/EXEC SQL
023100150311     C+ SELECT count(*) INTO  :numero
023200150311     C+ FROM aitra00f WHERE
023300150311     C+ TRADFI between :DATA_INI_wk and :DATA_FIN_wk
023400150313     C+ and TRASCO = ' ' AND traann = ' '
023500160113     C+ and tradipa ='A'
023600150311     C/END-EXEC
023700150311     c                   if        sqlcod =0
023800150311     c                   eval      T63AUTON = numero
023900150311     c                   end
024000150311      *
024100150311      * DISaccreditati DIPENDENTI
024200150311     c                   clear                   numero
024300150311     c                   clear                   T63DIPEN
024400150311     C/EXEC SQL
024500150311     C+ SELECT count(*) INTO  :numero
024600150311     C+ FROM aitra00f WHERE
024700150311     C+ TRADFI between :DATA_INI_wk and :DATA_FIN_wk
024800150313     C+ and TRASCO = ' ' AND traann = ' '
024900160113     C+ and tradipa ='D'
025000150311     C/END-EXEC
025100150311     c                   if        sqlcod =0
025200150311     c                   eval      T63DIPEN = numero
025300150311     c                   end
025400150311      *
025500150311      *  totale Autisti accreditati
025600150311     c                   eval      T63NRAUT = T63DIPEN + T63AUTON
025700150311      *
025800150311      * DISaccreditati ITALIANI
025900150311     c                   clear                   numero
026000150311     c                   clear                   T63ITALIA
026100150311     C/EXEC SQL
026200150311     C+ SELECT count(*)  INTO  :numero
026300150311     C+ FROM aitra00f WHERE
026400150311     C+ TRADFI between :DATA_INI_wk and :DATA_FIN_wk
026500160113     C+ and tradipa in ('D', 'A')
026600150313     C+ and TRASCO = ' ' AND traann = ' '
026700160113     C+ and traitaest ='I'
026800150311     C/END-EXEC
026900150311     c                   if        sqlcod =0
027000150311     c                   eval      T63ITALIA= numero
027100150311     c                   end
027200150311      *
027300150311      * DISaccreditati ESTERI
027400150311     c                   clear                   numero
027500150311     c                   clear                   T63ESTERO
027600150311     C/EXEC SQL
027700150311     C+ SELECT count(*) INTO  :numero
027800150311     C+ FROM aitra00f WHERE
027900150311     C+ TRADFI between :DATA_INI_wk and :DATA_FIN_wk
028000160113     C+ and tradipa in ('D', 'A')
028100150313     C+ and TRASCO = ' ' AND traann = ' '
028200160113     C+ and traitaest ='E'
028300150311     C/END-EXEC
028400150311     c                   if        sqlcod =0
028500150311     c                   eval      T63ESTERO= numero
028600150311     c                   end
028700150311      *
028800150311      * DISaccreditati NON definiti
028900150311     c                   clear                   numero
029000150311     c                   clear                   T63NOITES
029100150311     C/EXEC SQL
029200150311     C+ SELECT count(*) INTO  :numero
029300150311     C+ FROM aitra00f WHERE
029400150311     C+ TRADFI between :DATA_INI_wk and :DATA_FIN_wk
029500160113     C+ and tradipa in ('D', 'A')
029600150313     C+ and TRASCO = ' ' AND traann = ' '
029700160113     C+ and traitaest not in ('I', 'E')
029800150311     C/END-EXEC
029900150311     c                   if        sqlcod =0
030000150311     c                   eval      T63NOITES= numero
030100150311     c                   end
030200150311      *
030300150311      * DISaccreditati DIPENDENTI FULL TIME
030400150311     c                   clear                   numero
030500150311     c                   clear                   T63FULLT
030600150311     C/EXEC SQL
030700150311     C+ SELECT count(*) INTO  :numero
030800150311     C+ FROM aitra00f WHERE
030900150311     C+ TRADFI between :DATA_INI_wk and :DATA_FIN_wk
031000150313     C+ and TRASCO = ' ' AND traann = ' '
031100160113     C+ and tradipa ='D'
031200160113     C+ and (traorel >=40 OR  traorel = 0)
031300150311     C/END-EXEC
031400150311     c                   if        sqlcod =0
031500150311     c                   eval      T63FULLT = numero
031600150311     c                   end
031700150311      *
031800150311      * DISaccreditati DIPENDENTI PART TIME
031900150311     c                   clear                   numero
032000150311     c                   clear                   T63PARTT
032100150311     C/EXEC SQL
032200150311     C+ SELECT count(*) INTO  :numero
032300150311     C+ FROM aitra00f WHERE
032400150311     C+ TRADFI between :DATA_INI_wk and :DATA_FIN_wk
032500150313     C+ and TRASCO = ' ' AND traann = ' '
032600160113     C+ and tradipa ='D'
032700160113     C+ and traorel < 40  and traorel > 0
032800150311     C/END-EXEC
032900150311     c                   if        sqlcod =0
033000150311     c                   eval      T63PARTT = numero
033100150311     c                   end
033200150311      *
033300150311      *    scrive rec.(2)
033400150311     c                   write     trmz6300
033500150311      *    -------------------
033600070530      *
033700070530     c                   endsr
033800150311     c*-------------------------------------------------
033900150311     c     X_PROGR       begsr
034000150311     c*-------------------------------------------------
034100150311      ** prima la WEEK degli acccreditati
034200150311     c                   clear                   trmz6300
034300150311      *
034400150311     c                   eval      T63ACCDIS ='ACC'
034500150311     c                   eval      T63WEKPRG ='PRG'
034600150311     c                   eval      T63DAL   = data_INI
034700150311     c                   eval      T63AL    = data_FIN
034800150311      *
034900150311      *    calcola AAAAWW delle 2 date di riferimento
035000150311     c                   clear                   numero
035100150311     c     *iso          move      data_INI      dataiso
035200150311     C/EXEC SQL
035300150311     C+ SELECT DC_YOY_ISO*100 +DC_WOY_ISO  INTO :numero
035400150311     C+ FROM DATECNV0F WHERE DC_DATE = :dataiso
035500150311     C/END-EXEC
035600150311     c                   if        sqlcod =0
035700150311     c                   eval      T63DAANWW = numero
035800150311     c                   end
035900150311      *
036000150311     c                   clear                   numero
036100150311     c     *iso          move      data_FIN      dataiso
036200150311     C/EXEC SQL
036300150311     C+ SELECT DC_YOY_ISO*100 +DC_WOY_ISO  INTO :numero
036400150311     C+ FROM DATECNV0F WHERE DC_DATE = :dataiso
036500150311     C/END-EXEC
036600150311     c                   if        sqlcod =0
036700150311     c                   eval      T63AANWW  = numero
036800150311     c                   end
036900150311      *
037000150311      * accreditati AUTONOMI
037100150311     c                   clear                   numero
037200150311     C/EXEC SQL
037300150311     C+ SELECT count(*)  INTO  :numero
037400150311     C+ FROM aitra00f WHERE TRADFI =0 and
037500150311     C+ TRADIN between :DATA_INI    and :DATA_FIN
037600150313     C+ and TRASCO = ' ' AND traann = ' '
037700160113     C+ and tradipa ='A'
037800150311     C/END-EXEC
037900150311     c                   if        sqlcod =0
038000150311     c                   eval      T63AUTON = numero
038100150311     c                   end
038200150311      *
038300150311      * accreditati DIPENDENTI
038400150311     c                   clear                   numero
038500150311     C/EXEC SQL
038600150311     C+ SELECT count(*) INTO  :numero
038700150311     C+ FROM aitra00f WHERE TRADFI =0 and
038800150311     C+ TRADIN between :DATA_INI    and :DATA_FIN
038900150313     C+ and TRASCO = ' ' AND traann = ' '
039000160113     C+ and tradipa ='D'
039100150311     C/END-EXEC
039200150311     c                   if        sqlcod =0
039300150311     c                   eval      T63DIPEN = numero
039400150311     c                   end
039500150311      *
039600150311      *  totale Autisti accreditati
039700150311     c                   eval      T63NRAUT = T63DIPEN + T63AUTON
039800150311      *
039900150311      * accreditati ITALIANI
040000150311     c                   clear                   numero
040100150311     C/EXEC SQL
040200150311     C+ SELECT count(*) INTO  :numero
040300150311     C+ FROM aitra00f WHERE TRADFI =0 and
040400150311     C+ TRADIN between :DATA_INI    and :DATA_FIN
040500150313     C+ and TRASCO = ' ' AND traann = ' '
040600160113     C+ and traitaest ='I'
040700150311     C/END-EXEC
040800150311     c                   if        sqlcod =0
040900150311     c                   eval      T63ITALIA= numero
041000150311     c                   end
041100150311      *
041200150311      * accreditati ESTERI
041300150311     c                   clear                   numero
041400150311     C/EXEC SQL
041500150311     C+ SELECT count(*) INTO  :numero
041600150311     C+ FROM aitra00f WHERE TRADFI =0 and
041700150311     C+ TRADIN between :DATA_INI    and :DATA_FIN
041800150313     C+ and TRASCO = ' ' AND traann = ' '
041900160113     C+ and traitaest ='E'
042000150311     C/END-EXEC
042100150311     c                   if        sqlcod =0
042200150311     c                   eval      T63ESTERO= numero
042300150311     c                   end
042400150311      *
042500150311      * accreditati NON definiti
042600150311     c                   clear                   numero
042700150311     C/EXEC SQL
042800150311     C+ SELECT count(*) INTO  :numero
042900150311     C+ FROM aitra00f WHERE TRADFI =0 and
043000150311     C+ TRADIN between :DATA_INI    and :DATA_FIN
043100150313     C+ and TRASCO = ' ' AND traann = ' '
043200160113     C+ and traitaest not in ('I', 'E')
043300150311     C/END-EXEC
043400150311     c                   if        sqlcod =0
043500150311     c                   eval      T63NOITES= numero
043600150311     c                   end
043700150311      *
043800150311      * accreditati DIPENDENTI FULL TIME
043900150311     c                   clear                   numero
044000150311     C/EXEC SQL
044100150311     C+ SELECT count(*) INTO  :numero
044200150311     C+ FROM aitra00f WHERE TRADFI =0 and
044300150311     C+ TRADIN between :DATA_INI    and :DATA_FIN
044400150313     C+ and TRASCO = ' ' AND traann = ' '
044500160113     C+ and tradipa ='D'
044600160113     C+ and (traorel >=40 OR  traorel = 0)
044700150311     C/END-EXEC
044800150311     c                   if        sqlcod =0
044900150311     c                   eval      T63FULLT = numero
045000150311     c                   end
045100150311      *
045200150311      * accreditati DIPENDENTI PART TIME
045300150311     c                   clear                   numero
045400150311     C/EXEC SQL
045500150311     C+ SELECT count(*) INTO  :numero
045600150311     C+ FROM aitra00f WHERE TRADFI =0 and
045700150311     C+ TRADIN between :DATA_INI    and :DATA_FIN
045800150313     C+ and TRASCO = ' ' AND traann = ' '
045900160113     C+ and tradipa ='D'
046000160113     C+ and traorel < 40  and traorel > 0
046100150311     C/END-EXEC
046200150311     c                   if        sqlcod =0
046300150311     c                   eval      T63PARTT = numero
046400150311     c                   end
046500150311      *    scrive rec.(3)
046600150311     c                   write     trmz6300
046700150311      *    -------------------
046800150311      *
046900150311      ** poi la WEEK dei disacccreditati
047000150311     c                   eval      T63ACCDIS ='DIS'
047100150311      *
047200150311      * DISaccreditati AUTONOMI
047300150311     c                   clear                   numero
047400150311     c                   clear                   T63AUTON
047500150311     C/EXEC SQL
047600150311     C+ SELECT count(*) INTO  :numero
047700150311     C+ FROM aitra00f WHERE
047800150311     C+ TRADFI between :DATA_INI    and :DATA_FIN
047900150313     C+ and TRASCO = ' ' AND traann = ' '
048000160113     C+ and tradipa ='A'
048100150311     C/END-EXEC
048200150311     c                   if        sqlcod =0
048300150311     c                   eval      T63AUTON = numero
048400150311     c                   end
048500150311      *
048600150311      * DISaccreditati DIPENDENTI
048700150311     c                   clear                   numero
048800150311     c                   clear                   T63DIPEN
048900150311     C/EXEC SQL
049000150311     C+ SELECT count(*) INTO  :numero
049100150311     C+ FROM aitra00f WHERE
049200150311     C+ TRADFI between :DATA_INI    and :DATA_FIN
049300150313     C+ and TRASCO = ' ' AND traann = ' '
049400160113     C+ and tradipa ='D'
049500150311     C/END-EXEC
049600150311     c                   if        sqlcod =0
049700150311     c                   eval      T63DIPEN = numero
049800150311     c                   end
049900150311      *
050000150311      *  totale Autisti accreditati
050100150311     c                   eval      T63NRAUT = T63DIPEN + T63AUTON
050200150311      *
050300150311      * DISaccreditati ITALIANI
050400150311     c                   clear                   numero
050500150311     c                   clear                   T63ITALIA
050600150311     C/EXEC SQL
050700150311     C+ SELECT count(*)  INTO  :numero
050800150311     C+ FROM aitra00f WHERE
050900150311     C+ TRADFI between :DATA_INI    and :DATA_FIN
051000160113     C+ and tradipa in ('D', 'A')
051100150313     C+ and TRASCO = ' ' AND traann = ' '
051200160113     C+ and traitaest ='I'
051300150311     C/END-EXEC
051400150311     c                   if        sqlcod =0
051500150311     c                   eval      T63ITALIA= numero
051600150311     c                   end
051700150311      *
051800150311      * DISaccreditati ESTERI
051900150311     c                   clear                   numero
052000150311     c                   clear                   T63ESTERO
052100150311     C/EXEC SQL
052200150311     C+ SELECT count(*) INTO  :numero
052300150311     C+ FROM aitra00f WHERE
052400150311     C+ TRADFI between :DATA_INI    and :DATA_FIN
052500160113     C+ and tradipa in ('D', 'A')
052600150313     C+ and TRASCO = ' ' AND traann = ' '
052700160113     C+ and traitaest ='E'
052800150311     C/END-EXEC
052900150311     c                   if        sqlcod =0
053000150311     c                   eval      T63ESTERO= numero
053100150311     c                   end
053200150311      *
053300150311      * DISaccreditati NON definiti
053400150311     c                   clear                   numero
053500150311     c                   clear                   T63NOITES
053600150311     C/EXEC SQL
053700150311     C+ SELECT count(*) INTO  :numero
053800150311     C+ FROM aitra00f WHERE
053900150311     C+ TRADFI between :DATA_INI    and :DATA_FIN
054000160113     C+ and tradipa in ('D', 'A')
054100150313     C+ and TRASCO = ' ' AND traann = ' '
054200160113     C+ and traitaest not in ('I', 'E')
054300150311     C/END-EXEC
054400150311     c                   if        sqlcod =0
054500150311     c                   eval      T63NOITES= numero
054600150311     c                   end
054700150311      *
054800150311      * DISaccreditati DIPENDENTI FULL TIME
054900150311     c                   clear                   numero
055000150311     c                   clear                   T63FULLT
055100150311     C/EXEC SQL
055200150311     C+ SELECT count(*) INTO  :numero
055300150311     C+ FROM aitra00f WHERE
055400150311     C+ TRADFI between :DATA_INI    and :DATA_FIN
055500150313     C+ and TRASCO = ' ' AND traann = ' '
055600160113     C+ and tradipa ='D'
055700160113     C+ and (traorel >=40 OR  traorel = 0)
055800150311     C/END-EXEC
055900150311     c                   if        sqlcod =0
056000150311     c                   eval      T63FULLT = numero
056100150311     c                   end
056200150311      *
056300150311      * DISaccreditati DIPENDENTI PART TIME
056400150311     c                   clear                   numero
056500150311     c                   clear                   T63PARTT
056600150311     C/EXEC SQL
056700150311     C+ SELECT count(*) INTO  :numero
056800150311     C+ FROM aitra00f WHERE
056900150311     C+ TRADFI between :DATA_INI    and :DATA_FIN
057000150313     C+ and TRASCO = ' ' AND traann = ' '
057100160113     C+ and tradipa ='D'
057200160113     C+ and traorel < 40  and traorel > 0
057300150311     C/END-EXEC
057400150311     c                   if        sqlcod =0
057500150311     c                   eval      T63PARTT = numero
057600150311     c                   end
057700150311      *
057800150311      *    scrive rec.(4)
057900150311     c                   write     trmz6300
058000150311      *    -------------------
058100150311      *
058200150311     c                   endsr
058300011026     c*-------------------------------------------------
058400150309     c     srstampa      begsr
058500011026     c*-------------------------------------------------
058600150309     c                   do        *hival
058700150311      *
058800150311      *  Prima i 2 records della week
058900150311      *    poi i 2 records dei progressivi
059000150310     C                   read      trmz630f
059100150311      *
059200150310     c                   if        %eof(trmz630f)
059300150309     c                   leave
059400150309     c                   end
059500150311      *   WEEK
059600150311     c                   IF         T63WEKPRG ='WEK'
059700150311      *
059800150311     c                   IF           T63ACCDIS ='ACC'
059900150311     c                   exsr      MOVE_ACCWEK
060000150311     c                   elseIF       T63ACCDIS ='DIS'
060100150311     c                   exsr      MOVE_DISWEK
060200150311     c                   END
060300150311      *
060400150311      *   PROGRESSIVO
060500150311     c                   elseIF     T63WEKPRG ='PRG'
060600150311      *
060700150311     c                   IF           T63ACCDIS ='ACC'
060800150311     c                   exsr      MOVE_ACCPRG
060900150311     c                   elseIF       T63ACCDIS ='DIS'
061000150311     c                   exsr      MOVE_DISPRG
061100150311     c                   END
061200150311      *
061300150311     c                   END
061400150311      *
061500150309     c                   enddo
061600150311      *
061700150311     c                   WRITE     RIEPILOGO
061800150311      *
061900150311     c                   endsr
062000150311     c*-------------------------------------------------
062100150311     c     MOVE_ACCWEK   begsr
062200150311     c*-------------------------------------------------
062300150311     c                   eval      W63AAWW  = T63DAANWW
062400150312     c     *ISO          move      T63dal        dataiso
062500150312     c                   move      dataiso       dataeur
062600150312     c                   move      dataeur       W63DAL
062700150312     c     *ISO          move      T63al         dataiso
062800150312     c                   move      dataiso       dataeur
062900150312     c                   move      dataeur       W63AL
063000150311     c                   eval      W63AAUTON= T63AUTON
063100150311     c                   eval      W63ADIPEN= T63DIPEN
063200150311     c                   eval      W63APART = T63PARTT
063300150311     c                   eval      W63AFULT = T63FULLT
063400150311     c                   eval      W63ATOT  = T63NRAUT
063500150311     c                   eval      W63AITA  = T63ITALIA
063600150311     c                   eval      W63AEST  = T63ESTERO
063700150316     c******             if          T63NOITES <> 0
063800150316     c******             eval      W63ANIE  = 'Senza I/E'
063900150316     c******             eval      W63ANOIE = T63NOITES
064000150316     c******             end
064100150311      *
064200150311     c                   endsr
064300150311     c*-------------------------------------------------
064400150311     c     MOVE_DISWEK   begsr
064500150311     c*-------------------------------------------------
064600150311     c                   eval      W63DAUTON= T63AUTON
064700150311     c                   eval      W63DDIPEN= T63DIPEN
064800150311     c                   eval      W63DPART = T63PARTT
064900150311     c                   eval      W63DFULT = T63FULLT
065000150311     c                   eval      W63DTOT  = T63NRAUT
065100150311     c                   eval      W63DITA  = T63ITALIA
065200150311     c                   eval      W63DEST  = T63ESTERO
065300150316     c******             if          T63NOITES <> 0
065400150316     c******             eval      W63DNIE  = 'Senza I/E'
065500150316     c******             eval      W63DNOIE = T63NOITES
065600150316     c******             end
065700150311      *
065800150311     C                   eval      W63saldo = W63ATOT-W63DTOT
065900150311      *
066000150311     c                   endsr
066100150311     c*-------------------------------------------------
066200150311     c     MOVE_ACCPRG   begsr
066300150311     c*-------------------------------------------------
066400150312     c     *ISO          move      T63dal        dataiso
066500150312     c                   move      dataiso       dataeur
066600150312     c                   move      dataeur       P63DAL
066700150312     c     *ISO          move      T63al         dataiso
066800150312     c                   move      dataiso       dataeur
066900150312     c                   move      dataeur       P63AL
067000150311     c                   eval      P63AAUTON= T63AUTON
067100150311     c                   eval      P63ADIPEN= T63DIPEN
067200150311     c                   eval      P63APART = T63PARTT
067300150311     c                   eval      P63AFULT = T63FULLT
067400150311     c                   eval      P63ATOT  = T63NRAUT
067500150311     c                   eval      P63AITA  = T63ITALIA
067600150311     c                   eval      P63AEST  = T63ESTERO
067700150316     c******             if          T63NOITES <> 0
067800150316     c******             eval      P63ANIE  = 'Senza I/E'
067900150316     c******             eval      P63ANOIE = T63NOITES
068000150316     c******             end
068100150311      *
068200150311     c                   endsr
068300150311     c*-------------------------------------------------
068400150311     c     MOVE_DISPRG   begsr
068500150311     c*-------------------------------------------------
068600150311     c                   eval      P63DAUTON= T63AUTON
068700150311     c                   eval      P63DDIPEN= T63DIPEN
068800150311     c                   eval      P63DPART = T63PARTT
068900150311     c                   eval      P63DFULT = T63FULLT
069000150311     c                   eval      P63DTOT  = T63NRAUT
069100150311     c                   eval      P63DITA  = T63ITALIA
069200150311     c                   eval      P63DEST  = T63ESTERO
069300150316     c******             if          T63NOITES <> 0
069400150316     c******             eval      P63DNIE  = 'Senza I/E'
069500150316     c******             eval      P63DNOIE = T63NOITES
069600150316     c******             end
069700150311      *
069800150312     C                   eval      P63saldo = P63ATOT-P63DTOT
069900150311      *
070000150311     c                   endsr
070100150311     c*-------------------------------------------------
070200150311     c     *inzsr        begsr
070300150311     c*-------------------------------------------------
070400150311     c     *entry        plist
070500150311     c                   parm                    kpjba
070600150311     C*
070700150311     C                   time                    w0120            14 0
070800150311     C                   move      w0120         wdat              8 0
070900150311     c                   move      wdat          dataeur
071000150311     c                   move      dataeur       dataiso
071100150311     c                   move      dataiso       wdat
071200150311      *
071300150311     C                   CLEAR                   Tibs36Ds
071400150311     C                   EVAL      I36ute = knmus
071500150311     C                   CALL      'TIBS36R'
071600150311     C                   PARM                    tibs36ds
071700150311      *
071800150311     C* INIZIALIZZA VARIABILI DI WRK
071900150311     C                   CLEAR                   TIBS34DS
072000150311     C                   CLEAR                   AZUTEDS
072100150311     C                   CLEAR                   DDATIUTE
072200150311     C*
072300150311     C     *DTAARA       DEFINE    §azute        azuteds
072400150311     C     *DTAARA       DEFINE    §datiute      ddatiute
072500150311     C                   IN(E)     *DTAARA
072600150311     C                   IF        %Error
072700150311     C                   EVAL      I34Tla = 'L'
072800150311     C                   CALL      'TIBS34R'
072900150311     C                   PARM                    Tibs34Ds
073000150311     C                   IN        *DTAARA
073100150311     C                   ENDIF
073200150311     C*
073300150311     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
073400150311     C                   MOVEL(P)  RSUT          DSFIRS           20
073500150311     C*
073600150311     c*  Controlla COME è stato richiamato
073700150311     C*   ed imposta i parametri di ingresso di default oppure li ha ricevuti
073800150311     c*   per una elaborazione particolare.
073900150311     c                   exsr      come_chiamato
074000150311     C*
074100150311      *clear file
074200150311     c                   movea     cmd1          coman
074300150311     c                   exsr      comandi
074400150311     C*
074500150311     c                   movea     cmd2          coman
074600150311     c                   exsr      comandi
074700150311     C*
074800150311     c                   open      trmz630f
074900150311
075000150311     c                   endsr
075100150311     c*-------------------------------------------------
075200150311      **  Verifica come è stato Richiamato se Skedulato
075300150311     c*-------------------------------------------------
075400150311     c     Come_chiamato begsr
075500150311      **
075600150311     c                   move      *blank        schedulato        1
075700150311      **
075800150311      **  se Skedulato da sottomissione automatica
075900150311      **     quindi deve prendere i dati della settimana scorsa ed
076000150311      **     il progressivo dall'inizio dell'anno.
076100150311     c                   if        kpjbu = *blank
076200150311     c                   eval      schedulato = 'S'
076300150311     c                   clear                   trmz63ds
076400150311     c                   eval      i63tipoela = 'S'
076500150311      **
076600150311      *** NON AVENDO PARAMETRI du INGRESSO   prendendo come riferimento OGGI
076700150311      ***    ricava il primo e ultimo giorno della settimana PRECEDENTE
076800150311      ***       lunedì e domenica della settimana precedente
076900150311     C/EXEC SQL
077000150311     C+ SELECT DISTINCT
077100150311     C+   date(days(DC_WEEK_STARTING_DATE)-7),
077200150311     C+   date(days(DC_WEEK_ENDING_DATE)-7),
077300150311     C+   DC_YOY_ISO,
077400150311     C+   curdate()
077500150311     C+ INTO  :da_Lunedi_scorso, :a_domenica_scorsa, :anno_week, :oggi
077600150311     C+ FROM DATECNV0F
077700150311     C+ WHERE DC_DATE = curdate()
077800150311     C/END-EXEC
077900150311     c                   eval      dataiso = da_Lunedi_scorso
078000150311     c                   move      dataiso       DATA_INI_wk
078100150311     c                   eval      dataiso = a_domenica_scorsa
078200150311     c                   move      dataiso       DATA_FIN_wk
078300150311     c                   move      dataiso       DATA_FIN
078400150311      **  deve calcolare la data inizio periodo dall'inizio dell'anno
078500150311     C/EXEC SQL
078600150311     C+   SELECT DISTINCT
078700150311     C+     date(days(DC_WEEK_STARTING_DATE))
078800150311     C+ INTO  :dataiso
078900150311     C+   FROM DATECNV0F
079000150311     C+   WHERE DC_YOY_ISO = :anno_week and DC_WOY_ISO= 01
079100150311     C/END-EXEC
079200150311     c                   move      dataiso       DATA_INI
079300150311      **
079400150311     c                   else
079500150311      **
079600150311      **   Avendo un range definito deve cercare la data di inizio settimana
079700150311      **   di riferimento dell'estrazione.
079800150311     c                   movel     kpjbu         trmz63ds
079900150311     c                   move      i63datdal     DATA_INI
080000150311     c                   move      i63datal      DATA_FIN
080100150311     c                   move      i63datal      DATA_FIN_wk
080200150311     c     *iso          move      i63datal      dataiso
080300150311      **
080400150311      **  deve calcolare la data inizio settimana
080500150311     C/EXEC SQL
080600150311     C+   SELECT DISTINCT
080700150311     C+     date(days(DC_WEEK_STARTING_DATE))
080800150311     C+ INTO  :dataiso
080900150311     C+   FROM DATECNV0F
081000150311     C+   WHERE date(days(DC_WEEK_ENDING_DATE)) = :dataISO
081100150311     C/END-EXEC
081200150311     c                   move      dataiso       DATA_INI_wk
081300150311     c                   end
081400150311      **
081500150309     c                   endsr
081600150311     c*-------------------------------------------------
081700150311     c     comandi       begsr
081800150311     c*-------------------------------------------------
081900150311     C                   z-add     99            luncmd
082000150311     C                   CALL      'QCMDEXC'                            34
082100150311     C                   parm                    coman
082200150311     C                   parm                    luncmd
082300150311      *
082400150311     c                   endsr
082500150311     c*-------------------------------------------------
082600090519**  CMD1
082700150310CLRPFM    FILE(trmz630f)
082800090519**  CMD2
082900150310CRTDUPOBJ OBJ(trmz630F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
083000090519**  CMD3
083100150310DLTF FILE(QTEMP/trmz630F)
083200090519**  CMD4
083300150310dltf file(WRKQUERY/trmz630f)
083400090519**  CMD5
083500150310CRTDUPOBJ OBJ(trmz630F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(trmz630f) DATA(*YES)
