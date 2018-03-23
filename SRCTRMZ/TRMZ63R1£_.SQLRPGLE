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
003000150311     d traCDFds      e ds
003100150311      *--
003200150309     d COM08           s              8  0
003300150311      *--
003400070605     D WLBDA8          DS
003500070605     D  G02DAT                 1      8  0
003600070605     D  G02INV                 9     16  0
003700070605     D  G02ERR                17     17
003800070605     D  G02TGI                18     22  0
003900080502      *
004000090519     d coman           s             99
004100090519     d luncmd          s             15  5
004200090519     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
004300090519     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
004400090519     D cmd3            S              1    DIM(99) CTDATA PERRCD(99)
004500090519     D cmd4            S              1    DIM(99) CTDATA PERRCD(99)
004600090519     D cmd5            S              1    DIM(99) CTDATA PERRCD(99)
004700150311      **--------------------
004800150311     D DATECNV0F     E DS
004900150311      **--------------------
005000150311      **
005100150311     D da_lunedi_scorso...
005200150311     D                 S               D
005300150311     D a_domenica_scorsa...
005400150311     D                 S               D
005500150311     D oggi            S               D
005600150311     d DataISO         s               d   datfmt(*iso)
005700150311     d DataEUR         s               d   datfmt(*eur)
005800150311      **
005900150311     D Anno_week       S              4  0
006000150311     D Data_08         S              8  0
006100150311      **
006200150311     D Data_INI        S              8  0
006300150311     D Data_FIN        S              8  0
006400150311     D Data_INI_wk     S              8  0
006500150311     D Data_FIN_wk     S              8  0
006600150311      **
006700150311     D Numero          S              7  0
006800150311      *---------------------------------------------------------------*
006900150311      *    calcola i 4 casi e li scrive in QTEMP sul File
007000050623     c                   exsr      elabora
007100150311     c* stampa
007200150311     c                   if        i63tipoela ='S'
007300150311     c                   exsr      srstampa
007400150311     c                   end
007500150311      *
007600090519      *verifica se esiste file in libreria wrkquery se c'è lo elimina
007700150310     c                   if        i63tipoela ='F'
007800090519     c                   movea     cmd4          coman
007900090519     c                   exsr      comandi
008000090519      *esegue crtdupobj da qtemp a wrkquery
008100090519     c                   movea     cmd5          coman
008200090519     c                   exsr      comandi
008300150309     c                   end
008400150311      *
008500090519      *elimina file in qtemp x la filiale
008600150310     c                   close     trmz630f
008700090519     c                   movea     cmd3          coman
008800090519     c                   exsr      comandi
008900150311      *
009000020308     c                   seton                                        lr
009100050802     c*-------------------------------------------------
009200090518     c     elabora       begsr
009300050802     c*-------------------------------------------------
009400150311      *  1-accreditato    WEEK
009500150311      *  2-disaccreditato WEEK
009600150311     c                   exsr      X_WEEK
009700150311      *
009800150311      *  3-accreditato    PROGRESSIVO
009900150311      *  4-disaccreditato PROGRESSIVO
010000150311     c                   exsr      X_PROGR
010100150311      *
010200150311     c                   endsr
010300150311     c*-------------------------------------------------
010400150311     c     X_WEEK        begsr
010500150311     c*-------------------------------------------------
010600150311      ** prima la WEEK degli acccreditati
010700150311     c                   clear                   trmz6300
010800150311      *
010900150311     c                   eval      T63ACCDIS ='ACC'
011000150311     c                   eval      T63WEKPRG ='WEK'
011100150311     c                   eval      T63DAL   = data_INI_wk
011200150311     c                   eval      T63AL    = data_FIN_wk
011300150311      *
011400150311      *    calcola AAAAWW delle 2 date di riferimento
011500150311     c                   clear                   numero
011600150311     c     *iso          move      data_INI_wk   dataiso
011700150311     C/EXEC SQL
011800150311     C+ SELECT DC_YOY_ISO*100 +DC_WOY_ISO  INTO :numero
011900150311     C+ FROM DATECNV0F WHERE DC_DATE = :dataiso
012000150311     C/END-EXEC
012100150311     c                   if        sqlcod =0
012200150311     c                   eval      T63DAANWW = numero
012300150311     c                   eval      T63AANWW  = numero
012400150311     c                   end
012500150311      *
012600150311      * accreditati AUTONOMI
012700150311     c                   clear                   numero
012800150311     C/EXEC SQL
012900150311     C+ SELECT count(*)  INTO  :numero
013000150311     C+ FROM aitra00f WHERE TRADFI =0 and
013100150311     C+ TRADIN between :DATA_INI_wk and :DATA_FIN_wk
013200150313     C+ and TRASCO = ' ' AND traann = ' '
013300150311     C+ and substr(tracdf, 2, 1) ='A'
013400150311     C/END-EXEC
013500150311     c                   if        sqlcod =0
013600150311     c                   eval      T63AUTON = numero
013700150311     c                   end
013800150311      *
013900150311      * accreditati DIPENDENTI
014000150311     c                   clear                   numero
014100150311     C/EXEC SQL
014200150311     C+ SELECT count(*) INTO  :numero
014300150311     C+ FROM aitra00f WHERE TRADFI =0 and
014400150311     C+ TRADIN between :DATA_INI_wk and :DATA_FIN_wk
014500150313     C+ and TRASCO = ' ' AND traann = ' '
014600150311     C+ and substr(tracdf, 2, 1) ='D'
014700150311     C/END-EXEC
014800150311     c                   if        sqlcod =0
014900150311     c                   eval      T63DIPEN = numero
015000150311     c                   end
015100150311      *
015200150311      *  totale Autisti accreditati
015300150311     c                   eval      T63NRAUT = T63DIPEN + T63AUTON
015400150311      *
015500150311      * accreditati ITALIANI
015600150311     c                   clear                   numero
015700150311     C/EXEC SQL
015800150311     C+ SELECT count(*) INTO  :numero
015900150311     C+ FROM aitra00f WHERE TRADFI =0 and
016000150311     C+ TRADIN between :DATA_INI_wk and :DATA_FIN_wk
016100150313     C+ and TRASCO = ' ' AND traann = ' '
016200150311     C+ and substr(tracdf, 3, 1) ='I'
016300150311     C/END-EXEC
016400150311     c                   if        sqlcod =0
016500150311     c                   eval      T63ITALIA= numero
016600150311     c                   end
016700150311      *
016800150311      * accreditati ESTERI
016900150311     c                   clear                   numero
017000150311     C/EXEC SQL
017100150311     C+ SELECT count(*) INTO  :numero
017200150311     C+ FROM aitra00f WHERE TRADFI =0 and
017300150311     C+ TRADIN between :DATA_INI_wk and :DATA_FIN_wk
017400150313     C+ and TRASCO = ' ' AND traann = ' '
017500150311     C+ and substr(tracdf, 3, 1) ='E'
017600150311     C/END-EXEC
017700150311     c                   if        sqlcod =0
017800150311     c                   eval      T63ESTERO= numero
017900150311     c                   end
018000150311      *
018100150311      * accreditati NON definiti
018200150311     c                   clear                   numero
018300150311     C/EXEC SQL
018400150311     C+ SELECT count(*) INTO  :numero
018500150311     C+ FROM aitra00f WHERE TRADFI =0 and
018600150311     C+ TRADIN between :DATA_INI_wk and :DATA_FIN_wk
018700150313     C+ and TRASCO = ' ' AND traann = ' '
018800150311     C+ and substr(tracdf, 3, 1) not in ('I', 'E')
018900150311     C/END-EXEC
019000150311     c                   if        sqlcod =0
019100150311     c                   eval      T63NOITES= numero
019200150311     c                   end
019300150311      *
019400150311      * accreditati DIPENDENTI FULL TIME
019500150311     c                   clear                   numero
019600150311     C/EXEC SQL
019700150311     C+ SELECT count(*) INTO  :numero
019800150311     C+ FROM aitra00f WHERE TRADFI =0 and
019900150311     C+ TRADIN between :DATA_INI_wk and :DATA_FIN_wk
020000150313     C+ and TRASCO = ' ' AND traann = ' '
020100150313     C+ and substr(tracdf, 2, 1) ='D'
020200150313     C+ and (tradec >=40 OR  tradec = 0)
020300150311     C/END-EXEC
020400150311     c                   if        sqlcod =0
020500150311     c                   eval      T63FULLT = numero
020600150311     c                   end
020700150311      *
020800150311      * accreditati DIPENDENTI PART TIME
020900150311     c                   clear                   numero
021000150311     C/EXEC SQL
021100150311     C+ SELECT count(*) INTO  :numero
021200150311     C+ FROM aitra00f WHERE TRADFI =0 and
021300150311     C+ TRADIN between :DATA_INI_wk and :DATA_FIN_wk
021400150313     C+ and TRASCO = ' ' AND traann = ' '
021500150313     C+ and substr(tracdf, 2, 1) ='D'
021600150311     C+ and tradec < 40  and tradec > 0
021700150311     C/END-EXEC
021800150311     c                   if        sqlcod =0
021900150311     c                   eval      T63PARTT = numero
022000150311     c                   end
022100150311      *    scrive rec.(1)
022200150310     c                   write     trmz6300
022300150311      *    -------------------
022400150311      *
022500150311      ** poi la WEEK dei disacccreditati
022600150311     c                   eval      T63ACCDIS ='DIS'
022700150311      *
022800150311      * DISaccreditati AUTONOMI
022900150311     c                   clear                   numero
023000150311     c                   clear                   T63AUTON
023100150311     C/EXEC SQL
023200150311     C+ SELECT count(*) INTO  :numero
023300150311     C+ FROM aitra00f WHERE
023400150311     C+ TRADFI between :DATA_INI_wk and :DATA_FIN_wk
023500150313     C+ and TRASCO = ' ' AND traann = ' '
023600150311     C+ and substr(tracdf, 2, 1) ='A'
023700150311     C/END-EXEC
023800150311     c                   if        sqlcod =0
023900150311     c                   eval      T63AUTON = numero
024000150311     c                   end
024100150311      *
024200150311      * DISaccreditati DIPENDENTI
024300150311     c                   clear                   numero
024400150311     c                   clear                   T63DIPEN
024500150311     C/EXEC SQL
024600150311     C+ SELECT count(*) INTO  :numero
024700150311     C+ FROM aitra00f WHERE
024800150311     C+ TRADFI between :DATA_INI_wk and :DATA_FIN_wk
024900150313     C+ and TRASCO = ' ' AND traann = ' '
025000150311     C+ and substr(tracdf, 2, 1) ='D'
025100150311     C/END-EXEC
025200150311     c                   if        sqlcod =0
025300150311     c                   eval      T63DIPEN = numero
025400150311     c                   end
025500150311      *
025600150311      *  totale Autisti accreditati
025700150311     c                   eval      T63NRAUT = T63DIPEN + T63AUTON
025800150311      *
025900150311      * DISaccreditati ITALIANI
026000150311     c                   clear                   numero
026100150311     c                   clear                   T63ITALIA
026200150311     C/EXEC SQL
026300150311     C+ SELECT count(*)  INTO  :numero
026400150311     C+ FROM aitra00f WHERE
026500150311     C+ TRADFI between :DATA_INI_wk and :DATA_FIN_wk
026600150316     C+ and substr(tracdf, 2, 1) in ('D', 'A')
026700150313     C+ and TRASCO = ' ' AND traann = ' '
026800150311     C+ and substr(tracdf, 3, 1) ='I'
026900150311     C/END-EXEC
027000150311     c                   if        sqlcod =0
027100150311     c                   eval      T63ITALIA= numero
027200150311     c                   end
027300150311      *
027400150311      * DISaccreditati ESTERI
027500150311     c                   clear                   numero
027600150311     c                   clear                   T63ESTERO
027700150311     C/EXEC SQL
027800150311     C+ SELECT count(*) INTO  :numero
027900150311     C+ FROM aitra00f WHERE
028000150311     C+ TRADFI between :DATA_INI_wk and :DATA_FIN_wk
028100150316     C+ and substr(tracdf, 2, 1) in ('D', 'A')
028200150313     C+ and TRASCO = ' ' AND traann = ' '
028300150311     C+ and substr(tracdf, 3, 1) ='E'
028400150311     C/END-EXEC
028500150311     c                   if        sqlcod =0
028600150311     c                   eval      T63ESTERO= numero
028700150311     c                   end
028800150311      *
028900150311      * DISaccreditati NON definiti
029000150311     c                   clear                   numero
029100150311     c                   clear                   T63NOITES
029200150311     C/EXEC SQL
029300150311     C+ SELECT count(*) INTO  :numero
029400150311     C+ FROM aitra00f WHERE
029500150311     C+ TRADFI between :DATA_INI_wk and :DATA_FIN_wk
029600150316     C+ and substr(tracdf, 2, 1) in ('D', 'A')
029700150313     C+ and TRASCO = ' ' AND traann = ' '
029800150311     C+ and substr(tracdf, 3, 1) not in ('I', 'E')
029900150311     C/END-EXEC
030000150311     c                   if        sqlcod =0
030100150311     c                   eval      T63NOITES= numero
030200150311     c                   end
030300150311      *
030400150311      * DISaccreditati DIPENDENTI FULL TIME
030500150311     c                   clear                   numero
030600150311     c                   clear                   T63FULLT
030700150311     C/EXEC SQL
030800150311     C+ SELECT count(*) INTO  :numero
030900150311     C+ FROM aitra00f WHERE
031000150311     C+ TRADFI between :DATA_INI_wk and :DATA_FIN_wk
031100150313     C+ and TRASCO = ' ' AND traann = ' '
031200150313     C+ and substr(tracdf, 2, 1) ='D'
031300150313     C+ and (tradec >=40 OR  tradec = 0)
031400150311     C/END-EXEC
031500150311     c                   if        sqlcod =0
031600150311     c                   eval      T63FULLT = numero
031700150311     c                   end
031800150311      *
031900150311      * DISaccreditati DIPENDENTI PART TIME
032000150311     c                   clear                   numero
032100150311     c                   clear                   T63PARTT
032200150311     C/EXEC SQL
032300150311     C+ SELECT count(*) INTO  :numero
032400150311     C+ FROM aitra00f WHERE
032500150311     C+ TRADFI between :DATA_INI_wk and :DATA_FIN_wk
032600150313     C+ and TRASCO = ' ' AND traann = ' '
032700150313     C+ and substr(tracdf, 2, 1) ='D'
032800150311     C+ and tradec < 40  and tradec > 0
032900150311     C/END-EXEC
033000150311     c                   if        sqlcod =0
033100150311     c                   eval      T63PARTT = numero
033200150311     c                   end
033300150311      *
033400150311      *    scrive rec.(2)
033500150311     c                   write     trmz6300
033600150311      *    -------------------
033700070530      *
033800070530     c                   endsr
033900150311     c*-------------------------------------------------
034000150311     c     X_PROGR       begsr
034100150311     c*-------------------------------------------------
034200150311      ** prima la WEEK degli acccreditati
034300150311     c                   clear                   trmz6300
034400150311      *
034500150311     c                   eval      T63ACCDIS ='ACC'
034600150311     c                   eval      T63WEKPRG ='PRG'
034700150311     c                   eval      T63DAL   = data_INI
034800150311     c                   eval      T63AL    = data_FIN
034900150311      *
035000150311      *    calcola AAAAWW delle 2 date di riferimento
035100150311     c                   clear                   numero
035200150311     c     *iso          move      data_INI      dataiso
035300150311     C/EXEC SQL
035400150311     C+ SELECT DC_YOY_ISO*100 +DC_WOY_ISO  INTO :numero
035500150311     C+ FROM DATECNV0F WHERE DC_DATE = :dataiso
035600150311     C/END-EXEC
035700150311     c                   if        sqlcod =0
035800150311     c                   eval      T63DAANWW = numero
035900150311     c                   end
036000150311      *
036100150311     c                   clear                   numero
036200150311     c     *iso          move      data_FIN      dataiso
036300150311     C/EXEC SQL
036400150311     C+ SELECT DC_YOY_ISO*100 +DC_WOY_ISO  INTO :numero
036500150311     C+ FROM DATECNV0F WHERE DC_DATE = :dataiso
036600150311     C/END-EXEC
036700150311     c                   if        sqlcod =0
036800150311     c                   eval      T63AANWW  = numero
036900150311     c                   end
037000150311      *
037100150311      * accreditati AUTONOMI
037200150311     c                   clear                   numero
037300150311     C/EXEC SQL
037400150311     C+ SELECT count(*)  INTO  :numero
037500150311     C+ FROM aitra00f WHERE TRADFI =0 and
037600150311     C+ TRADIN between :DATA_INI    and :DATA_FIN
037700150313     C+ and TRASCO = ' ' AND traann = ' '
037800150311     C+ and substr(tracdf, 2, 1) ='A'
037900150311     C/END-EXEC
038000150311     c                   if        sqlcod =0
038100150311     c                   eval      T63AUTON = numero
038200150311     c                   end
038300150311      *
038400150311      * accreditati DIPENDENTI
038500150311     c                   clear                   numero
038600150311     C/EXEC SQL
038700150311     C+ SELECT count(*) INTO  :numero
038800150311     C+ FROM aitra00f WHERE TRADFI =0 and
038900150311     C+ TRADIN between :DATA_INI    and :DATA_FIN
039000150313     C+ and TRASCO = ' ' AND traann = ' '
039100150311     C+ and substr(tracdf, 2, 1) ='D'
039200150311     C/END-EXEC
039300150311     c                   if        sqlcod =0
039400150311     c                   eval      T63DIPEN = numero
039500150311     c                   end
039600150311      *
039700150311      *  totale Autisti accreditati
039800150311     c                   eval      T63NRAUT = T63DIPEN + T63AUTON
039900150311      *
040000150311      * accreditati ITALIANI
040100150311     c                   clear                   numero
040200150311     C/EXEC SQL
040300150311     C+ SELECT count(*) INTO  :numero
040400150311     C+ FROM aitra00f WHERE TRADFI =0 and
040500150311     C+ TRADIN between :DATA_INI    and :DATA_FIN
040600150313     C+ and TRASCO = ' ' AND traann = ' '
040700150311     C+ and substr(tracdf, 3, 1) ='I'
040800150311     C/END-EXEC
040900150311     c                   if        sqlcod =0
041000150311     c                   eval      T63ITALIA= numero
041100150311     c                   end
041200150311      *
041300150311      * accreditati ESTERI
041400150311     c                   clear                   numero
041500150311     C/EXEC SQL
041600150311     C+ SELECT count(*) INTO  :numero
041700150311     C+ FROM aitra00f WHERE TRADFI =0 and
041800150311     C+ TRADIN between :DATA_INI    and :DATA_FIN
041900150313     C+ and TRASCO = ' ' AND traann = ' '
042000150311     C+ and substr(tracdf, 3, 1) ='E'
042100150311     C/END-EXEC
042200150311     c                   if        sqlcod =0
042300150311     c                   eval      T63ESTERO= numero
042400150311     c                   end
042500150311      *
042600150311      * accreditati NON definiti
042700150311     c                   clear                   numero
042800150311     C/EXEC SQL
042900150311     C+ SELECT count(*) INTO  :numero
043000150311     C+ FROM aitra00f WHERE TRADFI =0 and
043100150311     C+ TRADIN between :DATA_INI    and :DATA_FIN
043200150313     C+ and TRASCO = ' ' AND traann = ' '
043300150311     C+ and substr(tracdf, 3, 1) not in ('I', 'E')
043400150311     C/END-EXEC
043500150311     c                   if        sqlcod =0
043600150311     c                   eval      T63NOITES= numero
043700150311     c                   end
043800150311      *
043900150311      * accreditati DIPENDENTI FULL TIME
044000150311     c                   clear                   numero
044100150311     C/EXEC SQL
044200150311     C+ SELECT count(*) INTO  :numero
044300150311     C+ FROM aitra00f WHERE TRADFI =0 and
044400150311     C+ TRADIN between :DATA_INI    and :DATA_FIN
044500150313     C+ and TRASCO = ' ' AND traann = ' '
044600150313     C+ and substr(tracdf, 2, 1) ='D'
044700150313     C+ and (tradec >=40 OR  tradec = 0)
044800150311     C/END-EXEC
044900150311     c                   if        sqlcod =0
045000150311     c                   eval      T63FULLT = numero
045100150311     c                   end
045200150311      *
045300150311      * accreditati DIPENDENTI PART TIME
045400150311     c                   clear                   numero
045500150311     C/EXEC SQL
045600150311     C+ SELECT count(*) INTO  :numero
045700150311     C+ FROM aitra00f WHERE TRADFI =0 and
045800150311     C+ TRADIN between :DATA_INI    and :DATA_FIN
045900150313     C+ and TRASCO = ' ' AND traann = ' '
046000150313     C+ and substr(tracdf, 2, 1) ='D'
046100150311     C+ and tradec < 40  and tradec > 0
046200150311     C/END-EXEC
046300150311     c                   if        sqlcod =0
046400150311     c                   eval      T63PARTT = numero
046500150311     c                   end
046600150311      *    scrive rec.(3)
046700150311     c                   write     trmz6300
046800150311      *    -------------------
046900150311      *
047000150311      ** poi la WEEK dei disacccreditati
047100150311     c                   eval      T63ACCDIS ='DIS'
047200150311      *
047300150311      * DISaccreditati AUTONOMI
047400150311     c                   clear                   numero
047500150311     c                   clear                   T63AUTON
047600150311     C/EXEC SQL
047700150311     C+ SELECT count(*) INTO  :numero
047800150311     C+ FROM aitra00f WHERE
047900150311     C+ TRADFI between :DATA_INI    and :DATA_FIN
048000150313     C+ and TRASCO = ' ' AND traann = ' '
048100150311     C+ and substr(tracdf, 2, 1) ='A'
048200150311     C/END-EXEC
048300150311     c                   if        sqlcod =0
048400150311     c                   eval      T63AUTON = numero
048500150311     c                   end
048600150311      *
048700150311      * DISaccreditati DIPENDENTI
048800150311     c                   clear                   numero
048900150311     c                   clear                   T63DIPEN
049000150311     C/EXEC SQL
049100150311     C+ SELECT count(*) INTO  :numero
049200150311     C+ FROM aitra00f WHERE
049300150311     C+ TRADFI between :DATA_INI    and :DATA_FIN
049400150313     C+ and TRASCO = ' ' AND traann = ' '
049500150311     C+ and substr(tracdf, 2, 1) ='D'
049600150311     C/END-EXEC
049700150311     c                   if        sqlcod =0
049800150311     c                   eval      T63DIPEN = numero
049900150311     c                   end
050000150311      *
050100150311      *  totale Autisti accreditati
050200150311     c                   eval      T63NRAUT = T63DIPEN + T63AUTON
050300150311      *
050400150311      * DISaccreditati ITALIANI
050500150311     c                   clear                   numero
050600150311     c                   clear                   T63ITALIA
050700150311     C/EXEC SQL
050800150311     C+ SELECT count(*)  INTO  :numero
050900150311     C+ FROM aitra00f WHERE
051000150311     C+ TRADFI between :DATA_INI    and :DATA_FIN
051100150316     C+ and substr(tracdf, 2, 1) in ('D', 'A')
051200150313     C+ and TRASCO = ' ' AND traann = ' '
051300150311     C+ and substr(tracdf, 3, 1) ='I'
051400150311     C/END-EXEC
051500150311     c                   if        sqlcod =0
051600150311     c                   eval      T63ITALIA= numero
051700150311     c                   end
051800150311      *
051900150311      * DISaccreditati ESTERI
052000150311     c                   clear                   numero
052100150311     c                   clear                   T63ESTERO
052200150311     C/EXEC SQL
052300150311     C+ SELECT count(*) INTO  :numero
052400150311     C+ FROM aitra00f WHERE
052500150311     C+ TRADFI between :DATA_INI    and :DATA_FIN
052600150316     C+ and substr(tracdf, 2, 1) in ('D', 'A')
052700150313     C+ and TRASCO = ' ' AND traann = ' '
052800150311     C+ and substr(tracdf, 3, 1) ='E'
052900150311     C/END-EXEC
053000150311     c                   if        sqlcod =0
053100150311     c                   eval      T63ESTERO= numero
053200150311     c                   end
053300150311      *
053400150311      * DISaccreditati NON definiti
053500150311     c                   clear                   numero
053600150311     c                   clear                   T63NOITES
053700150311     C/EXEC SQL
053800150311     C+ SELECT count(*) INTO  :numero
053900150311     C+ FROM aitra00f WHERE
054000150311     C+ TRADFI between :DATA_INI    and :DATA_FIN
054100150316     C+ and substr(tracdf, 2, 1) in ('D', 'A')
054200150313     C+ and TRASCO = ' ' AND traann = ' '
054300150311     C+ and substr(tracdf, 3, 1) not in ('I', 'E')
054400150311     C/END-EXEC
054500150311     c                   if        sqlcod =0
054600150311     c                   eval      T63NOITES= numero
054700150311     c                   end
054800150311      *
054900150311      * DISaccreditati DIPENDENTI FULL TIME
055000150311     c                   clear                   numero
055100150311     c                   clear                   T63FULLT
055200150311     C/EXEC SQL
055300150311     C+ SELECT count(*) INTO  :numero
055400150311     C+ FROM aitra00f WHERE
055500150311     C+ TRADFI between :DATA_INI    and :DATA_FIN
055600150313     C+ and TRASCO = ' ' AND traann = ' '
055700150313     C+ and substr(tracdf, 2, 1) ='D'
055800150313     C+ and (tradec >=40 OR  tradec = 0)
055900150311     C/END-EXEC
056000150311     c                   if        sqlcod =0
056100150311     c                   eval      T63FULLT = numero
056200150311     c                   end
056300150311      *
056400150311      * DISaccreditati DIPENDENTI PART TIME
056500150311     c                   clear                   numero
056600150311     c                   clear                   T63PARTT
056700150311     C/EXEC SQL
056800150311     C+ SELECT count(*) INTO  :numero
056900150311     C+ FROM aitra00f WHERE
057000150311     C+ TRADFI between :DATA_INI    and :DATA_FIN
057100150313     C+ and TRASCO = ' ' AND traann = ' '
057200150313     C+ and substr(tracdf, 2, 1) ='D'
057300150311     C+ and tradec < 40  and tradec > 0
057400150311     C/END-EXEC
057500150311     c                   if        sqlcod =0
057600150311     c                   eval      T63PARTT = numero
057700150311     c                   end
057800150311      *
057900150311      *    scrive rec.(4)
058000150311     c                   write     trmz6300
058100150311      *    -------------------
058200150311      *
058300150311     c                   endsr
058400011026     c*-------------------------------------------------
058500150309     c     srstampa      begsr
058600011026     c*-------------------------------------------------
058700150309     c                   do        *hival
058800150311      *
058900150311      *  Prima i 2 records della week
059000150311      *    poi i 2 records dei progressivi
059100150310     C                   read      trmz630f
059200150311      *
059300150310     c                   if        %eof(trmz630f)
059400150309     c                   leave
059500150309     c                   end
059600150311      *   WEEK
059700150311     c                   IF         T63WEKPRG ='WEK'
059800150311      *
059900150311     c                   IF           T63ACCDIS ='ACC'
060000150311     c                   exsr      MOVE_ACCWEK
060100150311     c                   elseIF       T63ACCDIS ='DIS'
060200150311     c                   exsr      MOVE_DISWEK
060300150311     c                   END
060400150311      *
060500150311      *   PROGRESSIVO
060600150311     c                   elseIF     T63WEKPRG ='PRG'
060700150311      *
060800150311     c                   IF           T63ACCDIS ='ACC'
060900150311     c                   exsr      MOVE_ACCPRG
061000150311     c                   elseIF       T63ACCDIS ='DIS'
061100150311     c                   exsr      MOVE_DISPRG
061200150311     c                   END
061300150311      *
061400150311     c                   END
061500150311      *
061600150309     c                   enddo
061700150311      *
061800150311     c                   WRITE     RIEPILOGO
061900150311      *
062000150311     c                   endsr
062100150311     c*-------------------------------------------------
062200150311     c     MOVE_ACCWEK   begsr
062300150311     c*-------------------------------------------------
062400150311     c                   eval      W63AAWW  = T63DAANWW
062500150312     c     *ISO          move      T63dal        dataiso
062600150312     c                   move      dataiso       dataeur
062700150312     c                   move      dataeur       W63DAL
062800150312     c     *ISO          move      T63al         dataiso
062900150312     c                   move      dataiso       dataeur
063000150312     c                   move      dataeur       W63AL
063100150311     c                   eval      W63AAUTON= T63AUTON
063200150311     c                   eval      W63ADIPEN= T63DIPEN
063300150311     c                   eval      W63APART = T63PARTT
063400150311     c                   eval      W63AFULT = T63FULLT
063500150311     c                   eval      W63ATOT  = T63NRAUT
063600150311     c                   eval      W63AITA  = T63ITALIA
063700150311     c                   eval      W63AEST  = T63ESTERO
063800150316     c******             if          T63NOITES <> 0
063900150316     c******             eval      W63ANIE  = 'Senza I/E'
064000150316     c******             eval      W63ANOIE = T63NOITES
064100150316     c******             end
064200150311      *
064300150311     c                   endsr
064400150311     c*-------------------------------------------------
064500150311     c     MOVE_DISWEK   begsr
064600150311     c*-------------------------------------------------
064700150311     c                   eval      W63DAUTON= T63AUTON
064800150311     c                   eval      W63DDIPEN= T63DIPEN
064900150311     c                   eval      W63DPART = T63PARTT
065000150311     c                   eval      W63DFULT = T63FULLT
065100150311     c                   eval      W63DTOT  = T63NRAUT
065200150311     c                   eval      W63DITA  = T63ITALIA
065300150311     c                   eval      W63DEST  = T63ESTERO
065400150316     c******             if          T63NOITES <> 0
065500150316     c******             eval      W63DNIE  = 'Senza I/E'
065600150316     c******             eval      W63DNOIE = T63NOITES
065700150316     c******             end
065800150311      *
065900150311     C                   eval      W63saldo = W63ATOT-W63DTOT
066000150311      *
066100150311     c                   endsr
066200150311     c*-------------------------------------------------
066300150311     c     MOVE_ACCPRG   begsr
066400150311     c*-------------------------------------------------
066500150312     c     *ISO          move      T63dal        dataiso
066600150312     c                   move      dataiso       dataeur
066700150312     c                   move      dataeur       P63DAL
066800150312     c     *ISO          move      T63al         dataiso
066900150312     c                   move      dataiso       dataeur
067000150312     c                   move      dataeur       P63AL
067100150311     c                   eval      P63AAUTON= T63AUTON
067200150311     c                   eval      P63ADIPEN= T63DIPEN
067300150311     c                   eval      P63APART = T63PARTT
067400150311     c                   eval      P63AFULT = T63FULLT
067500150311     c                   eval      P63ATOT  = T63NRAUT
067600150311     c                   eval      P63AITA  = T63ITALIA
067700150311     c                   eval      P63AEST  = T63ESTERO
067800150316     c******             if          T63NOITES <> 0
067900150316     c******             eval      P63ANIE  = 'Senza I/E'
068000150316     c******             eval      P63ANOIE = T63NOITES
068100150316     c******             end
068200150311      *
068300150311     c                   endsr
068400150311     c*-------------------------------------------------
068500150311     c     MOVE_DISPRG   begsr
068600150311     c*-------------------------------------------------
068700150311     c                   eval      P63DAUTON= T63AUTON
068800150311     c                   eval      P63DDIPEN= T63DIPEN
068900150311     c                   eval      P63DPART = T63PARTT
069000150311     c                   eval      P63DFULT = T63FULLT
069100150311     c                   eval      P63DTOT  = T63NRAUT
069200150311     c                   eval      P63DITA  = T63ITALIA
069300150311     c                   eval      P63DEST  = T63ESTERO
069400150316     c******             if          T63NOITES <> 0
069500150316     c******             eval      P63DNIE  = 'Senza I/E'
069600150316     c******             eval      P63DNOIE = T63NOITES
069700150316     c******             end
069800150311      *
069900150312     C                   eval      P63saldo = P63ATOT-P63DTOT
070000150311      *
070100150311     c                   endsr
070200150311     c*-------------------------------------------------
070300150311     c     *inzsr        begsr
070400150311     c*-------------------------------------------------
070500150311     c     *entry        plist
070600150311     c                   parm                    kpjba
070700150311     C*
070800150311     C                   time                    w0120            14 0
070900150311     C                   move      w0120         wdat              8 0
071000150311     c                   move      wdat          dataeur
071100150311     c                   move      dataeur       dataiso
071200150311     c                   move      dataiso       wdat
071300150311      *
071400150311     C                   CLEAR                   Tibs36Ds
071500150311     C                   EVAL      I36ute = knmus
071600150311     C                   CALL      'TIBS36R'
071700150311     C                   PARM                    tibs36ds
071800150311      *
071900150311     C* INIZIALIZZA VARIABILI DI WRK
072000150311     C                   CLEAR                   TIBS34DS
072100150311     C                   CLEAR                   AZUTEDS
072200150311     C                   CLEAR                   DDATIUTE
072300150311     C*
072400150311     C     *DTAARA       DEFINE    §azute        azuteds
072500150311     C     *DTAARA       DEFINE    §datiute      ddatiute
072600150311     C                   IN(E)     *DTAARA
072700150311     C                   IF        %Error
072800150311     C                   EVAL      I34Tla = 'L'
072900150311     C                   CALL      'TIBS34R'
073000150311     C                   PARM                    Tibs34Ds
073100150311     C                   IN        *DTAARA
073200150311     C                   ENDIF
073300150311     C*
073400150311     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
073500150311     C                   MOVEL(P)  RSUT          DSFIRS           20
073600150311     C*
073700150311     c*  Controlla COME è stato richiamato
073800150311     C*   ed imposta i parametri di ingresso di default oppure li ha ricevuti
073900150311     c*   per una elaborazione particolare.
074000150311     c                   exsr      come_chiamato
074100150311     C*
074200150311      *clear file
074300150311     c                   movea     cmd1          coman
074400150311     c                   exsr      comandi
074500150311     C*
074600150311     c                   movea     cmd2          coman
074700150311     c                   exsr      comandi
074800150311     C*
074900150311     c                   open      trmz630f
075000150311
075100150311     c                   endsr
075200150311     c*-------------------------------------------------
075300150311      **  Verifica come è stato Richiamato se Skedulato
075400150311     c*-------------------------------------------------
075500150311     c     Come_chiamato begsr
075600150311      **
075700150311     c                   move      *blank        schedulato        1
075800150311      **
075900150311      **  se Skedulato da sottomissione automatica
076000150311      **     quindi deve prendere i dati della settimana scorsa ed
076100150311      **     il progressivo dall'inizio dell'anno.
076200150311     c                   if        kpjbu = *blank
076300150311     c                   eval      schedulato = 'S'
076400150311     c                   clear                   trmz63ds
076500150311     c                   eval      i63tipoela = 'S'
076600150311      **
076700150311      *** NON AVENDO PARAMETRI du INGRESSO   prendendo come riferimento OGGI
076800150311      ***    ricava il primo e ultimo giorno della settimana PRECEDENTE
076900150311      ***       lunedì e domenica della settimana precedente
077000150311     C/EXEC SQL
077100150311     C+ SELECT DISTINCT
077200150311     C+   date(days(DC_WEEK_STARTING_DATE)-7),
077300150311     C+   date(days(DC_WEEK_ENDING_DATE)-7),
077400150311     C+   DC_YOY_ISO,
077500150311     C+   curdate()
077600150311     C+ INTO  :da_Lunedi_scorso, :a_domenica_scorsa, :anno_week, :oggi
077700150311     C+ FROM DATECNV0F
077800150311     C+ WHERE DC_DATE = curdate()
077900150311     C/END-EXEC
078000150311     c                   eval      dataiso = da_Lunedi_scorso
078100150311     c                   move      dataiso       DATA_INI_wk
078200150311     c                   eval      dataiso = a_domenica_scorsa
078300150311     c                   move      dataiso       DATA_FIN_wk
078400150311     c                   move      dataiso       DATA_FIN
078500150311      **  deve calcolare la data inizio periodo dall'inizio dell'anno
078600150311     C/EXEC SQL
078700150311     C+   SELECT DISTINCT
078800150311     C+     date(days(DC_WEEK_STARTING_DATE))
078900150311     C+ INTO  :dataiso
079000150311     C+   FROM DATECNV0F
079100150311     C+   WHERE DC_YOY_ISO = :anno_week and DC_WOY_ISO= 01
079200150311     C/END-EXEC
079300150311     c                   move      dataiso       DATA_INI
079400150311      **
079500150311     c                   else
079600150311      **
079700150311      **   Avendo un range definito deve cercare la data di inizio settimana
079800150311      **   di riferimento dell'estrazione.
079900150311     c                   movel     kpjbu         trmz63ds
080000150311     c                   move      i63datdal     DATA_INI
080100150311     c                   move      i63datal      DATA_FIN
080200150311     c                   move      i63datal      DATA_FIN_wk
080300150311     c     *iso          move      i63datal      dataiso
080400150311      **
080500150311      **  deve calcolare la data inizio settimana
080600150311     C/EXEC SQL
080700150311     C+   SELECT DISTINCT
080800150311     C+     date(days(DC_WEEK_STARTING_DATE))
080900150311     C+ INTO  :dataiso
081000150311     C+   FROM DATECNV0F
081100150311     C+   WHERE date(days(DC_WEEK_ENDING_DATE)) = :dataISO
081200150311     C/END-EXEC
081300150311     c                   move      dataiso       DATA_INI_wk
081400150311     c                   end
081500150311      **
081600150309     c                   endsr
081700150311     c*-------------------------------------------------
081800150311     c     comandi       begsr
081900150311     c*-------------------------------------------------
082000150311     C                   z-add     99            luncmd
082100150311     C                   CALL      'QCMDEXC'                            34
082200150311     C                   parm                    coman
082300150311     C                   parm                    luncmd
082400150311      *
082500150311     c                   endsr
082600150311     c*-------------------------------------------------
082700090519**  CMD1
082800150310CLRPFM    FILE(trmz630f)
082900090519**  CMD2
083000150310CRTDUPOBJ OBJ(trmz630F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
083100090519**  CMD3
083200150310DLTF FILE(QTEMP/trmz630F)
083300090519**  CMD4
083400150310dltf file(WRKQUERY/trmz630f)
083500090519**  CMD5
083600150310CRTDUPOBJ OBJ(trmz630F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(trmz630f) DATA(*YES)
