000100130328     H* FIDG60R *-----------------------------------------------------*
000200930225     H*---------------------------------------------------------------*
000300110916     Ffipdc00t  uf   E             DISK
000400110916     Ffipdd00t  uf   E             DISK
000500110916     Ffipdo00t  uf   E             DISK
000600130328     Ffipct00f  uf   E             DISK
000700130328     Ffipcd03l  uf   E           k DISK
000800900514     D KPJBA         E DS
000900140211      *  pulizia FICAU
001000140211     d fnlrx4ds      e ds                  INZ
001100140211     d fidst         e ds                  INZ extname(fidst00f)
001200140211      *
001300110916     drrnpdc           s              9  0
001400110916     drrnpdd           s              9  0
001500110916     drrnpdo           s              9  0
001600130328     drrnpct           s              9  0
001700110916     C     *ENTRY        PLIST
001800110916     C                   PARM                    kpjba
001900140211      *
002000130328     c     kpcd          klist
002100130328     c                   kfld                    pctfgs
002200130328     c                   kfld                    pctndc
002300140211      *
002400110916     c* dettaglio bolle
002500110801     C/EXEC SQL
002600110916     C+ DECLARE A1 CURSOR FOR
002700110916     C+ SELECT rrn(fipdd00t) FROM fidst00f join fipdd00t on
002800110916     C+ dstfgs = pddfgs and dstnfv = pddndc WHERE dstatb <> ' ' and pddatb
002900110916     C+ <> ' ' and dstnpg = 4
003000110801     C/END-EXEC
003100110726
003200110726     C/EXEC SQL
003300110916     C+ OPEN a1
003400110726     C/END-EXEC
003500110726
003600110726     C                   do        *hival
003700110726
003800110726     C/EXEC SQL
003900110916     C+ FETCH NEXT FROM a1 INTO :rrnpdd
004000110726     C/END-EXEC
004100110726
004200110726     C                   select
004300110726     C                   when      sqlcod = 100
004400110726     c                   leave
004500110726     C                   WHEN      SqlCod < 0
004600110726     c                   if        sqlcod <> -913
004700110726     C                   seton                                        H1
004800110726     c                   end
004900110916     c                   return
005000110726     C                   other
005100110916     c     RRNpdd        chain(e)  fipdd00t
005200110801     c                   if        %error
005300110801     c                   iter
005400110801     c                   end
005500110916     c                   if        %found(fipdd00t)
005600110916     c                   delete    fipdd000
005700110801     c                   iter
005800110801     c                   end
005900110726     C                   endsl
006000110726     C*
006100110726     C                   enddo
006200110726
006300110726     C/EXEC SQL
006400110916     C+ CLOSE a1
006500110726     C/END-EXEC
006600140211
006700110916     c* testata bolle
006800110916     C/EXEC SQL
006900110916     C+ DECLARE B1 CURSOR FOR
007000110916     C+ SELECT rrn(fipdc00t) FROM fidst00f join fipdc00t on
007100110916     C+ dstfgs = pdcfgs and dstnfv = pdcndc WHERE dstatb <> ' ' and pdcatb
007200110916     C+ <> ' ' and dstnpg = 4
007300110916     C/END-EXEC
007400110916
007500110916     C/EXEC SQL
007600110916     C+ OPEN b1
007700110916     C/END-EXEC
007800110916
007900110916     C                   do        *hival
008000110916
008100110916     C/EXEC SQL
008200110916     C+ FETCH NEXT FROM B1 INTO :rrnpdc
008300110916     C/END-EXEC
008400110916
008500110916     C                   select
008600110916     C                   when      sqlcod = 100
008700110916     c                   leave
008800110916     C                   WHEN      SqlCod < 0
008900110916     c                   if        sqlcod <> -913
009000110916     C                   seton                                        H1
009100110916     c                   end
009200110916     c                   return
009300110916     C                   other
009400110916     c     RRNpdc        chain(e)  fipdc00t
009500110916     c                   if        %error
009600110916     c                   iter
009700110916     c                   end
009800110916     c                   if        %found(fipdc00t)
009900110916     c                   delete    fipdc000
010000110916     c                   iter
010100110916     c                   end
010200110916     C                   endsl
010300110916     C*
010400110916     C                   enddo
010500110916
010600110916     C/EXEC SQL
010700110916     C+ CLOSE b1
010800110916     C/END-EXEC
010900140211
011000110916     c* orm
011100110916     C/EXEC SQL
011200110916     C+ DECLARE c1 CURSOR FOR
011300110916     C+ SELECT rrn(fipdo00t) FROM fidst00f join fipdo00t on
011400110916     C+ dstfgs = pdofgs and dstnfv = pdondc WHERE dstatb <> ' ' and pdoatb
011500110916     C+ <> ' ' and dstnpg = 4
011600110916     C/END-EXEC
011700110916
011800110916     C/EXEC SQL
011900110916     C+ OPEN c1
012000110916     C/END-EXEC
012100110916
012200110916     C                   do        *hival
012300110916
012400110916     C/EXEC SQL
012500110916     C+ FETCH NEXT FROM c1 INTO :rrnpdo
012600110916     C/END-EXEC
012700110916
012800110916     C                   select
012900110916     C                   when      sqlcod = 100
013000110916     c                   leave
013100110916     C                   WHEN      SqlCod < 0
013200110916     c                   if        sqlcod <> -913
013300110916     C                   seton                                        H1
013400110916     c                   end
013500110916     c                   return
013600110916     C                   other
013700110916     c     RRNpdo        chain(e)  fipdo00t
013800110916     c                   if        %error
013900110916     c                   iter
014000110916     c                   end
014100110916     c                   if        %found(fipdo00t)
014200110916     c                   delete    fipdo000
014300110916     c                   iter
014400110916     c                   end
014500110916     C                   endsl
014600110916     C*
014700110916     C                   enddo
014800110916
014900110916     C/EXEC SQL
015000110916     C+ CLOSE c1
015100110916     C/END-EXEC
015200130328      *
015300130328     C/EXEC SQL
015400130328     C+ DECLARE d1 CURSOR FOR
015500130328     C+ SELECT rrn(fipct00f) FROM fidst00f join fipct00f on
015600130328     C+ dstfgs = pctfgs and dstnfv = pctndc WHERE dstfcf = 'S'
015700130328     C+ and dstnpg = 4
015800130328     C/END-EXEC
015900130328
016000130328     C/EXEC SQL
016100130402     C+ OPEN d1
016200130328     C/END-EXEC
016300130328
016400130328     C                   do        *hival
016500130328
016600130328     C/EXEC SQL
016700130328     C+ FETCH NEXT FROM d1 INTO :rrnpct
016800130328     C/END-EXEC
016900130328
017000130328     C                   select
017100130328     C                   when      sqlcod = 100
017200130328     c                   leave
017300130328     C                   WHEN      SqlCod < 0
017400130328     c                   if        sqlcod <> -913
017500130328     C                   seton                                        H1
017600130328     c                   end
017700130328     c                   return
017800130328     C                   other
017900130328     c     RRNpct        chain(e)  fipct00f
018000130328     c                   if        %error
018100130328     c                   iter
018200130328     c                   end
018300130328     c                   if        %found(fipct00f)
018400130328     c     kpcd          setll     fipcd03l
018500130328     c                   do        *hival
018600130328     c     kpcd          reade     fipcd03l
018700130328     c                   if        %eof(fipcd03l)
018800130328     c                   leave
018900130328     c                   endif
019000130328     c                   delete    fipcd000
019100130328     c                   enddo
019200130328     c                   delete    fipct000
019300130328     c                   iter
019400130328     c                   end
019500130328     C                   endsl
019600130328     C*
019700130328     C                   enddo
019800130328
019900130328     C/EXEC SQL
020000130328     C+ CLOSE d1
020100130328     C/END-EXEC
020200130912      *                 in exception
020300130912      *________________________________________________________________________
020400130912     c* dettaglio bolle
020500130912     C/EXEC SQL
020600130912     C+ DECLARE A2 CURSOR FOR
020700130912     C+ SELECT rrn(fipdd00t) FROM fipdd00t exception join fidst00f on
020800130912     C+ dstfgs = pddfgs and dstnfv = pddndc and dstnpg = 4
020900130912     C/END-EXEC
021000130912
021100130912     C/EXEC SQL
021200130912     C+ OPEN a2
021300130912     C/END-EXEC
021400130912
021500130912     C                   do        *hival
021600130912
021700130912     C/EXEC SQL
021800130912     C+ FETCH NEXT FROM a2 INTO :rrnpdd
021900130912     C/END-EXEC
022000130912
022100130912     C                   select
022200130912     C                   when      sqlcod = 100
022300130912     c                   leave
022400130912     C                   WHEN      SqlCod < 0
022500130912     c                   if        sqlcod <> -913
022600130912     C                   seton                                        H1
022700130912     c                   end
022800130912     c                   return
022900130912     C                   other
023000130912     c     RRNpdd        chain(e)  fipdd00t
023100130912     c                   if        %error
023200130912     c                   iter
023300130912     c                   end
023400130912     c                   if        %found(fipdd00t)
023500130912     c                   delete    fipdd000
023600130912     c                   iter
023700130912     c                   end
023800130912     C                   endsl
023900130912     C*
024000130912     C                   enddo
024100110726     c                   seton                                        lr
024200130912     C/EXEC SQL
024300130912     C+ CLOSE a2
024400130912     C/END-EXEC
024500130912     c* testata bolle
024600130912     C/EXEC SQL
024700130912     C+ DECLARE B2 CURSOR FOR
024800130912     C+ SELECT rrn(fipdc00t) FROM fipdc00t exception join fidst00f on
024900130912     C+ dstfgs = pdcfgs and dstnfv = pdcndc and dstnpg = 4
025000130912     C/END-EXEC
025100130912
025200130912     C/EXEC SQL
025300130912     C+ OPEN b2
025400130912     C/END-EXEC
025500130912
025600130912     C                   do        *hival
025700130912
025800130912     C/EXEC SQL
025900130912     C+ FETCH NEXT FROM B2 INTO :rrnpdc
026000130912     C/END-EXEC
026100130912
026200130912     C                   select
026300130912     C                   when      sqlcod = 100
026400130912     c                   leave
026500130912     C                   WHEN      SqlCod < 0
026600130912     c                   if        sqlcod <> -913
026700130912     C                   seton                                        H1
026800130912     c                   end
026900130912     c                   return
027000130912     C                   other
027100130912     c     RRNpdc        chain(e)  fipdc00t
027200130912     c                   if        %error
027300130912     c                   iter
027400130912     c                   end
027500130912     c                   if        %found(fipdc00t)
027600130912     c                   delete    fipdc000
027700130912     c                   iter
027800130912     c                   end
027900130912     C                   endsl
028000130912     C*
028100130912     C                   enddo
028200130912
028300130912     C/EXEC SQL
028400130912     C+ CLOSE b2
028500130912     C/END-EXEC
028600130912     c* orm
028700130912     C/EXEC SQL
028800130912     C+ DECLARE c2 CURSOR FOR
028900130912     C+ SELECT rrn(fipdo00t) FROM fipdo00t exception join fidst00f on
029000130912     C+ dstfgs = pdofgs and dstnfv = pdondc and dstnpg = 4
029100130912     C/END-EXEC
029200130912
029300130912     C/EXEC SQL
029400130912     C+ OPEN c2
029500130912     C/END-EXEC
029600130912
029700130912     C                   do        *hival
029800130912
029900130912     C/EXEC SQL
030000130912     C+ FETCH NEXT FROM c2 INTO :rrnpdo
030100130912     C/END-EXEC
030200130912
030300130912     C                   select
030400130912     C                   when      sqlcod = 100
030500130912     c                   leave
030600130912     C                   WHEN      SqlCod < 0
030700130912     c                   if        sqlcod <> -913
030800130912     C                   seton                                        H1
030900130912     c                   end
031000130912     c                   return
031100130912     C                   other
031200130912     c     RRNpdo        chain(e)  fipdo00t
031300130912     c                   if        %error
031400130912     c                   iter
031500130912     c                   end
031600130912     c                   if        %found(fipdo00t)
031700130912     c                   delete    fipdo000
031800130912     c                   iter
031900130912     c                   end
032000130912     C                   endsl
032100130912     C*
032200130912     C                   enddo
032300130912
032400130912     C/EXEC SQL
032500130912     C+ CLOSE c2
032600130912     C/END-EXEC
032700130912      *
032800130912     C/EXEC SQL
032900130912     C+ DECLARE d2 CURSOR FOR
033000130912     C+ SELECT rrn(fipct00f) FROM fipct00f exception join fidst00f on
033100130912     C+ dstfgs = pctfgs and dstnfv = pctndc and dstnpg = 4
033200130912     C/END-EXEC
033300130912
033400130912     C/EXEC SQL
033500130912     C+ OPEN d2
033600130912     C/END-EXEC
033700130912
033800130912     C                   do        *hival
033900130912
034000130912     C/EXEC SQL
034100130912     C+ FETCH NEXT FROM d2 INTO :rrnpct
034200130912     C/END-EXEC
034300130912
034400130912     C                   select
034500130912     C                   when      sqlcod = 100
034600130912     c                   leave
034700130912     C                   WHEN      SqlCod < 0
034800130912     c                   if        sqlcod <> -913
034900130912     C                   seton                                        H1
035000130912     c                   end
035100130912     c                   return
035200130912     C                   other
035300130912     c     RRNpct        chain(e)  fipct00f
035400130912     c                   if        %error
035500130912     c                   iter
035600130912     c                   end
035700130912     c                   if        %found(fipct00f)
035800130912     c     kpcd          setll     fipcd03l
035900130912     c                   do        *hival
036000130912     c     kpcd          reade     fipcd03l
036100130912     c                   if        %eof(fipcd03l)
036200130912     c                   leave
036300130912     c                   endif
036400130912     c                   delete    fipcd000
036500130912     c                   enddo
036600130912     c                   delete    fipct000
036700130912     c                   iter
036800130912     c                   end
036900130912     C                   endsl
037000130912     C*
037100130912     C                   enddo
037200130912
037300130912     C/EXEC SQL
037400130912     C+ CLOSE d2
037500130912     C/END-EXEC
037600140211     C*
037700140211     C*  Pulizia del FICAU
037800140211      *
037900140211     C/EXEC SQL
038000140211     C+ DECLARE dstficau CURSOR FOR
038100140211     C+ SELECT * from fidst00f where
038200140211     C+  dstnpg = 4  and  (dstfcf = 'S' or dstatb <> ' ')
038300140211     C/END-EXEC
038400140211
038500140211     C/EXEC SQL
038600140211     C+ OPEN dstficau
038700140211     C/END-EXEC
038800140211
038900140211     C                   do        *hival
039000140211
039100140211     C/EXEC SQL
039200140211     C+ FETCH NEXT FROM dstficau INTO :fidst
039300140211     C/END-EXEC
039400140211
039500140211     C                   select
039600140211     C                   when      sqlcod = 100
039700140211     c                   leave
039800140211     C                   WHEN      SqlCod < 0
039900140211     c                   if        sqlcod <> -913
040000140211     C                   seton                                        H1
040100140211     c                   end
040200140211     c                   return
040300140211     C                   other
040400140211      ***
040500140211     C*  Pulizia del FICAU
040600140211      *
040700140211     c                   clear                   fnlrx4ds
040800140211     c                   eval      IX4FGS =  dstfgs
040900140211     c                   eval      IX4NFV =  dstnfv
041000140211     c                   call      'FNLRX4R'
041100140211     c                   parm                    kpjba
041200140211     c                   parm                    fnlrx4ds
041300140211      *
041400140211     C                   endsl
041500140211     C*
041600140211     C                   enddo
041700140211
041800140211     C/EXEC SQL
041900140211     C+ CLOSE dstficau
042000140211     C/END-EXEC
042100140211     C*
042200140211     C*
