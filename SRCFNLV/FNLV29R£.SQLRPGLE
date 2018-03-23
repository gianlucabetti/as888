000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000102090310     ffiapd01l  if   e           k disk
000813090316     d kpjba         e ds
000814090831     d fnlv29ds      e ds
000815090831     d udatiso         s               d   Datfmt(*iso)
000816090831     d oggim1          s              8  0
000817090831     D WLBDAT          DS
000818090831     D  G02DAT                 1      8  0
000819090831     D  G02INV                 9     16  0
000820090831     D  G02ERR                17     17
000821090831     D  G02TGI                18     22  0
000847090310     c                   seton                                        lr
000848090831     C* AUTISTI CITTÀ
000849090831     c     'A'           setll     fiapd01l
000850090831     c                   do        *hival
000851090831     c     'A'           reade     fiapd01l
000852090831     c                   if        %eof(fiapd01l)
000853090831     c                   leave
000854090831     c                   end
000855090831     c                   if        apdatb <> ' '
000856090831     c                             or apddtr >= oggim1
000857090831     c                   iter
000858090831     c                   end
000859090831     c* bolle di sede
000860090831     c                   if        flgtas0 ='S'
000861090831     c                   clear                   conta
006726090720     C/EXEC SQL
006727090831     C+ SELECT count(*) INTO :conta FROM titas00f WHERE taspdc = :apdpdr
006728090831     C+ or taspdr = :apdpdr
006739090720     C/END-EXEC
006749090831     c                   if        conta > 0
006750090831     c                   iter
006751090831     c                   end
006752090831     c                   end
006753090831     c                   if        flgtas10 ='S'
006754090831     c                   clear                   conta
006755090831     C/EXEC SQL
006756090831     C+ SELECT count(*) INTO :conta FROM titas10f WHERE taspdc = :apdpdr
006757090831     C+ or taspdr = :apdpdr
006758090831     C/END-EXEC
006759090831     c                   if        conta > 0
006760090831     c                   iter
006761090831     c                   end
006762090831     c                   end
006763090831     c                   if        flgtasp ='S'
006764090831     c                   clear                   conta
006765090831     C/EXEC SQL
006766090831     C+ SELECT count(*) INTO :conta FROM titasp0f WHERE taspdc = :apdpdr
006767090831     C+ or taspdr = :apdpdr
006768090831     C/END-EXEC
006769090831     c                   if        conta > 0
006770090831     c                   iter
006771090831     c                   end
006772090831     c                   end
006773090831     C* CERTIFICAZIONI AUT
006774090831     c                   if        flgTRAA ='S'
006775090831     c                   clear                   conta
006776090831     C/EXEC SQL
006777090831     C+ SELECT count(*) INTO :conta FROM AITRA00f WHERE TRAKAU = :apdpdr
006778090831     C/END-EXEC
006779090831     c                   if        conta > 0
006780090831     c                   iter
006781090831     c                   end
006782090831     c                   end
006783090831     c* bolle di filiale
006784090831     c                   if        flgarb  ='S'
006785090831     c                   clear                   conta
006786090831     C/EXEC SQL
006787090831     C+ SELECT count(*) INTO :conta FROM FNARB00f WHERE ARBPDC = :apdpdr
006788090831     C/END-EXEC
006789090831     c                   if        conta > 0
006790090831     c                   iter
006791090831     c                   end
006792090831     c                   end
006793090831     c                   if        flgBLP  ='S'
006794090831     c                   clear                   conta
006795090831     C/EXEC SQL
006796090831     C+ SELECT count(*) INTO :conta FROM FNBLP00f WHERE ARBPDR = :apdpdr
006797090831     C/END-EXEC
006798090831     c                   if        conta > 0
006799090831     c                   iter
006800090831     c                   end
006801090831     c                   end
006802090831     C* TARIFFE AUT
006803090831     c                   if        flgTGT  ='S'
006804090831     c                   clear                   conta
006805090831     C/EXEC SQL
006806090831     C+ SELECT count(*) INTO :conta FROM FITGT00f WHERE TGTPDR = :apdpdr
006807090831     C/END-EXEC
006808090831     c                   if        conta > 0
006809090831     c                   iter
006810090831     c                   end
006811090831     c                   end
006812090831     C* VALORIZZAZIONI AUT
006813090831     c                   if        flgFTT  ='S'
006814090831     c                   clear                   conta
006815090831     C/EXEC SQL
006816090831     C+ SELECT count(*) INTO :conta FROM FIFTT00f WHERE FTTPDR = :apdpdr
006817090831     C/END-EXEC
006818090831     c                   if        conta > 0
006819090831     c                   iter
006820090831     c                   end
006821090831     c                   end
006822090831     C* DISTINTE AUT
006823090831     c                   if        flgDST  ='S'
006824090831     c                   clear                   conta
006825090831     C/EXEC SQL
006826090831     C+ SELECT count(*) INTO :conta FROM FIDST00f WHERE DSTPDR = :apdpdr
006827090831     C/END-EXEC
006828090831     c                   if        conta > 0
006829090831     c                   iter
006830090831     c                   end
006831090831     c                   end
006832090831     C* SE SONO ARRIVATO QUI SIGNIFICA CHE POSSO ANNULLARE L'ANAGRAFICO
006833090831     C                   EVAL      APDATB = 'A'
006834090831     C                   UPDATE    FIAPD000
006835090720     C*
006836090720     C                   enddo
006837090831     C* AUTISTI AFF/DEFL
006838090831     c     'D'           setll     fiapd01l
006839090831     c                   do        *hival
006840090831     c     'D'           reade     fiapd01l
006841090831     c                   if        %eof(fiapd01l)
006842090831     c                   leave
006843090831     c                   end
006844090831     c                   if        apdatb <> ' '
006845090831     c                             or apddtr >= oggim1
006846090831     c                   iter
006847090831     c                   end
006879090831     C* CERTIFICAZIONI AUT
006880090831     c                   if        flgTRAD ='S'
006881090831     c                   clear                   conta
006882090831     C/EXEC SQL
006883090831     C+ SELECT count(*) INTO :conta FROM AITRA00f WHERE TRABPT = :apdpdr
006884090831     C/END-EXEC
006885090831     c                   if        conta > 0
006886090831     c                   iter
006887090831     c                   end
006888090831     c                   end
006908090831     C* TARIFFE
006909090831     c                   if        flgADT  ='S'
006910090831     c                   clear                   conta
006911090831     C/EXEC SQL
006912090831     C+ SELECT count(*) INTO :conta FROM FIADT00f WHERE ADTPDR = :apdpdr
006913090831     C/END-EXEC
006914090831     c                   if        conta > 0
006915090831     c                   iter
006916090831     c                   end
006917090831     c                   end
006918090831     C* VALORIZZAZIONI
006919090831     c                   if        flgATT  ='S'
006920090831     c                   clear                   conta
006921090831     C/EXEC SQL
006922090831     C+ SELECT count(*) INTO :conta FROM FIATT00f WHERE ATTPDR = :apdpdr
006923090831     C/END-EXEC
006924090831     c                   if        conta > 0
006925090831     c                   iter
006926090831     c                   end
006927090831     c                   end
006938090831     C* SE SONO ARRIVATO QUI SIGNIFICA CHE POSSO ANNULLARE L'ANAGRAFICO
006939090831     C                   EVAL      APDATB = 'A'
006940090831     C                   UPDATE    FIAPD000
006941090831     C*
006942090831     C                   enddo
018425070921     C**********************************************************************
018426070921     C     *inzsr        BEGSR
018427070921     C**********************************************************************
018428070921     c     *entry        plist
018429070730     c                   parm                    kpjba
018430090831     C                   MOVEL     kpjbu         fnlv29ds
018431090831     C                   TIME                    W0140            14 0
018432090831     C                   MOVE      W0140         UDATE8            8 0
018433090831     C                   Z-ADD     UDATE8        G02DAT
018434090831     C                   MOVE      *ZEROS        G02INV
018435090831     C                   MOVE      *BLANKS       G02ERR
018436090831     C                   CALL      'XSRDA8'
018437090831     C                   PARM                    WLBDAT
018438090831     C                   Z-ADD     G02INV        DATEU8            8 0
018439090831     C                   MOVE      g02inv        UDATiso
018440090831     c     udatiso       subdur    30:*d         udatiso
018441090831     c                   move      udatiso       oggim1
019416070731      *
019417070731     c                   ENDSR
