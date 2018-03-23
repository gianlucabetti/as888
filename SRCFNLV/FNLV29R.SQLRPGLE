000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000102090831     ffiapd01l  uf   e           k disk
000813090316     d kpjba         e ds
000814090831     d fnlv29ds      e ds
000815090831     d udatiso         s               d   Datfmt(*iso)
000816090831     d oggim1          s              8  0
000817090831     d conta           s              9  0
000818090831     D WLBDAT          DS
000819090831     D  G02DAT                 1      8  0
000820090831     D  G02INV                 9     16  0
000821090831     D  G02ERR                17     17
000822090831     D  G02TGI                18     22  0
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
006787090831     C* CERTIFICAZIONI AUT
006788090831     c                   if        flgTRAA ='S'
006789090831     c                   clear                   conta
006790090831     C/EXEC SQL
006791090831     C+ SELECT count(*) INTO :conta FROM AITRA00f WHERE TRAKAU = :apdpdr
006792110524     C+ and tradfi = 0
006793090831     C/END-EXEC
006794090831     c                   if        conta > 0
006795090831     c                   iter
006796090831     c                   end
006797090831     c                   end
006798090901     C* TARIFFE AUT
006799090901     c                   if        flgTGT  ='S'
006800090901     c                   clear                   conta
006801090901     C/EXEC SQL
006802090901     C+ SELECT count(*) INTO :conta FROM FITGT00f WHERE TGTPDR = :apdpdr
006803110524     C+ and TGTDST > oggim1
006804090901     C/END-EXEC
006805090901     c                   if        conta > 0
006806090901     c                   iter
006807090901     c                   end
006808090901     c                   end
006809090901     C* DISTINTE AUT
006810090901     c                   if        flgDST  ='S'
006811090901     c                   clear                   conta
006812090901     C/EXEC SQL
006813090901     C+ SELECT count(*) INTO :conta FROM FIDST00f WHERE DSTPDR = :apdpdr
006814110524     C+ and DSTDFV > oggim1
006815090901     C/END-EXEC
006816090901     c                   if        conta > 0
006817090901     c                   iter
006818090901     c                   end
006819090901     c                   end
006820090831     c* bolle di filiale
006821090831     c                   if        flgarb  ='S'
006822090831     c                   clear                   conta
006823090831     C/EXEC SQL
006824090831     C+ SELECT count(*) INTO :conta FROM FNARB00f WHERE ARBPDC = :apdpdr
006825090831     C/END-EXEC
006826090831     c                   if        conta > 0
006827090831     c                   iter
006828090831     c                   end
006829090831     c                   end
006830090831     c                   if        flgBLP  ='S'
006831090831     c                   clear                   conta
006832090831     C/EXEC SQL
006833090831     C+ SELECT count(*) INTO :conta FROM FNBLP00f WHERE ARBPDR = :apdpdr
006834090831     C/END-EXEC
006835090831     c                   if        conta > 0
006836090831     c                   iter
006837090831     c                   end
006838090831     c                   end
006839090831     C* VALORIZZAZIONI AUT
006840090831     c                   if        flgFTT  ='S'
006841090831     c                   clear                   conta
006842090831     C/EXEC SQL
006843090831     C+ SELECT count(*) INTO :conta FROM FIFTT00f WHERE FTTPDR = :apdpdr
006844110524     C+ and fttddc > oggim1
006845090831     C/END-EXEC
006846090831     c                   if        conta > 0
006847090831     c                   iter
006848090831     c                   end
006849090831     c                   end
006850090901     c* bolle di sede
006851090901     c                   if        flgtas0 ='S'
006852090901     c                   clear                   conta
006853090901     c                   if        flgprova = 'S'
006854090901     C/EXEC SQL
006855090901     C+ SELECT count(*) INTO :conta FROM gaitragrps/titas00f WHERE taspdc
006856090901     C+ = :apdpdr or taspdr = :apdpdr
006857090901     C/END-EXEC
006858090901     c                   else
006859090901     C/EXEC SQL
006860090901     C+ SELECT count(*) INTO :conta FROM gaitragru/titas00f WHERE taspdc
006861090901     C+ = :apdpdr or taspdr = :apdpdr
006862090901     C/END-EXEC
006863090901     c                   end
006864090901     c                   if        conta > 0
006865090901     c                   iter
006866090901     c                   end
006867090901     c                   end
006868090901     c                   if        flgtas10 ='S'
006869090901     c                   clear                   conta
006870090901     c                   if        flgprova = 'S'
006871090901     C/EXEC SQL
006872090901     C+ SELECT count(*) INTO :conta FROM gaitragrps/titas10f WHERE taspdc
006873090901     C+ = :apdpdr or taspdr = :apdpdr
006874090901     C/END-EXEC
006875090901     c                   else
006876090901     C/EXEC SQL
006877090901     C+ SELECT count(*) INTO :conta FROM gaitragru/titas10f WHERE taspdc
006878090901     C+ = :apdpdr or taspdr = :apdpdr
006879090901     C/END-EXEC
006880090901     c                   end
006881090901     c                   if        conta > 0
006882090901     c                   iter
006883090901     c                   end
006884090901     c                   end
006885090901     c                   if        flgtasp ='S'
006886090901     c                   clear                   conta
006887090901     c                   if        flgprova = 'S'
006888090901     C/EXEC SQL
006889090901     C+ SELECT count(*) INTO :conta FROM gaitragrps/titasp0f WHERE taspdc
006890090901     C+ = :apdpdr or taspdr = :apdpdr
006891090901     C/END-EXEC
006892090901     c                   else
006893090901     C/EXEC SQL
006894090901     C+ SELECT count(*) INTO :conta FROM gaitragrus/titasp0f WHERE taspdc
006895090901     C+ = :apdpdr or taspdr = :apdpdr
006896090901     C/END-EXEC
006897090901     c                   end
006898090901     c                   if        conta > 0
006899090901     c                   iter
006900090901     c                   end
006901090901     c                   end
006902090831     C* SE SONO ARRIVATO QUI SIGNIFICA CHE POSSO ANNULLARE L'ANAGRAFICO
006903090831     C                   EVAL      APDATB = 'A'
006904090831     C                   UPDATE    FIAPD000
006905090720     C*
006906090720     C                   enddo
006907090831     C* AUTISTI AFF/DEFL
006908090831     c     'D'           setll     fiapd01l
006909090831     c                   do        *hival
006910090831     c     'D'           reade     fiapd01l
006911090831     c                   if        %eof(fiapd01l)
006912090831     c                   leave
006913090831     c                   end
006914090831     c                   if        apdatb <> ' '
006915090831     c                             or apddtr >= oggim1
006916090831     c                   iter
006917090831     c                   end
006918090831     C* CERTIFICAZIONI AUT
006919090831     c                   if        flgTRAD ='S'
006920090831     c                   clear                   conta
006921090831     C/EXEC SQL
006922090831     C+ SELECT count(*) INTO :conta FROM AITRA00f WHERE TRABPT = :apdpdr
006923090831     C/END-EXEC
006924090831     c                   if        conta > 0
006925090831     c                   iter
006926090831     c                   end
006927090831     c                   end
006928090831     C* TARIFFE
006929090831     c                   if        flgADT  ='S'
006930090831     c                   clear                   conta
006931090831     C/EXEC SQL
006932090831     C+ SELECT count(*) INTO :conta FROM FIADT00f WHERE ADTPDR = :apdpdr
006933090831     C/END-EXEC
006934090831     c                   if        conta > 0
006935090831     c                   iter
006936090831     c                   end
006937090831     c                   end
006938090831     C* VALORIZZAZIONI
006939090831     c                   if        flgATT  ='S'
006940090831     c                   clear                   conta
006941090831     C/EXEC SQL
006942090831     C+ SELECT count(*) INTO :conta FROM FIATT00f WHERE ATTPDR = :apdpdr
006943090831     C/END-EXEC
006944090831     c                   if        conta > 0
006945090831     c                   iter
006946090831     c                   end
006947090831     c                   end
006948090831     C* SE SONO ARRIVATO QUI SIGNIFICA CHE POSSO ANNULLARE L'ANAGRAFICO
006949090831     C                   EVAL      APDATB = 'A'
006950090831     C                   UPDATE    FIAPD000
006951090831     C*
006952090831     C                   enddo
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
