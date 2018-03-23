000100030117      *****************************************************************
000200110929      * Aut: costi e competenze x giro + peso/volume
000300030117      *****************************************************************
000400110929     Ffiftt04L  IF   E           K DISK                                         x Data Prestazione
000500110929     Ffiftd06L  IF   E           K DISK                                         x Data Prestazione
000600100114     Feccet30c  IF   E           K DISK                                         x Data Prestazione
000700100114     Fecced30c  IF   E           K DISK                                         x Data Prestazione
000800110929     Fwfficnd00fO    E             disk
000900110929     Fwfficnd0l uf a E           k disk
001000030131      *
001100030129      *****************************************************************
001200030117     d KPJBA         E DS
001300030825      *
001400110929     D ficnd00ds     e DS
001500110930     d  PO                    17     19s 0
001600110930     d  aut                   20     23s 0
001700091112     D ddatiute      e ds
001800091112     D azuteds       e ds                  extname(AZUTE00F)
001900091112     D tibs34ds      E DS                  inz
002000110929     D dswfficnd0    E DS                  extname(wfficnd0f)
002100110930     d STP             s                   like(FTDSTP)
002200110930     d kfa             s                   like(fnd0kfa)
002300110929     d tvl             s                   like(fnd0tvl)
002400110929     d cla             s                   like(fnd0cla)
002500110929     d nsp             s                   like(fnd0nsp)
002600110929     d costi           s                   like(fnd0costi)
002700110929     d comp            s                   like(fnd0comp)
002800110929     d kfao            s                   like(fnd0kfao)
002900110929     d tvlo            s                   like(fnd0tvlo)
003000110929     d clao            s                   like(fnd0clao)
003100110929     d nspo            s                   like(fnd0nspo)
003200110929     d costio          s                   like(fnd0costio)
003300110929     d compo           s                   like(fnd0compo)
003400030826      *
003500030117      *****************************************************************
003600030117     C     *ENTRY        PLIST
003700030117     C                   PARM                    KPJBA
003800110929     C                   movel     KPJBU         ficnd00ds
003900091113     c* pulisce il wf
004000091113     c                   if        parclr = 'S'
004100091113     C/EXEC SQL
004200110929     C+ DELETE FROM WFficnd0F
004300091113     C/END-EXEC
004400091113     c                   end
004500110929     c* file di lavoro mi serve per saldificare le competenze
004600110929     C/EXEC SQL
004700110929     C+ DELETE FROM WFficnd00f
004800110929     C/END-EXEC
004900091113     c*
005000110930     c* file di lavoro mi serve per saldificare le competenze
005100110930     C/EXEC SQL
005200110930     C+ DELETE FROM WFficndCEf
005300110930     C/END-EXEC
005400110930     c*
005500091112     c     *dtaara       define    §azute        azuteds
005600091112     c     *dtaara       define    §datiute      ddatiute
005700091112     C                   in(E)     *dtaara
005800091112     C                   IF        %Error  or  RSUT = *blanks
005900091112     C                   call      'TIBS34R'
006000091112     C                   parm                    Tibs34Ds
006100091112     C                   in        *dtaara
006200091112     c                   ENDIF
006300091113     C     KFTT          KLIST
006400100114     C                   KFLD                    fttfvl
006500100114     C                   KFLD                    pardd1
006600091113     c                   movel     'C'           fttfvl
006700100114     C     KFTd          KLIST
006800100114     C                   KFLD                    fttpdr
006900100114     C                   KFLD                    fttddc
007000100114     C                   KFLD                    ftttsr
007100100114     C                   KFLD                    fttndc
007200100114     C     Kcet          KLIST
007300100114     C                   KFLD                    ftdaas
007400100114     C                   KFLD                    ftdlnp
007500100114     C                   KFLD                    ftdnrs
007600100114     C                   KFLD                    ftdnsp
007700110929     C     kfnd0         KLIST
007800110929     C                   KFLD                    FND0PDR
007900110929     C                   KFLD                    FND0NDC
008000110929     C                   KFLD                    FND0DDC
008100110929     C                   KFLD                    FND0TSR
008200110929     C                   KFLD                    FND0CGI
008300110929     C                   KFLD                    FND0ESI
008400030120      *
008500030117      *--------------------------------------------------------------*
008600030117      *             M A I N      L I N E
008700030117      *--------------------------------------------------------------*
008800030825      *
008900100114     C     KFtt          setll     FiFtt04l
009000091112     c                   do        *hival
009100100114     C     'C'           READe     FiFtt04l
009200100114     C                   if        %Eof(FiFtt04l)
009300091112     c                   leave
009400091112     c                   end
009500100114     c                   if        Fttddc > pardd2
009600091116     c                   leave
009700091116     c                   end
009800110930     c                   if        FttFGS <> PO
009900110929     c                   iter
010000110929     c                   end
010100110930     c                   if        Fttpdr <> parpdr AND AUT <> 0
010200110930     c                   iter
010300110930     c                   end
010400100114     c                   if        ftttsr = ' '
010500091112     c                   iter
010600091112     c                   end
010700110929     c* scrivo il file di work per le competenze
010800110929     c                   Exsr      srcompetenze
010900110929     c* estraggo il file x consegne
011000110929     c                   if        ftttsr = 'C'
011100110929     c                   Exsr      srscriviC
011200110929     c                   end
011300110929     c* estraggo il file x ritiri
011400110929     c                   if        ftttsr = 'R'
011500110929     c                   Exsr      srscriviR
011600110929     c                   end
011700030825      *
011800030826     c                   EndDO
011900030416     c*
012000030415     C                   SETON                                        LR
012100110929      *--------------------------------------------------------------*
012200110930      *  estraggo file di lavoro per le competenze E I COSTI
012300110929      *--------------------------------------------------------------*
012400110929     c     Srcompetenze  BEGSR
012500110929     c*
012600110929     c     kftd          setll     fiftd06l
012700110929     c                   do        *hival
012800110929     c     kftd          reade     fiftd06l
012900110929     c                   if        %eof(fiftd06l)
013000110929     c                   leave
013100110929     c                   end
013200110929     c*
013300110929     c     kcet          chain     eccet30c
013400110929     c                   if        %found(eccet30c)
013500110929     c                   if        ftttsr = 'C'
013600110929     c                   z-add     cetard        d00cmp
013700110929     c                   else
013800110929     c                   z-add     cetpar        d00cmp
013900110929     c                   end
014000110929     c                   end
014100110929      *
014200110929     c     kcet          setll     ecced30c
014300110929     c                   do        *hival
014400110929     c     kcet          reade     ecced30c
014500110929     c                   if        %eof(ecced30c)
014600110929     c                   leave
014700110929     c                   end
014800110929      *
014900110929     c                   select
015000110929     c                   when      cedcmp = 16 and ftttsr = 'C'
015100110929     c                   add       cedimp        d00cmp
015200110929     c                   when      cedcmp = 19 and ftttsr = 'C'
015300110929     c                   add       cedimp        d00cmp
015400110929     c                   when      cedcmp = 34 and ftttsr = 'C'
015500110929     c                   add       cedimp        d00cmp
015600110929     c                   when      cedcmp = 35 and ftttsr = 'R'
015700110929     c                   add       cedimp        d00cmp
015800110929     c                   when      cedcmp = 24 and ftttsr = 'C'
015900110929     c                   add       cedimp        d00cmp
016000110929     c                   when      cedcmp = 43 and ftttsr = 'C'
016100110929     c                   add       cedimp        d00cmp
016200110929     c                   when      cedcmp = 62 and ftttsr = 'C'
016300110929     c                   add       cedimp        d00cmp
016400110929     c                   endsl
016500110929     c                   enddo
016600110929     c*
016700110929     c                   eval      d00tsr     =  ftttsr
016800110929     c                   eval      d00aas     =  ftdaas
016900110929     c                   eval      d00lnp     =  ftdlnp
017000110929     c                   eval      d00nrs     =  ftdnrs
017100110929     c                   eval      d00nsp     =  ftdnsp
017200110929     c                   write     wfficnd00
017300110929     c                   enddo
017400110929     c                   feod      wfficnd00f
017500110930     C*
017600110930     C/EXEC SQL
017700110930     C+ INSERT INTO WFFICNDCEF (DCEAAS, DCELNP, DCENRS, DCENSP,
017800110930     C+ DCETSR, DCECOS) SELECT FCEAAS, FCELNP, FCENRS, FCENSP, FCETSR,
017900110930     C+ SUM(FCEICE) FROM FIFCE00F WHERE fCEndc = :fttndc and fCEddc =
018000110930     C+ :fttddc and fCEpdr = :fttpdr and fCEfgs = :fttfgs and fCEtsr =
018100110930     C+ :ftttsr GROUP BY FCEAAS, FCELNP, FCENRS, FCENSP, FCETSR
018200110930     C/END-EXEC
018300110929     c                   ENDSR
018400030825      *--------------------------------------------------------------*
018500100114      *  scrivo record
018600030825      *--------------------------------------------------------------*
018700110930     c     SrscriviC     BEGSR
018800110929     c                   clear                   dswfficnd0
018900110929     c*
019000110929     c* consegnate < 0 - 5
019100110929     c*
019200110929     C/EXEC SQL
019300110929     C+ DECLARE a1 CURSOR FOR
019400110929     C+ SELECT ftdddc, ftdndc, ftdpdr, ifnull(argcgi, ' '),
019500110929     C+ ftdtsr, sum(ftdpkl),
019600110930     C+ sum(ftdvlu), sum(ftdncl), count(*), sum(DCECOS),
019700110930     c+ sum(d00cmp)
019800110929     C+ FROM fiftd00f left outer join filtra201/
019900110929     C+ fiarg00f on ftdaas = argaas and ftdlnp = arglnp and ftdnrs = argnrs
020000110929     C+ and ftdnsp = argnsp join
020100110930     C+ WFFICNDCEf on ftdaas = Dceaas and ftdlnp =
020200110930     C+ Dcelnp and ftdnrs = Dcenrs and ftdnsp = Dcensp
020300110930     C+ join
020400110929     C+ wfficnd00f on ftdaas = d00aas and ftdlnp =
020500110929     C+ d00lnp and ftdnrs = d00nrs and ftdnsp = d00nsp and
020600110929     C+ ftdtsr = d00tsr
020700110929     C+ WHERE ftdtmc in (' ', 'P')
020800110930     C+ and ftdpkl < 5 and ftdvlu < 0,05
020900110929     C+ and ftdndc = :fttndc and ftdddc = :fttddc
021000110929     C+ and ftdpdr = :fttpdr and ftdfgs = :fttfgs and ftdtsr = :ftttsr
021100110929     C+ GROUP BY ftdddc, ftdndc, ftdpdr, argcgi, ftdtsr
021200110929     C/END-EXEC
021300110929     C/EXEC SQL
021400110929     C+ OPEN a1
021500110929     C/END-EXEC
021600110929     C                   do        *hival
021700110929
021800110929     C/EXEC SQL
021900110929     C+ FETCH NEXT FROM a1 INTO
022000110929     C+ :fnd0ddc, :fnd0ndc, :fnd0pdr, :fnd0cgi, :fnd0tsr, :kfa,
022100110930     C+ :tvl, :cla, :nsp, :costi,
022200110929     c+ :comp
022300110929     C/END-EXEC
022400110929
022500110929     C                   select
022600110929     C                   when      sqlcod = 100
022700110929     c                   leave
022800110929     C                   WHEN      SqlCod < 0
022900110929     c                   if        sqlcod <> -913
023000110929     C                   seton                                        H1
023100110929     c                   end
023200110929     c                   seton                                        lr
023300110929     c                   return
023400110929     C                   other
023500110929     c                   clear                   fnd0esi
023600110929     c     kfnd0         chain     wfficnd0l
023700110929     c                   if        %found(wfficnd0l)
023800110929     c                   leavesr
023900110929     c                   else
024000110929     c* per ritiro non ho il giro
024100110929     c                   eval      fnd0kfa   = kfa
024200110929     c                   eval      fnd0tvl   = tvl
024300110929     c                   eval      fnd0cla   = cla
024400110929     c                   eval      fnd0nsp   = nsp
024500110929     c                   eval      fnd0costi = costi
024600110929     c                   eval      fnd0comp  = comp
024700110929     c                   write     wfficnd0
024800110929     c                   end
024900110929     C                   endsl
025000110929     C                   enddo
025100110929     C/EXEC SQL
025200110929     C+ CLOSE a1
025300110929     C/END-EXEC
025400110929     c                   clear                   dswfficnd0
025500110929     c*
025600110929     c* NON consegnate < 0-5
025700110929     c*
025800110929     C/EXEC SQL
025900110929     C+ DECLARE b1 CURSOR FOR
026000110929     C+ SELECT ftdddc, ftdndc, ftdpdr, ifnull(argcgi, ' '),
026100110929     C+ ftdtsr, sum(ftdpkl),
026200110930     C+ sum(ftdvlu), sum(ftdncl), count(*), sum(ifnull(DCECOS, 0)),
026300110930     c+ sum(d00cmp)
026400110929     C+ FROM fiftd00f left outer join filtra201/
026500110929     C+ fiarg00f on ftdaas = argaas and ftdlnp = arglnp and ftdnrs = argnrs
026600110929     C+ and ftdnsp = argnsp left outer join
026700110930     C+ WFFICNDCEf on ftdaas = Dceaas and ftdlnp =
026800110930     C+ Dcelnp and ftdnrs = Dcenrs and ftdnsp = Dcensp
026900110930     C+ JOIN wfficnd00f on ftdaas = d00aas and ftdlnp =
027000110929     C+ d00lnp and ftdnrs = d00nrs and ftdnsp = d00nsp and
027100110929     C+ ftdtsr = d00tsr
027200110929     C+ WHERE ftdtmc not in (' ', 'P')
027300110930     C+ and ftdpkl < 5 and ftdvlu < 0,05
027400110929     C+ and ftdndc = :fttndc and ftdddc = :fttddc
027500110929     C+ and ftdpdr = :fttpdr and ftdfgs = :fttfgs and ftdtsr = :ftttsr
027600110929     C+ GROUP BY ftdddc, ftdndc, ftdpdr, argcgi, ftdtsr
027700110929     C/END-EXEC
027800110929     C/EXEC SQL
027900110929     C+ OPEN b1
028000110929     C/END-EXEC
028100110929     C                   do        *hival
028200110929
028300110929     C/EXEC SQL
028400110929     C+ FETCH NEXT FROM b1 INTO
028500110929     C+ :fnd0ddc, :fnd0ndc, :fnd0pdr, :fnd0cgi, :fnd0tsr, :kfa,
028600110930     C+ :tvl, :cla, :nsp, :costi,
028700110929     c+ :comp
028800110929     C/END-EXEC
028900110929
029000110929     C                   select
029100110929     C                   when      sqlcod = 100
029200110929     c                   leave
029300110929     C                   WHEN      SqlCod < 0
029400110929     c                   if        sqlcod <> -913
029500110929     C                   seton                                        H1
029600110929     c                   end
029700110929     c                   seton                                        lr
029800110929     c                   return
029900110929     C                   other
030000110929     c                   eval      fnd0esi = 'N'
030100110929     c     kfnd0         chain     wfficnd0l
030200110929     c                   if        %found(wfficnd0l)
030300110929     c                   leavesr
030400110929     c                   else
030500110929     c                   eval      fnd0kfa   = kfa
030600110929     c                   eval      fnd0tvl   = tvl
030700110929     c                   eval      fnd0cla   = cla
030800110929     c                   eval      fnd0nsp   = nsp
030900110929     c                   eval      fnd0costi = costi
031000110929     c                   eval      fnd0comp  = comp
031100110929     c                   write     wfficnd0
031200110929     c                   end
031300110929     C                   endsl
031400110929     C                   enddo
031500110929     C/EXEC SQL
031600110929     C+ CLOSE b1
031700110929     C/END-EXEC
031800110929     c                   clear                   dswfficnd0
031900110929     c*
032000110929     c* consegnate >= 5
032100110929     c*
032200110929     C/EXEC SQL
032300110929     C+ DECLARE c1 CURSOR FOR
032400110929     C+ SELECT ftdddc, ftdndc, ftdpdr, ifnull(argcgi, ' '),
032500110929     C+ ftdtsr, sum(ftdpkl),
032600110930     C+ sum(ftdvlu), sum(ftdncl), count(*), sum(DCECOS),
032700110930     c+ sum(d00cmp)
032800110929     C+ FROM fiftd00f left outer join filtra201/
032900110929     c+ fiarg00f on ftdaas = argaas and ftdlnp = arglnp and ftdnrs = argnrs
033000110929     C+ and ftdnsp = argnsp join
033100110930     C+ WFFICNDCEf on ftdaas = Dceaas and ftdlnp =
033200110930     C+ Dcelnp and ftdnrs = Dcenrs and ftdnsp = Dcensp
033300110930     C+ JOIN wfficnd00f on ftdaas = d00aas and ftdlnp =
033400110929     C+ d00lnp and ftdnrs = d00nrs and ftdnsp = d00nsp and
033500110929     C+ ftdtsr = d00tsr
033600110929     C+ WHERE ftdtmc in (' ', 'P')
033700110930     C+ and (ftdpkl >=5 or ftdvlu >=0,05)
033800110929     C+ and ftdndc = :fttndc and ftdddc = :fttddc
033900110929     C+ and ftdpdr = :fttpdr and ftdfgs = :fttfgs and ftdtsr = :ftttsr
034000110929     C+ GROUP BY ftdddc, ftdndc, ftdpdr, argcgi, ftdtsr
034100110929     C/END-EXEC
034200110929     C/EXEC SQL
034300110929     C+ OPEN c1
034400110929     C/END-EXEC
034500110929     C                   do        *hival
034600110929
034700110929     C/EXEC SQL
034800110929     C+ FETCH NEXT FROM c1 INTO
034900110929     C+ :fnd0ddc, :fnd0ndc, :fnd0pdr, :fnd0cgi, :fnd0tsr, :kfao,
035000110930     C+ :tvlo, :clao, :nspo, :costio,
035100110929     c+ :compo
035200110929     C/END-EXEC
035300110929
035400110929     C                   select
035500110929     C                   when      sqlcod = 100
035600110929     c                   leave
035700110929     C                   WHEN      SqlCod < 0
035800110929     c                   if        sqlcod <> -913
035900110929     C                   seton                                        H1
036000110929     c                   end
036100110929     c                   seton                                        lr
036200110929     c                   return
036300110929     C                   other
036400110929     c                   clear                   fnd0esi
036500110929     c     kfnd0         chain     wfficnd0l
036600110930     c                   eval      fnd0kfao  = kfao
036700110930     c                   eval      fnd0tvlo  = tvlo
036800110930     c                   eval      fnd0clao  = clao
036900110930     c                   eval      fnd0nspo  = nspo
037000110930     c                   eval      fnd0costio= costio
037100110930     c                   eval      fnd0compO = compo
037200110929     c                   if        %found(wfficnd0l)
037300110929     c                   update    wfficnd0
037400110929     c                   else
037500110929     c                   clear                   fnd0kfa
037600110929     c                   clear                   fnd0tvl
037700110929     c                   clear                   fnd0cla
037800110929     c                   clear                   fnd0nsp
037900110929     c                   clear                   fnd0costi
038000110929     c                   clear                   fnd0comp
038100110929     c                   write     wfficnd0
038200110929     c                   end
038300110929     C                   endsl
038400110929     C                   enddo
038500110929     C/EXEC SQL
038600110929     C+ CLOSE c1
038700110929     C/END-EXEC
038800110929     c                   clear                   dswfficnd0
038900110929     c*
039000110929     c* NON consegnate < 0-5
039100110929     c*
039200110929     C/EXEC SQL
039300110929     C+ DECLARE d1 CURSOR FOR
039400110929     C+ SELECT ftdddc, ftdndc, ftdpdr, ifnull(argcgi, ' '),
039500110929     C+ ftdtsr, sum(ftdpkl),
039600110930     C+ sum(ftdvlu), sum(ftdncl), count(*), sum(ifnull(DCECOS, 0)),
039700110930     c+ sum(d00cmp)
039800110929     C+ FROM fiftd00f left outer join filtra201/
039900110929     C+ fiarg00f on ftdaas = argaas and ftdlnp = arglnp and ftdnrs = argnrs
040000110929     C+ and ftdnsp = argnsp left outer join
040100110930     C+ WFFICNDCEf on ftdaas = Dceaas and ftdlnp =
040200110930     C+ Dcelnp and ftdnrs = Dcenrs and ftdnsp = Dcensp
040300110930     C+ JOIN wfficnd00f on ftdaas = d00aas and ftdlnp =
040400110929     C+ d00lnp and ftdnrs = d00nrs and ftdnsp = d00nsp and
040500110929     C+ ftdtsr = d00tsr
040600110929     C+ WHERE ftdtmc not in (' ', 'P')
040700110930     C+ and (ftdpkl >=5 or ftdvlu >=0,05)
040800110929     C+ and ftdndc = :fttndc and ftdddc = :fttddc
040900110929     C+ and ftdpdr = :fttpdr and ftdfgs = :fttfgs and ftdtsr = :ftttsr
041000110929     C+ GROUP BY ftdddc, ftdndc, ftdpdr, argcgi, ftdtsr
041100110929     C/END-EXEC
041200110929     C/EXEC SQL
041300110929     C+ OPEN d1
041400110929     C/END-EXEC
041500110929     C                   do        *hival
041600110929
041700110929     C/EXEC SQL
041800110929     C+ FETCH NEXT FROM d1 INTO
041900110929     C+ :fnd0ddc, :fnd0ndc, :fnd0pdr, :fnd0cgi, :fnd0tsr, :kfao,
042000110930     C+ :tvlo, :clao, :nspo, :costio,
042100110929     c+ :compo
042200110929     C/END-EXEC
042300110929
042400110929     C                   select
042500110929     C                   when      sqlcod = 100
042600110929     c                   leave
042700110929     C                   WHEN      SqlCod < 0
042800110929     c                   if        sqlcod <> -913
042900110929     C                   seton                                        H1
043000110929     c                   end
043100110929     c                   seton                                        lr
043200110929     c                   return
043300110929     C                   other
043400110929     c                   eval      fnd0esi = 'N'
043500110929     c     kfnd0         chain     wfficnd0l
043600110930     c                   eval      fnd0kfao  = kfao
043700110930     c                   eval      fnd0tvlo  = tvlo
043800110930     c                   eval      fnd0clao  = clao
043900110930     c                   eval      fnd0nspo  = nspo
044000110930     c                   eval      fnd0costio= costio
044100110930     c                   eval      fnd0compO = compo
044200110929     c                   if        %found(wfficnd0l)
044300110929     c                   update    wfficnd0
044400110929     c                   else
044500110929     c                   clear                   fnd0kfa
044600110929     c                   clear                   fnd0tvl
044700110929     c                   clear                   fnd0cla
044800110929     c                   clear                   fnd0nsp
044900110929     c                   clear                   fnd0costi
045000110929     c                   clear                   fnd0comp
045100110929     c                   write     wfficnd0
045200110929     c                   end
045300110929     C                   endsl
045400110929     C                   enddo
045500110929     C/EXEC SQL
045600110929     C+ CLOSE d1
045700110929     C/END-EXEC
045800091113     c                   ENDSR
045900110930      *--------------------------------------------------------------*
046000110930      *  scrivo record
046100110930      *--------------------------------------------------------------*
046200110930     c     SrscriviR     BEGSR
046300110930     c                   clear                   dswfficnd0
046400110930     c*
046500110930     c* consegnate < 0 - 5
046600110930     c*
046700110930     C/EXEC SQL
046800110930     C+ DECLARE a2 CURSOR FOR
046900110930     C+ SELECT ftdddc, ftdndc, ftdpdr, FTDSTP,
047000110930     C+ ftdtsr, sum(ftdpkl),
047100110930     C+ sum(ftdvlu), sum(ftdncl), count(*), sum(DCECOS),
047200110930     c+ sum(d00cmp)
047300110930     C+ FROM fiftd00f JOIN
047400110930     C+ WFFICNDCEf on ftdaas = Dceaas and ftdlnp =
047500110930     C+ Dcelnp and ftdnrs = Dcenrs and ftdnsp = Dcensp
047600110930     C+ JOIN wfficnd00f on ftdaas = d00aas and ftdlnp =
047700110930     C+ d00lnp and ftdnrs = d00nrs and ftdnsp = d00nsp and
047800110930     C+ ftdtsr = d00tsr
047900110930     C+ WHERE ftdtmc in (' ', 'P')
048000110930     C+ and ftdndc = :fttndc and ftdddc = :fttddc
048100110930     C+ and ftdpdr = :fttpdr and ftdfgs = :fttfgs and ftdtsr = :ftttsr
048200110930     C+ GROUP BY ftdddc, ftdndc, ftdpdr, FTDSTP, ftdtsr
048300110930     C+ HAVING SUM(ftdpkl) < 5 and SUM(ftdvlu) < 0,05
048400110930     C/END-EXEC
048500110930     C/EXEC SQL
048600110930     C+ OPEN a2
048700110930     C/END-EXEC
048800110930     C                   do        *hival
048900110930
049000110930     C/EXEC SQL
049100110930     C+ FETCH NEXT FROM a2 INTO
049200110930     C+ :fnd0ddc, :fnd0ndc, :fnd0pdr, :STP, :fnd0tsr, :kfa,
049300110930     C+ :tvl, :cla, :nsp, :costi,
049400110930     c+ :comp
049500110930     C/END-EXEC
049600110930
049700110930     C                   select
049800110930     C                   when      sqlcod = 100
049900110930     c                   leave
050000110930     C                   WHEN      SqlCod < 0
050100110930     c                   if        sqlcod <> -913
050200110930     C                   seton                                        H1
050300110930     c                   end
050400110930     c                   seton                                        lr
050500110930     c                   return
050600110930     C                   other
050700110930     c                   clear                   fnd0esi
050800110930     c     kfnd0         chain     wfficnd0l
050900110930     c                   if        %found(wfficnd0l)
051000110930     c                   eval      fnd0kfa   = kfa    + fnd0kfa
051100110930     c                   eval      fnd0tvl   = tvl    + fnd0tvl
051200110930     c                   eval      fnd0cla   = cla    + fnd0cla
051300110930     c                   eval      fnd0nsp   = 1      + fnd0nsp
051400110930     c                   eval      fnd0costi = costi  + fnd0costi
051500110930     c                   eval      fnd0comp  = comp   + fnd0comp
051600110930     c                   UPDATE    wfficnd0
051700110930     c                   else
051800110930     c* per ritiro non ho il giro
051900110930     c                   eval      fnd0kfa   = kfa
052000110930     c                   eval      fnd0tvl   = tvl
052100110930     c                   eval      fnd0cla   = cla
052200110930     c                   eval      fnd0nsp   = 1
052300110930     c                   eval      fnd0costi = costi
052400110930     c                   eval      fnd0comp  = comp
052500110930     c                   write     wfficnd0
052600110930     c                   end
052700110930     C                   endsl
052800110930     C                   enddo
052900110930     C/EXEC SQL
053000110930     C+ CLOSE a2
053100110930     C/END-EXEC
053200110930     c                   clear                   dswfficnd0
053300110930     c*
053400110930     c* NON consegnate < 0-5
053500110930     c*
053600110930     C/EXEC SQL
053700110930     C+ DECLARE b2 CURSOR FOR
053800110930     C+ SELECT ftdddc, ftdndc, ftdpdr, FTDSTP,
053900110930     C+ ftdtsr, sum(ftdpkl),
054000110930     C+ sum(ftdvlu), sum(ftdncl), count(*), sum(ifnull(DCECOS, 0)),
054100110930     c+ sum(d00cmp)
054200110930     C+ FROM fiftd00f  JOIN
054300110930     C+ WFFICNDCEf on ftdaas = Dceaas and ftdlnp =
054400110930     C+ Dcelnp and ftdnrs = Dcenrs and ftdnsp = Dcensp
054500110930     C+ JOIN wfficnd00f on ftdaas = d00aas and ftdlnp =
054600110930     C+ d00lnp and ftdnrs = d00nrs and ftdnsp = d00nsp and
054700110930     C+ ftdtsr = d00tsr
054800110930     C+ WHERE ftdtmc not in (' ', 'P')
054900110930     C+ and ftdndc = :fttndc and ftdddc = :fttddc
055000110930     C+ and ftdpdr = :fttpdr and ftdfgs = :fttfgs and ftdtsr = :ftttsr
055100110930     C+ GROUP BY ftdddc, ftdndc, ftdpdr, FTDSTP, ftdtsr
055200110930     C+ HAVING SUM(ftdpkl) < 5 and SUM(ftdvlu) < 0,05
055300110930     C/END-EXEC
055400110930     C/EXEC SQL
055500110930     C+ OPEN b2
055600110930     C/END-EXEC
055700110930     C                   do        *hival
055800110930
055900110930     C/EXEC SQL
056000110930     C+ FETCH NEXT FROM b2 INTO
056100110930     C+ :fnd0ddc, :fnd0ndc, :fnd0pdr, :STP, :fnd0tsr, :kfa,
056200110930     C+ :tvl, :cla, :nsp, :costi,
056300110930     c+ :comp
056400110930     C/END-EXEC
056500110930
056600110930     C                   select
056700110930     C                   when      sqlcod = 100
056800110930     c                   leave
056900110930     C                   WHEN      SqlCod < 0
057000110930     c                   if        sqlcod <> -913
057100110930     C                   seton                                        H1
057200110930     c                   end
057300110930     c                   seton                                        lr
057400110930     c                   return
057500110930     C                   other
057600110930     c                   eval      fnd0esi = 'N'
057700110930     c     kfnd0         chain     wfficnd0l
057800110930     c                   if        %found(wfficnd0l)
057900110930     c                   eval      fnd0kfa   = kfa      + fnd0kfa
058000110930     c                   eval      fnd0tvl   = tvl      + fnd0tvl
058100110930     c                   eval      fnd0cla   = cla      + fnd0cla
058200110930     c                   eval      fnd0nsp   = 1        + fnd0nsp
058300110930     c                   eval      fnd0costi = costi    + fnd0costi
058400110930     c                   eval      fnd0comp  = comp     + fnd0comp
058500110930     c                   UPDATE    wfficnd0
058600110930     c                   else
058700110930     c                   eval      fnd0kfa   = kfa
058800110930     c                   eval      fnd0tvl   = tvl
058900110930     c                   eval      fnd0cla   = cla
059000110930     c                   eval      fnd0nsp   = 1
059100110930     c                   eval      fnd0costi = costi
059200110930     c                   eval      fnd0comp  = comp
059300110930     c                   write     wfficnd0
059400110930     c                   end
059500110930     C                   endsl
059600110930     C                   enddo
059700110930     C/EXEC SQL
059800110930     C+ CLOSE b2
059900110930     C/END-EXEC
060000110930     c                   clear                   dswfficnd0
060100110930     c*
060200110930     c* consegnate >= 5
060300110930     c*
060400110930     C/EXEC SQL
060500110930     C+ DECLARE c2 CURSOR FOR
060600110930     C+ SELECT ftdddc, ftdndc, ftdpdr, FTDSTP,
060700110930     C+ ftdtsr, sum(ftdpkl),
060800110930     C+ sum(ftdvlu), sum(ftdncl), count(*), sum(DCECOS),
060900110930     c+ sum(d00cmp)
061000110930     C+ FROM fiftd00f join
061100110930     C+ WFFICNDCEf on ftdaas = Dceaas and ftdlnp =
061200110930     C+ Dcelnp and ftdnrs = Dcenrs and ftdnsp = Dcensp
061300110930     C+ JOIN wfficnd00f on ftdaas = d00aas and ftdlnp =
061400110930     C+ d00lnp and ftdnrs = d00nrs and ftdnsp = d00nsp and
061500110930     C+ ftdtsr = d00tsr
061600110930     C+ WHERE ftdtmc in (' ', 'P')
061700110930     C+ and ftdndc = :fttndc and ftdddc = :fttddc
061800110930     C+ and ftdpdr = :fttpdr and ftdfgs = :fttfgs and ftdtsr = :ftttsr
061900110930     C+ GROUP BY ftdddc, ftdndc, ftdpdr, FTDSTP, ftdtsr
062000110930     C+ HAVING SUM(ftdpkl) >=5 or SUM(ftdvlu) >=0,05
062100110930     C/END-EXEC
062200110930     C/EXEC SQL
062300110930     C+ OPEN c2
062400110930     C/END-EXEC
062500110930     C                   do        *hival
062600110930
062700110930     C/EXEC SQL
062800110930     C+ FETCH NEXT FROM c2 INTO
062900110930     C+ :fnd0ddc, :fnd0ndc, :fnd0pdr, :STP, :fnd0tsr, :kfao,
063000110930     C+ :tvlo, :clao, :nspo, :costio,
063100110930     c+ :compo
063200110930     C/END-EXEC
063300110930
063400110930     C                   select
063500110930     C                   when      sqlcod = 100
063600110930     c                   leave
063700110930     C                   WHEN      SqlCod < 0
063800110930     c                   if        sqlcod <> -913
063900110930     C                   seton                                        H1
064000110930     c                   end
064100110930     c                   seton                                        lr
064200110930     c                   return
064300110930     C                   other
064400110930     c                   clear                   fnd0esi
064500110930     c     kfnd0         chain     wfficnd0l
064600110930     c                   eval      fnd0kfao  = kfao    +  fnd0kfao
064700110930     c                   eval      fnd0tvlo  = tvlo    +  fnd0tvlo
064800110930     c                   eval      fnd0clao  = clao    +  fnd0clao
064900110930     c                   eval      fnd0nspo  = 1       +  fnd0nspo
065000110930     c                   eval      fnd0costio= costio  +  fnd0costio
065100110930     c                   eval      fnd0compO = compo   +  fnd0compO
065200110930     c                   if        %found(wfficnd0l)
065300110930     c                   update    wfficnd0
065400110930     c                   else
065500110930     c                   clear                   fnd0kfa
065600110930     c                   clear                   fnd0tvl
065700110930     c                   clear                   fnd0cla
065800110930     c                   clear                   fnd0nsp
065900110930     c                   clear                   fnd0costi
066000110930     c                   clear                   fnd0comp
066100110930     c                   write     wfficnd0
066200110930     c                   end
066300110930     C                   endsl
066400110930     C                   enddo
066500110930     C/EXEC SQL
066600110930     C+ CLOSE c2
066700110930     C/END-EXEC
066800110930     c                   clear                   dswfficnd0
066900110930     c*
067000110930     c* NON consegnate < 0-5
067100110930     c*
067200110930     C/EXEC SQL
067300110930     C+ DECLARE d2 CURSOR FOR
067400110930     C+ SELECT ftdddc, ftdndc, ftdpdr, FTDSTP,
067500110930     C+ ftdtsr, sum(ftdpkl),
067600110930     C+ sum(ftdvlu), sum(ftdncl), count(*), sum(ifnull(DCECOS, 0)),
067700110930     c+ sum(d00cmp)
067800110930     C+ FROM fiftd00f join
067900110930     C+ WFFICNDCEf on ftdaas = Dceaas and ftdlnp =
068000110930     C+ Dcelnp and ftdnrs = Dcenrs and ftdnsp = Dcensp
068100110930     C+ JOIN wfficnd00f on ftdaas = d00aas and ftdlnp =
068200110930     C+ d00lnp and ftdnrs = d00nrs and ftdnsp = d00nsp and
068300110930     C+ ftdtsr = d00tsr
068400110930     C+ WHERE ftdtmc not in (' ', 'P')
068500110930     C+ and ftdndc = :fttndc and ftdddc = :fttddc
068600110930     C+ and ftdpdr = :fttpdr and ftdfgs = :fttfgs and ftdtsr = :ftttsr
068700110930     C+ GROUP BY ftdddc, ftdndc, ftdpdr, FTDSTP, ftdtsr
068800110930     C+ HAVING SUM(ftdpkl) >=5 or SUM(ftdvlu) >=0,05
068900110930     C/END-EXEC
069000110930     C/EXEC SQL
069100110930     C+ OPEN d2
069200110930     C/END-EXEC
069300110930     C                   do        *hival
069400110930
069500110930     C/EXEC SQL
069600110930     C+ FETCH NEXT FROM d2 INTO
069700110930     C+ :fnd0ddc, :fnd0ndc, :fnd0pdr, :fnd0cgi, :fnd0tsr, :kfao,
069800110930     C+ :tvlo, :clao, :nspo,  :costio,
069900110930     c+ :compo
070000110930     C/END-EXEC
070100110930
070200110930     C                   select
070300110930     C                   when      sqlcod = 100
070400110930     c                   leave
070500110930     C                   WHEN      SqlCod < 0
070600110930     c                   if        sqlcod <> -913
070700110930     C                   seton                                        H1
070800110930     c                   end
070900110930     c                   seton                                        lr
071000110930     c                   return
071100110930     C                   other
071200110930     c                   eval      fnd0esi = 'N'
071300110930     c     kfnd0         chain     wfficnd0l
071400110930     c                   eval      fnd0kfao  = kfao    +  fnd0kfao
071500110930     c                   eval      fnd0tvlo  = tvlo    +  fnd0tvlo
071600110930     c                   eval      fnd0clao  = clao    +  fnd0clao
071700110930     c                   eval      fnd0nspo  = 1       +  fnd0nspo
071800110930     c                   eval      fnd0costio= costio  +  fnd0costio
071900110930     c                   eval      fnd0compO = compo   +  fnd0compO
072000110930     c                   if        %found(wfficnd0l)
072100110930     c                   update    wfficnd0
072200110930     c                   else
072300110930     c                   clear                   fnd0kfa
072400110930     c                   clear                   fnd0tvl
072500110930     c                   clear                   fnd0cla
072600110930     c                   clear                   fnd0nsp
072700110930     c                   clear                   fnd0costi
072800110930     c                   clear                   fnd0comp
072900110930     c                   write     wfficnd0
073000110930     c                   end
073100110930     C                   endsl
073200110930     C                   enddo
073300110930     C/EXEC SQL
073400110930     C+ CLOSE d2
073500110930     C/END-EXEC
073600110930     c                   ENDSR
