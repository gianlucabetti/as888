000100950524     H DECEDIT('0,') DATEDIT(*DMY.)
000102070205     h dftactgrp(*no) actgrp(*caller)
000103070322     H* FIDG19R *----------------------------------------------------*
000104070322     H*         assegnazione automatica distinte consegna x giro
000105070322     H*--------------------------------------------------------------*
000201070222     FFidg18p   O    E             PRINTER OFLIND(*IN80)
000600070518     FFIDST09L  uF   E           K DISK    commit infds(dstinfds)
000601071107     FAZORG01L  IF   E           K DISK
000602080326     Ffidsf02l  IF   E           K DISK
000800950524      *--------------------------------------------------------------*
000900950524      *   DS
001000950524      *--------------------------------------------------------------*
001001070518     d dstinfds        ds
001002070518     d  dstnrr               397    400i 0
001003070222     ddtaiso           s               d
001004070222     ddtaeur           s               d   datfmt(*Eur)
001005070518     d trul82ds      e ds
001100070205     D fidst3ds      e ds                  extname(fidst30f)
001101070302     D fidstds       e ds                  extname(fidst00f)
001102050520     D ddatiute      e ds
001103150508     D dDSTFLR       e ds
001200030910     D azuteds       e ds                  extname(AZUTE00F)
001300030910     D tibs34ds      E DS                  inz
001400950524     D KPJBA         E DS
001500070206     D Fidg19ds      E DS
001501070205     D Fidg21ds      E DS
001502080115     D Fidg23ds      E DS
001800070206     D savndd          S                   INZ like(d19nfdi)
001900070206     D savnda          S                   INZ like(d19nfai)
001901070205     D compkl          S                   INZ like(dstpkl)
001902070205     D comvlu          S                   INZ like(dstvlu)
001903070205     D comnftl         S                   INZ like(dstnftl)
001904080201     D slb             S                   INZ like(d21slbi)
001905080204     D loop            S              1  0
001906070209     d distinta        ds
001907070209     d  dfv                                like(dst3dfv)
001908070209     d  nfv                                like(dst3nfv)
001909080115     d savdist         s                   like(distinta)
001911070518     D Errmsg          S             78    DIM(1) CTDATA PERRCD(1)              MSG DI ERRORE
001912080115     D OG146         E DS                  INZ
001913080115     D OG147         E DS                  INZ
001914071107     D OG150         E DS                  INZ
002000950524     C*--------------------------------------------------------------*
002100950524     C*  Ciclo principale
002200950524     C*--------------------------------------------------------------*
002201080201     c* loop mi dice se è la prima volta che tratto la priorità 1 (=1) o è
002202080201     c* la seconda volta (=2)
002203080201     c* slb mi dice qual'è il limite minimo della seq.lettura bolla da
002204080204     c* considerare. le spedizio con slb da 0 a 250 sono le spedizioni
002205080201     c* meno urgenti
002206080201     c                   eval      loop  = 1
002207080204     c                   eval      slb = 250
002209080201     c*
002210070212     C                   do        99            xx                3 0
002211080201     C* Alla fine della priorità 1 mi rimetto in loop perchè prima
002212080204     c* elaboro le seq. lettura bolle con valore > di 250 e poi tutte
002213080201     c* le altre. Questo per mettere in distinta prima le spedizioni
002214080201     c* + urgenti e poi tutte le altre
002215080201     c*
003800070206      * leggo in Fidst30F ordinato per data, priorità, distinta e squenza
003801070206      * giro
003802080201     c                   if        loop = 2 and xx = 2
003804080201     c                   eval      xx = 1
003805080201     c                   end
003808160505     C/EXEC SQL
003809160505     C+ DECLARE d1 CURSOR FOR
003810160505     C+ SELECT fidst30f.*  FROM fidst30f join fidst00f on
003811160505     C+ dst3fgs = dstfgs and dst3npg = dstnpg and dst3dfv = dstdfv and
003812160505     C+ dst3nfv = dstnfv
003813160505     C+ WHERE
003814160505     C+ dst3fgs =:d19fgsi and dst3npg=4 and dst3dfv = :d19dfvi and dst3atb
003815160505     C+  = ' ' and dst3nfv between :d19nfdi and :d19nfai and dst3prgg = :xx
003816160505     C+ ORDER BY DST3FGS, DST3NPG, DST3DFV,
003817160506     C+ substr(dstflr, 2, 1) , DST3NFV, DST3SQL
003818160505     C/END-EXEC
004034061003
004037160505     C/EXEC SQL
004038160505     C+ OPEN d1
004039160505     C/END-EXEC
004046061003
004047061003     C                   do        *hival
004048061003
004051061003     C/EXEC SQL
004052160505     C+ FETCH NEXT FROM d1 INTO :fidst3ds
004053061003     C/END-EXEC
004060061003
004061061003     C                   select
004062061003     C                   when      sqlcod = 100
004063061003     c                   leave
004064070206     C                   WHEN      SqlCod < 0
004065070206     c* record allocato
004066070206     c                   if        sqlcod <> -913
004067070206     C                   seton                                        H1
004068070206     c                   end
004069070222     c                   seton                                        81
004070070222     c                   exsr      srstampa
004071070206     c                   exsr      srfine
004072080116     c                   other
004073080116     c*                  eval      dfv = dst3dfv
004074080116     c*                  eval      nfv = dst3nfv
004075080116     c*                  if        distinta <> savdist and d19cmti='S'
004088080116     c*                  commit
004089080116     c*                  eval      savdist = distinta
004090080116     c*                  end
004091061003     C                   exsr      gesdis
004092061003     C                   endsl
004093061003     C*
004094061003     C                   enddo
004095061003
004098061003     C/EXEC SQL
004099160505     C+ CLOSE d1
004100061003     C/END-EXEC
004107080201     C* Alla fine della priorità 1 mi rimetto in loop perchè prima
004108080204     c* elaboro le seq. lettura bolle con valore > di 250 e poi tutte
004109080201     c* le altre. Questo per mettere in distinta prima le spedizioni
004110080201     c* + urgenti e poi tutte le altre
004111080201     c                   if        xx = 1 and loop = 1
004112080201     c                   eval      loop = 2
004113080201     c                   else
004114080201     c                   eval      loop = 1
004115080201     c                   end
004116080201     c                   clear                   slb
006200950524     C*
006201070212     c                   if        xx = 99
006202070212     c                   leave
006203070212     c                   end
006300070202     C                   ENDdo                                                  ---- 01 ----
006400950525     C*
006401070206     c                   if        d19cmti= 'S'
006402070206     c                   commit
006403070206     c                   end
006404070323     c                   eval      d19pag = pages
006405070323     c                   eval      d19riga= riga
006406070206     c*
006407070206     c                   eval      kpjbu = fidg19ds
006500950524     C                   SETON                                        LR
006600030708     C*--------------------------------------------------------------*
006700030708     C*  Gestione singola distinta
006800030708     C*--------------------------------------------------------------*
006900030708     C     GESDIS        BEGSR
007000030721     c*
007101070410     c     kdst          chain(e)  FIDST09l
007103070206     c                   if        %error
007104070518     c* reperisco il RRN del file
007105070518     c     kdst          chain(n)  FIDST09l
007106070518     c                   if        %found(FIDST09l)
007107070518     c                   exsr      geslck
007108070518     c                   if        ul82§sts = 'A'
007109070518     c                   seton                                        82
007110070222     c                   exsr      srstampa
007111070206     c                   leavesr
007112070518     c                   end
007113070518     c                   end
007114070206     c                   end
007115070518     c*
007116070518     c     kdst          chain(e)  FIDST09l
007117070518     c                   if        not %found(FIDST09l) or %error
007118070222     c                   seton                                        82
007119070222     c                   exsr      srstampa
007120070206     c                   else
007121070402     c* controllo
007123070202     c                   exsr      srctrdis
007124070205     c* se non ci sono errori sulla distinta proseguo
007125070223     c                   if        not *in28
007126070402     c                   seton                                        87
007127070402     c                   exsr      srstampa
007128070205     c                   clear                   fidg21ds
007129071210     c                   eval      d21ldcri=d19ldcri
007130071210     c                   eval      d21dtai=d19dtai
007131070827     c                   eval      d21orai=d19orai
007132150508     c                   if        (dstpda='E' or dstpda ='C') and
007133150508     c                             (§DSTTSTPDA ='O' or §DSTTSTPDA =' ')
007134150430     c                   eval      d21p88i=d19p88i
007135150508     C                   ELSE
007136150508     c                   CLEAR                   d21p88i
007137150508     C                   END
007138071107     c                   eval      d21fppi=d19fppi
007139071107     c                   eval      d21tpmi=d19tpmi
007140160420     c                   eval      d21tur1=d19tur1
007141160420     c                   eval      d21tur2=d19tur2
007142160506     c                   clear                   d21turno
007143160506     c                   if        dsttpm = ' '
007144160420     c                   eval      d21turno = §dstturno
007145160506     c                   if        §dstturno = ' '
007146160506     c                   eval      d21turno = '1'
007147160506     c                   end
007148160506     c                   end
007149071107     c                   eval      d21cleari=d19cleari
007150070205     c                   eval      D21FGSI = dstFGS
007151070205     c                   eval      d21NPGI = dstNPG
007152070205     c                   eval      d21DFVI = dstDFV
007153070205     c                   eval      d21NFVI = DstNFV
007154070205     c                   eval      D21PDRI = DstPDR
007155070205     c                   eval      D21FGSGI= Dst3FGSG
007156070205     c                   eval      D21CGII = Dst3CGI
007157070205     c                   eval      D21PKLI = compkl
007158070205     c                   eval      D21VLUI = comvlu
007159070205     c                   eval      D21NFTLI= comnftl
007160070323     c                   eval      D21PAG= PAGES
007161070323     c                   eval      D21RIGA= RIGA
007162080204     c                   eval      d21loopi= loop
007163080204     c                   eval      d21slbi = slb
010000070205     C                   MOVEL(p)  FiDG21DS      KPJBU
010100070205     C                   CALL      'FIDG21R'
010200030909     C                   PARM                    KPJBA
010201070302     C                   PARM                    fidstds
010202070212     c                   eval      fidg21ds=kpjbu
010203070326     c                   eval      pages = d21pag
010204070326     c                   eval      riga = d21riga
010205070209     c* se ritorno senza errore aggiorno la distinta con il peso/volume/
010206070205     c* fermate attribuite
010207070205     c                   if        d21erro= ' '
010208070212     c                   eval      DSTPKLA = d21pklo + dstpkla
010209070212     c                   eval      DSTVLUA = d21vluo + dstvlua
010210070215     c                   eval      DSTSTPA = d21nftlo + dststpa
010211070222     c                   eval      DSTnblA = d21nblo + dstnbla
010212070205     c                   update    fidst000
010213080115     c* aggiorno ARB solo se p.o. partito
010214080115     c                   if        dateu8 >= comdda
010216080115     c                   clear                   fidg23ds
010217080115     C                   eval      d23FGSi = dstfgs
010218080115     C                   eval      d23Dfvi = dstdfv
010219080115     C                   eval      d23NfDi = dstNfv
010220080115     C                   eval      d23NfAi = dstnfv
010222080115     c                   eval      D23CGII = Dst3CGI
010223080115     C                   eval      d23FPpi = d19fppi
010224080115     C                   eval      d23tpmi = d19tpmi
010225160420     C                   eval      d23tur1 = d19tur1
010226160420     C                   eval      d23tur2 = d19tur2
010227080115     C                   eval      d23ldcri = d19ldcri
010228080115     C                   eval      d23dtai = d19dtai
010229080115     C                   eval      d23orai = d19orai
010230150508     c                   if        (dstpda='E' or dstpda ='C') and
010231150508     c                             (§DSTTSTPDA ='O' or §DSTTSTPDA =' ')
010232150508     c                   eval      d23p88i=d19p88i
010233150508     C                   ELSE
010234150508     c                   CLEAR                   d23p88i
010235150508     C                   END
010236080115     C                   eval      d23cleari= d19cleari
010237080115     C                   EVAL      D23PAG = PAGES
010238080115     C                   EVAL      D23RIGA= RIGA
010239080115     c                   eval      kpjbu = fidg23ds
010240080115     c                   call      'FIDG23C'
010241080115     c                   parm                    kpjba
010242080115     c                   eval      fidg23ds = kpjbu
010243080115     c                   eval      pages = d23pag
010244080115     c                   eval      riga = d23riga
010245080115     c                   if        d23erro<>' '
010246080115     c                   exsr      srfine
010247080115     c                   end
010248080116     c                   if        d19cmti= 'S'
010249080116     c                   commit
010250080116     c                   end
010251080115     c                   end
010252070206     c                   else
010253070206     c*                  exsr      srstampa
010254070202     c                   end
010255070202     c                   end
010500070205     C                   END                                                    Trovate spunte
010600950524     C*
010700070205     C                   ENDSR
013201061003      *-----------------------------------------------
013202070202     C     srctrdis      begsr
013203061003      *-----------------------------------------------
013204070202     c                   setoff                                       28
013205160420     c                   eval      ddstflr = dstflr
013206070202     C* distinta annullata
013207070202     c                   if        dstatb <> ' '
013208070223     c                   seton                                        28
013209070202     c                   leavesr
013210070202     c                   end
013211070410     C* distinta chiusa
013212070410     c                   if        dstfcf <> ' '
013213070410     c                   seton                                        28
013214070410     c                   leavesr
013215070410     c                   end
013216070202     C* prestazione mattina/pomeriggio
013217070302     c                   if        dstfpp <> d19fppi and d19fppi <> ' '
013218070223     c                   seton                                        28
013219070202     c                   leavesr
013220070202     c                   end
013221070827     C* distinta non + elaborabile perchè già stampata
013222070827     c                   if        dstfasd <> ' '
013223070827     c                   seton                                        28
013224070827     c                   leavesr
013225070827     c                   end
013226080326     c* verifico che non esista già una fase
013227080326     c                   eval      dsftrd = 'STP'
013228080326     c     kdsf          setll     fidsf02l                               28
013229080326     c                   if        not *in28 and (dstpda='E' or dstpda='C')
013230080326     c                   eval      dsftrd = 'PDC'
013231080326     c     kdsf          setll     fidsf02l                               28
013232080326     c                   end
013233080326     c                   if        *in28
013234080326     c                   leavesr
013235080326     c                   end
013236070202     C* parcel/messaggerie
013237070302     c                   if        (d19tpmi= 'P' and dsttpm <>' ') or
013238070302     c                             (d19tpmi= 'M' and dsttpm <>'M')
013239070223     c                   seton                                        28
013240070202     c                   leavesr
013241070202     c                   end
013242160422     c* turno solo per parcel
013243160420     c                   if        (d19tur1 = ' ' or d19tur2 = ' ')
013244160420     c                             and d19tur1 <> 'U' and d19tur2 <> 'U'
013245160422     c                             and dsttpm = ' '
013246160420     c                   select
013247160420     c                   when      (d19tur1 = '1' or
013248160420     c                              d19tur2 = '1') and
013249160420     c                             §dstturno <> ' '
013250160420     c                   seton                                        28
013252160420     c                   when      (d19tur1 = '2' or
013253160420     c                              d19tur2 = '2') and
013254160420     c                             §dstturno <> '2'
013255160420     c                   seton                                        28
013256160420     c                   endsl
013257160420     c   28              leavesr
013258160420     c                   end
013259070202     C* capienza
013260070206     c                   if        d19reti<> 0
013261070209     c                   eval      compkl = (dstpkl+((dstpkl * d19reti)/100))
013262070209     c                   eval      comvlu = (dstvlu+(dstvlu * d19reti)/100)
013263070209     c                   eval      comnftl= (dstnftl+(dstnftl * d19reti)/100)
013264070202     c                   else
013265070202     c                   eval      compkl = dstpkl
013266070202     c                   eval      comvlu = dstvlu
013267070205     c                   eval      comnftl= dstnftl
013268070202     c                   end
013269070202     c*
013270070202     c                   if        dstpkla >= compkl or
013271070202     c                             dstvlua >= comvlu or
013272070222     c                             dststpa >= comnftl
013273070202     c                   seton                                        28
013274070202     c                   leavesr
013275070212     c                   else
013276070212     c                   eval      compkl = compkl - dstpkla
013277070212     c                   eval      comvlu = comvlu - dstvlua
013278070215     c                   eval      comnftl= comnftl- dststpa
013279070202     c                   end
013280070202     C*
013281070202     C                   ENDSR
013282070206     C**************************************************************************
013283070206     C* esco con codice d'errore
013284070206     C**************************************************************************
013285070206     C     srfine        BEGSR
013286070206     c*
013287070206     c                   if        d19cmti= 'S'
013288070206     c                   rolbk
013289070206     c                   end
013290070206     c*
013291070206     c                   eval      d19erro= '1'
013292070206     c                   eval      kpjbu = fidg19ds
013293070206     c                   seton                                        lr
013294070206     c                   return
013295070206     c*
013296070206     c                   endsr
013297070222     C*------------------------------------------------------------------------*
013298070222     C* stampa
013299070222     C*------------------------------------------------------------------------*
013300070222     C     srstampa      BEGSR
013301071107     c* stampo solo per assegnaz. automatica
013302071107     c                   if        kcdaz= 'DG17' or kcdaz='DG18'
013303070222     c                   move      d19dfvi       dtaiso
013304070222     c                   move      dtaiso        dtaeur
013305070222     c                   move      dtaeur        prtdfv
013306080201     c                   eval      prtloop=loop
013307080201     c                   eval      prtnfv=dst3nfv
013308070402     c                   eval      prtprgg=dst3prgg
013309070402     c                   eval      prtsql=dst3sql
013310070402     c                   eval      prtfgsg=dst3fgsg
013311070402     c                   eval      prtcgi=dst3cgi
013312071210     c                   if        d19ldcri <> 0
013313071210     c                   move      d19ldcri      dtaiso
013314071210     c                   move      dtaiso        dtaeur
013315071210     c                   move      dtaeur        decldcr
013316071210     c                   else
013317071210     c                   clear                   decldcr
013318071210     c                   end
013319071107     c* decodifico parametri di lancio
013320071107     c                   if        *in71
013321071107     c                   move      d19dtai       dtaiso
013322071107     c                   move      dtaiso        dtaeur
013323071107     c                   move      dtaeur        decdta
013324071107     c                   eval      decora = d19orai
013325071107     c                   end
013326071107     c                   if        d19cleari = 'S'
013327071107     c                   eval      decclear='SI'
013328071107     c                   else
013329071107     c                   eval      decclear='NO'
013330071107     c                   end
013331150430     c                   if        d19p88i = 'N'
013332150515     c                   eval      decp88r='Spediz. pistola 88 NO'
013333150515     c                   else
013334150515     c                   clear                   decp88r
013335150430     c                   end
013336071107     c                   if        *in70
013337071107     c                   select
013338071107     c                   when      d19tpmi ='M'
013339071107     c                   eval      dectpm ='MESSAGGERIE'
013340071107     c                   when      d19tpmi ='P'
013341071107     c                   eval      dectpm ='PARCEL'
013342071107     c                   when      d19tpmi =' '
013343071107     c                   eval      dectpm ='TUTTE'
013344071107     c                   endsl
013345071107     c                   end
013346160420     c                   if        *in72
013347160420     c                   eval      dectur1 = d19tur1
013348160420     c                   eval      dectur2 = d19tur2
013349160420     c                   end
013350071107     c                   select
013351071107     c                   when      d19fppi ='M'
013352071107     c                   eval      decfpp ='MAT'
013353071107     c                   when      d19fppi ='P'
013354071107     c                   eval      decfpp ='POM'
013355071107     c                   when      d19fppi =' '
013356071107     c                   eval      decfpp ='TUTTE'
013357071107     c                   endsl
013358070323     C* PAGINA IN STAMPA
013359070323     C                   IF        *IN80 OR RIGA >=60
013360070323     C                   SETON                                        80
013361070323     C                   ADD       1             PAGES
013362071107     C                   Z-ADD     5             RIGA              3 0
013363070323     C                   END
013364070222     c*
013365070222     c   80              write     testa
013366071107     c   80              write     testap
013367070222     c                   setoff                                       80
013368070222     c   81              write     riga1
013369070323     C   81              ADD       1             RIGA
013370070222     c   82              write     riga2
013371070323     C   82              ADD       1             RIGA
013372070402     c   87              write     riga7
013373070402     C   87              ADD       1             RIGA
013374070402     c                   setoff                                       818287
013375071107     c                   end
013376070222     c*
013377070222     c                   endsr
013378950525     C*--------------------------------------------------------------*
013400950525     C*  Operazioni iniziali
013500950525     C*--------------------------------------------------------------*
013600950525     C     *INZSR        BEGSR
013700950525     C*
013800950525     C     *ENTRY        PLIST
013900950525     C                   PARM                    KPJBA
014000070206     C                   MOVEL(P)  KPJBU         Fidg19ds
014001070206     c                   clear                   d19erro
014002070323     c                   eval      pages = d19pag
014003070323     c                   eval      riga = d19riga
014100030718     c*
014200030718     c     *dtaara       define    §azute        azuteds
014300030718     c     *dtaara       define    §datiute      ddatiute
014400030718     C                   in(E)     *dtaara
014500030718     C                   IF        %Error  or  RSUT = *blanks
014600030718     C                   call      'TIBS34R'
014700030718     C                   parm                    Tibs34Ds
014800030718     C                   in        *dtaara
014900030718     c                   ENDIF
014901071107     c*
014902071107     C     d19FGSi       CHAIN     AZORG01l
014903071107    1C                   IF        %found(azorg01l)
014904071107     c                   movel     orgdf0        og150
014905080115     C                   MOVEL     ORGde6        og146
014906071107     C                   MOVEL     ORGDE7        og147
014908160420     c     §OGPKM        comp      'S'                                    70
014909071107     c     §OGdoada      comp      'S'                                    71
014910160420     c     §OGturno      comp      'S'                                    72
014911080115     c* se data distinte automatiche < oggi p.o. partito
014912080115     c                   if        §ogdda = ' '
014913080115     c                   else
014914080115     c                   move      §ogdda        comdda            8 0
014915080115     c                   end
014916071107     c                   end
015000950525     C*  Definisco chiavi di accesso
015100070205     C     Kdst          KLIST
015101070410     C                   KFLD                    dst3fgs
015200070205     C                   KFLD                    dst3npg
015300070205     C                   KFLD                    dst3nfv
015301080326     C*
015302080326     C     Kdsf          KLIST
015304080326     C                   KFLD                    dstnpg
015305080326     C                   KFLD                    dstnfv
015306080326     C                   KFLD                    dstfgs
015307080326     C                   KFLD                    dsftrd
015401070410     C*
015402070223     C                   TIME                    TIMES             6 0
015403070223     C                   TIME                    WHDAT            12 0
015404070223     C                   MOVE      WHDAT         WOGGI             6 0
015405080115     C                   TIME                    WHHUDT           14 0
015407080115     C                   MOVE      WHHUDT        UDATE8            8 0
015408080115     c                   move      udate8        dtaeur
015409080115     c                   move      dtaeur        dtaiso
015412080115     c                   move      dtaiso        dateu8            8 0
017100950525     C                   ENDSR
017200070518     C*----------------------------------------------------*
017300070518     C     geslck        begsr
017400070518     C*----------------------------------------------------*
017401070518     c                   clear                   trul82ds
017500070518     c                   Eval      ul82§rrn = dstnrr
017600070518     c                   Eval      UL82§FIL = 'FIDST00F'
017700070518     c                   Eval      UL82§WIN = 'S'
017900070518     c                   Eval      UL82§num = 2
018000070518     c                   Eval      UL82§att = 2
018001070518     c                   eval      ul82§mss = ErrMsg(1)
018002070518     c                   Eval      UL82§msw = 'La Distinta '
018003070518     c                             + %editc(dst3nfv:'Z') +  ' è già utilizzata -
018004070518     c                             da un altro lavoro e non è manutenzionabile'
019000070518      * Effettuo la chiamata al *PGM d utilità
019100070518     c                   Call(e)   'TRUL82R'
019200070518     c                   Parm                    Trul82ds
019600070518
019700070518     C                   ENDSR
019701070518** ErrMsg
019800070518Si sta bloccando l'assegnazione DISTINTE: SI PREGA DI USCIRE dal lavoro!       27
