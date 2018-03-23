000100950524     H DECEDIT('0,') DATEDIT(*DMY.)
000102070205     h dftactgrp(*no) actgrp(*caller)
000103070322     H* FIDG19R *----------------------------------------------------*
000104070322     H*         assegnazione automatica distinte consegna x giro
000105070322     H*--------------------------------------------------------------*
000201070222     FFidg18p   O    E             PRINTER OFLIND(*IN80)
000600070518     FFIDST09L  uF   E           K DISK    commit infds(dstinfds)
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
001200030910     D azuteds       e ds                  extname(AZUTE00F)
001300030910     D tibs34ds      E DS                  inz
001400950524     D KPJBA         E DS
001500070206     D Fidg19ds      E DS
001501070205     D Fidg21ds      E DS
001800070206     D savndd          S                   INZ like(d19nfdi)
001900070206     D savnda          S                   INZ like(d19nfai)
001901070205     D compkl          S                   INZ like(dstpkl)
001902070205     D comvlu          S                   INZ like(dstvlu)
001903070205     D comnftl         S                   INZ like(dstnftl)
001905070209     d distinta        ds
001906070209     d  dfv                                like(dst3dfv)
001907070209     d  nfv                                like(dst3nfv)
001908070209     d savdist         s                   like(distinta)
001909070518     D Errmsg          S             78    DIM(1) CTDATA PERRCD(1)              MSG DI ERRORE
002000950524     C*--------------------------------------------------------------*
002100950524     C*  Ciclo principale
002200950524     C*--------------------------------------------------------------*
002201070212     C                   do        99            xx                3 0
003800070206      * leggo in Fidst30F ordinato per data, priorità, distinta e squenza
003801070206      * giro
004021061003     C/EXEC SQL
004022070202     C+ DECLARE B1 CURSOR FOR SELECT fidst30f.* FROM fidst30f WHERE
004023070206     C+ dst3fgs =:d19fgsi and dst3npg=4 and dst3dfv = :d19dfvi and dst3atb
004024070206     C+  = ' ' and dst3nfv between :d19nfdi and :d19nfai and dst3prgg = :xx
004025070202     C+ ORDER BY DST3FGS, DST3NPG, DST3DFV, DST3NFV, DST3SQL
004027061003     C/END-EXEC
004034061003
004035061003     C/EXEC SQL
004036061003     C+ OPEN b1
004037061003     C/END-EXEC
004038061003
004039061003     C                   do        *hival
004040061003
004041061003     C/EXEC SQL
004042070202     C+ FETCH NEXT FROM b1 INTO :fidst3ds
004043061003     C/END-EXEC
004044061003
004045061003     C                   select
004046061003     C                   when      sqlcod = 100
004047061003     c                   leave
004048070206     C                   WHEN      SqlCod < 0
004049070206     c* record allocato
004050070206     c                   if        sqlcod <> -913
004051070206     C                   seton                                        H1
004052070206     c                   end
004053070222     c                   seton                                        81
004054070222     c                   exsr      srstampa
004055070206     c                   exsr      srfine
004056070206     c                   other
004057070209     c                   eval      dfv = dst3dfv
004058070209     c                   eval      nfv = dst3nfv
004059070209     c                   if        distinta <> savdist and d19cmti='S'
004060070209     c                   commit
004061070209     c                   eval      savdist = distinta
004062070209     c                   end
004063061003     C                   exsr      gesdis
004064061003     C                   endsl
004065061003     C*
004066061003     C                   enddo
004067061003
004068061003     C/EXEC SQL
004069061003     C+ CLOSE b1
004070061003     C/END-EXEC
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
007122070202     c                   exsr      srctrdis
007123070205     c* se non ci sono errori sulla distinta proseguo
007124070223     c                   if        not *in28
007125070402     c                   seton                                        87
007126070402     c                   exsr      srstampa
007127070205     c                   clear                   fidg21ds
007128070205     c                   eval      D21FGSI = dstFGS
007129070205     c                   eval      d21NPGI = dstNPG
007130070205     c                   eval      d21DFVI = dstDFV
007131070205     c                   eval      d21NFVI = DstNFV
007132070205     c                   eval      D21PDRI = DstPDR
007133070205     c                   eval      D21FGSGI= Dst3FGSG
007134070205     c                   eval      D21CGII = Dst3CGI
007135070205     c                   eval      D21PKLI = compkl
007136070205     c                   eval      D21VLUI = comvlu
007137070205     c                   eval      D21NFTLI= comnftl
007138070323     c                   eval      D21PAG= PAGES
007139070323     c                   eval      D21RIGA= RIGA
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
010213070206     c                   else
010214070206     c*                  exsr      srstampa
010215070202     c                   end
010216070202     c                   end
010500070205     C                   END                                                    Trovate spunte
010600950524     C*
010700070205     C                   ENDSR
013201061003      *-----------------------------------------------
013202070202     C     srctrdis      begsr
013203061003      *-----------------------------------------------
013204070202     c                   setoff                                       28
013205070202     C* distinta annullata
013206070202     c                   if        dstatb <> ' '
013207070223     c                   seton                                        28
013208070202     c                   leavesr
013209070202     c                   end
013210070410     C* distinta chiusa
013211070410     c                   if        dstfcf <> ' '
013212070410     c                   seton                                        28
013213070410     c                   leavesr
013214070410     c                   end
013215070202     C* prestazione mattina/pomeriggio
013216070302     c                   if        dstfpp <> d19fppi and d19fppi <> ' '
013217070223     c                   seton                                        28
013218070202     c                   leavesr
013219070202     c                   end
013220070202     C* parcel/messaggerie
013221070302     c                   if        (d19tpmi= 'P' and dsttpm <>' ') or
013222070302     c                             (d19tpmi= 'M' and dsttpm <>'M')
013223070223     c                   seton                                        28
013224070202     c                   leavesr
013225070202     c                   end
013226070202     C* capienza
013227070206     c                   if        d19reti<> 0
013228070209     c                   eval      compkl = (dstpkl+((dstpkl * d19reti)/100))
013229070209     c                   eval      comvlu = (dstvlu+(dstvlu * d19reti)/100)
013230070209     c                   eval      comnftl= (dstnftl+(dstnftl * d19reti)/100)
013231070202     c                   else
013232070202     c                   eval      compkl = dstpkl
013233070202     c                   eval      comvlu = dstvlu
013234070205     c                   eval      comnftl= dstnftl
013235070202     c                   end
013236070202     c*
013237070202     c                   if        dstpkla >= compkl or
013238070202     c                             dstvlua >= comvlu or
013239070222     c                             dststpa >= comnftl
013240070202     c                   seton                                        28
013241070202     c                   leavesr
013242070212     c                   else
013243070212     c                   eval      compkl = compkl - dstpkla
013244070212     c                   eval      comvlu = comvlu - dstvlua
013245070215     c                   eval      comnftl= comnftl- dststpa
013246070202     c                   end
013247070202     C*
013248070202     C                   ENDSR
013249070206     C**************************************************************************
013250070206     C* esco con codice d'errore
013251070206     C**************************************************************************
013252070206     C     srfine        BEGSR
013253070206     c*
013254070206     c                   if        d19cmti= 'S'
013255070206     c                   rolbk
013256070206     c                   end
013257070206     c*
013258070206     c                   eval      d19erro= '1'
013259070206     c                   eval      kpjbu = fidg19ds
013260070206     c                   seton                                        lr
013261070206     c                   return
013262070206     c*
013263070206     c                   endsr
013264070222     C*------------------------------------------------------------------------*
013265070222     C* stampa
013266070222     C*------------------------------------------------------------------------*
013267070222     C     srstampa      BEGSR
013268070222     c                   move      d19dfvi       dtaiso
013269070222     c                   move      dtaiso        dtaeur
013270070222     c                   move      dtaeur        prtdfv
013271070222     c                   eval      prtnfv=dst3nfv
013272070402     c                   eval      prtprgg=dst3prgg
013273070402     c                   eval      prtsql=dst3sql
013274070402     c                   eval      prtfgsg=dst3fgsg
013275070402     c                   eval      prtcgi=dst3cgi
013276070323     C* PAGINA IN STAMPA
013277070323     C                   IF        *IN80 OR RIGA >=60
013278070323     C                   SETON                                        80
013279070323     C                   ADD       1             PAGES
013280070323     C                   Z-ADD     3             RIGA              3 0
013281070323     C                   END
013282070222     c*
013283070222     c   80              write     testa
013284070222     c                   setoff                                       80
013285070222     c   81              write     riga1
013286070323     C   81              ADD       1             RIGA
013287070222     c   82              write     riga2
013288070323     C   82              ADD       1             RIGA
013289070402     c   87              write     riga7
013290070402     C   87              ADD       1             RIGA
013291070402     c                   setoff                                       818287
013292070222     c*
013293070222     c                   endsr
013300950525     C*--------------------------------------------------------------*
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
015000950525     C*  Definisco chiavi di accesso
015100070205     C     Kdst          KLIST
015101070410     C                   KFLD                    dst3fgs
015200070205     C                   KFLD                    dst3npg
015300070205     C                   KFLD                    dst3nfv
015401070410     C*
015402070223     C                   TIME                    TIMES             6 0
015403070223     C                   TIME                    WHDAT            12 0
015404070223     C                   MOVE      WHDAT         WOGGI             6 0
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
