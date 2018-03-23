000100950524     H DECEDIT('0,') DATEDIT(*DMY.)
000101070301     h dftactgrp(*no) actgrp(*caller)
000102070322     H* FIDG24R *----------------------------------------------------*
000104070322     H*         Aggiorno valori assegnato su FIDST
000105070322     H*--------------------------------------------------------------*
000201070228     FFidg18p   O    E             PRINTER OFLIND(*IN80)
000800950524      *--------------------------------------------------------------*
000900950524      *   DS
001000950524      *--------------------------------------------------------------*
001001070301     ddtaiso           s               d
001002070301     ddtaeur           s               d   datfmt(*eur)
001102050520     D ddatiute      e ds
001200070301     D fidstds       e ds                  extname(fidst00F)
001201070301     D azuteds       e ds                  extname(AZUTE00F)
001300030910     D tibs34ds      E DS                  inz
001400950524     D KPJBA         E DS
001500070206     D Fidg19ds      E DS
001501070517     D Fidg37ds      E DS
002000950524     C*--------------------------------------------------------------*
002100950524     C*  Ciclo principale
002200950524     C*--------------------------------------------------------------*
003800070322      * leggo tutte le distinte
004021061003     C/EXEC SQL
004022070517     C+ DECLARE A1 CURSOR FOR SELECT fidst09l.*  FROM
004024070410     C+ fidst09l WHERE dstnfv between :D19NFDI and :D19NFaI and
004025070301     C+ dstdfv=:d19dfvi and dstfgs = :d19fgsi and dstnpg = 4
004026070302     C+ and dstatb = ' '
004027061003     C/END-EXEC
004034061003
004035061003     C/EXEC SQL
004036070228     C+ OPEN a1
004037061003     C/END-EXEC
004038061003
004039061003     C                   do        *hival
004040061003
004041061003     C/EXEC SQL
004042070517     C+ FETCH NEXT FROM a1 INTO :fidstds
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
004053070227     c                   exsr      srstampa
004054070206     c                   exsr      srfine
004055070206     c                   other
004056070302     C* prestazione mattina/pomeriggio
004057070302     c                   if        dstfpp <> d19fppi and d19fppi <> ' '
004058070302     c                   iter
004060070302     c                   end
004061070302     C* parcel/messaggerie
004062070302     c                   if        (d19tpmi= 'P' and dsttpm <>' ') or
004063070302     c                             (d19tpmi= 'M' and dsttpm <>'M')
004064070302     c                   iter
004066070302     c                   end
004067070302     c*
004068070517     c                   clear                   fidg37ds
004069070517     c                   eval      D37FGSI = dstfgs
004070070517     c                   eval      D37DFVI = dstdfv
004071070517     c                   eval      D37NFVI = dstnfv
004072070517     c                   eval      D37cmtI = 'N'
004073070517     c                   eval      kpjbu = FIDG37ds
004074070517     c                   call      'FIDG37R'
004075070517     c                   parm                    kpjba
004076070517     c                   eval      FIDG37ds = kpjbu
004077070517     c                   if        d37erro <>' '
004080070301     c                   seton                                        81
004081070301     c                   exsr      srstampa
004082070301     c                   iter
004083070301     c                   end
004096061003     C                   endsl
004097061003     C*
004098061003     C                   enddo
004099061003
004100061003     C/EXEC SQL
004101070228     C+ CLOSE a1
004102061003     C/END-EXEC
006200950524     C*
006201070228     c                   if        d19cmti= 'S'
006301070228     c                   commit
006302070228     c                   end
006303070323     c                   eval      d19pag = pages
006304070323     c                   eval      d19riga= riga
006305070323     c                   eval      kpjbu = fidg19ds
006404070206     c*
006500950524     C                   SETON                                        LR
010701070227     C*------------------------------------------------------------------------*
010702070227     C* stampa
010703070227     C*------------------------------------------------------------------------*
010704070227     C     srstampa      BEGSR
010705070227     c                   move      d19dfvi       dtaiso
010706070227     c                   move      dtaiso        dtaeur
010707070227     c                   move      dtaeur        prtdfv
010708070323     C* PAGINA IN STAMPA
010709070323     C                   IF        *IN80 OR RIGA >=60
010710070323     C                   SETON                                        80
010711070323     C                   ADD       1             PAGES
010712070323     C                   Z-ADD     3             RIGA              3 0
010713070323     C                   END
010714070227     c*
010715070227     c   80              write     testa
010716070227     c                   setoff                                       80
010717070301     c   81              write     riga3
010718070323     c   81              add       1             riga
010719070301     c   82              write     riga4
010720070323     c   82              add       1             riga
010721070301     c                   setoff                                       8182
010722070227     c*
010723070227     c                   endsr
013310070222     C**************************************************************************
013311070222     C* esco con codice d'errore
013312070222     C**************************************************************************
013313070222     C     srfine        BEGSR
013314070222     c*
013315070222     c                   if        d19cmti= 'S'
013316070206     c                   rolbk
013317070206     c                   end
013318070206     c*
013319070222     c                   eval      d19erro= '1'
013320070222     c                   eval      kpjbu = fidg19ds
013321070206     c                   seton                                        lr
013322070206     c                   return
013323070206     c*
013324070206     c                   endsr
013345950525     C*--------------------------------------------------------------*
013400950525     C*  Operazioni iniziali
013500950525     C*--------------------------------------------------------------*
013600950525     C     *INZSR        BEGSR
013700950525     C*
013800950525     C     *ENTRY        PLIST
013900950525     C                   PARM                    KPJBA
014000070206     C                   MOVEL(P)  KPJBU         Fidg19ds
014001070323     c                   eval      pages = d19pag
014002070323     c                   eval      riga = d19riga
014003070206     c                   clear                   d19erro
014100030718     c*
014200030718     c     *dtaara       define    §azute        azuteds
014300030718     c     *dtaara       define    §datiute      ddatiute
014400030718     C                   in(E)     *dtaara
014500030718     C                   IF        %Error  or  RSUT = *blanks
014600030718     C                   call      'TIBS34R'
014700030718     C                   parm                    Tibs34Ds
014800030718     C                   in        *dtaara
014900030718     c                   ENDIF
017100950525     C                   ENDSR
