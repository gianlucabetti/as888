000100950524     H DECEDIT('0,') DATEDIT(*DMY.)
000101070301     h dftactgrp(*no) actgrp(*caller)
000102070322     H* FIDG24R *----------------------------------------------------*
000104070322     H*         Aggiorno valori assegnato su FIDST
000105070322     H*--------------------------------------------------------------*
000201070228     FFidg18p   O    E             PRINTER OFLIND(*IN80)
000202071107     FAZORG01L  IF   E           K DISK
000203080326     Ffidsf02l  IF   E           K DISK
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
001502071107     D OG147         E DS                  INZ
001503071107     D OG150         E DS                  INZ
002000950524     C*--------------------------------------------------------------*
002100950524     C*  Ciclo principale
002200950524     C*--------------------------------------------------------------*
003800070322      * leggo tutte le distinte
004021061003     C/EXEC SQL
004022070827     C+ DECLARE A1 CURSOR FOR SELECT fidst09l.* FROM fidst09l WHERE
004024070827     C+ dstnfv between :D19NFDI and :D19NFaI and dstdfv=:d19dfvi and
004025070827     C+ dstfgs = :d19fgsi and dstnpg = 4 and dstatb = ' ' and dstfasd =
004026070827     C+ ' '
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
004067080326     c* verifico che non esista già una fase
004068080326     c                   eval      dsftrd = 'STP'
004069080326     c     kdsf          setll     fidsf02l                               28
004070080326     c                   if        not *in28 and (dstpda='E' or dstpda='C')
004071080326     c                   eval      dsftrd = 'PDC'
004072080326     c     kdsf          setll     fidsf02l                               28
004073080326     c                   end
004074080326     c                   if        *in28
004075080326     c                   iter
004076080326     c                   end
004077070302     c*
004078070517     c                   clear                   fidg37ds
004079070827     c                   eval      D37oraI = D19oraI
004081070827     c                   eval      D37dtaI = D19dtaI
004082070517     c                   eval      D37FGSI = dstfgs
004083070517     c                   eval      D37DFVI = dstdfv
004084070517     c                   eval      D37NFVI = dstnfv
004085070517     c                   eval      D37cmtI = 'N'
004086070517     c                   eval      kpjbu = FIDG37ds
004087070517     c                   call      'FIDG37R'
004088070517     c                   parm                    kpjba
004089070517     c                   eval      FIDG37ds = kpjbu
004090070517     c                   if        d37erro <>' '
004091070301     c                   seton                                        81
004092070301     c                   exsr      srstampa
004093070301     c                   iter
004094070301     c                   end
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
010705071107     c* stampo solo per assegnaz. automatica
010706071107     c                   if        kcdaz= 'DG17' or kcdaz='DG18'
010707070227     c                   move      d19dfvi       dtaiso
010708070227     c                   move      dtaiso        dtaeur
010709070227     c                   move      dtaeur        prtdfv
010710071107     c* decodifico parametri di lancio
010711071210     c                   if        d19ldcri <> 0
010712071210     c                   move      d19ldcri      dtaiso
010713071107     c                   move      dtaiso        dtaeur
010714071210     c                   move      dtaeur        decldcr
010715071210     c                   else
010716071210     c                   clear                   decldcr
010717071210     c                   end
010718071210     c                   if        *in71
010719071210     c                   move      d19dtai       dtaiso
010720071210     c                   move      dtaiso        dtaeur
010721071210     c                   move      dtaeur        decdta
010722071107     c                   eval      decora = d19orai
010723071107     c                   end
010724071107     c                   if        d19cleari = 'S'
010725071107     c                   eval      decclear='SI'
010726071107     c                   else
010727071107     c                   eval      decclear='NO'
010728071107     c                   end
010729150430     c                   if        d19p88i = 'N'
010733150515     c                   eval      decp88r='Spediz. pistola 88 NO'
010734150515     c                   else
010735150515     c                   clear                   decp88r
010736150430     c                   end
010737071107     c                   if        *in70
010738071107     c                   select
010739071107     c                   when      d19tpmi ='M'
010740071107     c                   eval      dectpm ='MESSAGGERIE'
010741071107     c                   when      d19tpmi ='P'
010742071107     c                   eval      dectpm ='PARCEL'
010743071107     c                   when      d19tpmi =' '
010744071107     c                   eval      dectpm ='TUTTE'
010745071107     c                   endsl
010746071107     c                   end
010747071107     c                   select
010748071107     c                   when      d19fppi ='M'
010749071107     c                   eval      decfpp ='MAT'
010750071107     c                   when      d19fppi ='P'
010751071107     c                   eval      decfpp ='POM'
010752071107     c                   when      d19fppi =' '
010753071107     c                   eval      decfpp ='TUTTE'
010754071107     c                   endsl
010755070323     C* PAGINA IN STAMPA
010756070323     C                   IF        *IN80 OR RIGA >=60
010757070323     C                   SETON                                        80
010758070323     C                   ADD       1             PAGES
010759071107     C                   Z-ADD     5             RIGA              3 0
010760070323     C                   END
010761070227     c*
010762070227     c   80              write     testa
010763071107     c   80              write     testap
010764070227     c                   setoff                                       80
010765070301     c   81              write     riga3
010766070323     c   81              add       1             riga
010767070301     c   82              write     riga4
010768070323     c   82              add       1             riga
010769070301     c                   setoff                                       8182
010770071107     c                   end
010771070227     c*
010772070227     c                   endsr
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
014901071107     C     d19FGSi       CHAIN     AZORG01l
014902071107    1C                   IF        %found(azorg01l)
014903071107     c                   movel     orgdf0        og150
014905071107     C                   MOVEL     ORGDE7        og147
014906071107     c     §OGPKM        comp      'S'                                    70
014907071107     c     §OGdoada      comp      'S'                                    71
014908071107     c                   end
014909080326     C*
014910080326     C     Kdsf          KLIST
014911080326     C                   KFLD                    dstnpg
014912080326     C                   KFLD                    dstnfv
014913080326     C                   KFLD                    dstfgs
014914080326     C                   KFLD                    dsftrd
017100950525     C                   ENDSR
