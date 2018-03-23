000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000600090518     Faitra01l  if   e           k disk
000800100219     Ftrmza10f  uf a e           k disk    usropn
001000020304     dkpjba          e ds
001400070605     d dataiso         s               d   datfmt(*iso)
001500070605     d dataeur         s               d   datfmt(*eur)
001600070605     D WLBDA8          DS
001700070605     D  G02DAT                 1      8  0
001800070605     D  G02INV                 9     16  0
001900070605     D  G02ERR                17     17
002000070605     D  G02TGI                18     22  0
004200080502      *
004300090519     d coman           s             99
004400090519     d luncmd          s             15  5
004500090519     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
004600090519     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
004700090519     D cmd3            S              1    DIM(99) CTDATA PERRCD(99)
004800090519     D cmd4            S              1    DIM(99) CTDATA PERRCD(99)
004900090519     D cmd5            S              1    DIM(99) CTDATA PERRCD(99)
005000000000     C*---------------------------------------------------------------*
005001100219     c                   exsr      elabora
005100020304      *
007000090519      *verifica se esiste file in libreria wrkquery se c'è lo elimina
007100090519     c                   movea     cmd4          coman
007200090519     c                   exsr      comandi
007300090519      *esegue crtdupobj da qtemp a wrkquery
007400090519     c                   movea     cmd5          coman
007500090519     c                   exsr      comandi
007600090519      *elimina file in qtemp x la filiale
007700100219     c                   close     trmza10f
007800090519     c                   movea     cmd3          coman
007900090519     c                   exsr      comandi
007902100219      /FREE
007903100219         EXEC SQL
007904100219          delete   from wrkquery/trmza10f WHERE
007905100219          za1taa in(select za1taa from( SELECT
007906100219          za1taa, count(*) FROM wrkquery/trmza10f
007907100219           GROUP BY za1taa HAVING count(*)
007908100219          = 1 )l) ;
007909100219
007910100219      /END-FREE
007911100219     c                   seton                                        lr
008001100219     c*-------------------------------------------------
008002100219     c     elabora       begsr
008003100219     c*-------------------------------------------------
012500100219     c     *loval        setll     aitra01l
012600090924     c                   do        *hival
012700100219     c                   read      aitra01l
012800090924     c                   if        %eof(aitra01l)
012900090924     c                   leave
013000090924     c                   endif
013001100219      * record da scartare
013200100219     c                   if        tradfi > 0 or
013201100219     c                             traann <> *blank or
013202100219     c                             trataa = *blank
013400100219     c                   iter
013500090518     c                   endif
013501100219      *
013502100219     c                   clear                   diffe             1
013503100219     c                   clear                   trmza100
013504100219     c     trataa        chain     trmza10f
013505100219     c                   if        %found(trmzA10f)
013506100219     c                   exsr      confronti
013507100219     c                   endif
013508100219     c                   if        not %found(trmzA10f) or diffe <> *blank
013509100219     c                   eval      ZA1FIL    =  traFIL
013510100219     c                   eval      ZA1AUT    =  traAUT
013511100219     c                   eval      ZA1NRC    =  traNRC
013512100219     c                   eval      ZA1RIS    =  traRIS
013514100219     c                   eval      ZA1TIA    =  traTIA
013516100219     c                   eval      ZA1SCO    =  traSCO
013518100219     c                   eval      ZA1TAA    =  traTAA
013519100219     c                   eval      ZA1COR    =  traCOR
013521100219     c                   eval      ZA1BPO    =  traBPO
013523100219     c                   eval      ZA1ANT    =  traANT
013525100219     c                   eval      ZA1TMP    =  traTMP
013527100219     c                   eval      ZA1PUB    =  traPUB
013529100219     c                   eval      ZA1FUR    =  traFUR
017700100219     c                   eval      ZA1SPI    =  traSPI
017702100219     c                   eval      ZA1FF1    =  traFF1
017704100219     c                   eval      ZA1FF2    =  traFF2
017706100219     c                   eval      ZA1RIF    =  traRIF
017707100219     C                   WRITE     TRMZA100
017708100219     c                   endif
017709100219     c                   enddo
017900070530      *
018000070530     c                   endsr
018001100219     c*-------------------------------------------------
018002100219     c     confronti     begsr
018003100219     c*-------------------------------------------------
018005100219     c                   if        ZA1AUT    <> traAUT
018006100219     c                   eval      diffe = 'X'
018007100219     c                   eval      za1autf = '1'
018008100219     c                   endif
018009100219     c                   if        ZA1NRC   <>  traNRC
018010100219     c                   eval      diffe = 'X'
018011100219     c                   eval      za1nrcf = '1'
018012100219     c                   endif
018013100219     c                   if        ZA1RIS   <>  traRIS
018014100219     c                   eval      diffe = 'X'
018015100219     c                   eval      za1risf = '1'
018016100219     c                   endif
018017100219     c                   if        ZA1TIA   <>  traTIA
018018100219     c                   eval      diffe = 'X'
018019100219     c                   eval      za1tiaf = '1'
018020100219     c                   endif
018021100219     c                   if        ZA1SCO   <>  traSCO
018022100219     c                   eval      diffe = 'X'
018023100219     c                   eval      za1scof = '1'
018024100219     c                   endif
018025100219     c                   if        ZA1COR    <> traCOR
018026100219     c                   eval      diffe = 'X'
018027100219     c                   eval      za1corf = '1'
018028100219     c                   endif
018029100219     c                   if        ZA1BPO    <> traBPO
018030100219     c                   eval      diffe = 'X'
018031100219     c                   eval      za1bpof = '1'
018032100219     c                   endif
018033100219     c                   if        ZA1ANT    <> traANT
018034100219     c                   eval      diffe = 'X'
018035100219     c                   eval      za1antf = '1'
018036100219     c                   endif
018037100219     c                   if        ZA1TMP    <> traTMP
018038100219     c                   eval      diffe = 'X'
018039100219     c                   eval      za1tmpf = '1'
018040100219     c                   endif
018041100219     c                   if        ZA1PUB    <> traPUB
018042100219     c                   eval      diffe = 'X'
018043100219     c                   eval      za1pubf = '1'
018044100219     c                   endif
018045100219     c                   if        ZA1FUR    <> traFUR
018046100219     c                   eval      diffe = 'X'
018047100219     c                   eval      za1furf = '1'
018048100219     c                   endif
018049100219     c                   if        ZA1SPI    <> traSPI
018050100219     c                   eval      diffe = 'X'
018051100219     c                   eval      za1spif = '1'
018052100219     c                   endif
018053100219     c                   if        ZA1FF1    <> traFF1
018054100219     c                   eval      diffe = 'X'
018055100219     c                   eval      za1ff1f = '1'
018056100219     c                   endif
018057100219     c                   if        ZA1FF2    <> traFF2
018058100219     c                   eval      diffe = 'X'
018059100219     c                   eval      za1ff2f = '1'
018060100219     c                   endif
018061100219     c                   if        ZA1RIF    <> traRIF
018062100219     c                   eval      diffe = 'X'
018063100219     c                   eval      za1riff = '1'
018064100219     c                   endif
018065100219     c                   endsr
021900090519     c*-------------------------------------------------
022000090519     c     comandi       begsr
022100090519     c*-------------------------------------------------
022200090519      *
022300090519     C                   z-add     99            luncmd
022400090519     C                   CALL      'QCMDEXC'                            34
022500090519     C                   parm                    coman
022600090519     C                   parm                    luncmd
022700090519     c                   endsr
022800011026     c*-------------------------------------------------
022900011026     c     *inzsr        begsr
023000011026     c*-------------------------------------------------
023100020304     c     *entry        plist
023200020304     c                   parm                    kpjba
023300050801
024100050801
024200020218     C                   time                    w0120            14 0
024300020218     C                   move      w0120         wdat              8 0
025300090518
025400090519      *clear file
025500090519     c                   movea     cmd1          coman
025600090519     c                   exsr      comandi
025700090519     c                   movea     cmd2          coman
025800090519     c                   exsr      comandi
025900100219     c                   open      trmza10f
026000090519
026100090518     c                   endsr
026200090519**  CMD1
026300100219CLRPFM    FILE(trmza10f)
026400090519**  CMD2
026500100219CRTDUPOBJ OBJ(trmza10F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
026600090519**  CMD3
026700100219DLTF FILE(QTEMP/trmza10F)
026800090519**  CMD4
026900100219dltf file(WRKQUERY/trmza10f)
027000090519**  CMD5
027100100219CRTDUPOBJ OBJ(trmza10F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(trmza10f) DATA(*YES)
