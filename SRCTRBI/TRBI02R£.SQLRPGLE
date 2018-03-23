000100161020     H DEBUG DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200000000     H*--------------------------------------------------------------*
000300161017     Ffidst01l  if   e           k disk
000400161017     Fazorg01l  if   e           k disk
000500161017     Ffnarb01l  if   e           k disk
000600161017     Ffnart01l  if   e           k disk
000700161017     Ffnlbl01l  if   e           k disk
000800161019     Ffiar501l  if   e           k disk
000900161103     Ffiar404l  if   e           k disk
001000161213     Ffiar401l  if   e           k disk    rename(fiar4000:fiar41) prefix(x)
001100161017     Ffiftd03l  if   e           k disk
001200161019     Ffiarg01l  if   e           k disk
001300161104     Ffnorg01l  if   e           k disk
001400161019     Ffigrg03l  if   e           k disk
001500161020     Ffiar901l  if   e           k disk
001600161020     Ffiar601l  if   e           k disk
001700161017     Ftabel00f  if   e           k disk
001800161102     Ftntbe01l  if   e           k disk
001900161102     Ffnorf05l  if   e           k disk
002000161102     Ffnorm01l  if   e           k disk
002100161103     Ffnore01l  if   e           k disk
002200161017     Fbitdi00f  o    e             disk
002300161020     Fbiddc00f  o    e             disk
002400161102     Fbiddr00f  o    e             disk
002500161013      * ?___________________________________________________________________
002600161013     dparam            ds
002700161013     d datap                          8  0
002800170421     d datap2                         8  0
002900161013      *
003000161017     D fiqdt         E DS                  extname(fiqdt00f)
003100161201     D fnarbds       E DS                  extname(fnarb00f)
003200161201     D fnarbdss      E DS                  extname(fnarb00f) prefix(S)
003300161201     D arbmamds      E DS                  extname(fnarb00f) prefix(m)
003400161201     d $mamma          s                     like(§3Abcm)
003500020304     dkpjba          e ds
003600151116     d Tibs36Ds      e ds
003700161010     d og150         e ds
003800161019     d ds3a          e ds
003900161108     d ds5e          e ds
004000161019     d ds1p          e ds
004100161019     d ds2a          e ds
004200161108     d dcmr          e ds
004300161019     d trulorsds     e ds
004400161019     d trulor2ds     e ds
004500161213     d trulor3ds     e ds
004600161103     d fior900ds     e ds
004700161103     d doreorari     e ds
004800161020     d fnlv80ds      e ds
004900161019     d dar5EMD       e ds
005000161021     d dar5rcc       e ds
005100161019     d dar5bnb       e ds
005200161019     d diore01       e ds
005300161020     d ddstflr       e ds
005400161020     d dgrgflo       e ds
005500161103     d dorm01        e ds
005600170112     d dorf01        e ds
005700161019     d kpjbus          s                   like(kpjbu)
005800070605     d dataiso         s               d   datfmt(*iso)
005900070605     d dataeur         s               d   datfmt(*eur)
006000070605     D WLBDA8          DS
006100070605     D  G02DAT                 1      8  0
006200070605     D  G02INV                 9     16  0
006300070605     D  G02ERR                17     17
006400070605     D  G02TGI                18     22  0
006500161102     d primo           s              1
006600161020     d tmp             s             17
006700170404     d diffe           s              5  0
006800161102     d wcolr           s              5  0
006900161102     d wcolnr          s              5  0
007000161102     d wormf           s              5  0
007100170120     d wpkgaf          s                   like(orgpkg)
007200170120     d wvlmaf          s                   like(orgvlm)
007300170120     d wormaf          s              5  0
007400170120     d wbncaf          s                   like(ormbnc)
007500170120     d wferaf          s              5  0
007600170120     d wcolpr          s                   like(arbncl)
007700161201     d wvolpr          s                   like(artvuc)
007800161201     d wpespr          s                   like(artpuc)
007900161102     d wormnf          s              5  0
008000161102     d exORFPOE        s                   like(ORFPOE)
008100161102     d exORFNSR        s                   like(ORFNSR)
008200161102     d exORFNOR        s                   like(ORFNOR)
008300161102     d exORFNRV        s                   like(ORFNRV)
008400161102     d exORFFAR        s                   like(ORFFAR)
008500161102     d exORFCAR        s                   like(ORFCAR)
008600161102     d exORFDAE        s                   like(ORFDAE)
008700161102     d exORFORE        s                   like(ORFORE)
008800170112     d exORFflo        s                   like(ORFflo)
008900161130     d uORFDAE         s                   like(ORFDAE)
009000161130     d uORFORE         s                   like(ORFORE)
009100161213     d zero            s              5  0 inz
009200161213     d st              s              3  0 inz
009300161213     d stop            s              5  0 dim(900)
009400170120     d fer             s             70    dim(500)
009500170120     d como70          s             70
009600170120     d como70b         s             70
009700161011      *
009800161013      * ?___________________________________________________________________
009900020304      *
010000161125      *elaboro senza parametri ricevuti quindi le distinte elaborate come consegne rientri
010100161125     c                   if        datadis = *blank
010200161017     c* Lettura fiqdt00F x data elaborazione consegne
010300161017     C/EXEC SQL
010400161017     C+ DECLARE A1 CURSOR FOR SELECT * from fiqdt00f where
010500161017     C+  qdtatb = ' ' and qdtdaeco = :datap
010600161115     C+  order by qdtfgs, qdtnfv, qdtddc
010700161017     C/END-EXEC
010800161017
010900161017     C/EXEC SQL
011000161017     C+ OPEN A1
011100161017     C/END-EXEC
011200161017     C                   DOU       SqlCod <> 0
011300161017     C/EXEC SQL
011400161017     C+ FETCH NEXT FROM A1 INTO :fiqdt
011500161017     C/END-EXEC
011600161017     C                   SELECT
011700161017     c*
011800161017     C                   WHEN      SqlCod = 0
011900170404      * ? controlli di sicurezza verifico che non sia passata per errore una data di un foglio
012000170404      * ? annullato quindi 99/99/9999 e anche che non sia maggiore di udate di 30 gg
012100170404     c                   if        qdtddc > wdat30
012200170404     c                   iter
012300170404     c                   endif
012400161017     c                   exsr      distinta
012500161017     C*
012600161017      **
012700161017      * ? EoF
012800161017     C                   WHEN      SqlCod = 100
012900161017     c                   leave
013000161017      **
013100161017      * ? Errori
013200161017     C                   WHEN      SqlCod <> 0
013300161017     c                   leave
013400161017      **
013500161017     C                   ENDSL
013600161017      **
013700161017     C                   ENDDO
013800161017     C/EXEC SQL
013900161017     C+ CLOSE A1
014000161017     C/END-EXEC
014100161125     c                   else
014200170421      * ?effettua una delete preventiva per non scrivere rekord doppi nel periodo da elaborare
014300170421     C/EXEC SQL
014400170421     C+ delete from bitdi00F where tdidfv between :datap and :datap2
014500170421     C/END-EXEC
014600161125      *elaboro con la data distinta passata come parametro quindi data distinta
014700161125     c* Lettura fiqdt00F x data distinta
014800161125      *
014900161125     C/EXEC SQL
015000161125     C+ DECLARE B1 CURSOR FOR SELECT * from fiqdt00f where
015100170421     C+  qdtatb = ' ' and qdtddc between :datap and :datap2
015200161125     C+  order by qdtfgs, qdtnfv, qdtddc
015300161125     C/END-EXEC
015400161125
015500161125     C/EXEC SQL
015600161125     C+ OPEN B1
015700161125     C/END-EXEC
015800161125     C                   DOU       SqlCod <> 0
015900161125     C/EXEC SQL
016000161125     C+ FETCH NEXT FROM B1 INTO :fiqdt
016100161125     C/END-EXEC
016200161125     C                   SELECT
016300161125     c*
016400161125     C                   WHEN      SqlCod = 0
016500161125     c                   exsr      distinta
016600161125     C*
016700161125      **
016800161125      * ? EoF
016900161125     C                   WHEN      SqlCod = 100
017000161125     c                   leave
017100161125      **
017200161125      * ? Errori
017300161125     C                   WHEN      SqlCod <> 0
017400161125     c                   leave
017500161125      **
017600161125     C                   ENDSL
017700161125      **
017800161125     C                   ENDDO
017900161125     C/EXEC SQL
018000161125     C+ CLOSE B1
018100161125     C/END-EXEC
018200161125     c                   endif
018300161125      *
018400161017     c                   seton                                        lr
018500161017      * ?___________________________________________________________________
018600161017     c     distinta      begsr
018700161017      * ?___________________________________________________________________
018800161018     c                   clear                   bitdi000
018900161017     c     kdst          chain     fidst01l
019000161020     c                   if        %found(fidst01l) and
019100161020     c                             dstatb = *blank
019200161017     C                   movel     dstpdr        filpdr            3 0
019300161017     c                   if        filpdr <> exfilpdr
019400161017     c                   move      filpdr        exfilpdr          3 0
019500161017     c     exfilpdr      chain     azorg01l
019600161017     C                   MOVEL     ORGdf0        og150
019700161017     c                   endif
019800161021     c                   eval      TDIFGS  = QDTFGS
019900161021     c                   eval      TDIDFV  = QDTddc
020000161021     c                   eval      TDINFV  = QDTnfv
020100161021     c                   eval      TDIPDR  = QDTPDR
020200170630      *
020300170630      * decodifica il filler x TEST/PRODUZIONE e x TURNO
020400170630     c                   movel     dstflr        ddstflr
020500161010      * utilizzo pda
020600161017     c                   if        §dsttstpda = 'E' or §dsttstpda = 'C'
020700161017     c                   move      'T'           tdipdac
020800161017     c                   eval      tdipdacd = 'TEST PDA CONSEGNE'
020900161010     c                   else
021000161017     c                   if        dstpda = 'E' or dstpda = 'C'
021100161017     c                   move      'S'           tdipdac
021200161017     c                   eval      tdipdacd = 'PDA CONSEGNE'
021300161010     c                   else
021400161017     c                   move      'N'           tdipdac
021500161017     c                   eval      tdipdacd = 'NO PDA CONSEGNE'
021600161010     c                   endif
021700161010     c                   endif
021800161017     c                   if        §dsttstpda = 'E' or §dsttstpda = 'O'
021900161017     c                   move      'T'           tdipdar
022000161017     c                   eval      tdipdard = 'TEST PDA RITIRI'
022100161010     c                   else
022200161017     c                   if        dstpda = 'E' or dstpda = 'O'
022300161017     c                   move      'S'           tdipdar
022400161017     c                   eval      tdipdard = 'PDA RITIRI'
022500161010     c                   else
022600161017     c                   move      'N'           tdipdar
022700161017     c                   eval      tdipdard = 'NO PDA RITIRI'
022800161010     c                   endif
022900161010     c                   endif
023000161010      *
023100161010     c                   if        §ogturno = 'S'
023200161017     c                   move      §dstTURno     tdiTUR
023300161010     c                   select
023400161121     c                   when      §dstturno = 'U'
023500161017     c                   eval      tditur  = 'U'
023600161017     c                   eval      tdiTURD = 'TURNO UNICO'
023700161121     c                   when      §dstturno = ' '
023800161017     c                   eval      tdiTURD = 'PRIMO TURNO'
023900161124     c                   eval      tditur  = '1'
024000161017     c                   when      §dstturno = '2'
024100161017     c                   eval      tdiTURD = 'SECONDO TURNO'
024200161010     c                   endsl
024300161010     C                   ELSE
024400161017     c                   eval      tdiTURD = 'TURNO UNICO'
024500161121     c                   eval      tditur  = 'U'
024600161010     C                   ENDIF
024700170630      *
024800161017     c                   move      dstTPM        tdiTPM
024900161010      * parcel messaggerie
025000161017     c                   if        dsttpm = 'M'
025100161017     c                   movel     'MESSAGGERIA' tdiTPMD
025200161010     c                   else
025300161017     c                   move      'P'           tdiTPM
025400161017     c                   movel     'PARCEL'      tdiTPMD
025500161010     c                   endif
025600161017      * mattina pomeriggio
025700161017     C                   EVAL      TDIFPP = DSTFPP
025800161017     c                   if        dstfpp = 'M'
025900161017     c                   EVAL      TDIFPPD = 'MATTINO'
026000161017     c                   else
026100161017     c                   EVAL      TDIFPPD = 'POMERIGGIO'
026200161017     c                   endif
026300161102      *cerco i valori di ritiro legati alla distinta
026400161102     C                   exsr      Rit_dis
026500161017      *
026600161102      *cerco i valori di consegna legati alla distinta
026700161102     C                   exsr      Con_dis
026800161018      *
026900161018     c                   eval      TDIPKLA   =   DSTPKLA
027000161018     c                   eval      TDIVLUA   =   DSTVLUA
027100161018     c                   eval      TDISTPA   =   DSTSTPA
027200161018     c                   eval      TDINBLA   =   DSTNBLA
027300161018     c                   eval      TDINBLC   =   tdinbla - tdinblnc
027400161209     c*M                 eval      TDINCLC   =   tdincla - tdinclnc
027500161209     c*M                 eval      TDIPKFC   =   tdipkla- tdipkfnc
027600161209     c*M                 eval      TDIVLFC   =   tdivlua - tdivlfnc
027700161201     c                   eval      TDINBaa   =   qdtnbnaf
027800161201     c                   eval      TDINBNRI  =   qdtnbnri
027900170120     c                   eval      TDIPKLR   =   wpkgaf
028000170120     c                   eval      TDIVLUR   =   wvlmaf
028100170120     c                   eval      TDINBLR   =   wormaf
028200170120     c                   eval      TDINBAR   =   wbncaf
028300161104     c                   eval      TDINCLR   =   wcolr
028400161104     c                   eval      TDINCLNR  =   wcolnr
028500161104     c                   eval      TDIORMF   =   wormf
028600161104     c                   eval      TDIORMNF  =   wormnf
028700170120      * imposto numero fermate
028800170120     c                   z-add     1             x
028900170120     c     como70b       lookup    fer(x)                                 80
029000170120     c   80              eval      tdistpr   = x - 1
029100161020     c                   eval      tmp= %char(%timestamp: *iso0)
029200161020     c                   eval      TDIDatora  =   tmp
029300161017      *
029400161017     c                   write     bitdi000
029500161020     c                   endif
029600161017     c                   endsr
029700161102     c*==================================================================*
029800161102      *  Calcola i valori da riportare per distinta ritiri
029900161102     c*==================================================================*
030000161102     c     Rit_dis       begsr
030100161102      * ------------------ *
030200161102     c                   clear                   primo
030300161102     c                   clear                   biddr000
030400161102      * -    Azzera il totalizzatore x  Distinta
030500161102     c                   clear                   wcolr
030600161102     c                   clear                   wcolnr
030700161102     c                   clear                   wormf
030800161102     c                   clear                   wormnf
030900170120     c                   clear                   wpkgaf
031000170120     c                   clear                   wvlmaf
031100170120     c                   clear                   wormaf
031200170120     c                   clear                   wbncaf
031300170120     c                   clear                   fer
031400170120     c                   clear                   como70
031500161102      * ------------------ *
031600161130     c                   clear                   exorfpoe
031700161130     c                   clear                   exorfnsr
031800161130     c                   clear                   exorfnor
031900161130     c                   clear                   exorfnrv
032000161130     c                   clear                   exorffar
032100161130     c                   clear                   exorfcar
032200161130     c                   clear                   exorfdae
032300161130     c                   clear                   exorfore
032400170112     c                   clear                   exorfflo
032500161130     c                   clear                   uorfdae
032600161130     c                   clear                   uorfore
032700161102      * leggo fasi orm prendendo l'ultima per orm/distinta
032800161102      * per totalizzare fatti e non fatti a seconda della fase
032900161102     c     korf          setll     fnorf05l
033000161102     c                   do        *hival
033100161102     c     korf          reade     fnorf05l
033200161107     c                   if        %eof(fnorf05l)
033300161107     c                   leave
033400161107     c                   endif
033500170112      *SPECIFICA DI SICUREZZA per i dati ORM prima della sistemazione gennaio 2017
033600170112      * se non è impostato nella fase 390 il flag fatto o non fatto non consideriamo la fase 390
033700170112     c                   movel     orfflo        dorf01
033800170112     c                   if        orffar = 390 and §orffatto = *blank
033900170112     c                   iter
034000170112     c                   endif
034100161102      * primo orm per distinta forzo impostazione campi rottura
034200161102     c                   if        primo = *blank
034300161102     c                   eval      exORFPOE = ORFPOE
034400161102     c                   eval      exORFNSR = ORFNSR
034500161102     c                   eval      exORFNOR = ORFNOR
034600161102     c                   eval      exORFNRV = ORFNRV
034700161102     c                   eval      exORFfar = ORFfar
034800161102     c                   eval      exORFcar = ORFcar
034900161102     c                   eval      exORFDAE = ORFDAE
035000161102     c                   eval      exORFORE = ORFORE
035100170112     c                   eval      exORFflo = ORFflo
035200161130     c                   eval      uORFDAE = ORFDAE
035300161130     c                   eval      uORFORE = ORFORE
035400161102     c                   move      'X'           primo
035500161102     c                   endif
035600161102      * verifico rottura se cambia orm verifico ultima fase del precedente letto
035700161102      * totalizzo e scrivo il vecchio orm e imposto chiavi per il nuovo
035800161102     c                   if        ORFPOE <> exORFPOE or
035900161102     c                             ORFNSR <> exORFNSR or
036000161102     c                             ORFNOR <> exORFNOR or
036100161102     c                             ORFNRV <> exORFNRV
036200161102     c     korm          chain     fnorm01l
036300161102     c                   if        %found(fnorm01l)
036400161130     c* scrivo il dettaglio
036500161130     c                   exsr      datiorm
036600161107     c                   endif
036700161102     c*imposta nuova chiave rottura
036800161102     c                   eval      exORFPOE = ORFPOE
036900161102     c                   eval      exORFNSR = ORFNSR
037000161102     c                   eval      exORFNOR = ORFNOR
037100161102     c                   eval      exORFNRV = ORFNRV
037200161102     c                   eval      exORFfar = ORFfar
037300161102     c                   eval      exORFcar = ORFcar
037400170112     c                   eval      exORFflo = ORFflo
037500161130     c* salvo la data e ora del ritiro
037600161130     c                   if        orffar = 410 or
037700161130     c                             orffar = 420 or
037800161130     c                             orffar = 500 or
037900161130     c                             orffar = 600
038000161102     c                   eval      exORFDAE = ORFDAE
038100161102     c                   eval      exORFORE = ORFORE
038200161130     c                   end
038300161130     c                   eval      uORFDAE = ORFDAE
038400161130     c                   eval      uORFORE = ORFORE
038500161102     c                   else
038600161102      * ultima fase orm
038700161102     c                   eval      exORFfar = ORFfar
038800161102     c                   eval      exORFcar = ORFcar
038900161201     c* salvo la data e ora del ritiro
039000161201     c                   if        orffar = 410 or
039100161201     c                             orffar = 420 or
039200161201     c                             orffar = 500 or
039300161201     c                             orffar = 600
039400161201     c                   eval      exORFDAE = ORFDAE
039500161201     c                   eval      exORFORE = ORFORE
039600161201     c                   end
039700161201     c                   eval      uORFDAE = ORFDAE
039800161201     c                   eval      uORFORE = ORFORE
039900161104     c                   endif
040000161102     c                   enddo
040100161130     c* se ho letto almeno un record scrivo il dettaglio
040200161130     c                   if        primo = 'X'
040300170123     c     korm          chain     fnorm01l
040400170123     c                   if        %found(fnorm01l)
040500161130     c                   exsr      datiORM
040600170123     c                   endif
040700161130     c                   end
040800161102     c                   endsr
040900161107      *_____________________________________________________________
041000161130     c     datiORM       begsr
041100161107      *_____________________________________________________________
041200170112     c                   movel     exorfflo      dorf01
041300161130     c                   select
041400170112     c* ritirato
041500161130     c                   when      exorffar = 900 or
041600170112     c                             exorffar = 910 or
041700161130     c                             exorffar = 600 or
041800170112     c                             exorffar = 420 or
041900170112     c                             exorffar = 390 and
042000170112     c                             §orffatto = 'S'
042100161130     c                   add       ormncl        wcolr
042200161130     c                   add       1             wormf
042300170120     c                   add       1             wormaf
042400161130     c* non ritirato
042500161130     c                   when      exorffar = 999 or
042600161130     c                             exorffar = 500 or
042700161130     c                             exorffar = 410 or
042800170112     c                             exorffar = 400 or
042900170112     c                             exorffar = 390 and
043000170112     c                             §orffatto = 'N'
043100161130     c                   add       ormncl        wcolnr
043200161130     c                   add       1             wormnf
043300170120     c                   add       1             wormaf
043400161130     c                   endsl
043500161130     c*
043600161130     c                   movel     ormflo        dorm01
043700161130     c                   eval      DDRFGS  =  qdtfgs
043800161130     c                   eval      DDRNDC  =  qdtnfv
043900161130     c                   eval      DDRDfv  =  qdtddc
044000161130     c                   eval      DDRPDR  =  qdtpdr
044100161130     c                   eval      DDRPOE  =  exORFPOE
044200161130     c                   eval      DDRNSR  =  exORFNSR
044300161130     c                   eval      DDRNOR  =  exORFNOR
044400161130     c                   eval      DDRNRV  =  exORFNRV
044500161130     c                   eval      DDRPOR  =  ORmpor
044600161130     c                   eval      DDRCAR  =  exORFcar
044700161130     c* decodifica mancato ritiro
044800161130     c                   if        ddrcar <> ' '
044900161130     c                   clear                   dcmr
045000161130     C                   movel     ddrcar        tbeke1
045100161130     C                   move      'CMR'         tbecod
045200161130     c     ktbe          chain     tntbe01l
045300161130     c                   if        %found(tntbe01l)
045400161130     c                   movel     tbeuni        dcmr
045500161130     c                   endif
045600161130     c                   eval      DDRCARD =  D§cmrDES
045700161130     c                   else
045800161206     c                   if        exorffar = 400
045900161206     c                   eval      DDRCARD =  'NON RITIRATO'
046000161206     c                   else
046100161130     c                   eval      DDRCARD =  'RITIRATO'
046200161206     c                   end
046300161130     c                   end
046400161130     c* imposto data e ora del ritiro o mancato ritiro (420/410 oppure
046500161130     c* 600/500). Nel caso non siano valorizzate
046600161130     c* imposto quelle dell'ultima fase (bollettato)
046700161130     c                   if        exorfdae <> 0
046800161130     c                   eval      DDRDATAR=  exORFDAE
046900161130     c                   else
047000161130     c                   eval      DDRDATAR=  uORFDAE
047100161130     c                   end
047200161130     c                   if        exorfore <> 0
047300161130     c                   movel     exorfore      DDRORAR
047400161130     c                   else
047500161130     c                   movel     uorfore       DDRORAR
047600161130     c                   end
047700161130     c                   eval      ddrCOR  =  'N'
047800161130     c                   eval      ddrmit  =  'N'
047900161130     c                   eval      ddrdes  =  'N'
048000161130     c                   if        ORMCOR  > 0
048100161130     c                   eval      ddrCOR  =  'S'
048200161130     c                   endif
048300161130     c                   if        ORMCrc  > 0
048400161130     c                   eval      ddrdes  =  'S'
048500161130     c                   endif
048600161130     c                   if        ORMcra  > 0
048700161130     c                   eval      ddrmit  =  'S'
048800161130     c                   endif
048900161130      *reperimento ore e alert
049000161130     c                   exsr      OraAlertRit
049100161130      *reperimento giro e descrizione
049200161130     c     kormb         chain     fnorg01l
049300161130     c                   if        %found(fnorg01l)
049400161130     c                   eval      DDRGIRO =  ORgcgi
049500161130     c                   clear                   dgrgflo
049600161130     c     kgrgo         chain     figrg03l
049700161130     c                   if        %found(figrg03l)
049800161130     c                   movel     grgdes        ddrgirod
049900161130     c                   movel     grgflo        dgrgflo
050000161130     c                   endif
050100161130     c                   endif
050200161130     c                   move      §GRGCSTO      ddrSTO
050300161130     c                   move      §GRGFGM       ddrFGM
050400161130      *
050500161130     c                   eval      DDRRSM  =  ormrsr
050600161130     c                   eval      DDRINM  =  orminr
050700161130     c                   eval      DDRLOM  =  ormlor
050800161130     c                   eval      DDRPRM  =  ormprr
050900161130     c                   eval      DDRNZM  =  ormnar
051000161130     c                   eval      DDRNCLR =  ormncl
051100161130     c                   eval      DDRPKLR =  ormpkg
051200161130     c                   eval      DDRVLUR =  ormvlm
051300161130     c                   eval      DDRNBAR =  ormbnc
051400161130     c                   eval      DDRBLC  =  ormblc
051500161130     c                   eval      DDRATT  =  ormatt
051600161130     c                   eval      DDRMTC  =  ormmtc
051700170120     c                   add       orgpkg        wpkgaf
051800170120     c                   add       orgvlm        wvlmaf
051900170120     c                   add       ormbnc        wbncaf
052000170120     c                   exsr      fermate
052100161130      * tipo ORM
052200161130     c                   eval      DDRTOR  =  ORmtor
052300161130     c                   clear                   DDRTORd
052400161130     c                   if        ormtor <> *blank
052500161130     c                   move      'TOR'         tbecod
052600161130     c                   movel(p)  ormtor        tbeke1
052700161130     c     ktbe          chain     tntbe01l
052800161130     c                   if        %found(tntbe01l)
052900161130     c                   movel     tbeuni        ddrtord
053000161130     c                   endif
053100161130     c                   endif
053200161130     c                   clear                   DDrDIV
053300161130     c                   clear                   DDRIMPP
053400161130      * ORM prepagato
053500161130     c                   if        ormtor = 'P'
053600161130      * recupero la bolla
053700161130     c                   eval      ar4trc = 'M'
053800161130     c                   eval      ar4n14 = %editc(ormpoe:'X') +
053900161130     c                                      %editc(ormnsr:'X') +
054000161130     c                                      %editc(ormnor:'X') +
054100161130     c                                      %editc(ormnrv:'X')
054200161130     c     kfiar4        chain     fiar404l
054300161130     c                   if        %found(fiar404l)
054400161130
054500161130      *// Recupero importo e divisa
054600161130     c     kfiar6        chain     fiar601l
054700161130     c                   if        %found(fiar601l)
054800161130     c                   z-add     ar6ift        ddrimpp
054900161130     c                   move      ar6div        ddrdiv
055000161130     c                   endif
055100161130     c                   endif
055200161130     c                   endif
055300161130      * tipo comunicazione ORM
055400161130     c                   eval      DDRTCO  =  ORmtco
055500161130     c                   clear                   DDRTcod
055600161130     c                   if        ormtco <> *blank
055700161130     c                   move      'TCO'         tbecod
055800161130     c                   movel(p)  ormtco        tbeke1
055900161130     c     ktbe          chain     tntbe01l
056000161130     c                   if        %found(tntbe01l)
056100161130     c                   movel     tbeuni        ddrtcod
056200161130     c                   endif
056300161130     c                   endif
056400161130     c                   eval      DDRRICR =  §orsrm
056500161130     c                   eval      DDRCOM   = §orcom
056600161130      *contestuale
056700161130     c                   if        ormnsr <> 0
056800161130     c                   eval      DDRBOLLA = 'S'
056900161130     c                   else
057000161130     c                   eval      DDRBOLLA = 'N'
057100161130     c                   endif
057200161130      *sponda idraulica
057300161130     c                   if        ormspi <> *blank
057400161130     c                   eval      ddrspi =  ORMSPI
057500161130     c                   else
057600161130     c                   eval      DDRspi = 'N'
057700161130     c                   endif
057800161130     c                   eval      tmp= %char(%timestamp: *iso0)
057900161130     c                   eval      DDRDATORA= tmp
058000161130      *
058100161130     c                   write     biddr000
058200161130     c                   clear                   biddr000
058300161107     c                   endsr
058400170120      *___________________________________________________
058500170120     c     fermate       begsr
058600170120      *___________________________________________________
058700170120     c                   z-add     1             x                 3 0
058800170120     c                   movel     ormrsr        como70
058900170120     c                   move      orminr        como70
059000170120     c     como70        lookup    fer                                    80
059100170120     c                   if        not *in80
059200170120     c     como70b       lookup    fer(x)                                 80
059300170120     c   80              movel     como70        fer(x)
059400170120     c                   endif
059500170120
059600170120    1c                   endsr
059700161017     c*==================================================================*
059800161102      *  Calcola i valori da riportare per distinta consegne
059900161017     c*==================================================================*
060000161102     c     Con_dis       begsr
060100161017      * leggo il dettaglio dei conteggi per valorizzare il rekord        .
060200161017      * principale e reperire i dati di consegna
060300161201     c                   z-add     qdtnfv        wndc
060400161213     c                   clear                   stop
060500161213     c                   z-add     1             st
060600161017     c     kftd          setll     fiftd03l
060700161017     c                   do        *hival
060800161017     c     kftd          reade     fiftd03l
060900161017     c                   if        %eof(fiftd03l)
061000161017     c                   leave
061100161017     c                   endif
061200161021     c                   clear                   biddc000
061300161213     c                   z-add     ftdSTp        ddcSTOP
061400161213     c     ddcstop       lookup    stop                                   66
061500161213     c                   if        *in66
061600161213     c                   z-add     0             ddcstp
061700161213     c                   else
061800161213     c                   z-add     1             st
061900161213     c     zero          lookup    stop(st)                               65
062000161213     c                   z-add     ddcstop       stop(st)
062100161213     c                   z-add     1             ddcSTP
062200161213     c                   endif
062300161017      * leggo la bolla in arrivo per le informazioni generali sulla bolla
062400161017      * AFFIDATI
062500161017     c     karb          chain     fnarb01l
062600161017     c                   if        %found(fnarb01l)
062700161201     c                   movel     fnarbds       fnarbdss
062800161017      * testo su tabella 3A se la bolla è di recupero nel caso non deve
062900161017      * sommare le quantità ma come per i solo incassi mantenere solo
063000161017      * i valori economici
063100161019     c                   movel     '3A'          tblcod
063200161017     c                   movel     arbcbo        tblkey
063300161017     c                   clear                   ds3a
063400161017     c     ktab2         chain     tabel00f
063500161017     c                   if        %found(tabel00f) and tblflg = *blank
063600161017     c                   movel     tbluni        ds3a
063700161017     c                   endif
063800161108      * tipo porto
063900161201     c                   if        %subst(§3atb1:1:1) = 'A'
064000161017     c                   exsr      lett_ar6
064100161108     c                   move      'A'           ddctpo
064200161108     c                   eval      ddctpod = 'ASSEGNATO'
064300161108     c                   else
064400161108     c                   move      'F'           ddctpo
064500161108     c                   eval      ddctpod = 'FRANCO'
064600161017     c                   endif
064700161017      * Verifico se la bolla ha dei legami devo leggere l'originale per
064800161017      * reperire le informazioni relative al C/A e al dettaglio colli
064900161201     c* quindi solo se non consegnato (altrimenti l'incasso è nel FTD)
065000161201     c* oppure se parziale (perchè devo leggere il dettaglio colli)
065100161201     c                   eval      $Mamma = §3Abcm
065200161201     c                   if        $mamma = 'S' and
065300161201     c                             (ftdtmc <> ' ' or ftdcmc = 'P')
065400161017     c     kbol          chain     fnlbl01l
065500161201     c                   if        %found(fnlbl01l)
065600161017      * con legame reperisco dati da bolla originale
065700161017     c     karbl         chain     fnarb01l
065800161201     C                   IF        %found(fnarb01l)
065900161201     c                   eval      arbmamds = fnarbds
066000161201     c                   end
066100161201     C* RImposto i dati della FIGLIA
066200161201     C                   movel     fnarbdss      fnarbds
066300161201     c                   endif
066400161201     c                   endif
066500161017      * reperisco dati relativi al contrassegno se esiste
066600161201     c                   if        ftdtmc <> ' '
066700161201     c                   if        §3afca <> 0
066800161017     c                   exsr      lett_ar9
066900161201     c                   end
067000161201     c                   else
067100161201     c                   z-add     ftdcas        ddccas
067200161201     c                   move      ftdvca        ddcdiv
067300161201     c                   end
067400161201      * colli originali
067500161201     c                   If        %Subst(Arbgva:1:1) = 'O'
067600161201     c                   exsr      srnban
067700161201     c                   EndIf
067800161201      * non calcolo pesi volumi e colli se spediz. solo incasso
067900161201     c                   if        ftdsic <>'S' and §3atb1<> 'A3'
068000161201     c                             and §3atb1<> 'F3'
068100161209      * imposto da arb i colli totali della spedizione  anche pesi e volumi bollettati
068200161207     c                   move      arbNCL        ddcNCL
068300161209     c                   move      arbpkb        ddcpkl
068400161209     c                   move      arbvlb        ddcvlu
068500161130     c* conteggio colli peso e volume
068600161201     c                   if        ftdtmc <> *blanks
068700161130     c                   eval      ddcNCLn  =  ftdncl
068800161130     c                   eval      ddcvlun  =  ftdvlu
068900161130     c                   eval      ddcPKLn  =  ftdpkl
069000161130     c                   else
069100161130     c                   eval      ddcNCLc  =  ftdncl
069200161130     c                   eval      ddcvluc  =  ftdvlu
069300161130     c                   eval      ddcPKLc  =  ftdpkl
069400161130     c* sono i non consegnati
069500161130     c                   if        ftdcmc = 'P'
069600161130     c* lettura segnacolli non ancora consegnati
069700161130     c                   exsr      lett_art
069800161130     c                   eval      ddcNCLn  =  wcolpr
069900161130     c                   eval      ddcvlun  =  wvolpr
070000161130     c                   eval      ddcPKLn  =  wpespr
070100161130     c                   end
070200161130     c                   end
070300161130     c                   end
070400161019      * dettaglio
070500161020     c                   z-add     ftdFGS        ddcFGS
070600161020     c                   z-add     ftdNDC        ddcNDC
070700161130     c                   z-add     ftdDDC        ddcDfv
070800161020     c                   z-add     ftdPDR        ddcPDR
070900161020     c                   move      ftdTMC        ddcTMC
071000161019     C                   SELECT
071100161019     c                   WHEN      ftdtmc = 'A'
071200161020     c                   movel(p)  'AVVISO'      ddcTMCD
071300161019     C                   WHEN      FTDTMC = 'G'
071400161020     c                   movel(p)  'GIACENZA'    ddcTMCD
071500161021     C                   WHEN      FTDTMC = 'N'
071600161021     c                   movel(p)  'NON FATTA'   ddcTMCD
071700161019     C                   OTHER
071800161020     C                   CLEAR                   ddcTMCD
071900161019     C                   ENDSL
072000161130     c* decodifico mancata consegna
072100161130     c                   if        ftdcmc <> ' '
072200161020     c                   clear                   ddcCMCD
072300161020     c                   move      ftdCMC        ddcCMC
072400161019     c                   move      '2A'          tblcod
072500161206     c                   movel     ftdcmc        tblkey
072600161019     c     ktab2         chain     tabel00f
072700161019     c                   if        %found(tabel00f)
072800161019     c                   movel     tbluni        ds2a
072900161020     c                   movel     §2ades        ddccmcd
073000161213     c                   if        §2antc <> 'S'
073100161213     c                   eval      ddcntc = 'N'
073200161213     c                   else
073300161213     c                   eval      ddcntc  = 'S'
073400161213     c                   endif
073500161019     c                   endif
073600161130     c                   else
073700161130     c                   eval      ddccmcd = 'CONSEGNATO'
073800161213     c                   eval      ddcntc  = 'S'
073900161130     c                   end
074000161130     C* decodifico riserva
074100161201     c                   if        ftdcrc <> ' '
074200161020     c                   clear                   ddcCrcD
074300161020     c                   move      ftdCRC        ddcCRC
074400161020     c                   move      '2A'          tblcod
074500161206     c                   movel     ftdcrc        tblkey
074600161020     c     ktab2         chain     tabel00f
074700161020     c                   if        %found(tabel00f)
074800161020     c                   movel     tbluni        ds2a
074900161020     c                   movel     §2ades        ddccrcd
075000161020     c                   endif
075100161201     c                   endif
075200161130     c*
075300161020     c                   move      ftdAAS        ddcAAS
075400161020     c                   move      ftdLNP        ddcLNP
075500161020     c                   move      ftdNRS        ddcNRS
075600161020     c                   move      ftdNSP        ddcNSP
075700161020     c                   move      arbRSM        ddcRSM
075800161020     c                   move      arbLOM        ddcLOM
075900161020     c                   move      arbPRM        ddcPRM
076000161020     c                   move      arbNZM        ddcNZM
076100161020     c                   move      arbRSD        ddcRSD
076200161020     c                   move      arbIND        ddcIND
076300161020     c                   move      arbLOD        ddcLOD
076400161020     c                   move      arbPRD        ddcPRD
076500161020     c                   move      arbNZD        ddcNZD
076600161020     c                   move      arblna        ddclna
076700161020     c                   move      ftdSIC        ddcSIC
076800161021     c                   z-add     arbdcm        ddcdcm
076900161021     c                   z-add     arbhmc        ddchcm
077000161021     c                   move      arbTCR        ddcTCR
077100161021     c                   z-add     arbDCR        ddcDCR
077200161021     c                   z-add     arbHCR        ddcHCR
077300161130     c* consegne partivcolari
077400161021     c                   move      arbTC1        ddcTC1
077500161020     c                   move      arbTC2        ddcTC2
077600161019     c                   clear                   ds1p
077700161019     c                   if        arbtc1 <> *blank
077800161019     C                   movel     '1P'          tblcod
077900161019     C                   movel(P)  arbtc1        tblkey
078000161020     C     ktab2         chain     TABEL00f
078100161019     c                   if        %found(tabel00f)
078200161019     C                   movel     TBLUNI        DS1P
078300161108     c                   movel     §1pdes        ddcTC1D
078400161019     c                   endif
078500161019     c                   endif
078600161019     c                   clear                   ds1p
078700161019     c                   if        arbtc2 <> *blank
078800161019     C                   movel(P)  arbtc2        tblkey
078900161020     C     ktab2         chain     TABEL00f
079000161019     c                   if        %found(tabel00f)
079100161019     C                   movel     TBLUNI        DS1P
079200161108     c                   movel     §1pdes        ddcTC2D
079300161019     c                   endif
079400161019     c                   endif
079500161130     c*
079600161020     c                   move      arbGGA        ddcGGA
079700161020     c                   move      arbGMA        ddcGMA
079800161020     c                   move      arbGVA        ddcGVA
079900161108      * tipo servizio
080000161108     c                   clear                   ddctspD
080100161108     C                   clear                   DS5e
080200161020     c                   move      arbTSP        ddcTSP
080300161108     c                   if        arbtsp <> *blank
080400161108     C                   movel     '5E'          tblcod
080500161108     C                   movel(P)  arbtsp        tblkey
080600161108     C     ktab2         chain     TABEL00f
080700161108     c                   if        %found(tabel00f)
080800161108     C                   movel     TBLUNI        DS5e
080900161108     c                   movel     §5EDES        ddctspD
081000161108     c                   endif
081100161108     c                   endif
081200161020      *verifica consegna tassativa
081300161201     c                   if        arbdcr <> 0  or
081400161201     c                             arbtc1 <> ' ' or arbtc2 <> ' '
081500161020     c                   clear                   fnlv80ds
081600161020     c                   z-add     arbAAS        ILV80AAS
081700161020     c                   z-add     arbLNP        ILV80LNP
081800161020     c                   z-add     arbNRS        ILV80NRS
081900161020     c                   z-add     arbNSP        ILV80NSP
082000161020     c                   z-add     arbMGS        ILV80MGS
082100161020     c                   move      arbTCR        ILV80TCR
082200161020     c                   z-add     arbDCR        ILV80DCR
082300161020     c                   z-add     arbHCR        ILV80HCR
082400161020     c                   move      arbTC1        ILV80TC1
082500161020     c                   move      arbTC2        ILV80TC2
082600161020     c                   call      'FNLV80R'
082700161020     c                   parm                    kpjba
082800161020     c                   parm                    fnlv80ds
082900161020     c                   eval      ddcTAS =  olv80tat
083000161201     c                   end
083100161019      *
083200161104     c                   exsr      OraAlertCon
083300161130     c                   endif
083400161201      * determinazione  GIRO
083500161201     c                   clear                   dgrgflo
083600161201     c     karb          chain     fiarg01l
083700161201     c                   if        %found(fiarg01l)
083800161201     c     kgrg          chain     figrg03l
083900161201     c                   if        %found(figrg03l)
084000161201     c                   movel     grgflo        dgrgflo
084100161020     c                   move      argcgi        ddcGIRO
084200161020     c                   movel     grgdes        ddcGIROD
084300161020     c                   move      §GRGCSTO      ddcSTO
084400161020     c                   move      §GRGFGM       ddcFGM
084500161201     c                   endif
084600161201     c                   move      argxlo        ddcXLO
084700161201     c                   move      argyla        ddcYLO
084800161201     c                   endif
084900161020     c                   eval      tmp= %char(%timestamp: *iso0)
085000161020     c                   eval      ddcDatora  =   tmp
085100161020     c                   write     biddc000
085200161201      * prepara i valori della singola bolla da sommare sul record di
085300161201      * riepilogo della distinta
085400161201     c                   exsr      tot_bol
085500161130     c                   enddo
085600161017     c                   endsr
085700161019     c*==================================================================*
085800161104     c     OraAlertCon   begsr
085900161019     c*==================================================================*
086000161103      * ar5 trek EMD campo ALERT
086100161103     c                   move      'EMD'         ar5trd            3
086200161103     c                   clear                   dar5EMD
086300161103     c                   clear                   ddcmail
086400161103     c                   clear                   ddcsms
086500161103     c     kfiar5        chain     fiar501l
086600161103     c                   if        %found(fiar501l)
086700161103     c                   movel     ar5uni        dar5emd
086800161103     c                   if        §AR5eml <> *blank
086900161103     c                   eval      ddcmail =  'S'
087000161103     c                   endif
087100161103     c                   if        §AR5tel <> *blank
087200161103     c                   eval      ddcsms  =  'S'
087300161103     c                   endif
087400161103     c                   endif
087500161021      * ar5 trek RCC ritiro contestuale
087600161201     c                   if        arbgma <> ' '
087700161021     c                   move      'RCC'         ar5trd
087800161021     c                   clear                   dar5rcc
087900161021     c     kfiar5        chain     fiar501l
088000161021     c                   if        %found(fiar501l)
088100161021     c                   movel     ar5uni        dar5Rcc
088200161021     c                   eval      DDCPOER   =   §AR5RPOE
088300161021     c                   eval      DDCNSRR   =   §AR5RNSR
088400161021     c                   eval      DDCNORR   =   §AR5RNOR
088500161021     c                   eval      DDCNRVR   =   §AR5RNRV
088600161021     c                   endif
088700161201     c                   endif
088800161019      * reperimento ora standard
088900161019     C                   clear                   trulorsds
089000161019     C                   clear                   trulor2ds
089100161213     C                   clear                   trulor3ds
089200161213     c     kfiar4x       chain     fiar401l
089300161213     c                   if        %found(fiar401l)
089400161213     c                   eval      oor3loc_n =   xar4not
089500161213     c                   else
089600161213     c                   eval      oor3loc_n =   arblod
089700161213     c                   endif
089800161019     c                   eval      kpjbus = kpjbu
089900161019     c                   eval      IORETLA   =   ' '
090000161019     c                   eval      IOREDTA   =   arbddc
090100161019     c                   eval      IOREFIL   =   arblna
090200161019     c                   eval      IORECAP   =   arbcad
090300161019     c                   eval      IORELOC   =   arblod
090400161019     c                   eval      IORENAR   =   arbnzd
090500161019     c                   eval      IORETSEr  =   'C'
090600161019     c                   eval      IORECONs  =   'S'
090700161216     c                   eval      IOREDDC   =   ftddDC
090800161216     c                   eval      IORENDC   =   ftdnDC
090900161019     c                   eval      IORETFP   =   arbtfp
091000161019     c                   eval      IORETFA   =   arbtfa
091100161216      * verifico quale data passare per calcolo ore basandomi su data arrivo ultimo collo
091200161216     c                   if        ARBDTI = 0 or ARBDTI > ftdDDC
091300161216     c                   eval      IOREDTI   =   arbdam
091400161216     c                   eval      IOREHTI   =   9999
091500161216     c                   else
091600161019     c                   eval      IOREDTI   =   arbdti
091700161019     c                   eval      IOREHTI   =   arbhti
091800161216     c                   endif
091900170112      *
092000170112     c                   if        arbdcr <= ftdddc
092100161019     c                   eval      IOREDCR   =   arbdcr
092200161019     c                   eval      IOREHCR   =   arbhcr
092300161019     c                   eval      IORETCR   =   arbtcr
092400170112     c                   endif
092500161019     c                   eval      IORETSP   =   arbtsp
092600161209     c                   call      'TRULORSR'
092700161209     C                   PARM                    kpjba
092800161209     C                   PARM                    trulorsds
092900161209     C                   PARM                    trulor2ds
093000161019     c                   if        ooreerr <> '1' and oor2stis > 0
093100161020     c                   eval      ddcORAD = oor2stis
093200161020     c                   eval      ddcORAA = oor2stfs
093300161019     c                   endif
093400161019     c                   eval      kpjbu = kpjbus
093500161019     c                   endsr
093600161103     c*==================================================================*
093700161103     c     OraAlertRit   begsr
093800161103     c*==================================================================*
093900161103      * reperimento orari orm
094000161103     c     kfnore        chain     fnore01l
094100161103     c                   if        %found(fnore01l)
094200161103     c                   movel     oredati       doreorari
094300161103     c                   eval      DDRHAD1   =   §OREORAMDA
094400161103     c                   eval      DDRHAA1   =   §OREORAMA
094500161103     c                   eval      DDRHAD2   =   §OREORAPDA
094600161103     c                   eval      DDRHAA2   =   §OREORAPA
094700161103     c                   endif
094800161103      *reperimento ALERT
094900161103     c                   eval      IOR900trc = 'A'
095000161103     c                   eval      IOR900rcdm = 'MA'
095100161103     c                   eval      IOR900rcds = 'S '
095200161103     c                   eval      IOR900poe = ORMpoe
095300161103     c                   eval      IOR900nsr = ORMnsr
095400161103     c                   eval      IOR900nor = ORMnor
095500161103     c                   eval      IOR900nrv = ORMnrv
095600161103     c                   move      'N'           ddrmail
095700161103     c                   move      'N'           ddrsms
095800161103     c                   call      'FIOR900R'
095900161103     C                   PARM                    FIOR900DS
096000161103     C                   IF        OOR900err = *BLANK
096100161103     C                   if        OOR900MAIL <> *blank
096200161103     c                   move      'S'           ddrmail
096300161103     c                   endif
096400161103     C                   if        OOR900sms <> *blank
096500161103     c                   move      'S'           ddrsms
096600161103     c                   endif
096700161104     c                   endif
096800161103      *reperimento ora prevista
096900161103     C                   clear                   trulorsds
097000161103     C                   clear                   trulor2ds
097100161103     c                   eval      kpjbus = kpjbu
097200161104     c                   eval      IOREfil = qdtfgs
097300161103     c                   eval      IOREcap = ORMcar
097400161103     c                   eval      IOREloc = ORMlor
097500161103     c                   eval      IOREnar = ORMnar
097600161103     c                   eval      IOREdta = ORMdar
097700161103     c                   eval      IOREtser = 'R'
097800161209     c                   call      'TRULORSR'
097900161209     C                   PARM                    kpjba
098000161209     C                   PARM                    trulorsds
098100161209     C                   PARM                    trulor2ds
098200161103     c                   if        ooreerr <> '1' and oor2stis > 0
098300161103     c                   eval      ddrORAD = oor2stis
098400161103     c                   eval      ddrORAA = oor2stfs
098500161103     c                   endif
098600161103     c                   eval      kpjbu = kpjbus
098700161103     c                   endsr
098800161017     c*==================================================================*
098900161017     c     lett_art      begsr
099000161017     c*==================================================================*
099100161130     c                   clear                   wcolpr
099200161130     c                   clear                   wvolpr
099300161130     c                   clear                   wpespr
099400161201     c                   if        $mamma <> 'S'
099500161201     c     kbol          setll     fnart01l
099600161201     c                   else
099700161201     c     karbm         setll     fnart01l
099800161201     c                   end
099900161017     c                   do        *hival
100000161201     c                   if        $mamma <> 'S'
100100161201     c     kbol          reade     fnart01l
100200161201     c                   else
100300161201     c     karbm         reade     fnart01l
100400161201     c                   end
100500161017     c                   if        %eof(fnart01l)
100600161017     c                   leave
100700161017     c                   endif
100800161017      * in caso di consegna parziale precedente:
100900161017      * deve scalare il numero dei colli di una o più precedenti consegne
101000161017      * dal totale della bolla per avere il numero reale degli affidati
101100161130     c                   if        artdcm = 0
101200161130     c                   add       1             wcolpr
101300161130     c                   add       artvuc        wvolpr
101400161130     c                   add       artpuc        wpespr
101500161017     c                   endif
101600161130     C*
101700161017     c                   enddo
101800161017     c                   endsr
101900161017     c*==================================================================*
102000161017     c     tot_bol       begsr
102100161017     c*==================================================================*
102200161201     c                   add       1             tdiNBLA
102300161017      * mancata consegna
102400161017     c                   if        ftdtmc <> *blank
102500161018     c                   add       1             tdiNBLNC
102600161207     c                   add       ddcncln       tdiNCLa
102700161207     c                   add       ddcncln       tdiNCLNC
102800161201     c                   add       ddcpkln       tdiPKFNC
102900161201     c                   add       ddcvlun       tdiVLFNC
103000161201     c                   else
103100161201     c                   add       ddcNCLc       tdiNCLA
103200161209     c                   add       ddcNCLc       tdiNCLC
103300161201     c                   add       ddcvluc       TDIvlfC
103400161201     c                   add       ddcPKLc       TDIpkfC
103500161201     c* se parziale aggiunge i non consegnati anche agli affidati
103600161201     c                   if        ftdcmc = 'P'
103700161207     c                   add       ddcNCLn       tdiNCLA
103800161201     c                   add       ddcvlun       TDIPKFC
103900161201     c                   add       ddcPKLn       TDIVLFC
104000170509     c                   add       ddcNCLn       TDINCLNC
104100161201     c                   add       ddcvlun       TDIPKFNC
104200161201     c                   add       ddcPKLn       TDIVLFNC
104300161201     c                   end
104400161201     c                   end
104500161201     c*
104600161017     c                   endsr
104700161018     c*==================================================================*
104800161018     c     lett_ar6      begsr
104900161018     c*==================================================================*
105000161201     c* cerco l'importo della fattura solo se non è stato incassato
105100161201     c                   if        ftdtmc <> ' '
105200161018     c     kbol          chain     fiar601l
105300161018     c                   if        %found(fiar601l)
105400161018      * verifico se non ci sono stati precedenti incassi parziali per
105500161018      * l'assegnato che sto calcolando
105600161020     c                   z-add     ar6ift        ddcift
105700161020     c                   move      ar6div        ddcdiv
105800161018     c                   endif
105900161201     c                   else
106000161201     c                   z-add     ftdifp        ddcift
106100161201     c                   move      ftddiv        ddcdiv
106200161201     c                   end
106300161018     c                   endsr
106400161018     c*==================================================================*
106500161018     c     lett_ar9      begsr
106600161018     c*==================================================================*
106700161201     c* cerco l'importo della fattura solo se non è stato incassato
106800161201     c                   if        $mamma <> 'S'
106900161201     c     kbol          chain     fiar901l
107000161201     c                   else
107100161201     c     karbm         chain     fiar901l
107200161201     c                   end
107300161018     c                   if        %found(fiar901l)
107400161020     c                   z-add     ar9cas        ddccas
107500161020     c                   move      ar9vca        ddcdiv
107600161018     c                   endif
107700161018     c                   endsr
107800161017      *----------------------------------------------------------------
107900161017     C     srnban        BEGSR
108000161017      *----------------------------------------------------------------
108100161017      * Bancali Bartolini
108200161017     c                   Clear                   dAr5BNB
108300161017     C                   move      'BNB'         ar5trd
108400161017     c     kFiar5        Chain     Fiar501l
108500161017     c                   If        %Found(Fiar501l)
108600161017     c                   Movel     Ar5Uni        dAr5BNB
108700161017     c                   EndIf
108800161021     c                   move      §AR5BCOR      ddcNCLO
108900161017     c                   endsr
109000161017      * ?___________________________________________________________________
109100011026     c     *inzsr        begsr
109200161017      * ?___________________________________________________________________
109300020304     c     *entry        plist
109400020304     c                   parm                    kpjba
109500050801
109600020218     C                   time                    w0120            14 0
109700020218     C                   move      w0120         wdat              8 0
109800161013     c                   move      wdat          dataeur
109900161013     c                   move      dataeur       dataiso
110000161013     c                   move      dataiso       wdat
110100170404     c     dataiso       adddur    30:*d         dataiso
110200170404     c                   move      dataiso       wdat30            8 0
110300161013     c                   if        kpjbu <> *blank
110400161013     c                   movel     kpjbu         param
110500161125     c                   move      'X'           datadis           1
110600161013     c                   else
110700170421     c                   move      wdat          datap
110800170421     c                   move      wdat          datap2
110900161013     c                   endif
111000161017      *
111100161017     c     kdst          klist
111200161017     c                   kfld                    catf
111300161017     c                   kfld                    qdtnfv
111400161017     c                   kfld                    qdtfgs
111500161020     c                   z-add     4             catf              1 0
111600161018     c     kftd          klist
111700161018     c                   kfld                    qdtpdr
111800161018     c                   kfld                    wtsr              1
111900161018     c                   kfld                    wndc              7 0
112000161018     c                   kfld                    qdtddc
112100161018     c                   move      'C'           wtsr
112200161018      *
112300161018     c     karb          klist
112400161018     c                   kfld                    ftdaas
112500161018     c                   kfld                    ftdlnp
112600161018     c                   kfld                    ftdnrs
112700161018     c                   kfld                    ftdnsp
112800161018      *
112900161018     c     karbl         klist
113000161018     c                   kfld                    lblaao
113100161018     c                   kfld                    lbllpo
113200161018     c                   kfld                    lblnro
113300161018     c                   kfld                    lblnso
113400161018      *
113500161018     c     kbol          klist
113600161018     c                   kfld                    arbaas
113700161018     c                   kfld                    arblnp
113800161018     c                   kfld                    arbnrs
113900161018     c                   kfld                    arbnsp
114000161201     c     karbm         klist
114100161201     c                   kfld                    marbAAS
114200161201     c                   kfld                    marbLNP
114300161201     c                   kfld                    marbNRS
114400161201     c                   kfld                    marbNSP
114500161018      *
114600161018     C     Kfiar5        KLIST
114700161018     C                   KFLD                    arbaas
114800161018     C                   KFLD                    arblnp
114900161018     C                   KFLD                    arbnrs
115000161018     C                   KFLD                    arbnsp
115100161018     C                   KFLD                    ar5trd
115200161213     C     Kfiar4x       KLIST
115300161213     C                   KFLD                    arbaas
115400161213     C                   KFLD                    arblnp
115500161213     C                   KFLD                    arbnrs
115600161213     C                   KFLD                    arbnsp
115700161213     C                   KFLD                    trcx              1
115800161213     C                   move      'X'           trcx
115900161018      *
116000161018     c     ktab2         klist
116100161018     c                   kfld                    tblkut
116200161018     c                   kfld                    tblcod
116300161018     c                   kfld                    tblkey
116400161018     c                   z-add     1             tblkut
116500161104     c     ktbe          klist
116600161104     c                   kfld                    tbecod
116700161104     c                   kfld                    tbeke1
116800161018     c     kgrg          klist
116900161018     c                   kfld                    argpoc
117000161018     c                   kfld                    argcgi
117100161013
117200161103     c     kgrgO         klist
117300161103     c                   kfld                    ORGPOCGI
117400161103     c                   kfld                    Orgcgi
117500161103
117600161102     c     korf          klist
117700161102     c                   kfld                    QDTFGS
117800161102     c                   kfld                    QDTNFV
117900161102
118000161102     c     korm          klist
118100161102     c                   kfld                    exORFPOE
118200161102     c                   kfld                    exORFNSR
118300161102     c                   kfld                    exORFNOR
118400161102     c                   kfld                    exORFNRV
118500161104     c     kormb         klist
118600161104     c                   kfld                    ORmPOE
118700161104     c                   kfld                    ORmNSR
118800161104     c                   kfld                    ORmNOR
118900161104     c                   kfld                    ORmNRV
119000161103     c     kfnore        klist
119100161103     c                   kfld                    ORmPOE
119200161103     c                   kfld                    ORmNSR
119300161103     c                   kfld                    ORmNOR
119400161103     c                   kfld                    ORmNRV
119500161103     c                   kfld                    ORetrc
119600161103     c                   eval      OREtrc = 'O'
119700161103     c     kfiar4        klist
119800161103     c                   kfld                    ar4trc
119900161103     c                   kfld                    ar4n14
120000161103
120100161103
120200161103     c     kfiar6        klist
120300161103     c                   kfld                    ar4aas
120400161103     c                   kfld                    ar4lnp
120500161103     c                   kfld                    ar4nrs
120600161103     c                   kfld                    ar4nsp
120700161102
120800030630      *profilo di filiale estraggo solo quelli relativi al P.O.
120900030630     C                   CLEAR                   Tibs36Ds
121000030630     C                   EVAL      I36ute = knmus
121100030630     C                   CALL      'TIBS36R'
121200030630     C                   PARM                    tibs36ds
121300161201     c                   endsr
