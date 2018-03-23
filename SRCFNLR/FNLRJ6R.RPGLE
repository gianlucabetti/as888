000100030718     H DECEDIT('0,') DATEDIT(*DMY.)  OPTION(*NODEBUGIO)
000200140205     H* FNLRJ6R *----------------------------------------------------*
000300140207     H* - Verifica colli spuntati di una spedizine
000400000000     H*--------------------------------------------------------------*
000500120417     FAZCLN01L  IF   E           K DISK
000600070313     FFNART01L  IF   E           K DISK
000700160516     FFnblt01L  IF   E           K DISK    prefix(ART:3)
000800070316     FFNARB01L  IF   E           K DISK
000900160516     FFNblp01L  IF   E           K DISK    prefix(ARB:3)
001000140205     FFNbrv07l  IF   E           K DISK
001100070313     FFNfvv08L  IF   E           K DISK
001200070313     F                                     RENAME(FNfvv000:FNfvv008)
001300070313     FFNDCD02L  IF   E           K DISK
001400070313     FFNDCT01L  IF   E           K DISK
001500070313     F                                     RENAME(FNDCT000:FNDCT001)
001600030722     F**
001700120417     D K31             S              1    DIM(31)                              SKI DI COMODO
001800140207     D MSG             S             78    DIM(3) CTDATA PERRCD(1)
001900070313     d
002000051123      * ds di passaggio dati
002100140205     D fnlrJ6ds      E DS
002200070313     d
002300070313     D FNLV53DS      E DS
002400140210     D FNLV55DS      E DS
002500070313     D DSBS02        E DS                  EXTNAME(TIBS02DS)
002600070313     D DCCH          E DS
002700070313     D DTAD          E DS
002800070314     D KPJBA         E DS
002900070313     d
003000070316     D DSRECARB      E DS                  extname(fnarb00f)
003100070327     D DSparARB      E DS                  extname(fnarb00f) prefix(PAR:3)
003200120417     d
003300120417     D                 DS
003400120417     D  WAA                    1      4  0
003500120417     D  WMM                    5      6  0
003600120417     D  WGG                    7      8  0
003700120417     D  WDATAdal               1      8  0
003800070316     d
003900120417     d wfest           s              1
004000120417     d wmanca          s              1
004100070316     d wdamarr         s              1
004200120417     d kZERO           s              3  0
004300120417     d ArrNoFEST       s              8  0
004400120417     D WDATAal         s              8  0
004500160516     D Wfile           s              1
004600120417     D knpg4           S                   LIKE(fvvnpg) inz(4)
004700070313     D WARTDAM         S                   LIKE(arbdam)
004800070420     D kdata           S                   LIKE(arbdam)
004900070313     d Dataiso         s               d   datfmt(*iso)
005000140205     d Datasys         s               d   datfmt(*iso) inz(*sys)
005100900518     C***************************************************************
005200000000     C     *ENTRY        PLIST
005300140205     C                   PARM                    fnlrJ6ds
005400070327     C                   PARM                    dspararb
005500940223     C*---------------------------------------------------------------*
005600140205     C* TIPO LANCIO, ilrj6TLA "C" -           CHIUSO CON LR
005700140205     C* TIPO LANCIO, ilrj6TLA "L" - ELABORO E CHIUDO CON LR
005800140205     C* TIPO LANCIO, ilrj6TLA " " - ELABORO E CHIUDO IN RETRN
005900941216     C*
006000140205    1C     ilrj6TLA      IFNE      'C'
006100140205     c                   clear                   olrj6err
006200140205     c                   clear                   olrj6msg
006300140207     c                   clear                   olrj6cSPU
006400140207     c                   clear                   olrj6cNOSP
006500140205     c                   clear                   olrj6cDCM
006600140205     c                   clear                   olrj6cDa1G
006700140205     c                   clear                   olrj6cCA
006800140205     c                   clear                   olrj6ncl
006900110131     c                   clear                   fnlv53ds
007000051123     c
007100070313     C* SE NON PASSATo record spedizione, lo chaino
007200160516     c                   clear                   wfile
007300070829     C                   IF        (%parms>=2 and parnsp=0) or
007400070315     c                              %parms=1
007500070313     c
007600070313     c     karb          chain     fnarb01l
007700070313     c                   if        not %found(fnarb01l)
007800160516     c     karb          chain     fnblp01l
007900160516    3c                   if        %found(fnblp01l)
008000160516     c                   eval      wfile='P'
008100160516    3c                   endif
008200160516   x2c                   else
008300160516     c                   eval      wfile='A'
008400160516    2c                   endif
008500070313     c* restituisco errore al chiamante ed esco
008600160516    2c                   if        wfile=*blanks
008700140205     c                   eval      olrj6msg=msg(1)
008800140205     c                   eval      olrj6err='E'
008900070313     c                   goto      fine
009000070313     c                   endif
009100070327     c                   else
009200070327     c                   movel     dspararb      dsrecarb
009300160516     c                   eval      wfile='A'
009400070313     c                   endif
009500140207     c*
009600140207     c* se non passato ambito di ricerca ERRORE
009700140207     c                   if        ilrj6tric<>'P' and ilrJ6tric<>'A' and
009800140207     c                             ilrj6tric<>'T'
009900140207     c                   eval      olrj6msg=msg(2)
010000140207     c                   eval      olrj6err='E'
010100140207     c                   goto      fine
010200140207     c                   endif
010300070829     c
010400140205     c* Se non passata data imposto udate
010500140205     c                   if        ilrj6drf=0
010600140205     c                   movel     datasys       ilrj6drf
010700140205     c                   endif
010800140205     c
010900140207     c* Se non passata la linea di spunta:
011000140207     c* 1) per transito errore
011100140205     c                   if        ilrj6lin=0
011200140207     c                   select
011300140207     c                   when      ilrj6tric= 'T'
011400140207     c                   eval      olrj6msg=msg(3)
011500140207     c                   eval      olrj6err='E'
011600140207     c                   goto      fine
011700140207     c
011800140207     c                   when      ilrj6tric= 'A'
011900140205     c                   eval      ilrj6lin=arblna
012000140207     c
012100140207     c                   when      ilrj6tric= 'P'
012200140207     c                   eval      ilrj6lin=arbtfp
012300140207     c
012400140207     c                   endsl
012500140207     c                   endif
012600140210     c
012700140210     c* carico la filiale CAPOFILA
012800140210     c                   clear                   fnlv55ds
012900140211     C                   EVAL      D55TPT='6'
013000140210     c                   eval      d55lin=ilrj6lin
013100140210     c                   eval      d55drf=ilrj6drf
013200140210     c                   call      'FNLV55R'
013300140210     c                   parm                    fnlv55ds
013400070313     c
013500140205     c                   eval      olrj6ncl=arbncl
013600140210     c
013700140205     c
013800140207     c* Verifico stato colli di una spedizione
013900070316     c                   exsr      CTRColli
014000070314     c
014100070314     c     fine          TAG
014200070314     c
014300070314     c                   endif
014400051123     c
014500140205     C     ilrj6tla      IFNE      ' '
014600051123     C                   SETON                                        LR
014700140205     c                   clear                   fnlv53ds
014800140205     c                   eval      d53tla='C'
014900140205     c                   call      'FNLV53R'
015000140205     c                   parm                    fnlv53ds
015100140210     c                   clear                   fnlv55ds
015200140210     c                   eval      d55tla='C'
015300140210     c                   call      'FNLV55R'
015400140210     c                   parm                    fnlv55ds
015500140205     c                   clear                   dsbs02
015600140205     c                   eval      t02tla='C'
015700140205     c                   call      'TIBS02R'
015800150615     c                   parm                    kpjba
015900140205     c                   parm                    dsbs02
016000140205     c
016100051123     C                   ELSE
016200051123     C                   SETON                                        RT
016300051123     C                   ENDIF
016400991206     C**************************************************************************
016500051123     C     *INZSR        BEGSR
016600051123     c
016700070313     c     kARB          Klist
016800140205     c                   Kfld                    ilrj6Aas
016900140205     c                   Kfld                    ilrj6Lnp
017000140205     c                   Kfld                    ilrj6nrs
017100140205     c                   Kfld                    ilrj6nsp
017200120417     C     Kfvv8D        KLIST
017300140205     C                   KFLD                    ilrj6lin
017400120417     C                   KFLD                    knpg4
017500120417     C                   KFLD                    kdata
017600070314     C     KART          KLIST
017700070314     C                   KFLD                    ARTFLS
017800070314     C                   KFLD                    ARTLNA
017900070314     C                   KFLD                    ARTNRS
018000070314     C                   KFLD                    ARTNSC
018100070314     C     KDCT          KLIST
018200070314     C                   KFLD                    DCDAAC
018300070314     C                   KFLD                    DCDFIL
018400070314     C                   KFLD                    DCDNCA
018500140207     C     KCLNA         KLIST
018600120417     C                   KFLD                    KZERO
018700140207     C                   KFLD                    ilrj6lin
018800120417     C                   KFLD                    WAA               4 0
018900120417     C                   KFLD                    WMM               2 0
019000140207     C     KCLNPT        KLIST
019100140207     C                   KFLD                    ilrj6lin
019200140207     C                   KFLD                    KZERO
019300140207     C                   KFLD                    WAA               4 0
019400140207     C                   KFLD                    WMM               2 0
019500051123     c
019600070313     c                   endsr
019700070313     C**************************************************************************
019800070316     C     CTRColli      BEGSR
019900140207     c                   eval      olrj6fall='S'
020000070316     c
020100160516     c                   if        wfile='A'
020200070313     C     KARB          SETLL     FNART01L
020300070316     C     KARB          READE     FNART01L
020400160516     c                   else
020500160516     C     KARB          SETLL     FnblT01L
020600160516     C     KARB          READE     FnblT01L
020700160516     c                   endif
020800070313     C*
020900160516    1C****               DOW       not %eof(fnart01l)
021000160516    1C                   DOW       not %eof
021100140205    c
021200140205     c* Leggo le spunte per vedere se spuntato alla filiale indicata
021300140205     c                   exsr      LeggiSpu
021400070316     c
021500070316     c* Controllo se c'e' C.A.
021600070316     c                   EXSR      CTRCA
021700140205     c
021800120417     c* Verifico se la data di arrivo <> dalla data elaborazione, cade in giorno festivo
021900120417     c                   exsr      CTRFest
022000070316     c
022100140205    2c                   select
022200070316     c* collo mancante/reso/distrutto da c.a.
022300140205    2c                   when      wmanca<>' '
022400140205     c                   add       1             olrj6cca
022500140207     c                   eval      olrj6fall=' '
022600070316     c
022700140205     c* collo consegnato in data <= data riferimento
022800140205    2c                   when      artdcm>0 and artdcm<=ilrj6drf
022900170111     c                             and (wfile='A' or arbcca<>'7')
023000140205     c                   add       1             olrj6cdcm
023100070316     c
023200140207     c* Collo NON spuntato se spuntato dopo data riferimento
023300140205    2c                   when      wartdam>0 and wartdam>ilrj6drf
023400140207     c                   add       1             olrj6cnosp
023500140207     c                   eval      olrj6fall=' '
023600070830     c
023700140207     c* collo SPUNTATO
023800140205    2c                   when      wartdam>0
023900070830     c
024000140207     c                   add       1             olrj6cspu
024100140207     c* ...di cui arrivati da almeno un giorno rispetto la data di riferimento
024200140205     c                   if        ArrNoFest < ilrj6drf
024300140205     c                   add       1             olrj6cda1g
024400070316     c                   endif
024500070316     c
024600140207     c* altrimenti è NON spuntato
024700140205    2c                   other
024800140207     c                   add       1             olrj6cnosp
024900140207     c                   eval      olrj6fall=' '
025000140205    2c                   ENDSL
025100070316     c
025200160516     c                   if        wfile='A'
025300070316     C     KARB          READE     FNART01L
025400160516     c                   else
025500160516     C     KARB          READE     Fnblt01L
025600160516     c                   endif
025700140205    1c                   enddo
025800070316     c
025900140205     C                   ENDSR
026000070313     C**************************************************************************
026100070313     C* CALCOLO DATA PREV.CONS/CONSEGNABILITA'/ORE RITARDO DI 1 BOLLA
026200070313     C**************************************************************************
026300070313     C     CTRCA         BEGSR
026400070313     C                   CLEAR                   WMANCA
026500070313     C**
026600070313     C     KART          CHAIN     FNDCD02L                           33
026700070313    2C     *IN33         IFEQ      *OFF
026800070726     C     DCDATB        ANDEQ     ' '
026900070313     C     KDCT          CHAIN     FNDCT01L                           33
027000070726      *
027100070726      * se dctdt2 > 0 la c.a. non è valida x la filiale
027200070726     c                   If        Not *In33 and dctdt2 > 0
027300070726     c                   Eval      *In33 = *On
027400070726     c                   EndIf
027500070313     C*
027600070313    3C     *IN33         IFEQ      *OFF
027700070313     C     DCTDCH        ANDEQ     DCDDCH
027800070313     C     DCDDCH        OREQ      0
027900070313     C* TESTO PRIMA LA CAUSALE DI CHIUSURA PER VEDERE SE MI INDICA LA
028000070313     C*  PRESENZA DELLA MERCE
028100070316     c                   clear                   dcch
028200070316     c                   if        dctcch<>*blanks
028300070313     C                   CLEAR                   DSBS02
028400070313     C                   MOVEL     'C'           T02MOD
028500070313     C                   MOVEL     'CCH'         T02COD
028600070313     C                   MOVEL     DCTCCH        T02KE1
028700070313     C                   CALL      'TIBS02R'
028800070313     C                   PARM                    KPJBA
028900070313     C                   PARM                    DSBS02
029000070313     C**
029100070313    4C     T02ERR        IFEQ      ' '
029200070313     C                   MOVEL     T02UNI        DCCH
029300070313     C                   ELSE
029400070313     C                   MOVEL     'S'           §CCHIMA
029500070313    4C                   ENDIF
029600070316     c                   endif
029700070313     C*
029800070313    4C     §CCHIMA       IFEQ      ' '
029900070313     C                   CLEAR                   DSBS02
030000070313     C                   MOVEL     'C'           T02MOD
030100070313     C                   MOVEL     'TAD'         T02COD
030200070313     C                   MOVEL     DCTTAD        T02KE1
030300070313     C                   CALL      'TIBS02R'
030400070313     C                   PARM                    KPJBA
030500070313     C                   PARM                    DSBS02
030600070313     C**
030700070313    5C     T02ERR        IFEQ      ' '
030800070313     C                   MOVEL     T02UNI        DTAD
030900070313     C                   ELSE
031000070313     C                   MOVEL     'M'           §TADRAGR
031100070313    5C                   ENDIF
031200070313     C*
031300070313     C* AVARIA RESA O DISTRUTTA
031400070313    5C     §TADRAGR      IFEQ      'V'
031500070313    6C     DCTDSD        IFEQ      'D'
031600070313     C     DCTDSD        OREQ      'R'
031700070316     C                   MOVEL     'V'           WMANCA
031800070313    6C                   ENDIF
031900070313    5C                   ENDIF
032000070313     C* MANCANZA
032100070313    5C     §TADRAGR      IFEQ      'M'
032200070316     C                   MOVEL     'M'           WMANCA
032300070313    5C                   ENDIF
032400070313     C*
032500070313    4C                   ENDIF
032600070313    3C                   ENDIF
032700070313    2C                   ENDIF
032800070313     C*
032900070313     C                   ENDSR
033000120417     C**************************************************************************
033100120417     C* Controllo se arrivo in giorno festivo
033200120417     C**************************************************************************
033300120417     C     CTRfest       BEGSR
033400140205     c                   eval      wdataal=ilrj6drf
033500120417     c                   eval      wfest='F'
033600120417     c                   eval      wdatadal=wartdam
033700120417     c
033800140207    0c                   if        wdatadal>0 and wdatadal<99999999
033900120417    1c                   dow       wfest='F' and wdataDAL<wdataAL
034000120417     c                   eval      kdata=wdataDAL
034100140207     c* ambito arrivi --> calendario arrivi
034200140207     c                   if        ilrj6tric='A'
034300140207     C     KCLNA         CHAIN     AZCLN01L
034400140207     c                   else
034500140207     c* ambito par/transito --> calendario partenza
034600140207     C     KCLNPT        CHAIN     AZCLN01L
034700140207     c                   endif
034800140207     c
034900120417    2c                   if        not %found(azcln01l)
035000120417     c                   clear                   clnpom
035100120417     c                   clear                   clnmat
035200120417    2c                   endif
035300120417     c
035400120417     c                   movea     clnpom        k31
035500120417     c                   movel     k31(wgg)      wfest
035600120417    2c                   if        wfest<>'F'
035700120417     c                   movea     clnmat        k31
035800120417     c                   movel     k31(wgg)      wfest
035900120417    2c                   endif
036000140207     c
036100120417     c* se festivo, verifico se nella data arrivo ci sono distinte
036200120417    2c                   if        wfest='F'
036300120417     c
036400140207   2ac                   if        ilrj6tric='A' and  brvlna=ilrj6lin
036500120417     c     kfvv8d        setll     fnfvv08l
036600130610     c     kfvv8d        reade     fnfvv08l
036700130610    3c                   dow       not %eof(fnfvv08l)
036800130610     c
036900130610     c                   if        fvvatb=' '
037000130610     c                   leave
037100130610     c                   else
037200130610     c     kfvv8d        reade     fnfvv08l
037300130610     c                   endif
037400130610     c
037500130610     c                   enddo
037600140207     c                   endif
037700130610     c
037800120417     c* se trovo distinte non considero più come giorno festivo
037900140207     c                   if        ilrj6tric='A' and brvlna=ilrj6lin and
038000140207     c                             not %eof(fnfvv08l)
038100120417     c                   clear                   wfest
038200120417   x3c                   else
038300120417     c                   movel     wdatadal      dataiso
038400120417     c                   adddur    1:*d          dataiso
038500120417     c                   movel     dataiso       wdatadal
038600120417    3c                   endif
038700120417    2c                   endif
038800120417    c
038900120417    1c                   enddo
039000120417    0c                   endif
039100120417     c
039200120417     c* imposto data arrivo calcolata in un campo di comodo
039300120417     c                   eval      ArrNoFest=wdatadal
039400120417     C                   ENDSR
039500140205     C**************************************************************************
039600140205     C* Leggo spunte per vedere se collo spuntato alla linea indicata
039700140205     C**************************************************************************
039800140205     C     Leggispu      BEGSR
039900140205     c                   eval      wartdam=99999999
040000140205     c
040100140205     c     kart          setll     fnbrv07l
040200140205     c     kart          reade     fnbrv07l
040300140205     c                   dow       not %eof(fnbrv07l)
040400140205     c
040500140211     c* Elaboro solo le bolle della linea richeista o del sui "gestore"
040600140211     c* ed escludo le spunte autogenerate
040700140211     c                   if        (brvpes=ilrj6lin or brvpes=d55tfa) and
040800140211     c                             brvdcs>0
040900140211     c
041000140211     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLIo
041100140205     c                   clear                   fnlv53ds
041200140205     C                   z-add     brvnFV        D53NFV
041300140205     C                   MOVEL     brvNPG        D53NPG
041400140205     C                   MOVEL     brvFGS        D53FGS
041500140205     C                   CALL      'FNLV53R'
041600140205     C                   PARM                    fnlv53ds
041700140205     c
041800140210     c* Se la bolla è locale ovvero lnp=lna, escludo le spunte categoria partenza
041900140210     c                   select
042000140210     c                   when      ilrj6tric='A' and
042100140210     c                              arblnp=arblna and ilrj6lin=arblna
042200140205     c                             and (brvnpg=1 or brvnpg=5 or d53spg='P')
042300140210     c                   when      ilrj6tric='A' and
042400140210     c                             arbtfp=arbtfa and ilrj6lin=arbtfa
042500140210     c                             and (brvnpg=1 or brvnpg=5 or d53spg='P')
042600140210     c                   when      ilrj6tric='A' and
042700140210     c                             arbtfp=arbtfa and d55tfa=arbtfa
042800140210     c                             and (brvnpg=1 or brvnpg=5 or d53spg='P')
042900140210     c                   other
043000140205     c
043100140205     c* memorizzo data più recente di spunta
043200140205     c                   if        d53err<>' '
043300140205     c                   eval      d53dfv=brvdcs
043400140205     c                   endif
043500140205     c
043600140205    3c                   if        d53dfv<wartdam
043700140205     c                   eval      wartdam=d53dfv
043800140205     c                   endif
043900140205
044000140210     c                   endsl
044100140205     c                   endif
044200140205     c
044300140205     c     kart          reade     fnbrv07l
044400140205     c                   enddo
044500140205     c                   ENDSR
044600070313**
044700070313Numero di spedizione inesistente
044800140207Non impostato ambito di ricerca
044900140207Non impostata la filiale di spunta per ambito TRANSITO
