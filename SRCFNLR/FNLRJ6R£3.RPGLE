000100030718     H DECEDIT('0,') DATEDIT(*DMY.)  OPTION(*NODEBUGIO)
000200140205     H* FNLRJ6R *----------------------------------------------------*
000300140207     H* - Verifica colli spuntati di una spedizine
000400000000     H*--------------------------------------------------------------*
000500120417     FAZCLN01L  IF   E           K DISK
000600070313     FFNART01L  IF   E           K DISK
000700070316     FFNARB01L  IF   E           K DISK
000800140205     FFNbrv07l  IF   E           K DISK
000900070313     FFNfvv08L  IF   E           K DISK
001000070313     F                                     RENAME(FNfvv000:FNfvv008)
001100070313     FFNDCD02L  IF   E           K DISK
001200070313     FFNDCT01L  IF   E           K DISK
001300070313     F                                     RENAME(FNDCT000:FNDCT001)
001400030722     F**
001500120417     D K31             S              1    DIM(31)                              SKI DI COMODO
001600140207     D MSG             S             78    DIM(3) CTDATA PERRCD(1)
001700070313     d
001800051123      * ds di passaggio dati
001900140205     D fnlrJ6ds      E DS
002000070313     d
002100070313     D FNLV53DS      E DS
002200140210     D FNLV55DS      E DS
002300070313     D DSBS02        E DS                  EXTNAME(TIBS02DS)
002400070313     D DCCH          E DS
002500070313     D DTAD          E DS
002600070314     D KPJBA         E DS
002700070313     d
002800070316     D DSRECARB      E DS                  extname(fnarb00f)
002900070327     D DSparARB      E DS                  extname(fnarb00f) prefix(PAR:3)
003000120417     d
003100120417     D                 DS
003200120417     D  WAA                    1      4  0
003300120417     D  WMM                    5      6  0
003400120417     D  WGG                    7      8  0
003500120417     D  WDATAdal               1      8  0
003600070316     d
003700120417     d wfest           s              1
003800120417     d wmanca          s              1
003900070316     d wdamarr         s              1
004000120417     d kZERO           s              3  0
004100120417     d ArrNoFEST       s              8  0
004200120417     D WDATAal         s              8  0
004300120417     D knpg4           S                   LIKE(fvvnpg) inz(4)
004400070313     D WARTDAM         S                   LIKE(arbdam)
004500070420     D kdata           S                   LIKE(arbdam)
004600070313     d Dataiso         s               d   datfmt(*iso)
004700140205     d Datasys         s               d   datfmt(*iso) inz(*sys)
004800900518     C***************************************************************
004900000000     C     *ENTRY        PLIST
005000140205     C                   PARM                    fnlrJ6ds
005100070327     C                   PARM                    dspararb
005200940223     C*---------------------------------------------------------------*
005300140205     C* TIPO LANCIO, ilrj6TLA "C" -           CHIUSO CON LR
005400140205     C* TIPO LANCIO, ilrj6TLA "L" - ELABORO E CHIUDO CON LR
005500140205     C* TIPO LANCIO, ilrj6TLA " " - ELABORO E CHIUDO IN RETRN
005600941216     C*
005700140205    1C     ilrj6TLA      IFNE      'C'
005800140205     c                   clear                   olrj6err
005900140205     c                   clear                   olrj6msg
006000140207     c                   clear                   olrj6cSPU
006100140207     c                   clear                   olrj6cNOSP
006200140205     c                   clear                   olrj6cDCM
006300140205     c                   clear                   olrj6cDa1G
006400140205     c                   clear                   olrj6cCA
006500140205     c                   clear                   olrj6ncl
006600110131     c                   clear                   fnlv53ds
006700051123     c
006800070313     C* SE NON PASSATo record spedizione, lo chaino
006900070829     C                   IF        (%parms>=2 and parnsp=0) or
007000070315     c                              %parms=1
007100070313     c
007200070313     c     karb          chain     fnarb01l
007300070313     c                   if        not %found(fnarb01l)
007400070313     c* restituisco errore al chiamante ed esco
007500140205     c                   eval      olrj6msg=msg(1)
007600140205     c                   eval      olrj6err='E'
007700070313     c                   goto      fine
007800070313     c                   endif
007900070327     c                   else
008000070327     c                   movel     dspararb      dsrecarb
008100070313     c                   endif
008200140207     c*
008300140207     c* se non passato ambito di ricerca ERRORE
008400140207     c                   if        ilrj6tric<>'P' and ilrJ6tric<>'A' and
008500140207     c                             ilrj6tric<>'T'
008600140207     c                   eval      olrj6msg=msg(2)
008700140207     c                   eval      olrj6err='E'
008800140207     c                   goto      fine
008900140207     c                   endif
009000070829     c
009100140205     c* Se non passata data imposto udate
009200140205     c                   if        ilrj6drf=0
009300140205     c                   movel     datasys       ilrj6drf
009400140205     c                   endif
009500140205     c
009600140207     c* Se non passata la linea di spunta:
009700140207     c* 1) per transito errore
009800140205     c                   if        ilrj6lin=0
009900140207     c                   select
010000140207     c                   when      ilrj6tric= 'T'
010100140207     c                   eval      olrj6msg=msg(3)
010200140207     c                   eval      olrj6err='E'
010300140207     c                   goto      fine
010400140207     c
010500140207     c                   when      ilrj6tric= 'A'
010600140205     c                   eval      ilrj6lin=arblna
010700140207     c
010800140207     c                   when      ilrj6tric= 'P'
010900140207     c                   eval      ilrj6lin=arbtfp
011000140207     c
011100140207     c                   endsl
011200140207     c                   endif
011300140210     c
011400140210     c* carico la filiale CAPOFILA
011500140210     c                   clear                   fnlv55ds
011600140211     C                   EVAL      D55TPT='6'
011700140210     c                   eval      d55lin=ilrj6lin
011800140210     c                   eval      d55drf=ilrj6drf
011900140210     c                   call      'FNLV55R'
012000140210     c                   parm                    fnlv55ds
012100070313     c
012200140205     c                   eval      olrj6ncl=arbncl
012300140210     c
012400140205     c
012500140207     c* Verifico stato colli di una spedizione
012600070316     c                   exsr      CTRColli
012700070314     c
012800070314     c     fine          TAG
012900070314     c
013000070314     c                   endif
013100051123     c
013200140205     C     ilrj6tla      IFNE      ' '
013300051123     C                   SETON                                        LR
013400140205     c                   clear                   fnlv53ds
013500140205     c                   eval      d53tla='C'
013600140205     c                   call      'FNLV53R'
013700140205     c                   parm                    fnlv53ds
013800140210     c                   clear                   fnlv55ds
013900140210     c                   eval      d55tla='C'
014000140210     c                   call      'FNLV55R'
014100140210     c                   parm                    fnlv55ds
014200140205     c                   clear                   dsbs02
014300140205     c                   eval      t02tla='C'
014400140205     c                   call      'TIBS02R'
014401150615     c                   parm                    kpjba
014500140205     c                   parm                    dsbs02
014600140205     c
014700051123     C                   ELSE
014800051123     C                   SETON                                        RT
014900051123     C                   ENDIF
015000991206     C**************************************************************************
015100051123     C     *INZSR        BEGSR
015200051123     c
015300070313     c     kARB          Klist
015400140205     c                   Kfld                    ilrj6Aas
015500140205     c                   Kfld                    ilrj6Lnp
015600140205     c                   Kfld                    ilrj6nrs
015700140205     c                   Kfld                    ilrj6nsp
015800120417     C     Kfvv8D        KLIST
015900140205     C                   KFLD                    ilrj6lin
016000120417     C                   KFLD                    knpg4
016100120417     C                   KFLD                    kdata
016200070314     C     KART          KLIST
016300070314     C                   KFLD                    ARTFLS
016400070314     C                   KFLD                    ARTLNA
016500070314     C                   KFLD                    ARTNRS
016600070314     C                   KFLD                    ARTNSC
016700070314     C     KDCT          KLIST
016800070314     C                   KFLD                    DCDAAC
016900070314     C                   KFLD                    DCDFIL
017000070314     C                   KFLD                    DCDNCA
017100140207     C     KCLNA         KLIST
017200120417     C                   KFLD                    KZERO
017300140207     C                   KFLD                    ilrj6lin
017400120417     C                   KFLD                    WAA               4 0
017500120417     C                   KFLD                    WMM               2 0
017600140207     C     KCLNPT        KLIST
017700140207     C                   KFLD                    ilrj6lin
017800140207     C                   KFLD                    KZERO
017900140207     C                   KFLD                    WAA               4 0
018000140207     C                   KFLD                    WMM               2 0
018100051123     c
018200070313     c                   endsr
018300070313     C**************************************************************************
018400070316     C     CTRColli      BEGSR
018500140207     c                   eval      olrj6fall='S'
018600070316     c
018700070313     C     KARB          SETLL     FNART01L
018800070316     C     KARB          READE     FNART01L
018900070313     C*
019000140205    1C                   DOW       not %eof(fnart01l)
019100140205    c
019200140205     c* Leggo le spunte per vedere se spuntato alla filiale indicata
019300140205     c                   exsr      LeggiSpu
019400070316     c
019500070316     c* Controllo se c'e' C.A.
019600070316     c                   EXSR      CTRCA
019700140205     c
019800120417     c* Verifico se la data di arrivo <> dalla data elaborazione, cade in giorno festivo
019900120417     c                   exsr      CTRFest
020000070316     c
020100140205    2c                   select
020200070316     c* collo mancante/reso/distrutto da c.a.
020300140205    2c                   when      wmanca<>' '
020400140205     c                   add       1             olrj6cca
020500140207     c                   eval      olrj6fall=' '
020600070316     c
020700140205     c* collo consegnato in data <= data riferimento
020800140205    2c                   when      artdcm>0 and artdcm<=ilrj6drf
020900140205     c                   add       1             olrj6cdcm
021000070316     c
021100140207     c* Collo NON spuntato se spuntato dopo data riferimento
021200140205    2c                   when      wartdam>0 and wartdam>ilrj6drf
021300140207     c                   add       1             olrj6cnosp
021400140207     c                   eval      olrj6fall=' '
021500070830     c
021600140207     c* collo SPUNTATO
021700140205    2c                   when      wartdam>0
021800070830     c
021900140207     c                   add       1             olrj6cspu
022000140207     c* ...di cui arrivati da almeno un giorno rispetto la data di riferimento
022100140205     c                   if        ArrNoFest < ilrj6drf
022200140205     c                   add       1             olrj6cda1g
022300070316     c                   endif
022400070316     c
022500140207     c* altrimenti è NON spuntato
022600140205    2c                   other
022700140207     c                   add       1             olrj6cnosp
022800140207     c                   eval      olrj6fall=' '
022900140205    2c                   ENDSL
023000070316     c
023100070316     C     KARB          READE     FNART01L
023200140205    1c                   enddo
023300070316     c
023400140205     C                   ENDSR
023500070313     C**************************************************************************
023600070313     C* CALCOLO DATA PREV.CONS/CONSEGNABILITA'/ORE RITARDO DI 1 BOLLA
023700070313     C**************************************************************************
023800070313     C     CTRCA         BEGSR
023900070313     C                   CLEAR                   WMANCA
024000070313     C**
024100070313     C     KART          CHAIN     FNDCD02L                           33
024200070313    2C     *IN33         IFEQ      *OFF
024300070726     C     DCDATB        ANDEQ     ' '
024400070313     C     KDCT          CHAIN     FNDCT01L                           33
024500070726      *
024600070726      * se dctdt2 > 0 la c.a. non è valida x la filiale
024700070726     c                   If        Not *In33 and dctdt2 > 0
024800070726     c                   Eval      *In33 = *On
024900070726     c                   EndIf
025000070313     C*
025100070313    3C     *IN33         IFEQ      *OFF
025200070313     C     DCTDCH        ANDEQ     DCDDCH
025300070313     C     DCDDCH        OREQ      0
025400070313     C* TESTO PRIMA LA CAUSALE DI CHIUSURA PER VEDERE SE MI INDICA LA
025500070313     C*  PRESENZA DELLA MERCE
025600070316     c                   clear                   dcch
025700070316     c                   if        dctcch<>*blanks
025800070313     C                   CLEAR                   DSBS02
025900070313     C                   MOVEL     'C'           T02MOD
026000070313     C                   MOVEL     'CCH'         T02COD
026100070313     C                   MOVEL     DCTCCH        T02KE1
026200070313     C                   CALL      'TIBS02R'
026300070313     C                   PARM                    KPJBA
026400070313     C                   PARM                    DSBS02
026500070313     C**
026600070313    4C     T02ERR        IFEQ      ' '
026700070313     C                   MOVEL     T02UNI        DCCH
026800070313     C                   ELSE
026900070313     C                   MOVEL     'S'           §CCHIMA
027000070313    4C                   ENDIF
027100070316     c                   endif
027200070313     C*
027300070313    4C     §CCHIMA       IFEQ      ' '
027400070313     C                   CLEAR                   DSBS02
027500070313     C                   MOVEL     'C'           T02MOD
027600070313     C                   MOVEL     'TAD'         T02COD
027700070313     C                   MOVEL     DCTTAD        T02KE1
027800070313     C                   CALL      'TIBS02R'
027900070313     C                   PARM                    KPJBA
028000070313     C                   PARM                    DSBS02
028100070313     C**
028200070313    5C     T02ERR        IFEQ      ' '
028300070313     C                   MOVEL     T02UNI        DTAD
028400070313     C                   ELSE
028500070313     C                   MOVEL     'M'           §TADRAGR
028600070313    5C                   ENDIF
028700070313     C*
028800070313     C* AVARIA RESA O DISTRUTTA
028900070313    5C     §TADRAGR      IFEQ      'V'
029000070313    6C     DCTDSD        IFEQ      'D'
029100070313     C     DCTDSD        OREQ      'R'
029200070316     C                   MOVEL     'V'           WMANCA
029300070313    6C                   ENDIF
029400070313    5C                   ENDIF
029500070313     C* MANCANZA
029600070313    5C     §TADRAGR      IFEQ      'M'
029700070316     C                   MOVEL     'M'           WMANCA
029800070313    5C                   ENDIF
029900070313     C*
030000070313    4C                   ENDIF
030100070313    3C                   ENDIF
030200070313    2C                   ENDIF
030300070313     C*
030400070313     C                   ENDSR
030500120417     C**************************************************************************
030600120417     C* Controllo se arrivo in giorno festivo
030700120417     C**************************************************************************
030800120417     C     CTRfest       BEGSR
030900140205     c                   eval      wdataal=ilrj6drf
031000120417     c                   eval      wfest='F'
031100120417     c                   eval      wdatadal=wartdam
031200120417     c
031300140207    0c                   if        wdatadal>0 and wdatadal<99999999
031400120417    1c                   dow       wfest='F' and wdataDAL<wdataAL
031500120417     c                   eval      kdata=wdataDAL
031600140207     c* ambito arrivi --> calendario arrivi
031700140207     c                   if        ilrj6tric='A'
031800140207     C     KCLNA         CHAIN     AZCLN01L
031900140207     c                   else
032000140207     c* ambito par/transito --> calendario partenza
032100140207     C     KCLNPT        CHAIN     AZCLN01L
032200140207     c                   endif
032300140207     c
032400120417    2c                   if        not %found(azcln01l)
032500120417     c                   clear                   clnpom
032600120417     c                   clear                   clnmat
032700120417    2c                   endif
032800120417     c
032900120417     c                   movea     clnpom        k31
033000120417     c                   movel     k31(wgg)      wfest
033100120417    2c                   if        wfest<>'F'
033200120417     c                   movea     clnmat        k31
033300120417     c                   movel     k31(wgg)      wfest
033400120417    2c                   endif
033500140207     c
033600120417     c* se festivo, verifico se nella data arrivo ci sono distinte
033700120417    2c                   if        wfest='F'
033800120417     c
033900140207   2ac                   if        ilrj6tric='A' and  brvlna=ilrj6lin
034000120417     c     kfvv8d        setll     fnfvv08l
034100130610     c     kfvv8d        reade     fnfvv08l
034200130610    3c                   dow       not %eof(fnfvv08l)
034300130610     c
034400130610     c                   if        fvvatb=' '
034500130610     c                   leave
034600130610     c                   else
034700130610     c     kfvv8d        reade     fnfvv08l
034800130610     c                   endif
034900130610     c
035000130610     c                   enddo
035100140207     c                   endif
035200130610     c
035300120417     c* se trovo distinte non considero più come giorno festivo
035400140207     c                   if        ilrj6tric='A' and brvlna=ilrj6lin and
035500140207     c                             not %eof(fnfvv08l)
035600120417     c                   clear                   wfest
035700120417   x3c                   else
035800120417     c                   movel     wdatadal      dataiso
035900120417     c                   adddur    1:*d          dataiso
036000120417     c                   movel     dataiso       wdatadal
036100120417    3c                   endif
036200120417    2c                   endif
036300120417    c
036400120417    1c                   enddo
036500120417    0c                   endif
036600120417     c
036700120417     c* imposto data arrivo calcolata in un campo di comodo
036800120417     c                   eval      ArrNoFest=wdatadal
036900120417     C                   ENDSR
037000140205     C**************************************************************************
037100140205     C* Leggo spunte per vedere se collo spuntato alla linea indicata
037200140205     C**************************************************************************
037300140205     C     Leggispu      BEGSR
037400140205     c                   eval      wartdam=99999999
037500140205     c
037600140205     c     kart          setll     fnbrv07l
037700140205     c     kart          reade     fnbrv07l
037800140205     c                   dow       not %eof(fnbrv07l)
037900140205     c
038000140211     c* Elaboro solo le bolle della linea richeista o del sui "gestore"
038100140211     c* ed escludo le spunte autogenerate
038200140211     c                   if        (brvpes=ilrj6lin or brvpes=d55tfa) and
038300140211     c                             brvdcs>0
038400140211     c
038500140211     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLIo
038600140205     c                   clear                   fnlv53ds
038700140205     C                   z-add     brvnFV        D53NFV
038800140205     C                   MOVEL     brvNPG        D53NPG
038900140205     C                   MOVEL     brvFGS        D53FGS
039000140205     C                   CALL      'FNLV53R'
039100140205     C                   PARM                    fnlv53ds
039200140205     c
039300140210     c* Se la bolla è locale ovvero lnp=lna, escludo le spunte categoria partenza
039400140210     c                   select
039500140210     c                   when      ilrj6tric='A' and
039600140210     c                              arblnp=arblna and ilrj6lin=arblna
039700140205     c                             and (brvnpg=1 or brvnpg=5 or d53spg='P')
039800140210     c                   when      ilrj6tric='A' and
039900140210     c                             arbtfp=arbtfa and ilrj6lin=arbtfa
040000140210     c                             and (brvnpg=1 or brvnpg=5 or d53spg='P')
040100140210     c                   when      ilrj6tric='A' and
040200140210     c                             arbtfp=arbtfa and d55tfa=arbtfa
040300140210     c                             and (brvnpg=1 or brvnpg=5 or d53spg='P')
040400140210     c                   other
040500140205     c
040600140205     c* memorizzo data più recente di spunta
040700140205     c                   if        d53err<>' '
040800140205     c                   eval      d53dfv=brvdcs
040900140205     c                   endif
041000140205     c
041100140205    3c                   if        d53dfv<wartdam
041200140205     c                   eval      wartdam=d53dfv
041300140205     c                   endif
041400140205
041500140210     c                   endsl
041600140205     c                   endif
041700140205     c
041800140205     c     kart          reade     fnbrv07l
041900140205     c                   enddo
042000140205     c                   ENDSR
042100070313**
042200070313Numero di spedizione inesistente
042300140207Non impostato ambito di ricerca
042400140207Non impostata la filiale di spunta per ambito TRANSITO
