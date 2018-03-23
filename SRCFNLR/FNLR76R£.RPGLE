000100030718     H DECEDIT('0,') DATEDIT(*DMY.)  OPTION(*NODEBUGIO)
000200070313     H* FNLR76R *----------------------------------------------------*
000300070316     H* - Verifica arrivo merce di una spedizione
000400000000     H*--------------------------------------------------------------*
000500120417     FAZCLN01L  IF   E           K DISK
000600070313     FFNART01L  IF   E           K DISK
000700070316     FFNARB01L  IF   E           K DISK
000800070313     FFNfvv01L  IF   E           K DISK
000900070313     FFNfvv08L  IF   E           K DISK
001000070313     F                                     RENAME(FNfvv000:FNfvv008)
001100070313     FFNDCD02L  IF   E           K DISK
001200070313     FFNDCT01L  IF   E           K DISK
001300070313     F                                     RENAME(FNDCT000:FNDCT001)
001400030722     F**
001500120417     D K31             S              1    DIM(31)                              SKI DI COMODO
001600070313     D MSG             S             78    DIM(1) CTDATA PERRCD(1)
001700070313     d
001800051123      * ds di passaggio dati
001900070313     D fnlr76ds      E DS
002000070313     d
002100070313     D FNLV53DS      E DS
002200070313     D DSBS02        E DS                  EXTNAME(TIBS02DS)
002300070313     D DCCH          E DS
002400070313     D DTAD          E DS
002500070314     D KPJBA         E DS
002600070313     d
002700070316     D DSRECARB      E DS                  extname(fnarb00f)
002800070327     D DSparARB      E DS                  extname(fnarb00f) prefix(PAR:3)
002900120417     d
003000120417     D                 DS
003100120417     D  WAA                    1      4  0
003200120417     D  WMM                    5      6  0
003300120417     D  WGG                    7      8  0
003400120417     D  WDATAdal               1      8  0
003500070316     d
003600120417     d wfest           s              1
003700120417     d wmanca          s              1
003800070316     d wdamarr         s              1
003900070829     d CerArrivo       s              1
004000070829     d ArrEffet        s             12  0
004100120417     d kZERO           s              3  0
004200120417     d ArrNoFEST       s              8  0
004300120417     D WDATAal         s              8  0
004400070313     D knpg            S                   LIKE(fvvnpg) inz(2)
004500120417     D knpg4           S                   LIKE(fvvnpg) inz(4)
004600070313     D WARTDAM         S                   LIKE(arbdam)
004700070420     D kdata           S                   LIKE(arbdam)
004800070313     d Dataiso         s               d   datfmt(*iso)
004900900518     C***************************************************************
005000000000     C     *ENTRY        PLIST
005100070313     C                   PARM                    fnlr76ds
005200070327     C                   PARM                    dspararb
005300070829     C                   PARM                    dsSoglia         12 0
005400940223     C*---------------------------------------------------------------*
005500070313     C* TIPO LANCIO, ilr76TLA "C" -           CHIUSO CON LR
005600070313     C* TIPO LANCIO, ilr76TLA "L" - ELABORO E CHIUDO CON LR
005700070313     C* TIPO LANCIO, ilr76TLA " " - ELABORO E CHIUDO IN RETRN
005800941216     C*
005900070313    1C     ilr76TLA      IFNE      'C'
006000070313     c                   clear                   olr76err
006100070313     c                   clear                   olr76msg
006200070316     c                   clear                   olr76cNOCO
006300070316     c                   clear                   olr76cDACO
006400070316     c                   clear                   olr76cDCM
006500070316     c                   clear                   olr76cDC1G
006600070316     c                   clear                   olr76cCA
006700110131     c                   clear                   fnlv53ds
006800051123     c
006900070313     C* SE NON PASSATo record spedizione, lo chaino
007000070829     C                   IF        (%parms>=2 and parnsp=0) or
007100070315     c                              %parms=1
007200070313     c
007300070313     c     karb          chain     fnarb01l
007400070313     c                   if        not %found(fnarb01l)
007500070313     c* restituisco errore al chiamante ed esco
007600070313     c                   eval      olr76msg=msg(1)
007700070313     c                   eval      olr76err='E'
007800070313     c                   goto      fine
007900070313     c                   endif
008000070327     c                   else
008100070327     c                   movel     dspararb      dsrecarb
008200070313     c                   endif
008300070829     c
008400070829     c* Verifico se passata data e ora soglia per delterminare
008500070829     c*  i colli consegnabili
008600070829     c                   if        %parms=3 and dsSoglia>0
008700070829     c                   movel     dsSoglia      SogliaData        8 0
008800070829     c                   move      dsSoglia      Soglia           12 0
008900070829     c                   else
009000070829     c                   clear                   sogliaData
009100070829     c                   clear                   Soglia
009200070829     c                   endif
009300070313     c
009400070316     c* Verifico stato colli di uan spedizione
009500070316     c                   exsr      CTRColli
009600070314     c
009700070314     c     fine          TAG
009800070314     c
009900070314     c                   endif
010000051123     c
010100070313     C     Ilr76tla      IFNE      ' '
010200051123     C                   SETON                                        LR
010300051123     C                   ELSE
010400051123     C                   SETON                                        RT
010500051123     C                   ENDIF
010600991206     C**************************************************************************
010700051123     C     *INZSR        BEGSR
010800051123     c
010900070313     c     kARB          Klist
011000070313     c                   Kfld                    ilr76Aas
011100070313     c                   Kfld                    ilr76Lnp
011200070313     c                   Kfld                    ilr76nrs
011300070313     c                   Kfld                    ilr76nsp
011400070313     C     Kfvv          KLIST
011500070313     C                   KFLD                    D53NPA
011600070313     C                   KFLD                    D53NFA
011700070313     C                   KFLD                    D53LAI
011800070313     C     Kfvv8         KLIST
011900070313     C                   KFLD                    arbtfa
012000070313     C                   KFLD                    knpg
012100070420     C                   KFLD                    kdata
012200070313     C     Kfvv81        KLIST
012300070313     C                   KFLD                    arbtfa
012400070313     C                   KFLD                    knpg
012500120417     C     Kfvv8D        KLIST
012600120417     C                   KFLD                    arblna
012700120417     C                   KFLD                    knpg4
012800120417     C                   KFLD                    kdata
012900070314     C     KART          KLIST
013000070314     C                   KFLD                    ARTFLS
013100070314     C                   KFLD                    ARTLNA
013200070314     C                   KFLD                    ARTNRS
013300070314     C                   KFLD                    ARTNSC
013400070314     C     KDCT          KLIST
013500070314     C                   KFLD                    DCDAAC
013600070314     C                   KFLD                    DCDFIL
013700070314     C                   KFLD                    DCDNCA
013800120417     C     KCLN          KLIST
013900120417     C                   KFLD                    KZERO
014000120417     C                   KFLD                    arblna
014100120417     C                   KFLD                    WAA               4 0
014200120417     C                   KFLD                    WMM               2 0
014300051123     c
014400070313     c                   endsr
014500070313     C**************************************************************************
014600070316     C     CTRColli      BEGSR
014700070316     c
014800070313     C     KARB          SETLL     FNART01L
014900070316     C     KARB          READE     FNART01L
015000070313     C*
015100070316    2C                   DOW       not %eof(fnart01l)
015200070313     c                   clear                   Wartdam
015300070316     c                   clear                   Wdamarr
015400070829     c                   clear                   CerArrivo
015500070829     c
015600070829     c* Se richiesta data e ora soglia, devo sempre richiamare abbinamento
015700070829     c*  o se richiesta abilitazione in base alle spunte partenza
015800070829     c                   if        (artdam=0  and artdfv>0  and
015900070829     c                              ilr76sppar='S')
016000070829     c                   eval      CerArrivo='S'
016100070829     c                   endif
016200070829     c
016300070829     c                   if        SogliaData>*zeros or CerArrivo='S'
016400070829     c                   exsr      Abbinam
016500070829     c                   endif
016600070829     c
016700070313    3c                   if        artdam>0
016800070313     c                   eval      Wartdam=artdam
016900070316     c                   eval      Wdamarr='A'
017000070313   x3c                   else
017100070313     c
017200070313     c* Memorizzo la data di arrivo o l'ipotetico arrivo se collo partito
017300070313     c* su FGV abbinato di ida non ancora chiuso: come data di arrivo
017400070313     c*  prendo la data foglio arrivi, solo se richiesto
017500070829    4c***                if        artdam=0  and artdfv>0  and ilr76sppar='S'
017600070313     c* Se si tratta di bolla LOCALE, non esiste abbinamento quindi
017700070313     c* cerco di desumere la data del foglio arrivi per vedere se è
017800070313     c*  chiuso o perto
017900070829    5c***                if        arbtfp=arbtfa
018000070829     c***                exsr      BOLLALOC
018100070829   X5c***                else
018200070829     C***
018300070313     C* CONTROLLO SE LA BOLLA ARRIVI E RELATIVA A FOGLIO VIAGGIO ABBINATO
018400070829     C***                CLEAR                   fnlv53ds
018500070829     C***                MOVEL     'A'           D53TFO
018600070829     C***                MOVE      artNFV        D53NFV
018700070829     C***                MOVE      arbtfp        D53FGS
018800070829     C***                MOVEL     'B'           D53ABB
018900070829     C***                MOVE      ARBLNA        D53LNA
019000070829     C***                MOVE      ARBTFA        D53TFA
019100070829     C***                CALL      'FNLV53R'
019200070829     C***                PARM                    fnlv53ds
019300070829     c***
019400070829    6c***                if        d53err=*blanks and d53nfa>0
019500070829     c**verifico se fogli arrivi di abbinamento già chiuso
019600070829     C***  KFVV          CHAIN     FNFVV000
019700070829     C***                IF        %FOUND(FNFVV01L)
019800070419     C***                IF        FVVFCF=' '
019900070829     C***                EVAL      Wartdam=D53DFA
020000070419     C***                ENDIF
020100070829     C***                ENDIF
020200070829    6c***                endif
020300070829    5c***                endif
020400070829     c***
020500070829    4c***                endif
020600070313    3c                   endif
020700070316     c
020800070316     c* Controllo se c'e' C.A.
020900070316     c                   EXSR      CTRCA
021000120417     c* Verifico se la data di arrivo <> dalla data elaborazione, cade in giorno festivo
021100120417     c                   exsr      CTRFest
021200070316     c
021300070316     c                   select
021400070316     c* collo mancante/reso/distrutto da c.a.
021500070316     c                   when      wmanca<>' '
021600070316     c                   add       1             olr76cca
021700070316     c
021800070316     c* collo consegnato in data <= data consegnabilità
021900070316     c                   when      artdcm>0 and artdcm<=ilr76dco
022000070316     c                   add       1             olr76cdcm
022100070316     c
022200070830     c* Collo non consegnabile se arrivato dopo data consegnabilità
022300070830     c                   when      wartdam>0 and wartdam>ilr76dco
022400070830     c                   add       1             olr76cnoco
022500070830     c
022600070830     c***                when      SogliaData=0 and
022700070830     c***                          wartdam>0 and wartdam<=ilr76dco
022800070316     c* collo consegnabile
022900070830     c* Senza Soglia o con soglia ma senza data di partenza
023000070830     c*       (collo arrivato non partito)
023100080118     c* o con soglia ma errore nel richiamare FNLV53R
023200070830     c                   when      wartdam>0 and
023300070831     c                             (SogliaData=0 or artdfv=0 or
023400080118     c                              arbtfp=arbtfa or d53err<>' ' )
023500070830     c
023600070316     c                   add       1             olr76cdaco
023700070326     c* ...di cui arrivati da almeno un giorno rispetto all'arrivo 1 collo
023800070326     c*    o alla data consegnabilità)
023900120417     c****               if        wdamarr='A' and (wartdam< ilr76dco  or
024000120417     c                   if        wdamarr='A' and (ArrNoFest < ilr76dco  or
024100070326     c                             wartdam>arbdaM AND ARBDAM>0)
024200070316     c                   add       1             olr76cdc1g
024300070316     c                   endif
024400070829     c
024500070829     c* Con   Soglia
024600070829     c                   when      SogliaData>0 and
024700070829     c                             d53err=' ' and d53nfa>0  and
024800070829     c                             d53dfa<=ilr76dco and ArrEffet<=Soglia
024900070830     c                             and wartdam>0
025000070829     c                   add       1             olr76cdaco
025100070829     c* ...di cui arrivati da almeno un giorno rispetto all'arrivo 1 collo
025200070829     c*    o alla data consegnabilità)
025300120417     c***                if        wdamarr='A' and (wartdam< ilr76dco  or
025400120417     c                   if        wdamarr='A' and (ArrNoFest < ilr76dco  or
025500070829     c                             wartdam>arbdaM AND ARBDAM>0)
025600070829     c                   add       1             olr76cdc1g
025700070829     c                   endif
025800070316     c
025900070316     c* altrimenti è non consegnabile
026000070316     c                   other
026100070316     c                   add       1             olr76cnoco
026200070316     c                   ENDSL
026300070316     c
026400070316     C     KARB          READE     FNART01L
026500070316    2c                   enddo
026600070316     c
026700070316     C     ENDCtrco      ENDSR
026800070313     C**************************************************************************
026900070313     C* Per bolla locale desumo la data dell'arrivo bolla
027000070313     C**************************************************************************
027100070313     c     BOLLALOC      begsr
027200070420     c* Prendo data partenza collo se presente
027300070831     c                   clear                   fnlv53ds
027400070831     c                   if        artdfv>0
027500070420     c                   eval      kdata=artdfv
027600070313     c     kfvv8         SETGT     FNFVV08L
027700070313     c     kfvv81        READe     FNFVV08L
027800070420     C                   IF        not %eoF(FNFVV08L) AND FVVDFV>=ARtdfv
027900070419     C****               IF        FVVFCF=' '
028000070313     C                   EVAL      Wartdam=FVVDFV
028100070419     C****               ENDIF
028200070313     C*
028300070313     C                   ELSE
028400070831     C* SE NON TROVATO  SEMPRE CON DATA  + 1
028500070420     c     *iso          movel     artdfv        dataiso
028600070313     c                   adddur    1:*d          dataiso
028700070313     c                   movel     dataiso       wartdam
028800070829     c                   endif
028900070831     c
029000070831     c                   endif
029100070829     c                   ENDSR
029200070829     C**************************************************************************
029300070829     c* Reperisco dati di abbinamento
029400070829     C**************************************************************************
029500070829     c     ABBINAM       BEGSR
029600070829     c                   clear                   ArrEffet
029700070829    5c                   if        arbtfp=arbtfa
029800070829     c* ai fini della soglia i locali vanno sempre considerati
029900070829     c                   if        CerArrivo='S'
030000070829     c                   exsr      BOLLALOC
030100070829     c                   endif
030200070829     c* Nei locali ArrEffet lo lascio=0  perchè deve essere sempre
030300070829     c*  consegnabile
030400070829     c
030500070829   X5c                   else
030600070829     C
030700070829     C* CONTROLLO SE LA BOLLA ARRIVI E RELATIVA A FOGLIO VIAGGIO ABBINATO
030800070829     C                   CLEAR                   fnlv53ds
030900070829     C                   MOVEL     'A'           D53TFO
031000070829     C                   MOVE      artNFV        D53NFV
031100070829     C                   MOVE      arbtfp        D53FGS
031200070829     C                   MOVEL     'B'           D53ABB
031300070829     C                   MOVE      ARBLNA        D53LNA
031400120223     c* Lascio il campo vuoto per farlo ricalcolare in modo che se è cambiato
031500120223     c*  il terminal non ci siano problemi  sulla abbinabilità del foglio
031600120223     C****               MOVE      ARBTFA        D53TFA
031700070829     C                   CALL      'FNLV53R'
031800070829     C                   PARM                    fnlv53ds
031900070829     c*
032000070829    6c                   if        d53err=*blanks and d53nfa>0
032100070829    7c                   if        cerArrivo='S'
032200070829     c* verifico se fogli arrivi di abbinamento già chiuso
032300070829     C     KFVV          CHAIN     FNFVV000
032400070829     C                   IF        %FOUND(FNFVV01L)
032500070829     C***                IF        FVVFCF=' '
032600070829     C                   EVAL      Wartdam=D53DFA
032700070829     C***                ENDIF
032800070829     C                   ENDIF
032900070829    7c                   endif
033000070829     c
033100070829     c                   movel     d53aed        ArrEffet
033200070829     c                   move      d53aeh        ArrEffet
033300070829    6c                   endif
033400070829    5c                   endif
033500070829     c                   ENDSR
033600070313     C**************************************************************************
033700070313     C* CALCOLO DATA PREV.CONS/CONSEGNABILITA'/ORE RITARDO DI 1 BOLLA
033800070313     C**************************************************************************
033900070313     C     CTRCA         BEGSR
034000070313     C                   CLEAR                   WMANCA
034100070313     C**
034200070313     C     KART          CHAIN     FNDCD02L                           33
034300070313    2C     *IN33         IFEQ      *OFF
034400070726     C     DCDATB        ANDEQ     ' '
034500070313     C     KDCT          CHAIN     FNDCT01L                           33
034600070726      *
034700070726      * se dctdt2 > 0 la c.a. non è valida x la filiale
034800070726     c                   If        Not *In33 and dctdt2 > 0
034900070726     c                   Eval      *In33 = *On
035000070726     c                   EndIf
035100070313     C*
035200070313    3C     *IN33         IFEQ      *OFF
035300070313     C     DCTDCH        ANDEQ     DCDDCH
035400070313     C     DCDDCH        OREQ      0
035500070313     C* TESTO PRIMA LA CAUSALE DI CHIUSURA PER VEDERE SE MI INDICA LA
035600070313     C*  PRESENZA DELLA MERCE
035700070316     c                   clear                   dcch
035800070316     c                   if        dctcch<>*blanks
035900070313     C                   CLEAR                   DSBS02
036000070313     C                   MOVEL     'C'           T02MOD
036100070313     C                   MOVEL     'CCH'         T02COD
036200070313     C                   MOVEL     DCTCCH        T02KE1
036300070313     C                   CALL      'TIBS02R'
036400070313     C                   PARM                    KPJBA
036500070313     C                   PARM                    DSBS02
036600070313     C**
036700070313    4C     T02ERR        IFEQ      ' '
036800070313     C                   MOVEL     T02UNI        DCCH
036900070313     C                   ELSE
037000070313     C                   MOVEL     'S'           §CCHIMA
037100070313    4C                   ENDIF
037200070316     c                   endif
037300070313     C*
037400070313    4C     §CCHIMA       IFEQ      ' '
037500070313     C                   CLEAR                   DSBS02
037600070313     C                   MOVEL     'C'           T02MOD
037700070313     C                   MOVEL     'TAD'         T02COD
037800070313     C                   MOVEL     DCTTAD        T02KE1
037900070313     C                   CALL      'TIBS02R'
038000070313     C                   PARM                    KPJBA
038100070313     C                   PARM                    DSBS02
038200070313     C**
038300070313    5C     T02ERR        IFEQ      ' '
038400070313     C                   MOVEL     T02UNI        DTAD
038500070313     C                   ELSE
038600070313     C                   MOVEL     'M'           §TADRAGR
038700070313    5C                   ENDIF
038800070313     C*
038900070313     C* AVARIA RESA O DISTRUTTA
039000070313    5C     §TADRAGR      IFEQ      'V'
039100070313    6C     DCTDSD        IFEQ      'D'
039200070313     C     DCTDSD        OREQ      'R'
039300070316     C                   MOVEL     'V'           WMANCA
039400070313    6C                   ENDIF
039500070313    5C                   ENDIF
039600070313     C* MANCANZA
039700070313    5C     §TADRAGR      IFEQ      'M'
039800070316     C                   MOVEL     'M'           WMANCA
039900070313    5C                   ENDIF
040000070313     C*
040100070313    4C                   ENDIF
040200070313    3C                   ENDIF
040300070313    2C                   ENDIF
040400070313     C*
040500070313     C                   ENDSR
040600120417     C**************************************************************************
040700120417     C* Controllo se arrivo in giorno festivo
040800120417     C**************************************************************************
040900120417     C     CTRfest       BEGSR
041000120417     c                   eval      wdataal=ilr76dco
041100120417     c                   eval      wfest='F'
041200120417     c                   eval      wdatadal=wartdam
041300120417     c
041400120417    0c                   if        wdatadal>0
041500120417    1c                   dow       wfest='F' and wdataDAL<wdataAL
041600120417     c                   eval      kdata=wdataDAL
041700120417     C     KCLN          CHAIN     AZCLN01L
041800120417    2c                   if        not %found(azcln01l)
041900120417     c                   clear                   clnpom
042000120417     c                   clear                   clnmat
042100120417    2c                   endif
042200120417     c
042300120417     c                   movea     clnpom        k31
042400120417     c                   movel     k31(wgg)      wfest
042500120417    2c                   if        wfest<>'F'
042600120417     c                   movea     clnmat        k31
042700120417     c                   movel     k31(wgg)      wfest
042800120417    2c                   endif
042900120417     c* se festivo, verifico se nella data arrivo ci sono distinte
043000120417    2c                   if        wfest='F'
043100120417     c
043200120417     c     kfvv8d        setll     fnfvv08l
043300130610     c     kfvv8d        reade     fnfvv08l
043400130610    3c                   dow       not %eof(fnfvv08l)
043500130610     c
043600130610     c                   if        fvvatb=' '
043700130610     c                   leave
043800130610     c                   else
043900130610     c     kfvv8d        reade     fnfvv08l
044000130610     c                   endif
044100130610     c
044200130610     c                   enddo
044300130610     c
044400120417     c* se trovo distinte non considero più come giorno festivo
044500130610     c                   if        not %eof(fnfvv08l)
044600120417     c                   clear                   wfest
044700120417   x3c                   else
044800120417     c                   movel     wdatadal      dataiso
044900120417     c                   adddur    1:*d          dataiso
045000120417     c                   movel     dataiso       wdatadal
045100120417    3c                   endif
045200120417    2c                   endif
045300120417    c
045400120417    1c                   enddo
045500120417    0c                   endif
045600120417     c
045700120417     c* imposto data arrivo calcolata in un campo di comodo
045800120417     c                   eval      ArrNoFest=wdatadal
045900120417     C                   ENDSR
046000070313**
046100070313Numero di spedizione inesistente
