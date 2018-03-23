000100030718     H DECEDIT('0,') DATEDIT(*DMY.)  OPTION(*NODEBUGIO)
000200070313     H* FNLR76R *----------------------------------------------------*
000300070316     H* - Verifica arrivo merce di una spedizione
000400000000     H*--------------------------------------------------------------*
000500150421     Ftabel00f  IF   E           K DISK    usropn
000600150421     FAZCLN01L  IF   E           K DISK
000700070313     FFNART01L  IF   E           K DISK
000800070316     FFNARB01L  IF   E           K DISK
000900070313     FFNfvv01L  IF   E           K DISK
001000070313     FFNfvv08L  IF   E           K DISK
001100070313     F                                     RENAME(FNfvv000:FNfvv008)
001200070313     FFNDCD02L  IF   E           K DISK
001300070313     FFNDCT01L  IF   E           K DISK
001400070313     F                                     RENAME(FNDCT000:FNDCT001)
001500030722     F**
001600120417     D K31             S              1    DIM(31)                              SKI DI COMODO
001700150421     D npsf            S              2    DIM(50)
001800070313     D MSG             S             78    DIM(1) CTDATA PERRCD(1)
001900070313     d
002000051123      * ds di passaggio dati
002100070313     D fnlr76ds      E DS
002200070313     d
002300070313     D FNLV53DS      E DS
002400070313     D DSBS02        E DS                  EXTNAME(TIBS02DS)
002500070313     D DCCH          E DS
002600150421     D ds7J          E DS
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
004200070829     d CerArrivo       s              1
004300070829     d ArrEffet        s             12  0
004400120417     d kZERO           s              3  0
004500120417     d ArrNoFEST       s              8  0
004600120417     D WDATAal         s              8  0
004700150421     D XX              s              3  0
004800150421     D kcod            s                   like(tblcod)
004900070313     D knpg            S                   LIKE(fvvnpg) inz(2)
005000120417     D knpg4           S                   LIKE(fvvnpg) inz(4)
005100070313     D WARTDAM         S                   LIKE(arbdam)
005200070420     D kdata           S                   LIKE(arbdam)
005300070313     d Dataiso         s               d   datfmt(*iso)
005400900518     C***************************************************************
005500000000     C     *ENTRY        PLIST
005600070313     C                   PARM                    fnlr76ds
005700070327     C                   PARM                    dspararb
005800070829     C                   PARM                    dsSoglia         12 0
005900940223     C*---------------------------------------------------------------*
006000070313     C* TIPO LANCIO, ilr76TLA "C" -           CHIUSO CON LR
006100070313     C* TIPO LANCIO, ilr76TLA "L" - ELABORO E CHIUDO CON LR
006200070313     C* TIPO LANCIO, ilr76TLA " " - ELABORO E CHIUDO IN RETRN
006300941216     C*
006400070313    1C     ilr76TLA      IFNE      'C'
006500070313     c                   clear                   olr76err
006600070313     c                   clear                   olr76msg
006700070316     c                   clear                   olr76cNOCO
006800070316     c                   clear                   olr76cDACO
006900070316     c                   clear                   olr76cDCM
007000070316     c                   clear                   olr76cDC1G
007100070316     c                   clear                   olr76cCA
007200110131     c                   clear                   fnlv53ds
007300051123     c
007400070313     C* SE NON PASSATo record spedizione, lo chaino
007500070829     C                   IF        (%parms>=2 and parnsp=0) or
007600070315     c                              %parms=1
007700070313     c
007800070313     c     karb          chain     fnarb01l
007900070313     c                   if        not %found(fnarb01l)
008000070313     c* restituisco errore al chiamante ed esco
008100070313     c                   eval      olr76msg=msg(1)
008200070313     c                   eval      olr76err='E'
008300070313     c                   goto      fine
008400070313     c                   endif
008500070327     c                   else
008600070327     c                   movel     dspararb      dsrecarb
008700070313     c                   endif
008800070829     c
008900070829     c* Verifico se passata data e ora soglia per delterminare
009000070829     c*  i colli consegnabili
009100070829     c                   if        %parms=3 and dsSoglia>0
009200070829     c                   movel     dsSoglia      SogliaData        8 0
009300070829     c                   move      dsSoglia      Soglia           12 0
009400070829     c                   else
009500070829     c                   clear                   sogliaData
009600070829     c                   clear                   Soglia
009700070829     c                   endif
009800070313     c
009900070316     c* Verifico stato colli di uan spedizione
010000070316     c                   exsr      CTRColli
010100070314     c
010200070314     c     fine          TAG
010300070314     c
010400070314     c                   endif
010500051123     c
010600070313     C     Ilr76tla      IFNE      ' '
010700051123     C                   SETON                                        LR
010800051123     C                   ELSE
010900051123     C                   SETON                                        RT
011000051123     C                   ENDIF
011100991206     C**************************************************************************
011200051123     C     *INZSR        BEGSR
011300051123     c
011400070313     c     kARB          Klist
011500070313     c                   Kfld                    ilr76Aas
011600070313     c                   Kfld                    ilr76Lnp
011700070313     c                   Kfld                    ilr76nrs
011800070313     c                   Kfld                    ilr76nsp
011900070313     C     Kfvv          KLIST
012000070313     C                   KFLD                    D53NPA
012100070313     C                   KFLD                    D53NFA
012200070313     C                   KFLD                    D53LAI
012300070313     C     Kfvv8         KLIST
012400070313     C                   KFLD                    arbtfa
012500070313     C                   KFLD                    knpg
012600070420     C                   KFLD                    kdata
012700070313     C     Kfvv81        KLIST
012800070313     C                   KFLD                    arbtfa
012900070313     C                   KFLD                    knpg
013000120417     C     Kfvv8D        KLIST
013100120417     C                   KFLD                    arblna
013200120417     C                   KFLD                    knpg4
013300120417     C                   KFLD                    kdata
013400070314     C     KART          KLIST
013500070314     C                   KFLD                    ARTFLS
013600070314     C                   KFLD                    ARTLNA
013700070314     C                   KFLD                    ARTNRS
013800070314     C                   KFLD                    ARTNSC
013900070314     C     KDCT          KLIST
014000070314     C                   KFLD                    DCDAAC
014100070314     C                   KFLD                    DCDFIL
014200070314     C                   KFLD                    DCDNCA
014300120417     C     KCLN          KLIST
014400120417     C                   KFLD                    KZERO
014500120417     C                   KFLD                    arblna
014600120417     C                   KFLD                    WAA               4 0
014700120417     C                   KFLD                    WMM               2 0
014800150421     C     Ktab          KLIST
014900150421     C                   KFLD                    codut             1 0
015000150421     C                   KFLD                    kcod
015100150422     c                   z-add     1             codut
015200150421     c*
015300150421     c* Carico le pistole automatiche
015400150421     c                   open      tabel00f
015500150421     c                   eval      kcod='7J'
015600150421     c                   clear                   xx
015700150421     c     ktab          setll     tabel00f
015800150421     c     ktab          reade     tabel00f
015900150421     c                   dow       not %eof(tabel00f)
016000150421     c                   eval      ds7j=tbluni
016100150421     c                   if        §7jrps='A'
016200150421     c                   add       1             xx
016300150421     c                   eval      npsf(XX)=tblkey
016400150421     c                   endif
016500150421     c
016600150421     c     ktab          reade     tabel00f
016700150421     c                   enddo
016800150421     c
016900150421     c                   close     tabel00f
017000051123     c
017100070313     c                   endsr
017200070313     C**************************************************************************
017300070316     C     CTRColli      BEGSR
017400070316     c
017500070313     C     KARB          SETLL     FNART01L
017600070316     C     KARB          READE     FNART01L
017700070313     C*
017800070316    2C                   DOW       not %eof(fnart01l)
017900070313     c                   clear                   Wartdam
018000070316     c                   clear                   Wdamarr
018100070829     c                   clear                   CerArrivo
018200070829     c
018300070829     c* Se richiesta data e ora soglia, devo sempre richiamare abbinamento
018400070829     c*  o se richiesta abilitazione in base alle spunte partenza
018500070829     c                   if        (artdam=0  and artdfv>0  and
018600070829     c                              ilr76sppar='S')
018700070829     c                   eval      CerArrivo='S'
018800070829     c                   endif
018900070829     c
019000070829     c                   if        SogliaData>*zeros or CerArrivo='S'
019100070829     c                   exsr      Abbinam
019200070829     c                   endif
019300070829     c
019400070313    3c                   if        artdam>0
019500070313     c                   eval      Wartdam=artdam
019600070316     c                   eval      Wdamarr='A'
019700070313   x3c                   else
019800070313     c
019900070313     c* Memorizzo la data di arrivo o l'ipotetico arrivo se collo partito
020000070313     c* su FGV abbinato di ida non ancora chiuso: come data di arrivo
020100070313     c*  prendo la data foglio arrivi, solo se richiesto
020200070829    4c***                if        artdam=0  and artdfv>0  and ilr76sppar='S'
020300070313     c* Se si tratta di bolla LOCALE, non esiste abbinamento quindi
020400070313     c* cerco di desumere la data del foglio arrivi per vedere se è
020500070313     c*  chiuso o perto
020600070829    5c***                if        arbtfp=arbtfa
020700070829     c***                exsr      BOLLALOC
020800070829   X5c***                else
020900070829     C***
021000070313     C* CONTROLLO SE LA BOLLA ARRIVI E RELATIVA A FOGLIO VIAGGIO ABBINATO
021100070829     C***                CLEAR                   fnlv53ds
021200070829     C***                MOVEL     'A'           D53TFO
021300070829     C***                MOVE      artNFV        D53NFV
021400070829     C***                MOVE      arbtfp        D53FGS
021500070829     C***                MOVEL     'B'           D53ABB
021600070829     C***                MOVE      ARBLNA        D53LNA
021700070829     C***                MOVE      ARBTFA        D53TFA
021800070829     C***                CALL      'FNLV53R'
021900070829     C***                PARM                    fnlv53ds
022000070829     c***
022100070829    6c***                if        d53err=*blanks and d53nfa>0
022200070829     c**verifico se fogli arrivi di abbinamento già chiuso
022300070829     C***  KFVV          CHAIN     FNFVV000
022400070829     C***                IF        %FOUND(FNFVV01L)
022500070419     C***                IF        FVVFCF=' '
022600070829     C***                EVAL      Wartdam=D53DFA
022700070419     C***                ENDIF
022800070829     C***                ENDIF
022900070829    6c***                endif
023000070829    5c***                endif
023100070829     c***
023200070829    4c***                endif
023300070313    3c                   endif
023400070316     c
023500070316     c* Controllo se c'e' C.A.
023600070316     c                   EXSR      CTRCA
023700120417     c* Verifico se la data di arrivo <> dalla data elaborazione, cade in giorno festivo
023800120417     c                   exsr      CTRFest
023900070316     c
024000070316     c                   select
024100070316     c* collo mancante/reso/distrutto da c.a.
024200070316     c                   when      wmanca<>' '
024300070316     c                   add       1             olr76cca
024400070316     c
024500070316     c* collo consegnato in data <= data consegnabilità
024600070316     c                   when      artdcm>0 and artdcm<=ilr76dco
024700070316     c                   add       1             olr76cdcm
024800070316     c
024900070830     c* Collo non consegnabile se arrivato dopo data consegnabilità
025000070830     c                   when      wartdam>0 and wartdam>ilr76dco
025100070830     c                   add       1             olr76cnoco
025200070830     c
025300070830     c***                when      SogliaData=0 and
025400070830     c***                          wartdam>0 and wartdam<=ilr76dco
025500070316     c* collo consegnabile
025600070830     c* Senza Soglia o con soglia ma senza data di partenza
025700070830     c*       (collo arrivato non partito)
025800080118     c* o con soglia ma errore nel richiamare FNLV53R
025900070830     c                   when      wartdam>0 and
026000070831     c                             (SogliaData=0 or artdfv=0 or
026100080118     c                              arbtfp=arbtfa or d53err<>' ' )
026200070830     c
026300070316     c                   add       1             olr76cdaco
026400070326     c* ...di cui arrivati da almeno un giorno rispetto all'arrivo 1 collo
026500070326     c*    o alla data consegnabilità)
026600120417     c****               if        wdamarr='A' and (wartdam< ilr76dco  or
026700120417     c                   if        wdamarr='A' and (ArrNoFest < ilr76dco  or
026800070326     c                             wartdam>arbdaM AND ARBDAM>0)
026900070316     c                   add       1             olr76cdc1g
027000070316     c                   endif
027100150421     c
027200150421     c* ...di cui non arrivati e partiti in pistola automatica
027300150421     c                   if        wdamarr=' '
027400150421     c                   exsr      contr_NPP
027500150422     c                   if        wautnpp=' '
027600150422     c                   add       1             olr76cdc88
027700150422     c                   endif
027800150421     c                   endif
027900070829     c
028000070829     c* Con   Soglia
028100070829     c                   when      SogliaData>0 and
028200070829     c                             d53err=' ' and d53nfa>0  and
028300070829     c                             d53dfa<=ilr76dco and ArrEffet<=Soglia
028400070830     c                             and wartdam>0
028500070829     c                   add       1             olr76cdaco
028600070829     c* ...di cui arrivati da almeno un giorno rispetto all'arrivo 1 collo
028700070829     c*    o alla data consegnabilità)
028800120417     c***                if        wdamarr='A' and (wartdam< ilr76dco  or
028900120417     c                   if        wdamarr='A' and (ArrNoFest < ilr76dco  or
029000070829     c                             wartdam>arbdaM AND ARBDAM>0)
029100070829     c                   add       1             olr76cdc1g
029200070829     c                   endif
029300150514     c* ...di cui non arrivati e partiti in pistola automatica
029400150514     c                   if        wdamarr=' '
029500150514     c                   exsr      contr_NPP
029600150514     c                   if        wautnpp=' '
029700150514     c                   add       1             olr76cdc88
029800150514     c                   endif
029900150514     c                   endif
030000070316     c
030100070316     c* altrimenti è non consegnabile
030200070316     c                   other
030300070316     c                   add       1             olr76cnoco
030400070316     c                   ENDSL
030500070316     c
030600070316     C     KARB          READE     FNART01L
030700070316    2c                   enddo
030800070316     c
030900070316     C     ENDCtrco      ENDSR
031000150421     C**************************************************************************
031100150421     C* controllo pistolta partenza
031200150421     C**************************************************************************
031300150421     c     Contr_NPP     BEGSR
031400150422     c                   clear                   wautNPP           1
031500150422     c                   if        artdut>0
031600150422     c                   movel     artput        w002a             2
031700150422     c                   if        %lookup(w002a:npsf)=0
031800150422     c                   eval      wautnpp='N'
031900150422     c                   endif
032000150422     c                   endif
032100150422     c                   if        artdfv>0
032200150422     c                   movel     artnpp        w002a             2
032300150422     c                   if        %lookup(w002a:npsf)=0
032400150422     c                   eval      wautnpp='N'
032500150422     c                   endif
032600150422     c                   endif
032700150422     c
032800150421     c                   ENDSR
032900070313     C**************************************************************************
033000070313     C* Per bolla locale desumo la data dell'arrivo bolla
033100070313     C**************************************************************************
033200070313     c     BOLLALOC      begsr
033300070420     c* Prendo data partenza collo se presente
033400070831     c                   clear                   fnlv53ds
033500070831     c                   if        artdfv>0
033600070420     c                   eval      kdata=artdfv
033700070313     c     kfvv8         SETGT     FNFVV08L
033800070313     c     kfvv81        READe     FNFVV08L
033900070420     C                   IF        not %eoF(FNFVV08L) AND FVVDFV>=ARtdfv
034000070419     C****               IF        FVVFCF=' '
034100070313     C                   EVAL      Wartdam=FVVDFV
034200070419     C****               ENDIF
034300070313     C*
034400070313     C                   ELSE
034500070831     C* SE NON TROVATO  SEMPRE CON DATA  + 1
034600070420     c     *iso          movel     artdfv        dataiso
034700070313     c                   adddur    1:*d          dataiso
034800070313     c                   movel     dataiso       wartdam
034900070829     c                   endif
035000070831     c
035100070831     c                   endif
035200070829     c                   ENDSR
035300070829     C**************************************************************************
035400070829     c* Reperisco dati di abbinamento
035500070829     C**************************************************************************
035600070829     c     ABBINAM       BEGSR
035700070829     c                   clear                   ArrEffet
035800070829    5c                   if        arbtfp=arbtfa
035900070829     c* ai fini della soglia i locali vanno sempre considerati
036000070829     c                   if        CerArrivo='S'
036100070829     c                   exsr      BOLLALOC
036200070829     c                   endif
036300070829     c* Nei locali ArrEffet lo lascio=0  perchè deve essere sempre
036400070829     c*  consegnabile
036500070829     c
036600070829   X5c                   else
036700070829     C
036800070829     C* CONTROLLO SE LA BOLLA ARRIVI E RELATIVA A FOGLIO VIAGGIO ABBINATO
036900070829     C                   CLEAR                   fnlv53ds
037000070829     C                   MOVEL     'A'           D53TFO
037100070829     C                   MOVE      artNFV        D53NFV
037200070829     C                   MOVE      arbtfp        D53FGS
037300070829     C                   MOVEL     'B'           D53ABB
037400070829     C                   MOVE      ARBLNA        D53LNA
037500120223     c* Lascio il campo vuoto per farlo ricalcolare in modo che se è cambiato
037600120223     c*  il terminal non ci siano problemi  sulla abbinabilità del foglio
037700120223     C****               MOVE      ARBTFA        D53TFA
037800070829     C                   CALL      'FNLV53R'
037900070829     C                   PARM                    fnlv53ds
038000070829     c*
038100070829    6c                   if        d53err=*blanks and d53nfa>0
038200070829    7c                   if        cerArrivo='S'
038300070829     c* verifico se fogli arrivi di abbinamento già chiuso
038400070829     C     KFVV          CHAIN     FNFVV000
038500070829     C                   IF        %FOUND(FNFVV01L)
038600070829     C***                IF        FVVFCF=' '
038700070829     C                   EVAL      Wartdam=D53DFA
038800070829     C***                ENDIF
038900070829     C                   ENDIF
039000070829    7c                   endif
039100070829     c
039200070829     c                   movel     d53aed        ArrEffet
039300070829     c                   move      d53aeh        ArrEffet
039400070829    6c                   endif
039500070829    5c                   endif
039600070829     c                   ENDSR
039700070313     C**************************************************************************
039800070313     C* CALCOLO DATA PREV.CONS/CONSEGNABILITA'/ORE RITARDO DI 1 BOLLA
039900070313     C**************************************************************************
040000070313     C     CTRCA         BEGSR
040100070313     C                   CLEAR                   WMANCA
040200070313     C**
040300070313     C     KART          CHAIN     FNDCD02L                           33
040400070313    2C     *IN33         IFEQ      *OFF
040500070726     C     DCDATB        ANDEQ     ' '
040600070313     C     KDCT          CHAIN     FNDCT01L                           33
040700070726      *
040800070726      * se dctdt2 > 0 la c.a. non è valida x la filiale
040900070726     c                   If        Not *In33 and dctdt2 > 0
041000070726     c                   Eval      *In33 = *On
041100070726     c                   EndIf
041200070313     C*
041300070313    3C     *IN33         IFEQ      *OFF
041400070313     C     DCTDCH        ANDEQ     DCDDCH
041500070313     C     DCDDCH        OREQ      0
041600070313     C* TESTO PRIMA LA CAUSALE DI CHIUSURA PER VEDERE SE MI INDICA LA
041700070313     C*  PRESENZA DELLA MERCE
041800070316     c                   clear                   dcch
041900070316     c                   if        dctcch<>*blanks
042000070313     C                   CLEAR                   DSBS02
042100070313     C                   MOVEL     'C'           T02MOD
042200070313     C                   MOVEL     'CCH'         T02COD
042300070313     C                   MOVEL     DCTCCH        T02KE1
042400070313     C                   CALL      'TIBS02R'
042500070313     C                   PARM                    KPJBA
042600070313     C                   PARM                    DSBS02
042700070313     C**
042800070313    4C     T02ERR        IFEQ      ' '
042900070313     C                   MOVEL     T02UNI        DCCH
043000070313     C                   ELSE
043100070313     C                   MOVEL     'S'           §CCHIMA
043200070313    4C                   ENDIF
043300070316     c                   endif
043400070313     C*
043500070313    4C     §CCHIMA       IFEQ      ' '
043600070313     C                   CLEAR                   DSBS02
043700070313     C                   MOVEL     'C'           T02MOD
043800070313     C                   MOVEL     'TAD'         T02COD
043900070313     C                   MOVEL     DCTTAD        T02KE1
044000070313     C                   CALL      'TIBS02R'
044100070313     C                   PARM                    KPJBA
044200070313     C                   PARM                    DSBS02
044300070313     C**
044400070313    5C     T02ERR        IFEQ      ' '
044500070313     C                   MOVEL     T02UNI        DTAD
044600070313     C                   ELSE
044700070313     C                   MOVEL     'M'           §TADRAGR
044800070313    5C                   ENDIF
044900070313     C*
045000070313     C* AVARIA RESA O DISTRUTTA
045100070313    5C     §TADRAGR      IFEQ      'V'
045200070313    6C     DCTDSD        IFEQ      'D'
045300070313     C     DCTDSD        OREQ      'R'
045400070316     C                   MOVEL     'V'           WMANCA
045500070313    6C                   ENDIF
045600070313    5C                   ENDIF
045700070313     C* MANCANZA
045800070313    5C     §TADRAGR      IFEQ      'M'
045900070316     C                   MOVEL     'M'           WMANCA
046000070313    5C                   ENDIF
046100070313     C*
046200070313    4C                   ENDIF
046300070313    3C                   ENDIF
046400070313    2C                   ENDIF
046500070313     C*
046600070313     C                   ENDSR
046700120417     C**************************************************************************
046800120417     C* Controllo se arrivo in giorno festivo
046900120417     C**************************************************************************
047000120417     C     CTRfest       BEGSR
047100120417     c                   eval      wdataal=ilr76dco
047200120417     c                   eval      wfest='F'
047300120417     c                   eval      wdatadal=wartdam
047400120417     c
047500120417    0c                   if        wdatadal>0
047600120417    1c                   dow       wfest='F' and wdataDAL<wdataAL
047700120417     c                   eval      kdata=wdataDAL
047800120417     C     KCLN          CHAIN     AZCLN01L
047900120417    2c                   if        not %found(azcln01l)
048000120417     c                   clear                   clnpom
048100120417     c                   clear                   clnmat
048200120417    2c                   endif
048300120417     c
048400120417     c                   movea     clnpom        k31
048500120417     c                   movel     k31(wgg)      wfest
048600120417    2c                   if        wfest<>'F'
048700120417     c                   movea     clnmat        k31
048800120417     c                   movel     k31(wgg)      wfest
048900120417    2c                   endif
049000120417     c* se festivo, verifico se nella data arrivo ci sono distinte
049100120417    2c                   if        wfest='F'
049200120417     c
049300120417     c     kfvv8d        setll     fnfvv08l
049400130610     c     kfvv8d        reade     fnfvv08l
049500130610    3c                   dow       not %eof(fnfvv08l)
049600130610     c
049700130610     c                   if        fvvatb=' '
049800130610     c                   leave
049900130610     c                   else
050000130610     c     kfvv8d        reade     fnfvv08l
050100130610     c                   endif
050200130610     c
050300130610     c                   enddo
050400130610     c
050500120417     c* se trovo distinte non considero più come giorno festivo
050600130610     c                   if        not %eof(fnfvv08l)
050700120417     c                   clear                   wfest
050800120417   x3c                   else
050900120417     c                   movel     wdatadal      dataiso
051000120417     c                   adddur    1:*d          dataiso
051100120417     c                   movel     dataiso       wdatadal
051200120417    3c                   endif
051300120417    2c                   endif
051400120417    c
051500120417    1c                   enddo
051600120417    0c                   endif
051700120417     c
051800120417     c* imposto data arrivo calcolata in un campo di comodo
051900120417     c                   eval      ArrNoFest=wdatadal
052000120417     C                   ENDSR
052100070313**
052200070313Numero di spedizione inesistente
