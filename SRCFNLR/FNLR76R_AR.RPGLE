000100030718     H DECEDIT('0,') DATEDIT(*DMY.)  OPTION(*NODEBUGIO)
000200070313     H* FNLR76R *----------------------------------------------------*
000300070316     H* - Verifica arrivo merce di una spedizione
000400000000     H*--------------------------------------------------------------*
000500090309     FFiART01L  IF   E           K DISK
000600070316     FFNARB01L  IF   E           K DISK
000700070313     FFNfvv01L  IF   E           K DISK
000800070313     FFNfvv08L  IF   E           K DISK
000900070313     F                                     RENAME(FNfvv000:FNfvv008)
001000070313     FFNDCD02L  IF   E           K DISK
001100070313     FFNDCT01L  IF   E           K DISK
001200070313     F                                     RENAME(FNDCT000:FNDCT001)
001300030722     F**
001400070313     D MSG             S             78    DIM(1) CTDATA PERRCD(1)
001500070313     d
001600051123      * ds di passaggio dati
001700070313     D fnlr76ds      E DS
001800070313     d
001900070313     D FNLV53DS      E DS
002000070313     D DSBS02        E DS                  EXTNAME(TIBS02DS)
002100070313     D DCCH          E DS
002200070313     D DTAD          E DS
002300070314     D KPJBA         E DS
002400070313     d
002500070316     D DSRECARB      E DS                  extname(fnarb00f)
002600070327     D DSparARB      E DS                  extname(fnarb00f) prefix(PAR:3)
002700070316     d
002800070316     d wmanca          s              1
002900070316     d wdamarr         s              1
003000070829     d CerArrivo       s              1
003100070829     d ArrEffet        s             12  0
003200070313     D knpg            S                   LIKE(fvvnpg) inz(2)
003300070313     D WARTDAM         S                   LIKE(arbdam)
003400070420     D kdata           S                   LIKE(arbdam)
003500070313     d Dataiso         s               d   datfmt(*iso)
003600900518     C***************************************************************
003700000000     C     *ENTRY        PLIST
003800070313     C                   PARM                    fnlr76ds
003900070327     C                   PARM                    dspararb
004000070829     C                   PARM                    dsSoglia         12 0
004100940223     C*---------------------------------------------------------------*
004200070313     C* TIPO LANCIO, ilr76TLA "C" -           CHIUSO CON LR
004300070313     C* TIPO LANCIO, ilr76TLA "L" - ELABORO E CHIUDO CON LR
004400070313     C* TIPO LANCIO, ilr76TLA " " - ELABORO E CHIUDO IN RETRN
004500941216     C*
004600070313    1C     ilr76TLA      IFNE      'C'
004700070313     c                   clear                   olr76err
004800070313     c                   clear                   olr76msg
004900070316     c                   clear                   olr76cNOCO
005000070316     c                   clear                   olr76cDACO
005100070316     c                   clear                   olr76cDCM
005200070316     c                   clear                   olr76cDC1G
005300070316     c                   clear                   olr76cCA
005400051123     c
005500070313     C* SE NON PASSATo record spedizione, lo chaino
005600070829     C                   IF        (%parms>=2 and parnsp=0) or
005700070315     c                              %parms=1
005800070313     c
005900070313     c     karb          chain     fnarb01l
006000070313     c                   if        not %found(fnarb01l)
006100070313     c* restituisco errore al chiamante ed esco
006200070313     c                   eval      olr76msg=msg(1)
006300070313     c                   eval      olr76err='E'
006400070313     c                   goto      fine
006500070313     c                   endif
006600070327     c                   else
006700070327     c                   movel     dspararb      dsrecarb
006800070313     c                   endif
006900070829     c
007000070829     c* Verifico se passata data e ora soglia per delterminare
007100070829     c*  i colli consegnabili
007200070829     c                   if        %parms=3 and dsSoglia>0
007300070829     c                   movel     dsSoglia      SogliaData        8 0
007400070829     c                   move      dsSoglia      Soglia           12 0
007500070829     c                   else
007600070829     c                   clear                   sogliaData
007700070829     c                   clear                   Soglia
007800070829     c                   endif
007900070313     c
008000070316     c* Verifico stato colli di uan spedizione
008100070316     c                   exsr      CTRColli
008200070314     c
008300070314     c     fine          TAG
008400070314     c
008500070314     c                   endif
008600051123     c
008700070313     C     Ilr76tla      IFNE      ' '
008800051123     C                   SETON                                        LR
008900051123     C                   ELSE
009000051123     C                   SETON                                        RT
009100051123     C                   ENDIF
009200991206     C**************************************************************************
009300051123     C     *INZSR        BEGSR
009400051123     c
009500070313     c     kARB          Klist
009600070313     c                   Kfld                    ilr76Aas
009700070313     c                   Kfld                    ilr76Lnp
009800070313     c                   Kfld                    ilr76nrs
009900070313     c                   Kfld                    ilr76nsp
010000070313     C     Kfvv          KLIST
010100070313     C                   KFLD                    D53NPA
010200070313     C                   KFLD                    D53NFA
010300070313     C                   KFLD                    D53LAI
010400070313     C     Kfvv8         KLIST
010500070313     C                   KFLD                    arbtfa
010600070313     C                   KFLD                    knpg
010700070420     C                   KFLD                    kdata
010800070313     C     Kfvv81        KLIST
010900070313     C                   KFLD                    arbtfa
011000070313     C                   KFLD                    knpg
011100070314     C     KART          KLIST
011200070314     C                   KFLD                    ARTFLS
011300070314     C                   KFLD                    ARTLNA
011400070314     C                   KFLD                    ARTNRS
011500070314     C                   KFLD                    ARTNSC
011600070314     C     KDCT          KLIST
011700070314     C                   KFLD                    DCDAAC
011800070314     C                   KFLD                    DCDFIL
011900070314     C                   KFLD                    DCDNCA
012000051123     c
012100070313     c                   endsr
012200070313     C**************************************************************************
012300070316     C     CTRColli      BEGSR
012400070316     c
012500090309     C     KARB          SETLL     FiART01L
012600090309     C     KARB          READE     FiART01L
012700070313     C*
012800090309    2C                   DOW       not %eof(fiart01l)
012900070313     c                   clear                   Wartdam
013000070316     c                   clear                   Wdamarr
013100070829     c                   clear                   CerArrivo
013200070829     c
013300070829     c* Se richiesta data e ora soglia, devo sempre richiamare abbinamento
013400070829     c*  o se richiesta abilitazione in base alle spunte partenza
013500070829     c                   if        (artdam=0  and artdfv>0  and
013600070829     c                              ilr76sppar='S')
013700070829     c                   eval      CerArrivo='S'
013800070829     c                   endif
013900070829     c
014000070829     c                   if        SogliaData>*zeros or CerArrivo='S'
014100070829     c                   exsr      Abbinam
014200070829     c                   endif
014300070829     c
014400070313    3c                   if        artdam>0
014500070313     c                   eval      Wartdam=artdam
014600070316     c                   eval      Wdamarr='A'
014700070313   x3c                   else
014800070313     c
014900070313     c* Memorizzo la data di arrivo o l'ipotetico arrivo se collo partito
015000070313     c* su FGV abbinato di ida non ancora chiuso: come data di arrivo
015100070313     c*  prendo la data foglio arrivi, solo se richiesto
015200070829    4c***                if        artdam=0  and artdfv>0  and ilr76sppar='S'
015300070313     c* Se si tratta di bolla LOCALE, non esiste abbinamento quindi
015400070313     c* cerco di desumere la data del foglio arrivi per vedere se è
015500070313     c*  chiuso o perto
015600070829    5c***                if        arbtfp=arbtfa
015700070829     c***                exsr      BOLLALOC
015800070829   X5c***                else
015900070829     C***
016000070313     C* CONTROLLO SE LA BOLLA ARRIVI E RELATIVA A FOGLIO VIAGGIO ABBINATO
016100070829     C***                CLEAR                   fnlv53ds
016200070829     C***                MOVEL     'A'           D53TFO
016300070829     C***                MOVE      artNFV        D53NFV
016400070829     C***                MOVE      arbtfp        D53FGS
016500070829     C***                MOVEL     'B'           D53ABB
016600070829     C***                MOVE      ARBLNA        D53LNA
016700070829     C***                MOVE      ARBTFA        D53TFA
016800070829     C***                CALL      'FNLV53R'
016900070829     C***                PARM                    fnlv53ds
017000070829     c***
017100070829    6c***                if        d53err=*blanks and d53nfa>0
017200070829     c**verifico se fogli arrivi di abbinamento già chiuso
017300070829     C***  KFVV          CHAIN     FNFVV000
017400070829     C***                IF        %FOUND(FNFVV01L)
017500070419     C***                IF        FVVFCF=' '
017600070829     C***                EVAL      Wartdam=D53DFA
017700070419     C***                ENDIF
017800070829     C***                ENDIF
017900070829    6c***                endif
018000070829    5c***                endif
018100070829     c***
018200070829    4c***                endif
018300070313    3c                   endif
018400070316     c
018500070316     c* Controllo se c'e' C.A.
018600070316     c                   EXSR      CTRCA
018700070316     c
018800070316     c                   select
018900070316     c* collo mancante/reso/distrutto da c.a.
019000070316     c                   when      wmanca<>' '
019100070316     c                   add       1             olr76cca
019200070316     c
019300070316     c* collo consegnato in data <= data consegnabilità
019400070316     c                   when      artdcm>0 and artdcm<=ilr76dco
019500070316     c                   add       1             olr76cdcm
019600070316     c
019700070830     c* Collo non consegnabile se arrivato dopo data consegnabilità
019800070830     c                   when      wartdam>0 and wartdam>ilr76dco
019900070830     c                   add       1             olr76cnoco
020000070830     c
020100070830     c***                when      SogliaData=0 and
020200070830     c***                          wartdam>0 and wartdam<=ilr76dco
020300070316     c* collo consegnabile
020400070830     c* Senza Soglia o con soglia ma senza data di partenza
020500070830     c*       (collo arrivato non partito)
020600080118     c* o con soglia ma errore nel richiamare FNLV53R
020700070830     c                   when      wartdam>0 and
020800070831     c                             (SogliaData=0 or artdfv=0 or
020900080118     c                              arbtfp=arbtfa or d53err<>' ' )
021000070830     c
021100070316     c                   add       1             olr76cdaco
021200070326     c* ...di cui arrivati da almeno un giorno rispetto all'arrivo 1 collo
021300070326     c*    o alla data consegnabilità)
021400070326     c                   if        wdamarr='A' and (wartdam< ilr76dco  or
021500070326     c                             wartdam>arbdaM AND ARBDAM>0)
021600070316     c                   add       1             olr76cdc1g
021700070316     c                   endif
021800070829     c
021900070829     c* Con   Soglia
022000070829     c                   when      SogliaData>0 and
022100070829     c                             d53err=' ' and d53nfa>0  and
022200070829     c                             d53dfa<=ilr76dco and ArrEffet<=Soglia
022300070830     c                             and wartdam>0
022400070829     c                   add       1             olr76cdaco
022500070829     c* ...di cui arrivati da almeno un giorno rispetto all'arrivo 1 collo
022600070829     c*    o alla data consegnabilità)
022700070829     c                   if        wdamarr='A' and (wartdam< ilr76dco  or
022800070829     c                             wartdam>arbdaM AND ARBDAM>0)
022900070829     c                   add       1             olr76cdc1g
023000070829     c                   endif
023100070316     c
023200070316     c* altrimenti è non consegnabile
023300070316     c                   other
023400070316     c                   add       1             olr76cnoco
023500070316     c                   ENDSL
023600070316     c
023700090309     C     KARB          READE     FiART01L
023800070316    2c                   enddo
023900070316     c
024000070316     C     ENDCtrco      ENDSR
024100070313     C**************************************************************************
024200070313     C* Per bolla locale desumo la data dell'arrivo bolla
024300070313     C**************************************************************************
024400070313     c     BOLLALOC      begsr
024500070420     c* Prendo data partenza collo se presente
024600070831     c                   clear                   fnlv53ds
024700070831     c                   if        artdfv>0
024800070420     c                   eval      kdata=artdfv
024900070313     c     kfvv8         SETGT     FNFVV08L
025000070313     c     kfvv81        READe     FNFVV08L
025100070420     C                   IF        not %eoF(FNFVV08L) AND FVVDFV>=ARtdfv
025200070419     C****               IF        FVVFCF=' '
025300070313     C                   EVAL      Wartdam=FVVDFV
025400070419     C****               ENDIF
025500070313     C*
025600070313     C                   ELSE
025700070831     C* SE NON TROVATO  SEMPRE CON DATA  + 1
025800070420     c     *iso          movel     artdfv        dataiso
025900070313     c                   adddur    1:*d          dataiso
026000070313     c                   movel     dataiso       wartdam
026100070829     c                   endif
026200070831     c
026300070831     c                   endif
026400070829     c                   ENDSR
026500070829     C**************************************************************************
026600070829     c* Reperisco dati di abbinamento
026700070829     C**************************************************************************
026800070829     c     ABBINAM       BEGSR
026900070829     c                   clear                   ArrEffet
027000070829    5c                   if        arbtfp=arbtfa
027100070829     c* ai fini della soglia i locali vanno sempre considerati
027200070829     c                   if        CerArrivo='S'
027300070829     c                   exsr      BOLLALOC
027400070829     c                   endif
027500070829     c* Nei locali ArrEffet lo lascio=0  perchè deve essere sempre
027600070829     c*  consegnabile
027700070829     c
027800070829   X5c                   else
027900070829     C
028000070829     C* CONTROLLO SE LA BOLLA ARRIVI E RELATIVA A FOGLIO VIAGGIO ABBINATO
028100070829     C                   CLEAR                   fnlv53ds
028200070829     C                   MOVEL     'A'           D53TFO
028300070829     C                   MOVE      artNFV        D53NFV
028400070829     C                   MOVE      arbtfp        D53FGS
028500070829     C                   MOVEL     'B'           D53ABB
028600070829     C                   MOVE      ARBLNA        D53LNA
028700070829     C                   MOVE      ARBTFA        D53TFA
028800070829     C                   CALL      'FNLV53R'
028900070829     C                   PARM                    fnlv53ds
029000070829     c*
029100070829    6c                   if        d53err=*blanks and d53nfa>0
029200070829    7c                   if        cerArrivo='S'
029300070829     c* verifico se fogli arrivi di abbinamento già chiuso
029400070829     C     KFVV          CHAIN     FNFVV000
029500070829     C                   IF        %FOUND(FNFVV01L)
029600070829     C***                IF        FVVFCF=' '
029700070829     C                   EVAL      Wartdam=D53DFA
029800070829     C***                ENDIF
029900070829     C                   ENDIF
030000070829    7c                   endif
030100070829     c
030200070829     c                   movel     d53aed        ArrEffet
030300070829     c                   move      d53aeh        ArrEffet
030400070829    6c                   endif
030500070829    5c                   endif
030600070829     c                   ENDSR
030700070313     C**************************************************************************
030800070313     C* CALCOLO DATA PREV.CONS/CONSEGNABILITA'/ORE RITARDO DI 1 BOLLA
030900070313     C**************************************************************************
031000070313     C     CTRCA         BEGSR
031100070313     C                   CLEAR                   WMANCA
031200070313     C**
031300070313     C     KART          CHAIN     FNDCD02L                           33
031400070313    2C     *IN33         IFEQ      *OFF
031500070726     C     DCDATB        ANDEQ     ' '
031600070313     C     KDCT          CHAIN     FNDCT01L                           33
031700070726      *
031800070726      * se dctdt2 > 0 la c.a. non è valida x la filiale
031900070726     c                   If        Not *In33 and dctdt2 > 0
032000070726     c                   Eval      *In33 = *On
032100070726     c                   EndIf
032200070313     C*
032300070313    3C     *IN33         IFEQ      *OFF
032400070313     C     DCTDCH        ANDEQ     DCDDCH
032500070313     C     DCDDCH        OREQ      0
032600070313     C* TESTO PRIMA LA CAUSALE DI CHIUSURA PER VEDERE SE MI INDICA LA
032700070313     C*  PRESENZA DELLA MERCE
032800070316     c                   clear                   dcch
032900070316     c                   if        dctcch<>*blanks
033000070313     C                   CLEAR                   DSBS02
033100070313     C                   MOVEL     'C'           T02MOD
033200070313     C                   MOVEL     'CCH'         T02COD
033300070313     C                   MOVEL     DCTCCH        T02KE1
033400070313     C                   CALL      'TIBS02R'
033500070313     C                   PARM                    KPJBA
033600070313     C                   PARM                    DSBS02
033700070313     C**
033800070313    4C     T02ERR        IFEQ      ' '
033900070313     C                   MOVEL     T02UNI        DCCH
034000070313     C                   ELSE
034100070313     C                   MOVEL     'S'           §CCHIMA
034200070313    4C                   ENDIF
034300070316     c                   endif
034400070313     C*
034500070313    4C     §CCHIMA       IFEQ      ' '
034600070313     C                   CLEAR                   DSBS02
034700070313     C                   MOVEL     'C'           T02MOD
034800070313     C                   MOVEL     'TAD'         T02COD
034900070313     C                   MOVEL     DCTTAD        T02KE1
035000070313     C                   CALL      'TIBS02R'
035100070313     C                   PARM                    KPJBA
035200070313     C                   PARM                    DSBS02
035300070313     C**
035400070313    5C     T02ERR        IFEQ      ' '
035500070313     C                   MOVEL     T02UNI        DTAD
035600070313     C                   ELSE
035700070313     C                   MOVEL     'M'           §TADRAGR
035800070313    5C                   ENDIF
035900070313     C*
036000070313     C* AVARIA RESA O DISTRUTTA
036100070313    5C     §TADRAGR      IFEQ      'V'
036200070313    6C     DCTDSD        IFEQ      'D'
036300070313     C     DCTDSD        OREQ      'R'
036400070316     C                   MOVEL     'V'           WMANCA
036500070313    6C                   ENDIF
036600070313    5C                   ENDIF
036700070313     C* MANCANZA
036800070313    5C     §TADRAGR      IFEQ      'M'
036900070316     C                   MOVEL     'M'           WMANCA
037000070313    5C                   ENDIF
037100070313     C*
037200070313    4C                   ENDIF
037300070313    3C                   ENDIF
037400070313    2C                   ENDIF
037500070313     C*
037600070313     C                   ENDSR
037700070313**
037800070313Numero di spedizione inesistente
