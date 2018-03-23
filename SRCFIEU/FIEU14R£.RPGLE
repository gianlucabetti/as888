000100991124     H DECEDIT('0,') DATEDIT(*DMY/)
000200981217      ***************************************************************************
000300991213      *  CREA EVENTI DI MANCATA CONSEGNA PER IMPORT DPD                         *
000400971216      ***************************************************************************
000500160108     FFiapd01L  IF   E           K DISK
000600991214     FFNEVB06L  UF   E           K DISK
000700060608     FtIDP301L  UF A E           K DISK
000800050309     Ftigcp01L  IF   E           K DISK
000900061026     FfnBLP01L  IF   E           K DISK
001000061031     FfnARB01L  IF   E           K DISK
001100000127     FTABEL00F  IF   E           K DISK
001200991213     FTNTBE01L  IF   E           K DISK
001300020716     FAZORG01L  IF   E           K DISK
001400940321      *----------------------------------------------------*
001500000126     D K2A             S              3    DIM(200) inz                         CODICE NAZIONE
001600000126     D K2Z             S              3    DIM(200) inz                         CODICE NAZIONE
001700000126     D D2Z             S              3    DIM(200) inz                         CODICE NAZIONE
001800991214     D KEVD            S              3    DIM(30)  inz                         CODICE NAZIONE
001900991214     D EVD             S            256    DIM(30)  inz                         CODICE NAZIONE
002000991214     D WEC             S              4    DIM(30)  inz                         CODICE NAZIONE
002100000908      *-------------------
002200000908     D OFil            S              3S 0 DIM(100)  inz                        Filiali da AZORG
002300991213      *-------------------
002400000915     D yx              S              3  0 inz
002500000915     D XX              S              3  0 inz
002600000908     D FinoXX          S              3  0 inz
002700991214     D YY              S              3  0 inz
002800991214     D JJ              S              3  0 inz
002900991214     D Wdatagg         S              8  0 inz
003000000127     D W0080           S              8  0 inz
003100000127     D W0140           S             14  0 inz
003200000127     D Wday            S              5  0
003300000127     D Wflageve        S              1
003400000127     D Welabora        S              1
003500991214     D Wdataoggi       S               D   DATFMT(*eur)
003600000127     D Wdata           S               D   DATFMT(*eur)
003700991217     D Wevbcev         S                   LIKE(EVBcev)
003800000103     D Wdp3prn         S                   LIKE(DP3prn)
003900991213     D kdtc            S                   LIKE(EVBdtc)  INZ
004000991213     D klnp            S                   LIKE(EVBlnp)
004100991213     D Kcod            S                   LIKE(TBLCOD)
004200020919     D kpjbus          S                   LIKE(kpjbu)
004300160108     D ktip            S                   LIKE(apdtip)
004400160108     D kpdc            S                   LIKE(apdpdr)
004500131114      * Note degli Eventi
004600131114     D Devb01        e ds
004700971212      *-------------------
004800991214     D                 DS
004900991214     D DSWEC_EV2               1      2
005000991214     D DSWEC_EV3               1      3
005100991214     D DSWEC_UNI               4      4
005200991214     D DSWEC                   1      4
005300991214      *-------------------
005400991130     D KPJBA         E DS
005500991213     D DEVD          E DS
005600991213     D CNCR80        E DS
005700991213     D UT§DSE0F      E DS
005800991214     D OG143         E DS
005900000126     D DS2A          E DS
006000000126     D DS2Z          E DS
006100911213      *----------------------------------------------------*
006200151218    1c                   If        FinoXX > 0
006300000908      *
006400000908      *  per gestire più P.O. sulla stessa filiale
006500151218    2C                   DO        FinoXX        yx
006600000915     C                   eval      Klnp = OFil(yx)
006700000908      *
006800991214     C     KEVB          setll     fnevb000
006900991214     C     KEVB          reade     fnevb000
007000151218    3C                   DOW       NOT %eof(FNEVB06L)
007100131114     C*
007200131114      **  imposta sulla DS le note x testare il flag Progressivo
007300131114     c                   movel     evbNOT        Devb01
007400131114      *
007500151218    4c                   if         §notPRG = ' ' or §notPRG = 'U'
007600131114      **
007700060608      * Controllo che per la spedizione esistano record in TIDP301L
007800060608     C     KSPE          chain     tidp3000
007900151218    5c                   IF        NOT %found(tidp301l)
008000000127     c                   clear                   Wflageve
008100151218   x5c                   ELSE
008200000103
008300000103     c                   eval      Wdp3prn = dp3prn
008400000127     c                   eval      WEVBcev = EVBcev
008500060608     c                   clear                   tIDP3000
008600000127
008700000127      * Controlli per cod. evento che genera giacenza                       ti
008800000127     C                   eval      yy = 1
008900000127     c     WEVBcev       lookup    K2A(yy)                                31
009000130910      *
009100000127      *  Se l'evento non genera giacenza flaggo ed elaboro
009200151218    6C                   IF        *IN31 = *off
009300000127     c                   eval      Welabora = 'S'
009400000127     c                   eval      Wflageve = 'S'
009500151218   x6c                   ELSE
009600000127      *
009700000127     C     *iso          movel     EVBdtv        Wdata
009800000127     C     Wdata         subdur    wdataoggi     wday:*D
009900000127      *
010000050309     c     KSPE          chain     tigcp000
010100151218    7c                   IF         %found(tigcp01l)
010200000127     c                   z-add     GCPmgc        W0080
010300000127     c                   movel     GCPagc        W0080
010400151218    7c                   ENDIF
010500000127      *
010600151218    7c                   SELECT
010700130910      *
010800000127      * Se giacenza aperta dopo l'evento prendo il codice, fleggo ed elaboro
010900130910     c                   WHEN       %FOUND(tigcp01l) and  (EVBdev <= W0080  or
011000000128     c                             (GCPdur > *zeros  and  EVBdev <= GCPdur))
011100130910      *
011200000127     c                   eval      WEVBcev = GCPcmc
011300000127     c                   eval      Welabora = 'S'
011400000127     c                   eval      Wflageve = 'S'
011500130910      *
011600000127      * Se sono passati più di 7 gg dalla data evento fleggo e non elaboro
011700000127     c                   WHEN       Wday > 7
011800000127     c                   clear                   Welabora
011900000127     c                   eval      Wflageve = 'S'
012000130910      *
012100000127      * Se non sono passati 7 gg dalla data evento non fleggo e non elaboro
012200000127     c                   OTHER
012300000127     c                   clear                   Welabora
012400000127     c                   clear                   Wflageve
012500151218    7c                   ENDSL
012600000127      *
012700151218    6c                   ENDIF
012800991214
012900000127      * Proseguo l'elaborazione in base ai controlli sulla giacenza
013000151218    6c                   IF        Welabora = 'S'
013100000127
013200991214      * Controllo che l'evento sia da tradurre                              ti
013300991215     C                   eval      yy = 1
013400000127     c     WEVBcev       lookup    K2Z(yy)                                31
013500151218    7C                   IF        *IN31 = *on
013600991214
013700991214     C                   clear                   WEC
013800991214     C                   z-add     1             XX
013900991214     C                   eval      WEC(xx) = D2Z(yy)
014000991214      *
014100991214      * Elaboro evento + eventuali eventi collegati
014200151218    8C                   DO        XX            YY
014300991214
014400991214      * Aggancio tabella eventi DPD
014500991214     C                   eval      JJ = 1
014600991214     C                   movel     WEC(yy)       DSWEC
014700991214     c     DSWEC_EV3     lookup    KEVD(JJ)                               31
014800151218    9C                   IF        *IN31 = *on
014900991214
015000991214     C                   movel     EVD(JJ)       DEVD
015100991214      *
015200991214      *  Verifico se l' evento deve essereci solo una volta
015300151218   10C                   IF        DSWEC_UNI = 'S'
015400991215     C                   clear                   DSwec_uni
015500060608     C     KSPE          reade     tidp3000
015600151218   11C                   DOW       NOT %eof(tIDP301L)
015700130910      *
015800151218   12C                   If        DP3EVD = dswec_ev3
015900991215     C                   eval      DSwec_uni = 'S'
016000991214     c                   leave
016100151218   12c                   End
016200130910      *
016300130910     C     KSPE          reade     tidp3000
016400151218   11c                   ENDDO
016500151218   10C                   ENDIF
016600991214      *
016700991214      * Scrivo record di FIDP3
016800151218   10C                   IF        DSWEC_UNI <> 'S'
016900991214     C                   exsr      wrt_evedpd
017000151218   10C                   ENDIF
017100991214      *
017200991214      * Carico schiera con eventi collegati
017300991214     C                   If             §evdie1 <> *blanks
017400991214     C                   add       1             XX
017500991214     C                   eval      WEC(xx) = (§evdie1 + §evdis1)
017600991214     c                   Endif
017700130910      *
017800991215     C                   If             §evdie2 <> *blanks
017900991214     C                   add       1             XX
018000991214     C                   eval      WEC(xx) = (§evdie2 + §evdis2)
018100991214     c                   Endif
018200991214      *
018300151218    9c                   ENDIF
018400151218    8C                   ENDDO
018500991214      *
018600151218    7c                   ENDIF
018700151218    6c                   ENDIF
018800151218    5c                   ENDIF
018900131114      *
019000151218    4c                   end
019100991214      *
019200000127      * Fleggo evento tranne se:
019300060608      *    - non esiste record in TIDP3
019400000127      *    - genera giacenza e se entro 7 gg non è stata ancora aperta
019500040910      *    -   se la data dell'evento è futura rispetto alla data dell'invio dello SCAN
019600131114      *    - NUOVO CONCETTO derivato dai PDA con più EVENTI nella giornata il PRIMO NON
019700131114      *      deve più essere preso in considreazione.... quindi lo flaggo.
019800151218    4c                   IF         Wflageve = 'S'
019900131114     c                                or
020000131114     c                              §notPRG <>' ' and §notPRG <>'U'
020100991214     C                   z-add     WdataGG       EVBdtc
020200991214     C                   update    fnevb000
020300151218    4c                   Endif
020400991210      *
020500991214     C     KEVB          reade     fnevb000
020600151218    3c                   ENDDO
020700971215      *
020800151218    2c                   ENDDO
020900000908      *
021000151218    1c                   ENDIF
021100020918      *
021200020918     c     fine          tag
021300991129     C                   eval      *INLR = *ON
021400991214      *----------------------------------------------------*
021500991214      *   AGGIORNA FILE EVENTI DPD                         *
021600991214      *----------------------------------------------------*
021700991214     C     WRT_evedpd    BEGSR
021800991214      *
021900040910      *  Non si devono inviare SCAN con data superiore a quella del giorno di invio
022000040910     c                   if        evbdev <= WdataGG
022100061031
022200061031      * ? Controlla se il Pacco non è mai arrivato a Bartolini (Anomalia 7)
022300061031     c                   clear                   anomalia_si       1
022400151211     c                   clear                   datasped          8 0
022500160114     c                   clear                   hmcsped
022600160114     c                   clear                   dcmsped           8 0
022800131106     C     KSPE          chain     fnarb01l
022900151211     c                   if        %Found(fnarb01l)
023000151211     c                   movel     arbaas        datasped
023100151211     c                   move      arbmgs        datasped
023200160114     c                   move      arbhmc        hmcsped
023300160114     c                   move      arbdcm        dcmsped
023500151211     c                   if        arbcca ='7'
023600061031     c                   eval      anomalia_si ='S'
023700061031     c                   end
023800151211     c                   end
023900130910
024000061031     c                   if        anomalia_si =' '
024100131106     C     KSPE          chain     fnblp01l
024200151211     c                   if        %Found(fnblp01l)
024300151211     c                   movel     blpaas        datasped
024400151211     c                   move      blpmgs        datasped
024500160114     c                   move      blphmc        hmcsped           4 0
024600160114     c                   move      blpdcm        dcmsped
024800151211     c                   if        blpcca ='7'
024900061031     c                   eval      anomalia_si ='S'
025000061031     c                   end
025100151211     c                   end
025200061031     c                   end
025300061031      *
025400061031      * ?  e non devono avere anomalie 7 (ossia non presente il Parcel)
025500061031     c                   if        anomalia_si =' '
025600060608     c                   clear                   tidp3000
025700991214     c                   eval      DP3aas = EVbaas
025800991214     c                   eval      DP3lnp = EVblnp
025900991214     c                   eval      DP3nrs = EVbnrs
026000991214     c                   eval      DP3nsp = EVbnsp
026100991214     c                   eval      DP3evd = dswec_ev2
026200991217     c                   eval      DP3cev = WEVBcev
026300991214     c                   eval      DP3fle = EVBfle
026400160114
026500160114     c* verifico se distinta fittizia
026600160114     c                   clear                   apdpdd
026700160114     c                   if        (evbcev='MIC' or evbcev='NIC')
026800160115     c                             and  §NOTPDC>*zeros
026900160114     c                   monitor
027000160114     c                   movel     §notpdc       kpdc
027100160114     c                   on-error
027200160114     c                   clear                   kpdc
027300160114     c                   endmon
027400160114     c                   if        kpdc>0
027500160114     c     kapd          chain     fiapd01l
027600160114     c                   endif
027700160114     c                   endif
027800160114     c
027900160108     c* Se si tratta di MIC o NIC --> utilizzo data e ora immissione
028000160114     c*  se già presente ora consegna, il MIC non deve avere ora >
028100160114     c                   select
028200160114     c                   when      evbcev<>'MIC' and evbcev<>'NIC'
028300160114     c                   eval      DP3dev = EVBdev
028400160114     c                   eval      DP3hev = EVBhev
028500160114     c
028600160114     c                   when      evbdev<>evbdtv
028700160114     c                   eval      DP3dev = EVBdev
028800160114     c                   eval      DP3hev = EVBhev
028900160114
029000160114     c                   when      apdpdd='S'
029100160114     c                   eval      DP3dev = EVBdtv
029200160114     c                   eval      DP3hev = %int(%subst(%editc(EVBorv:'X'):1:4))
029300160114
029400160114     c                   when      dcmsped>0 and evbdtv>dcmsped
029500160114     c                   eval      DP3dev = EVBdev
029600160114     c                   eval      DP3hev = EVBhev
029700160114     c                   when      dcmsped>0 and evbdtv=dcmsped
029800160114     c                             and hmcsped<
029900160114     c                             %int(%subst(%editc(EVBorv:'X'):1:4))
030000160114     c                   eval      DP3dev = EVBdev
030100160114     c                   eval      DP3hev = EVBhev
030200160114     c
030300160114     c                   other
030400160114     c* solo in qeusto caso uso data e ora immissione
030500160114     c                   eval      DP3dev = EVBdtv
030600160114     c                   eval      DP3hev = %int(%subst(%editc(EVBorv:'X'):1:4))
030700160114     c                   endsl
030800160114     c***
030900160114     c***                if        (evbcev='MIC' or evbcev='NIC')
031000160114     c***                          and evbdev=evbdtv and (hmcsped=0 or
031100160114     c***                          hmcsped>evborv)
031200160114     c***
031300160114     c***                eval      DP3dev = EVBdtv
031400160114     c***                eval      DP3hev = %int(%subst(%editc(EVBorv:'X'):1:4))
031500160114     c***                else
031600160114     c***
031700160114     c***                eval      DP3dev = EVBdev
031800160114     c***                eval      DP3hev = EVBhev
031900160114     c***                endif
032000160108     c
032100000103     c                   eval      DP3prn = Wdp3prn
032200151211      *
032300151211     C                   if        datasped >0
032400151211     c                   movel     datasped      DP3DTSPE
032500151211     c                   else
032600151211     c                   movel     WdataGG       DP3DTSPE
032700151211     c                   end
032800151211      *
032900160108     c* non eseguo la  scrittura del MIC o NIC se si tratta di distinta fittizia
033000160108     c
033100060608     c                   WRITE     tidp3000
033200160108     c
033300061031     c                   end
033400991213      *
033500040910     c                   else
033600040910      *  Non deve flaggare EVB per rileggerlo nei giorni seguenti
033700040910      *   finchè tutto è OK per la trasmissione
033800040910     c                   clear                   Wflageve
033900040910     c                   end
034000040910      *
034100991213     C                   ENDSR
034200930126      *----------------------------------------------------*
034300930126      *   DEFINIZIONE CHIAVI                               *
034400930126      *----------------------------------------------------*
034500940321     C     *INZSR        BEGSR
034600940321      *
034700991129     c     *ENTRY        PLIST
034800991130     c                   PARM                    KPJBA
034900991213      *
035000991213     C     KTAB          KLIST
035100991213     C                   KFLD                    CODUT
035200991213     C                   KFLD                    KCOD
035300991213      *
035400991213     C     KEVB          KLIST
035500991213     C                   KFLD                    KDTC
035600991213     C                   KFLD                    KLNP
035700991129      *
035800991129     C     KSPE          KLIST
035900991214     C                   KFLD                    evbAAS
036000991214     C                   KFLD                    evbLNP
036100991214     C                   KFLD                    evbNRS
036200991214     C                   KFLD                    evbNSP
036300151211      *
036400151211     C     Kdp3          KLIST
036500151211     C                   KFLD                    dp3AAS
036600151211     C                   KFLD                    dp3LNP
036700151211     C                   KFLD                    dp3NRS
036800151211     C                   KFLD                    dp3NSP
036900160108     c     kapd          klist
037000160108     c                   kfld                    ktip
037100160108     c                   kfld                    kpdc
037200160108     c                   eval      ktip='A'
037300991213      * Dati società
037400991213     C                   Z-ADD     1             CODUT
037500991213     C                   CALL      'X§PARUT'
037600991213     C                   PARM                    UT§DSE0F
037700991213     C                   MOVEL     REC80         CNCR80
037800020919     C                   MOVEL     kpjbu         KPJBUs
037900020918      *
038000020716      *   la ricerca della filiale "DPD".
038100020716     C                   clear                   xx
038200130912     c                   read      azorg01l
038300130912     c                   dow       not %EoF(azorg01l)
038400020716     C                   movel     ORGDE3        OG143
038500020716     c                   If        §ogNTW = 'DPD'
038600020716     C                   eval      xx = xx + 1
038700020716     C                   eval      OFil(xx) = ORGfil
038800020716     c                   endif
038900130912     c                   read      azorg01l
039000020716     c                   enddo
039100020716      *
039200000908     C                   z-add     xx            FinoXX
039300000126      *
039400000127      * Carico Tabella Causali Eventi "2A" solo i cod. che generano giacenza
039500000126     C                   clear                   XX
039600000126     C                   MOVE      '2A'          KCOD
039700000126      *
039800000126     C     ktab          SETLL     TABEL00F
039900000126     C     ktab          READE     TABEL00F
040000000126     C                   DOW       NOT %eof
040100000127     c                   IF         tblflg = *blanks
040200000126     c                   movel     tbluni        ds2a
040300000127     c                   IF         §2Aftc = 'G'
040400000126     C                   add       1             XX
040500000126     C                   movel     TBLKEY        K2A(xx)
040600000127     c                   ENDIF
040700000127     c                   ENDIF
040800000126      *
040900000126     C     ktab          reade     TABEL00F
041000000126     C                   ENDDO
041100991213      *
041200991213      * Carico Continuazione Tabella Causali Eventi "2Z"
041300991213     C                   clear                   XX
041400991213     C                   MOVE      '2Z'          KCOD
041500991213      *
041600991213     C     ktab          SETLL     TABEL00F
041700991213     C     ktab          READE     TABEL00F
041800991213     C                   DOW       NOT %eof
041900000127     c                   IF         tblflg = *blanks
042000991213     c                   movel     tbluni        ds2z
042100000217     c                   IF        §2zevd <> *blanks
042200991213     C                   add       1             XX
042300991213     C                   movel     TBLKEY        K2Z(xx)
042400991213     C                   movel     §2Zevd        D2Z(xx)
042500991214     C                   ENDIF
042600000127     C                   ENDIF
042700991213      *
042800991213     C     ktab          reade     TABEL00F
042900991213     C                   ENDDO
043000991213      *
043100991213      * Carico Tabella Eventi DPD
043200991213     C                   clear                   XX
043300991213     c     'EVD'         chain     tntbe000                           31
043400991213      *
043500991213     c                   DOW       *in31 = *off
043600991213      * Se il S.I. è indicato deve essere uguale al mio
043700991213     c                   IF        TBEatb = *BLANKS  and
043800991213     c                              (TBEsif = Knsif  or  TBEsif = *blanks)
043900991214      * Carico solo eventi da tradurre in import
044000991214     c                   movel     TBEuni        Devd
044100991214     c                   IF        §evditr = 'S'
044200991213     c                   add       1             xx
044300991214     c                   movel     TBEke1        KEVD(XX)
044400991214     c                   movel     TBEuni        EVD(XX)
044500991213     c                   ENDIF
044600991214     c                   ENDIF
044700991213      *
044800991213     c     'EVD'         reade     tntbe000                               31
044900991213     c                   ENDDO
045000991213      *
045100991214      * DATA DEL GIORNO
045200991214     C                   TIME                    W0140
045300991214     C                   MOVE      W0140         Wdatagg
045400991214     C     *eur          MOVEL     Wdatagg       Wdataoggi
045500991214     C     *iso          MOVEL     Wdataoggi     Wdatagg
045600991214      *
045700020918     C     endinz        ENDSR
045800941230      *-------------------------------------------------------------------------
