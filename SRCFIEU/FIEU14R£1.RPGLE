000100991124     H DECEDIT('0,') DATEDIT(*DMY/)
000200981217      ***************************************************************************
000300991213      *  CREA EVENTI DI MANCATA CONSEGNA PER IMPORT DPD                         *
000400971216      ***************************************************************************
000500991214     FFNEVB06L  UF   E           K DISK
000600060608     FtIDP301L  UF A E           K DISK
000700050309     Ftigcp01L  IF   E           K DISK
000800061026     FfnBLP01L  IF   E           K DISK
000900061031     FfnARB01L  IF   E           K DISK
001000000127     FTABEL00F  IF   E           K DISK
001100991213     FTNTBE01L  IF   E           K DISK
001200020716     FAZORG01L  IF   E           K DISK
001300940321      *----------------------------------------------------*
001400000126     D K2A             S              3    DIM(200) inz                         CODICE NAZIONE
001500000126     D K2Z             S              3    DIM(200) inz                         CODICE NAZIONE
001600000126     D D2Z             S              3    DIM(200) inz                         CODICE NAZIONE
001700991214     D KEVD            S              3    DIM(30)  inz                         CODICE NAZIONE
001800991214     D EVD             S            256    DIM(30)  inz                         CODICE NAZIONE
001900991214     D WEC             S              4    DIM(30)  inz                         CODICE NAZIONE
002000000908      *-------------------
002100000908     D OFil            S              3S 0 DIM(100)  inz                        Filiali da AZORG
002200991213      *-------------------
002300000915     D yx              S              3  0 inz
002400000915     D XX              S              3  0 inz
002500000908     D FinoXX          S              3  0 inz
002600991214     D YY              S              3  0 inz
002700991214     D JJ              S              3  0 inz
002800991214     D Wdatagg         S              8  0 inz
002900000127     D W0080           S              8  0 inz
003000000127     D W0140           S             14  0 inz
003100000127     D Wday            S              5  0
003200000127     D Wflageve        S              1
003300000127     D Welabora        S              1
003400991214     D Wdataoggi       S               D   DATFMT(*eur)
003500000127     D Wdata           S               D   DATFMT(*eur)
003600991217     D Wevbcev         S                   LIKE(EVBcev)
003700000103     D Wdp3prn         S                   LIKE(DP3prn)
003800991213     D kdtc            S                   LIKE(EVBdtc)  INZ
003900991213     D klnp            S                   LIKE(EVBlnp)
004000991213     D Kcod            S                   LIKE(TBLCOD)
004100020919     D kpjbus          S                   LIKE(kpjbu)
004200131114      * Note degli Eventi
004300131114     D Devb01        e ds
004400971212      *-------------------
004500991214     D                 DS
004600991214     D DSWEC_EV2               1      2
004700991214     D DSWEC_EV3               1      3
004800991214     D DSWEC_UNI               4      4
004900991214     D DSWEC                   1      4
005000991214      *-------------------
005100991130     D KPJBA         E DS
005200991213     D DEVD          E DS
005300991213     D CNCR80        E DS
005400991213     D UT§DSE0F      E DS
005500991214     D OG143         E DS
005600000126     D DS2A          E DS
005700000126     D DS2Z          E DS
005800911213      *----------------------------------------------------*
005900020801     c                   If        FinoXX > 0
006000000908      *
006100000908      *  per gestire più P.O. sulla stessa filiale
006200000915     C                   DO        FinoXX        yx
006300000915     C                   eval      Klnp = OFil(yx)
006400000908      *
006500991214     C     KEVB          setll     fnevb000
006600991214     C     KEVB          reade     fnevb000
006700991214     C                   DOW       NOT %eof(FNEVB06L)
006800131114     C*
006900131114      **  imposta sulla DS le note x testare il flag Progressivo
007000131114     c                   movel     evbNOT        Devb01
007100131114      *
007200131114     c                   if         §notPRG = ' ' or §notPRG = 'U'
007300131114      **
007400060608      * Controllo che per la spedizione esistano record in TIDP301L
007500060608     C     KSPE          chain     tidp3000
007600130910     c                   IF        NOT %found(tidp301l)
007700000127     c                   clear                   Wflageve
007800000127     c                   ELSE
007900000103
008000000103     c                   eval      Wdp3prn = dp3prn
008100000127     c                   eval      WEVBcev = EVBcev
008200060608     c                   clear                   tIDP3000
008300000127
008400000127      * Controlli per cod. evento che genera giacenza                       ti
008500000127     C                   eval      yy = 1
008600000127     c     WEVBcev       lookup    K2A(yy)                                31
008700130910      *
008800000127      *  Se l'evento non genera giacenza flaggo ed elaboro
008900000127     C                   IF        *IN31 = *off
009000000127     c                   eval      Welabora = 'S'
009100000127     c                   eval      Wflageve = 'S'
009200000127     c                   ELSE
009300000127      *
009400000127     C     *iso          movel     EVBdtv        Wdata
009500000127     C     Wdata         subdur    wdataoggi     wday:*D
009600000127      *
009700050309     c     KSPE          chain     tigcp000
009800130910     c                   IF         %found(tigcp01l)
009900000127     c                   z-add     GCPmgc        W0080
010000000127     c                   movel     GCPagc        W0080
010100000127     c                   ENDIF
010200000127      *
010300000127     c                   SELECT
010400130910      *
010500000127      * Se giacenza aperta dopo l'evento prendo il codice, fleggo ed elaboro
010600130910     c                   WHEN       %FOUND(tigcp01l) and  (EVBdev <= W0080  or
010700000128     c                             (GCPdur > *zeros  and  EVBdev <= GCPdur))
010800130910      *
010900000127     c                   eval      WEVBcev = GCPcmc
011000000127     c                   eval      Welabora = 'S'
011100000127     c                   eval      Wflageve = 'S'
011200130910      *
011300000127      * Se sono passati più di 7 gg dalla data evento fleggo e non elaboro
011400000127     c                   WHEN       Wday > 7
011500000127     c                   clear                   Welabora
011600000127     c                   eval      Wflageve = 'S'
011700130910      *
011800000127      * Se non sono passati 7 gg dalla data evento non fleggo e non elaboro
011900000127     c                   OTHER
012000000127     c                   clear                   Welabora
012100000127     c                   clear                   Wflageve
012200000127     c                   ENDSL
012300000127      *
012400000127     c                   ENDIF
012500991214
012600000127      * Proseguo l'elaborazione in base ai controlli sulla giacenza
012700000127     c                   IF        Welabora = 'S'
012800000127
012900991214      * Controllo che l'evento sia da tradurre                              ti
013000991215     C                   eval      yy = 1
013100000127     c     WEVBcev       lookup    K2Z(yy)                                31
013200991214     C                   IF        *IN31 = *on
013300991214
013400991214     C                   clear                   WEC
013500991214     C                   z-add     1             XX
013600991214     C                   eval      WEC(xx) = D2Z(yy)
013700991214      *
013800991214      * Elaboro evento + eventuali eventi collegati
013900991214     C                   DO        XX            YY
014000991214
014100991214      * Aggancio tabella eventi DPD
014200991214     C                   eval      JJ = 1
014300991214     C                   movel     WEC(yy)       DSWEC
014400991214     c     DSWEC_EV3     lookup    KEVD(JJ)                               31
014500991214     C                   IF        *IN31 = *on
014600991214
014700991214     C                   movel     EVD(JJ)       DEVD
014800991214      *
014900991214      *  Verifico se l' evento deve essereci solo una volta
015000991214     C                   IF        DSWEC_UNI = 'S'
015100991215     C                   clear                   DSwec_uni
015200060608     C     KSPE          reade     tidp3000
015300060608     C                   DOW       NOT %eof(tIDP301L)
015400130910      *
015500991214     C                   If        DP3EVD = dswec_ev3
015600991215     C                   eval      DSwec_uni = 'S'
015700991214     c                   leave
015800130910     c                   End
015900130910      *
016000130910     C     KSPE          reade     tidp3000
016100991214     C                   ENDDO
016200991214     C                   ENDIF
016300991214      *
016400991214      * Scrivo record di FIDP3
016500991214     C                   IF        DSWEC_UNI <> 'S'
016600991214     C                   exsr      wrt_evedpd
016700991214     C                   ENDIF
016800991214      *
016900991214      * Carico schiera con eventi collegati
017000991214     C                   If             §evdie1 <> *blanks
017100991214     C                   add       1             XX
017200991214     C                   eval      WEC(xx) = (§evdie1 + §evdis1)
017300991214     c                   Endif
017400130910      *
017500991215     C                   If             §evdie2 <> *blanks
017600991214     C                   add       1             XX
017700991214     C                   eval      WEC(xx) = (§evdie2 + §evdis2)
017800991214     c                   Endif
017900991214      *
018000991214     c                   ENDIF
018100991214     C                   ENDDO
018200991214      *
018300991213     c                   ENDIF
018400000127     c                   ENDIF
018500000127     c                   ENDIF
018600131114      *
018700131114     c                   end
018800991214      *
018900000127      * Fleggo evento tranne se:
019000060608      *    - non esiste record in TIDP3
019100000127      *    - genera giacenza e se entro 7 gg non è stata ancora aperta
019200040910      *    -   se la data dell'evento è futura rispetto alla data dell'invio dello SCAN
019300131114      *    - NUOVO CONCETTO derivato dai PDA con più EVENTI nella giornata il PRIMO NON
019400131114      *      deve più essere preso in considreazione.... quindi lo flaggo.
019500000127     c                   IF         Wflageve = 'S'
019600131114     c                                or
019700131114     c                              §notPRG <>' ' and §notPRG <>'U'
019800991214     C                   z-add     WdataGG       EVBdtc
019900991214     C                   update    fnevb000
020000000127     c                   Endif
020100991210      *
020200991214     C     KEVB          reade     fnevb000
020300991210     c                   ENDDO
020400971215      *
020500000908     c                   ENDDO
020600000908      *
020700991213     c                   ENDIF
020800020918      *
020900020918     c     fine          tag
021000991129     C                   eval      *INLR = *ON
021100991214      *----------------------------------------------------*
021200991214      *   AGGIORNA FILE EVENTI DPD                         *
021300991214      *----------------------------------------------------*
021400991214     C     WRT_evedpd    BEGSR
021500991214      *
021600040910      *  Non si devono inviare SCAN con data superiore a quella del giorno di invio
021700040910     c                   if        evbdev <= WdataGG
021800061031
021900061031      * ? Controlla se il Pacco non è mai arrivato a Bartolini (Anomalia 7)
022000061031     c                   clear                   anomalia_si       1
022100131106     C     KSPE          chain     fnarb01l
022200131106     c                   if        %Found(fnarb01l) and
022300131106     c                             arbcca ='7'
022400061031     c                   eval      anomalia_si ='S'
022500061031     c                   end
022600130910
022700061031     c                   if        anomalia_si =' '
022800131106     C     KSPE          chain     fnblp01l
022900131106     c                   if        %Found(fnblp01l) and
023000131106     c                             blpcca ='7'
023100061031     c                   eval      anomalia_si ='S'
023200061031     c                   end
023300061031     c                   end
023400061031      *
023500061031      * ?  e non devono avere anomalie 7 (ossia non presente il Parcel)
023600061031     c                   if        anomalia_si =' '
023700060608     c                   clear                   tidp3000
023800991214     c                   eval      DP3aas = EVbaas
023900991214     c                   eval      DP3lnp = EVblnp
024000991214     c                   eval      DP3nrs = EVbnrs
024100991214     c                   eval      DP3nsp = EVbnsp
024200991214     c                   eval      DP3evd = dswec_ev2
024300991217     c                   eval      DP3cev = WEVBcev
024400991214     c                   eval      DP3fle = EVBfle
024500991214     c                   eval      DP3dev = EVBdev
024600991214     c                   eval      DP3hev = EVBhev
024700000103     c                   eval      DP3prn = Wdp3prn
024800060608     c                   WRITE     tidp3000
024900061031     c                   end
025000991213      *
025100040910     c                   else
025200040910      *  Non deve flaggare EVB per rileggerlo nei giorni seguenti
025300040910      *   finchè tutto è OK per la trasmissione
025400040910     c                   clear                   Wflageve
025500040910     c                   end
025600040910      *
025700991213     C                   ENDSR
025800930126      *----------------------------------------------------*
025900930126      *   DEFINIZIONE CHIAVI                               *
026000930126      *----------------------------------------------------*
026100940321     C     *INZSR        BEGSR
026200940321      *
026300991129     c     *ENTRY        PLIST
026400991130     c                   PARM                    KPJBA
026500991213      *
026600991213     C     KTAB          KLIST
026700991213     C                   KFLD                    CODUT
026800991213     C                   KFLD                    KCOD
026900991213      *
027000991213     C     KEVB          KLIST
027100991213     C                   KFLD                    KDTC
027200991213     C                   KFLD                    KLNP
027300991129      *
027400991129     C     KSPE          KLIST
027500991214     C                   KFLD                    evbAAS
027600991214     C                   KFLD                    evbLNP
027700991214     C                   KFLD                    evbNRS
027800991214     C                   KFLD                    evbNSP
027900991213      * Dati società
028000991213     C                   Z-ADD     1             CODUT
028100991213     C                   CALL      'X§PARUT'
028200991213     C                   PARM                    UT§DSE0F
028300991213     C                   MOVEL     REC80         CNCR80
028400020919     C                   MOVEL     kpjbu         KPJBUs
028500020918      *
028600020716      *   la ricerca della filiale "DPD".
028700020716     C                   clear                   xx
028800130912     c                   read      azorg01l
028900130912     c                   dow       not %EoF(azorg01l)
029000020716     C                   movel     ORGDE3        OG143
029100020716     c                   If        §ogNTW = 'DPD'
029200020716     C                   eval      xx = xx + 1
029300020716     C                   eval      OFil(xx) = ORGfil
029400020716     c                   endif
029500130912     c                   read      azorg01l
029600020716     c                   enddo
029700020716      *
029800000908     C                   z-add     xx            FinoXX
029900000126      *
030000000127      * Carico Tabella Causali Eventi "2A" solo i cod. che generano giacenza
030100000126     C                   clear                   XX
030200000126     C                   MOVE      '2A'          KCOD
030300000126      *
030400000126     C     ktab          SETLL     TABEL00F
030500000126     C     ktab          READE     TABEL00F
030600000126     C                   DOW       NOT %eof
030700000127     c                   IF         tblflg = *blanks
030800000126     c                   movel     tbluni        ds2a
030900000127     c                   IF         §2Aftc = 'G'
031000000126     C                   add       1             XX
031100000126     C                   movel     TBLKEY        K2A(xx)
031200000127     c                   ENDIF
031300000127     c                   ENDIF
031400000126      *
031500000126     C     ktab          reade     TABEL00F
031600000126     C                   ENDDO
031700991213      *
031800991213      * Carico Continuazione Tabella Causali Eventi "2Z"
031900991213     C                   clear                   XX
032000991213     C                   MOVE      '2Z'          KCOD
032100991213      *
032200991213     C     ktab          SETLL     TABEL00F
032300991213     C     ktab          READE     TABEL00F
032400991213     C                   DOW       NOT %eof
032500000127     c                   IF         tblflg = *blanks
032600991213     c                   movel     tbluni        ds2z
032700000217     c                   IF        §2zevd <> *blanks
032800991213     C                   add       1             XX
032900991213     C                   movel     TBLKEY        K2Z(xx)
033000991213     C                   movel     §2Zevd        D2Z(xx)
033100991214     C                   ENDIF
033200000127     C                   ENDIF
033300991213      *
033400991213     C     ktab          reade     TABEL00F
033500991213     C                   ENDDO
033600991213      *
033700991213      * Carico Tabella Eventi DPD
033800991213     C                   clear                   XX
033900991213     c     'EVD'         chain     tntbe000                           31
034000991213      *
034100991213     c                   DOW       *in31 = *off
034200991213      * Se il S.I. è indicato deve essere uguale al mio
034300991213     c                   IF        TBEatb = *BLANKS  and
034400991213     c                              (TBEsif = Knsif  or  TBEsif = *blanks)
034500991214      * Carico solo eventi da tradurre in import
034600991214     c                   movel     TBEuni        Devd
034700991214     c                   IF        §evditr = 'S'
034800991213     c                   add       1             xx
034900991214     c                   movel     TBEke1        KEVD(XX)
035000991214     c                   movel     TBEuni        EVD(XX)
035100991213     c                   ENDIF
035200991214     c                   ENDIF
035300991213      *
035400991213     c     'EVD'         reade     tntbe000                               31
035500991213     c                   ENDDO
035600991213      *
035700991214      * DATA DEL GIORNO
035800991214     C                   TIME                    W0140
035900991214     C                   MOVE      W0140         Wdatagg
036000991214     C     *eur          MOVEL     Wdatagg       Wdataoggi
036100991214     C     *iso          MOVEL     Wdataoggi     Wdatagg
036200991214      *
036300020918     C     endinz        ENDSR
036400941230      *-------------------------------------------------------------------------
