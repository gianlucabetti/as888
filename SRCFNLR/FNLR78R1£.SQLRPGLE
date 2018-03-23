000100110727     h dftactgrp(*no) actgrp(*new)
000200110923     H* fnlr78r1*-----------------------------------------------------*
000300110927     H* Verifica se spedizioni consegnabili per un perfect order
000400930225     H*---------------------------------------------------------------*
000500110927     FFNEVB01L  IF   E           K DISK
000600110926     Ftntbe01l  IF   E           K DISK
000700110927     FTABEL00F  IF   E           K DISK
000800111012     FFNfvv01L  IF   E           K DISK
000900111012     FFNfvv08L  IF   E           K DISK    RENAME(FNfvv000:FNfvv008)
001000111012     Ffnart01l  IF   E           K DISK
001100110726     Ffnarb01l  IF   E           K DISK    prefix(£)
001200110927     f
001300900514     D KPJBA         E DS
001400061002     d* Parametri x Controllo profilo utenti
001500070302     d TIBS34ds      e ds
001600061002     d AZUTEds       e ds                  extname(AZUTE00F)
001700061002     d DDatiUte      e ds
001800110927     d fndctds       e ds                  extname(fndct00F)
001900110927     d fnarbds       e ds                  extname(fnarb00F)
002000110726     d fnarbds£      e ds                  extname(fnarb00F) prefix(£)
002100110726     d fnarbds$      e ds                  extname(fnarb00F) prefix($)
002200110926
002300110926     D fnlr78ds1     E DS
002400110926
002500110926     d trul52ds      e ds
002600110926     d dxcodta       e ds
002700110926     D tnsd99ds      E DS                  inz
002800110927     d DS2a          E DS
002900110927     d DS2Z          E DS
003000111012     D FNLV53DS      E DS
003100110927     d
003200110927      * ds per il controllo della presenza di CA per la spedizione richiesta
003300110927     d FIDN12DS      e ds
003400110927     d  skKey                 26    305    dim(20)
003500110927     d  skAnn                306    325    dim(20)
003600110927     d  skDca                326    485  0 dim(20)
003700110927     d  skFca                486    545  0 dim(20)
003800110927     d  skDch                546    705  0 dim(20)
003900110927     d  skCch                706    745    dim(20)
004000110927
004100110927     d dsKey           ds
004200110927     d  dsaac                         4  0
004300110927     d  dsfil                         3  0
004400110927     d  dsnca                         7  0
004500110927      *
004600110926     d
004700111013     d wdti            s              8  0
004800111013     d wdsp            s              8  0
004900111013     d dctdca          s              8  0
005000110927     d dateu           s              8  0
005100110926     d ora_u           s              6  0
005200110926     d conta           s              1  0
005300110926     d Bollacons       s              1
005400110927     d NOcons          s              1
005500111012     d Mancacollo      s              1
005600110927     d numora          s                   like(evbhev)
005700110927     d xco             s                   like(arbxco)
005800110727     d COMNSP          s                   like(arbNSP)
005900110727     d COMRMA          s                   like(arbRMA)
006000111012     D knpg            S                   LIKE(fvvnpg) inz(2)
006100111012     D Wartdam         S                   LIKE(artdam)
006200111012     D Kdataloc        S                   LIKE(artdam)
006300111012     D wtipolancio     s              2
006400111013     D NOconsMsg       s                   like(olr78msg)
006500111012     d
006600110926     d Datasys         s               d   datfmt(*iso)  inz(*sys)
006700110926     d Ora_sys         s               t   timfmt(*iso)  inz(*sys)
006800110926     d Ora_iso         s               t   timfmt(*iso)
006900110926     d Dataiso         s               d   datfmt(*iso)
007000070130     C**********************************************************************
007100110927     C     *ENTRY        PLIST
007200110927     C                   PARM                    kpjba
007300110927     C                   PARM                    fnarbds$
007400110927     c
007500110927     c                   movel     kpjbu         fnlr78ds1
007600110927     c
007700110927     c* C= solo chiusura file
007800110927   0bc                   if        ilr78tla<> 'C'
007900110927     c
008000070131     c                   exsr      srinz
008100110927     c
008200110927   0ac                   if        olr78err=' '
008300110926     c
008400111012     c* Controllo se sono passati i giorni per la bolla --> già consegnabile
008500111013     c                   eval      wdti=arbdti
008600111013     c                   eval      wdsp=(arbaas+10000)+arbmgs
008700110926     c                   exsr      CTRgiorni
008800110926     c
008900110926    0c                   if        Bollacons='S'
009000110926     c                   eval      olr78conso='S'
009100110926   x0c                   else
009200110926     c
009300110726     c* 1 bolla con cui sto verificando le sorelle
009400110726     c                   eval      conta = 1
009500110727     c                   eval      COMRMA = ARBRMA
009600110727     c                   eval      COMNSP = ARBNSP
009700110726     c* leggo le sorelle
009800110726     C/EXEC SQL
009900110727     C+ DECLARE B1 CURSOR FOR SELECT fnarb01l.* FROM fnarb01l WHERE
010000110727     C+ arbrma = :COMrma and arbxco  <> ' ' and arbnsp <> :COMnsp
010100110726     C/END-EXEC
010200110726
010300110726     C/EXEC SQL
010400110726     C+ OPEN b1
010500110726     C/END-EXEC
010600110726
010700110926    1C                   do        *hival
010800110726
010900110726     C/EXEC SQL
011000110727     C+ FETCH NEXT FROM b1 INTO :fnarbds£
011100110726     C/END-EXEC
011200110726
011300110926    2C                   select
011400110926    2C                   when      sqlcod = 100
011500110726     c                   leave
011600110926     c
011700110926    2C                   WHEN      SqlCod < 0
011800110926    3c                   if        sqlcod <> -913
011900110726     C                   seton                                        H1
012000110926    3c                   end
012100110926     c
012200110926   x2C                   other
012300110923     c
012400110926     c* se c'e' una differenza tra gli XCO delle sorelle libero tutte
012500110926    3c                   if        £arbxco <> arbxco
012600110926     c                   eval      olr78conso= 'S'
012700111013     c                   clear                   olr78msg
012800111013     c                   eval      olr78msg = 'Numero di bolle in XCO diverso'
012900110926     c                   leave
013000110926   x3c                   else
013100110923     c
013200110912     c* conteggio sorelle per controllare XCO
013300110726     c                   add       1             conta
013400110927     c* controllo l'assegnabilità spediz.
013500110927    4c                   if        arblna = £arblna
013600111014     c                   clear                   BollaCons
013700110923     c
013800111014     c*** Per le figlie non ricontrolliamo se passati i giorni ma solo la consegnabilità
013900111014     c***                eval      wdti=£arbdti
014000111014     c***                eval      wdsp=(£arbaas+10000)+£arbmgs
014100111014     c***                exsr      Ctrgiorni
014200110926     c
014300110926     c* se passati i giorni sempre consegnabile altrimenti eseguo gli altri controlli
014400110927    5c                   if        Bollacons=' '
014500110926     C                   exsr      SRspedcon
014600110927    5c                   endif
014700110927     c
014800110927     c* Se la bolla non è consegnabile --> memorizzo che ce ne è almeno una
014900110927    5c                   if        Bollacons='N'
015000110927     c                   eval      NOcons='N'
015100111014     c                   eval      NOconsmsg='Bolla '+%editc(£arbaas:'X') +
015200111013     c                             ' ' +%editc(£arblnp:'X') +
015300111014     c                             ' ' +%editc(£arbnrs:'4') +
015400111014     c                             ' ' +%editc(£arbnsp:'4') +
015500111014     c                             ' non consegnabile'
015600110927    5c                   endif
015700110927     c
015800110926    4c                   end
015900110926    3c                   end
016000110926    2C                   endsl
016100110726     C*
016200110926    1C                   enddo
016300110726
016400110726     C/EXEC SQL
016500110726     C+ CLOSE b1
016600110726     C/END-EXEC
016700110727     c* verifico se ho già tutte le spedizioni
016800111012     c*  se non è già stata dichiara consegnabile
016900110926    1c                   if        olr78conso<>'S'
017000110926     c
017100110727     c                   move      conta         xco
017200111027    2c                   if        xco <  arbxco
017300110926     c                   eval      olr78conso='N'
017400111013     c                   clear                   olr78msg
017500110926     c                   eval      olr78msg = 'Mancano spedizioni -
017600111013     c                             x Perfect Order'
017700111027   x2c                   else
017800110926     c
017900110927     c* A questo punto se ne ho trovata almeno una non consegnabile --> nessuna è cons
018000111027    3c                   if        olr78conso=' ' and nocons='N'
018100110927     c                   eval      olr78conso='N'
018200111013     c                   eval      olr78msg=NOconsMsg
018300110927     c                   else
018400110927     c                   eval      olr78conso='S'
018500111027    3c                   endif
018600111027    2c                   endif
018700110927     c
018800110926    1c                   endif
018900110926     c
019000110926    0c                   endif
019100110927   0ac                   endif
019200110727     c*
019300110926     c                   eval      kpjbu = fnlr78ds1
019400110927   0bc                   endif
019500110927     c
019600110927     c                   if        ilr78tla<>' '
019700110927     c                   clear                   fidn12ds
019800110927     c                   eval      i12tla='C'
019900110927     c                   call      'FIDN12R'
020000110927     c                   parm                    fidn12ds
020100110927     c
020200110927     c                   clear                   tnsd99ds
020300110927     c                   eval      d98tla='C'
020400110927     c                   call      'TNSD99R'
020500110927     c                   parm                    tnsd99ds
020600111013     c
020700111013     c                   clear                   fnlv53ds
020800111013     c                   eval      d53tla='C'
020900111013     c                   call      'FNLV53R'
021000111013     c                   parm                    fnlv53ds
021100110927     c
021200070131     c                   seton                                        lr
021300110927     c                   else
021400110927     c                   return
021500110927     c                   endif
021600110927     c
021700110926      *--------------------------------------------------------------------
021800110926      * eseguo gli altri controllo per la conseganbilità
021900110926      *--------------------------------------------------------------------
022000110926     C     SRspedcon     begsr
022100110926     c
022200110926     c* se la sorella è: - Bloccata B o *
022300110926     c*                  - fermo deposito
022400110926     c*                  - in distinta con data <=
022500110926     c*  la prima è consegnabile
022600110926     c                   if        £arbfbc='B' or £arbfbc='*' or
022700110927     c                             £arbffd<>' '   or  £arbfbc='G' or
022800110927     c                             (£arbndc>0 and £arbddc<=ilr78dtela
022900110927     c                              and £arbddc>0)
023000110926     c                   eval      Bollacons='S'
023100110926     c                   leavesr
023200110926     c                   endif
023300110927     c
023400110927     c* Se nel giorno della data elaborazione c'e' un evento --> era in consegna quindi
023500110927     c*  la prima è consegnabile
023600110927     c                   exsr      CONT3
023700110927     c
023800110927     C* EVENTO DA CONTEGGIARE  --> la sped era in consegna quel giorno
023900110927     c                   if        WWRT='S'
024000110927     c                   eval      Bollacons='S'
024100110927     c                   leavesr
024200110927     c                   endif
024300110927     c
024400110927     c* Se presente pratica di danno
024500110927     c*  la prima è consegnabile
024600110927      *
024700110927     c                   clear                   fidn12ds
024800110927     c                   eval      i12aas = £arbaas
024900110927     c                   eval      i12lnp = £arblnp
025000110927     c                   eval      i12nrs = £arbnrs
025100110927     c                   eval      i12nsp = £arbnsp
025200110927     c                   eval      i12fel = 'E'
025300110927     c                   eval      i12fan = 'E'
025400110927      *
025500110927     c                   call      'FIDN12R'
025600110927     c                   parm                    fidn12ds
025700110927      *
025800110927      * se non ci sono errori
025900110927     c                   clear                   WESCA             1
026000110927    1c                   if        o12err = ' '
026100110927      * se numero di CA trovate maggiore di zero
026200110927    2c                   if        o12nca > 0
026300110927     c
026400110927     c                   z-add     *zeros        ii                2 0
026500110927    3c     1             do        o12nca        ii
026600110927     c                   movea     skkey(ii)     dskey
026700110927     C                   Z-ADD     dsfil         dctfil
026800110927     C                   Z-ADD     dsnca         dctnca
026900110927     C                   Z-ADD     skdca(ii)     dctdca
027000110927     C                   Z-ADD     skdch(ii)     dctdch
027100110927     C                   movea     skcch(ii)     dctcch
027200110927     c
027300110927      * se CA aperta imposto il flag e vado a fine
027400110927    4c                   If        dctdch = 0
027500110927     c                   eval      wesca = 'S'
027600110927     c                   leave
027700110927     c                   else
027800110927     c                   eval      wesca = 'C'
027900110927    4c                   endif
028000110927    3C                   ENDDO
028100110927      *
028200110927    2c                   endif
028300110927    1c                   endif
028400110927     C*
028500111012     c* Se esiste c.a. aperta o chiusa --> slego le bolle come consegnabili
028600111012     c                   if        WESCA<>' '
028700110927     c                   eval      Bollacons='S'
028800110927     c                   leavesr
028900110927     c                   endif
029000110926     c
029100110926     c* Se non è ancora completamente arrivata alla data elab-->
029200110926     c*  la prima NON è consegnabile
029300110927     c                   if        £arbdti>ilr78dtela
029400110926     c                   eval      Bollacons='N'
029500110926     c                   leavesr
029600110926     c                   endif
029700111012     c
029800111012     c* Se la bolla non è completamente arrivata e considero anche le spunte
029900111012     c* partenza --> leggo dettaglio colli
030000111012     c                   clear                   Mancacollo
030100111012    1c                   if        £arbdti=0
030200111012     c
030300111012     C     KARB3         SETLL     FNART01L
030400111012     C     KARB3         READE     FNART01L                               30
030500111012     C*
030600111012    2C     *IN30         DOWEQ     *OFF
030700111012     c                   clear                   Wartdam
030800111012     c
030900111012    3c                   if        artdam>0
031000111012     c                   eval      Wartdam=artdam
031100111012   x3c                   else
031200111012     c
031300111012     c* Memorizzo la data di arrivo o l'ipotetico arrivo se collo partito
031400111012     c* su FGV abbinato di ida non ancora chiuso: come data di arrivo
031500111012     c*  prendo la data foglio arrivi, solo se richiesto
031600111012    4c                   if        artdam=0  and artdfv>0  and ilr78sppar ='S'
031700111012     c
031800111012     c* Se si tratta di bolla LOCALE, non esiste abbinamento quindi
031900111012     c* cerco di desumere la data del foglio arrivi per vedere se è
032000111012     c*  chiuso o perto
032100111012    5c                   if        £arbtfp=£arbtfa
032200111012     c                   clear                   wtipolancio
032300111012     c                   eval      kdataloc=artdfv
032400111012     c                   exsr      BOLLALOC
032500111012   X5c                   else
032600111012     C
032700111012     C* CONTROLLO SE LA BOLLA ARRIVI E RELATIVA A FOGLIO VIAGGIO ABBINATO
032800111012     C                   CLEAR                   fnlv53ds
032900111012     C                   MOVEL     'A'           D53TFO
033000111012     C                   MOVE      artNFV        D53NFV
033100111012     C                   MOVE      £arbtfp       D53FGS
033200111012     C                   MOVEL     'B'           D53ABB
033300111012     C                   MOVE      £ARBLNA       D53LNA
033400111012     C                   MOVE      £ARBTFA       D53TFA
033500111012     C                   CALL      'FNLV53R'
033600111012     C                   PARM                    fnlv53ds
033700111012     c*
033800111012    6c                   if        d53err=*blanks and d53nfa>0
033900111012     c* verifico se fogli arrivi di abbinamento già chiuso
034000111012     C     KFVV          CHAIN     FNFVV000
034100111012     C                   IF        %FOUND(FNFVV01L)
034200111012     C                   IF        FVVFCF=' '
034300111012     C                   EVAL      Wartdam=D53DFA
034400111012     C                   ENDIF
034500111012     C                   ENDIF
034600111012    6c                   endif
034700111012    5c                   endif
034800111012     c
034900111012    4c                   endif
035000111012    3c                   endif
035100111012     c
035200111012     C* SE NON ARRIVATO O ARRIVATO CON DATA > (E'COME SE FOSSE A 0 QUEL
035300111012     C*  GIORNO)
035400111012    3C     Wartdam       IFEQ      0
035500111012     C     Wartdam       ORGT      ilr78dtela
035600111012     c
035700111012     C                   eval      Mancacollo='S'
035800111012     C                   SETON                                        30
035900111012     C*
036000111012   X3C                   endif
036100111012     C**
036200111012     C  N30KARB3         READE     FNART01L                               30
036300111012    2C                   ENDDO
036400111012    c
036500111012    2c                   if        MancaCollo='S'
036600111012     c                   eval      Bollacons='N'
036700111012     c                   leavesr
036800111012     c                   endif
036900111012    2c                   endif
037000111012    1c
037100110926     c* Richiamo consegnabilità
037200110926     c                   eval      d98dfc=ilr78dtela
037300110926     c                   eval      d98ofc=2359
037400110926     c                   if        ilr78dtela=dateu
037500110926     c                   movel     ora_u         d98ofc
037600110926     c                   endif
037700110926     C*
037800110926     C                   MOVEL     ' '           D98TLA                         *TIPO LANCIO
037900110926     C                   MOVEL     'A'           D98TBO                         *TIPO BOLLA
038000111012     C                   Z-ADD     £ARBAAS       D98AAS
038100111012     C                   Z-ADD     £ARBLNP       D98LNP
038200111012     C                   Z-ADD     £ARBNRS       D98NRS
038300111012     C                   Z-ADD     £ARBNSP       D98NSP
038400110926     C                   CALL      'TNSD99R'
038500110927     C                   PARM                    tnsd99ds
038600110926     C*
038700110927     c* non consegnabile
038800110927     C                   if        D98OCI=0
038900110927     c                   eval      Bollacons='N'
039000110927     c                   endif
039100110926     C*
039200110926     c                   ENDSR
039300110926      *--------------------------------------------------------------------
039400110927      *   Controllo se passati i giorni per slegare la bolla dalle sorelle
039500110926      *--------------------------------------------------------------------
039600110926     C     CTRgiorni     begsr
039700111013     c* Verifico se passati i gg di attesa
039800110926     c
039900110926     c                   clear                   trul52ds
040000111013    1c                   if        wdti <> 0
040100110926     C                   Z-ADD     §XCODDTI      D52GIO                         *
040200111013     C                   MOVE      wdti          D52DIN                         *
040300110926     c                   else
040400110926     C                   Z-ADD     §XCODdsp      D52GIO                         *
040500111013     C                   movel     wdsp          d52din
040600110926    1c                   end
040700111013     c
040800110926     C                   CALL      'TRUL52R'
040900110926     C                   PARM                    TRUL52ds
041000110926     C                   MOVE      D52DFI        DAT1GG            8 0          *
041100110926     c
041200110926    1c                   if        dat1gg<=ilr78dtela
041300110926     c                   eval      Bollacons  = 'S'
041400111013     c                   eval      olr78msg = 'Trascorsi giorni di attesa'
041500110926    1c                   endif
041600110926     c
041700110726     C                   ENDSR
041800070131     C**********************************************************************
041900070131     c     *inzsr        begsr
042000070131     C**********************************************************************
042100070131      * Reperisco dati job
042200070131     c                   exsr      DatiJob
042300111013     c
042400110926     c                   eval      dataiso=datasys
042500110926     c                   eval      dateu=%dec(dataiso)
042600110926     c                   eval      ora_iso=ora_sys
042700110926     c                   eval      ora_u=%dec(ora_iso)
042800110926
042900070131     c*
043000070131     C     Karb          KLIST
043100110926     C                   KFLD                    ilr78aas
043200110923     C                   KFLD                    ilr78lnp
043300110923     C                   KFLD                    ilr78nrs
043400110923     C                   KFLD                    ilr78nsp
043500111012     C     Karb3         KLIST
043600111012     C                   KFLD                    £arbaas
043700111012     C                   KFLD                    £arblnp
043800111012     C                   KFLD                    £arbnrs
043900111012     C                   KFLD                    £arbnsp
044000070131     c*
044100110926     C     Ktbe          KLIST
044200110926     C                   KFLD                    tbecod
044300110926     C                   KFLD                    tbeke1
044400110927     C     KTAB          KLIST
044500110927     C                   KFLD                    CODUT
044600110927     C                   KFLD                    COD               2
044700110927     C                   KFLD                    KEY               8
044800110927     C     KARB2         KLIST
044900110927     C                   KFLD                    AR§AAS
045000110927     C                   KFLD                    £ARBLNP
045100110927     C                   KFLD                    £ARBNRS
045200110927     C                   KFLD                    £ARBNSP
045300110927     C                   KFLD                    NUM6A
045400110927     C     KARB22        KLIST
045500110927     C                   KFLD                    AR§AAS            4 0
045600110927     C                   KFLD                    £ARBLNP
045700110927     C                   KFLD                    £ARBNRS
045800110927     C                   KFLD                    £ARBNSP
045900110927     C                   KFLD                    NUM6A             8 0
046000110927     C                   KFLD                    NUMORA
046100111012     C     Kfvv8         KLIST
046200111012     C                   KFLD                    £arbtfa
046300111012     C                   KFLD                    knpg
046400111012     C                   KFLD                    kdataloc
046500111012     C     Kfvv81        KLIST
046600111012     C                   KFLD                    £arbtfa
046700111012     C                   KFLD                    knpg
046800111012     C     Kfvv          KLIST
046900111012     C                   KFLD                    D53NPA
047000111012     C                   KFLD                    D53NFA
047100111012     C                   KFLD                    D53LAI
047200110927     c
047300110927     C                   Z-ADD     1             CODUT             1 0
047400111013     c
047500111013     c* Carico tabella giorni di attesa Wurth
047600111013     C                   eval      tbecod = 'XCO'
047700111013     C                   eval      tbeke1 = 'DTA'
047800111013     C     ktbe          chain     Tntbe01l
047900111013    1c                   if        %found(tntbe01l)
048000111013     C                   MOVEL     TBeUNI        Dxcodta
048100111013     c                   else
048200111013     c                   clear                   dxcodta
048300111013    1c                   end
048400070131     c                   endsr
048500070131     C**********************************************************************
048600070131     c     srinz         begsr
048700070131     C**********************************************************************
048800110923     c                   clear                   olr78conso
048900110923     c                   clear                   olr78msg
049000110923     c                   clear                   olr78err
049100110927     c                   clear                   Bollacons
049200110927     c                   clear                   NOcons
049300111013     c                   clear                   NOconsMsg
049400070131     c*
049500110923     c                   if        %parms = 1  or $arbnsp=0
049600070130     c     karb          chain     fnarb01l
049700070131     c                   if        not %found(fnarb01l)
049800070131     c                   clear                   fnarbds
049900070131     c                   else
050000070131     c                   eval      fnarbds = fnarbds£
050100070130     c                   end
050200070131     c                   end
050300110923     c
050400070209     c                   if        (%parms > 1 and $arbnsp <>0)
050500070131     c                   eval      fnarbds = fnarbds$
050600070131     c                   end
050700070131     c*
050800070131     c                   if        arbnsp = 0
050900110923     c                   eval      olr78err='E'
051000110923     c                   eval      olr78msg = 'Spedizione non trovata'
051100070131     c                   end
051200070130     c*
051300070130     c                   endsr
051400110927     C**************************************************************************
051500110927     C* CONTROLLO LE NON CONSEGNATE DAGLI EVENTI
051600110927     C**************************************************************************
051700110927     C     CONT3         BEGSR
051800110927     C                   CLEAR                   WWRT
051900110927     C*
052000110927     C* PER OGNI BOLLA LEGGO SOLTANTO L'ULTIMO EVENTO
052100110927     C                   SETOFF                                       16
052200110927     C                   Z-ADD     ilr78dtela    NUM6A
052300110927     C                   Z-ADD     £ARBAAS       AR§AAS
052400110927     C                   MOVEL     *HIVAL        NUMORA
052500110927     C     KARB22        SETLL     FNEVB000                           32
052600110927     C     KARB2         READPE    FNEVB000                               32
052700110927     c                   eval      §2zcep='S'
052800110927     c* Escludo gli eventi particolari
052900110927    1c                   dow       not *in32 and §2zcep<>' '
053000110927     C*
053100110927     C                   MOVEL     '2Z'          COD
053200110927     C                   MOVEL(P)  EVBCEV        KEY
053300110927     C     KTAB          CHAIN(N)  TABEL                              30
053400110927     C   30              CLEAR                   DS2Z
053500110927     C  N30              MOVEL     TBLUNI        DS2z
053600110927     C*
053700110927    2c                   if        §2zcep<>' '
053800110927     C     KARB2         READPE    FNEVB000                               32
053900110927    2c                   endif
054000110927    1c                   enddo
054100110927     c
054200110927    1C     *IN32         IFEQ      *OFF
054300110927     C                   MOVEL     '2A'          COD
054400110927     C                   MOVEL(P)  EVBCEV        KEY
054500110927     C     KTAB          CHAIN     TABEL                              30
054600110927     C   30              CLEAR                   DS2a
054700110927     C  N30              MOVEL     TBLUNI        DS2a
054800110927     C*
054900110927     C* TIPO CONSEGNA
055000110927    2C     §2AFTC        IFNE      *BLANKS
055100110927     C                   MOVEL     'S'           WWRT              1
055200110927   X2C                   ELSE
055300110927     c
055400110927    3c                   if        §2atip='P' or §2atip='R' or
055500110927     c                             §2atip='S'
055600110927     C                   MOVEL     'S'           WWRT              1
055700110927     c                   endif
055800110927    2C                   ENDIF
055900110927     c
056000110927    1C                   ENDIF
056100110927     C*
056200110927     C                   ENDSR
056300061002      *---------------------------------------------------------------*
056400061002      * Reperimento Dati del job (Utente/Operativi)                   *
056500061002      *---------------------------------------------------------------*
056600061002     c     DatiJob       BEGSR
056700061002      *
056800061002     c     *dtaara       define    §azute        azuteds
056900061002     c     *dtaara       define    §datiute      ddatiute
057000061002      *
057100061002     c                   in(E)     *dtaara
057200061002     c                   IF        %ERROR or RSUT = *blanks
057300061002     c                   clear                   Tibs34Ds
057400061002     c                   call      'TIBS34R'
057500061002     c                   parm                    Tibs34Ds
057600061002     c                   in        *dtaara
057700061002     c                   ENDIF
057800061002      *
057900110926     c                   ENDSR
058000111012     C**************************************************************************
058100111012     C* Per bolla locale desumo la data dell'arrivo bolla
058200111012     C**************************************************************************
058300111012     c     BOLLALOC      begsr
058400111012     c     kfvv8         SETGT     FNFVV08L
058500111012     c     kfvv81        READe     FNFVV08L
058600111012    6C                   IF        not %eoF(FNFVV08L) AND FVVDFV>=kdataloc
058700111012    7C                   IF        FVVFCF=' '  or wtipolancio='SA'
058800111012     C                   EVAL      Wartdam=FVVDFV
058900111012    7C                   ENDIF
059000111012     C*
059100111012   X6C                   ELSE
059200111012     C* SE NON TROVATO LE STAMPO SEMPRE CON DATA  + 1
059300111012     c                   eval      dataiso=%date(kdataloc)
059400111012     c                   adddur    1:*d          dataiso
059500111012     c                   eval      wartdam=%dec(dataiso)
059600111012     c                   endif
059700111012     c
059800111012     c                   ENDSR
