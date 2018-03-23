000100970526     H*------------------------------------------------------------------------*
000200140904     H* Gestione log download in errore batch
000300970526     H*------------------------------------------------------------------------*
000400140702     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500140703     h dftactgrp(*no) bnddir('UBBNDDIR')
000600140702     h alwnull(*inputonly)
000700140703
000800140904      * per compilare questo pgm bisogna avere in lista di librerie il file WFLDE00F che viene,
000900140703      * però, creato e distrutto da questi pgm (è una table SQL).
001000140703      * quindi crearlo secondo lo statement in SRCTNDB nella propria libreria e mettere questa
001100140703      * in lista temporaneamente, ricordandosi di toglierla prima di lanciare l'esecuzione.
001200140703
001300970526     F*------------------------------------------------------------------------*
001400970526     F* DATA BASE
001500970526     F*------------------------------------------------------------------------*
001600970526     D*------------------------------------------------------------------------*
001700970526     D* SCHIERE
001800970526     D*------------------------------------------------------------------------*
001900970526     D*------------------------------------------------------------------------*
002000970526     D* INPUT
002100970526     D*------------------------------------------------------------------------*
002200970526     D*------------
002300970526     D* ARCHITETTURA
002400970526     D*------------
002500970526     D KPJBA         E DS
002600120928
002700121003     d Status         sds
002800121003     d  SDSpgm           *proc
002900121003     d  SDSprm           *parms
003000121003     d  SDSuser              254    263
003100121003
003200140904     d DS7GIR5         ds
003300140701     d pIn_DatDa                      8s 0
003400140701     d pIn_DatA                       8s 0
003500140701     d pIn_Ksc                        8
003600140701     d pIn_Tip                        2
003700140904     d pIn_ISV                        2
003800140702     d pIn_Modo                       1
003900140701     d pOut_Esito                     2  0
004000140703     d pIn_Email                     50
004100140702     D*------------------
004200140702     D* DS - dati per fetch
004300140904     D WFLDEDS       E DS                  extname(WFLDE00F)
004400140702     D DSFetch         DS
004500140904     D  KSC                                like(LDEKsc)
004600140904     D  ISV                                like(LDEISV)
004700140904     D  TIP                                like(LDETip)
004800140904     D  TIPERR                             like(LDETipErr)
004900140904     D  ERR                                like(LDECntErr)
005000140702     D*------------------
005100140908     d wX              s              3  0
005200140908     d wTipErr1        s              1a
005300140908     d wTipErr2        s              1a
005400140702     D* Stringa SQL da eseguire
005500140702     d wSQL            s           2048    inz  varying
005600140703     D currSysNeta     s              8A   inz('*NULL')
005700141021     D pMsgV           s           5000    inz varying
005800140703     D*------------------
005900140703     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
006000140703     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
006100140627
006200970526     C*------------------------------------------------------------------------*
006300000303     C* MAIN LINES
006400970526     C*------------------------------------------------------------------------*
006500140630     C     *entry        PLIST
006600140630     C                   PARM                    KPJBA
006700140904     C                   MOVEL     KPJBU         DS7GIR5
006800970526     C*
006900140702     C* operazioni iniziali
007000140702     C                   EXSR      OpeIniz
007100140703     C*
007200140703     C* eseguo l'estrazione
007300140703     C                   EXSR      Esegui
007400140702     C*
007500140703     C* invio via e-mail i dati estratti
007600140703     C                   EXSR      Invia
007700970526     C*
007800970526     C* FINE PGM
007900140702     C                   SETON                                        RT
008000140703     C*------------------------------------------------------------------------*
008100140703     C* ESEGUI - OPERAZIONI DI LANCIO PGM
008200140703     C*------------------------------------------------------------------------*
008300140703     C     ESEGUI        BEGSR
008400140703     C*
008500140703     C* chiamo
008600140904     C                   CALL      'TIS7GI1R2'
008700140703     c                   parm                    pIn_DatDa
008800140703     c                   parm                    pIn_DatA
008900140703     c                   parm                    pIn_Ksc
009000140703     c                   parm                    pIn_Tip
009100140904     c                   parm                    pIn_ISV
009200140703     c                   parm                    pIn_Modo
009300140703     c                   parm                    pOut_Esito
009400140703     C*
009500140703     C                   ENDSR
009600970526     C*------------------------------------------------------------------------*
009700140703     C* Invia - leggo i dati estratti, li raggruppo e preparo la e-mail
009800970526     C*------------------------------------------------------------------------*
009900140703     C     Invia         BEGSR
010000140702     C*
010100140702     C* leggo tutti i rcd a seconda dei parametri ricevuti dal lancio
010200140904     C                   EVAL      wSQL = 'with uni1 as ( +
010300140904     C                                    select +
010400140910     C                                    ldeksc, ldetip, ldeisv, ldetiperr, +
010500140904     C                                    sum(ldecnterr)  +
010600140904     C                                    from WFLDE00F +
010700140904     C                                    where ldetiperr = ''1'' +
010800140910     C                                    group by ldeksc, ldetip, ldeisv, +
010900140910     C                                    ldetiperr +
011000140905     C                                    having sum(ldecnterr) > 0 )'
011100140905     C                   EVAL      wSQL = %trim(wSQL) + ' , uni1_1 as ( +
011200140905     C                                    select * from uni1 +
011300140910     C                                    order by 5 desc , 1  , 2 , 3 +
011400140905     C                                    fetch first 10 rows only )'
011500140904     C                   EVAL      wSQL = %trim(wSQL) + ', uni2 as ( +
011600140904     C                                    select +
011700140910     C                                    ldeksc, ldetip, ldeisv, ldetiperr, +
011800140904     C                                    sum(ldecnterr)  +
011900140904     C                                    from WFLDE00F +
012000140904     C                                    where ldetiperr = ''2'' +
012100140910     C                                    group by ldeksc, ldetip, ldeisv, +
012200140910     C                                    ldetiperr +
012300140905     C                                    having sum(ldecnterr) > 0 )'
012400140905     C                   EVAL      wSQL = %trim(wSQL) + ' , uni2_1 as ( +
012500140905     C                                    select * from uni2 +
012600140910     C                                    order by 5 desc , 1  , 2 , 3 +
012700140905     C                                    fetch first 10 rows only )'
012800140905     C                   EVAL      wSQL = %trim(wSQL) +
012900140905     C                                   ' select * from uni1_1 +
013000140905     C                                   union all +
013100140905     C                                   select * from uni2_1'
013200140702     C*
013300140702     C/EXEC SQL
013400140702     C+ prepare S1 from :wSQL
013500140702     C/END-EXEC
013600140702     C*
013700140702     C*
013800140702     C/EXEC SQL
013900140702     C+ declare C1 cursor for S1
014000140702     C/END-EXEC
014100140702     C*
014200140702     C*
014300140702     C/EXEC SQL
014400140702     C+ open C1
014500140702     C/END-EXEC
014600140702     C*
014700140702     C                   DOW       SQLCod >= 0 and
014800140702     C                             SQLCod <> 100 and
014900140905     C                             wX < 20
015000140702     C*
015100140702     C/EXEC SQL
015200140702     C+ Fetch C1 into :DSFetch
015300140702     C/END-EXEC
015400140702     C*
015500140702     C                   SELECT
015600140702     C* se fine file, esco
015700140702     C                   WHEN      SQLCod = 100                                 *FINE FILE
015800140702     C                   LEAVE
015900140702     C* se errore, esco segnalando
016000140702     C                   WHEN      SQLCod < 0                                   *Errore
016100140702     C                   EVAL      pOut_Esito = 4
016200140702     C                   LEAVE
016300140702     C* se letto un rcd
016400140702     C                   WHEN      SQLCod <> 100 and SQLCod >= 0                *letto rcd
016500140908     C* se la tipologia errore è la 1
016600140908     C* ed è il primo errore per quella tipologia
016700140908     C                   IF        TipErr = '1'
016800140908     C                             and wTipErr1 = *off
016900140908     C* scrivo la segnalazione del tipo errore 1
017000140908     C                   EVAL      wTipErr1 = *on
017100141021     C                   EVAL      pMsgV = pMsgV +
017200140908     C                             ':/N' +
017300140908     C                             'Clienti non abilitati al servizio => +
017400140908     C                             verificare se occorre abilitarli al +
017500140908     C                             servizio ed eventualmente rieseguire +
017600140908     C                             l''operazione'
017700140908     C                   ENDIF
017800140908     C* se la tipologia errore è la 2
017900140908     C* ed è il primo errore per quella tipologia
018000140908     C                   IF        TipErr = '2'
018100140908     C                             and wTipErr2 = *off
018200140908     C* scrivo la segnalazione del tipo errore 1
018300140908     C                   EVAL      wTipErr2 = *on
018400141021     C                   EVAL      pMsgV = pMsgV +
018500140908     C                             ':/N' +
018600140908     C                             ':/N' +
018700140908     C                             'Tipo file non previsto per il cliente => ve+
018800140908     C                             rificare se necessario ed eventualmente annu+
018900140908     C                             llare l''abilitazione della specifica tabell+
019000140908     C                             a'
019100140908     C                   ENDIF
019200140702     C* lo aggiungo nella e-mail
019300140702     C                   EXSR      AggEmail
019400140702     C                   ENDSL
019500140702     C                   ENDDO
019600140702     C*
019700140702     C* se ho scritto almeno 1 errore
019800140702     C                   IF        wX > 0
019900140905     C* invio e-mail
020000140702     C                   EXSR      EmailInvia
020100140702     C                   ENDIF
020200140702     C*
020300140702     C/EXEC SQL
020400140702     C+ close C1
020500140702     C/END-EXEC
020600991214     C*
020700970526     C                   ENDSR
020800140702     C*------------------------------------------------------------------------*
020900140702     C* OpeIniz: operazioni iniziali
021000140702     C*------------------------------------------------------------------------*
021100140702     C     OpeIniz       BEGSR
021200140702     C*
021300140702     C                   EVAL      WX = 0
021400140908     C                   EVAL      wTipErr1 = *off
021500140908     C                   EVAL      wTipErr2 = *off
021600140703     C*
021700140703     C* se è stato passato un indirizzo email specifico, uso quello altrimenti uso i default
021800140703     C                   IF        pIn_Email <> *blank
021900140703     C                   EVAL      peml = pIn_Email
022000140703     C*
022100140703     C                   ELSE
022200140703     C*
022300140703     C* Reperisco il sistema AS/400 corrente
022400140703     C* e, a seconda di qual è, imposto il ricevente
022500140703     C                   callp     UBRTVNETA_Rtv(currSysNeta)
022600140703     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
022700140703     C                   EVAL      peml = 'cedalert@brt.it'
022800140703     C                   EVAL      pcceml = *blanks
022900140703     C                   else
023000140703     C                   EVAL      peml = 'cedvas@brt.it'
023100140703     C                   EVAL      pcceml = 'luciano.carini@brt.it'
023200140703     C                   endif
023300140702     C*
023400140703     C                   ENDIF
023500140703     C*
023600140924     C                   EVAL      pogg = 'BARTVAS - LOG DOWNLOAD IN ERRORE'
023700141021     C                   EVAL      pMsgV =
023800140702     C                             'Nel periodo: ' +
023900140703     C                             %subst(%editc(pIn_DatDa:'X'):7:2) + '/' +
024000140703     C                             %subst(%editc(pIn_DatDa:'X'):5:2) + '/' +
024100140703     C                             %subst(%editc(pIn_DatDa:'X'):1:4) +
024200140703     C                             ' - ' +
024300140703     C                             %subst(%editc(pIn_DatA:'X'):7:2) +  '/' +
024400140703     C                             %subst(%editc(pIn_DatA:'X'):5:2) +  '/' +
024500140703     C                             %subst(%editc(pIn_DatA:'X'):1:4) +
024600140702     C                             ':/N' +
024700140703     C                             'per il cliente: '
024800140703     C                   IF        pIn_Ksc = *blank
024900141021     C                   EVAL      pMsgV = pMsgV + ' tutti'
025000140703     C                   ELSE
025100141021     C                   EVAL      pMsgV = pMsgV + ' ' +
025200140703     C                             pIn_Ksc
025300140703     C                   ENDIF
025400141021     C                   EVAL      pMsgV = pMsgV +
025500140904     C                             '  e per il tipo servizio: '
025600140904     C                   IF        pIn_ISV = *blank
025700141021     C                   EVAL      pMsgV = pMsgV + ' tutti '
025800140703     C                   ELSE
025900141021     C                   EVAL      pMsgV = pMsgV + ' ' +
026000140703     C                             pIn_Tip
026100140703     C                   ENDIF
026200141021     C                   EVAL      pMsgV = pMsgV +
026300140904     C                             '  e per il tipo file: '
026400140904     C                   IF        pIn_Tip = *blank
026500141021     C                   EVAL      pMsgV = pMsgV + ' tutti '
026600140904     C                   ELSE
026700141021     C                   EVAL      pMsgV = pMsgV + ' ' +
026800140904     C                             pIn_Tip
026900140904     C                   ENDIF
027000141021     C                   EVAL      pMsgV = pMsgV +
027100140703     C                             ' :/N' +
027200140703     C                             'sono stati trovati errori significativi +
027300140904     C                             sui seguenti clienti/servizi/tipi file:' +
027400140702     C                             ':/N' +
027500140908     C                             '(ogni e-mail contiene 10 errori per ogni +
027600140908     C                             tipologia)' +
027700140702     C                             ':/N'
027800140702     C*
027900140702     C                   ENDSR
028000140702     C*------------------------------------------------------------------------*
028100140702     C* AggEmail: aggiunge rcd all'e-mail
028200140702     C*------------------------------------------------------------------------*
028300140702     C     AggEmail      BEGSR
028400140702     C*
028500141021     C                   EVAL      pMsgV = pMsgV +
028600140702     C                             ':/N' +
028700140702     C                             'Cliente: ' +
028800140909     C                             KSC
028900140910     C***                IF        TipErr = '2'
029000141021     C                   EVAL      pMsgV = pMsgV +
029100140702     C                             '  Tipo file: ' +
029200140909     C                             TIP
029300140910     C***                ENDIF
029400140910     C***                IF        TipErr = '1'
029500141021     C                   EVAL      pMsgV = pMsgV +
029600140904     C                             '  Tipo servizio: ' +
029700140909     C                             ISV
029800140910     C***                ENDIF
029900141021     C                   EVAL      pMsgV = pMsgV +
030000140702     C                             '  di cui errati: ' +
030100140908     C                             %editc(ERR:'4')
030200140702     C*
030300140702     C                   EVAL      wX = wX + 1
030400140702     C*
030500140702     C                   ENDSR
030600140702     C*------------------------------------------------------------------------*
030700140702     C* EmailInvia - Invio e-mail di conferma forzatura
030800140702     C*------------------------------------------------------------------------*
030900140702     C     EmailInvia    BEGSR
031000140702     C*
031100141021     C                   EVAL      pMsg = pMsgV
031200140702     C* invio e-mail
031300140702     C                   call      'TIS701C1'
031400140702     C                   parm                    peml            253
031500140702     C                   parm                    pcceml          253
031600140702     C                   parm                    pogg             44
031700140702     C                   parm                    pmsg           5000
031800140702     C                   parm                    pesito            1
031900140702     C*
032000140702     C                   ENDSR
