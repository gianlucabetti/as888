000100970526     H*------------------------------------------------------------------------*
000200140702     H* Gestione log upload in errore batch
000300970526     H*------------------------------------------------------------------------*
000400140702     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500140703     h dftactgrp(*no) bnddir('UBBNDDIR')
000600140702     h alwnull(*inputonly)
000700140703
000800140703      * per compilare questo pgm bisogna avere in lista di librerie il file WFLUE00F che viene,
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
003200140703     d DS760R5         ds
003300140701     d pIn_DatDa                      8s 0
003400140701     d pIn_DatA                       8s 0
003500140701     d pIn_Ksc                        8
003600140701     d pIn_Tip                        2
003700140701     d pIn_Perc                       3s 0
003800140702     d pIn_Log                        3s 0
003900140702     d pIn_Modo                       1
004000140701     d pOut_Esito                     2  0
004100140703     d pIn_Email                     50
004200140702     D*------------------
004300140702     D* DS - dati per fetch
004400140702     D WFLUEDS       E DS                  extname(WFLUE00F)
004500140702     D DSFetch         DS
004600140702     D  KSC                                like(LUEKsc)
004700140702     D  TIP                                like(LUETip)
004800140702     D  LOG                                like(LUECntLog)
004900140702     D  ERR                                like(LUECntErr)
005000140702     D  PERC                          5s 2
005100140702     D*------------------
005200140702     d WX              s              3  0
005300140702     D* Stringa SQL da eseguire
005400140702     d wSQL            s           2048    inz  varying
005500140703     D currSysNeta     s              8A   inz('*NULL')
005600140703     D*------------------
005700140703     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
005800140703     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
005900140627
006000970526     C*------------------------------------------------------------------------*
006100000303     C* MAIN LINES
006200970526     C*------------------------------------------------------------------------*
006300140630     C     *entry        PLIST
006400140630     C                   PARM                    KPJBA
006500140703     C                   MOVEL     KPJBU         DS760R5
006600970526     C*
006700140702     C* operazioni iniziali
006800140702     C                   EXSR      OpeIniz
006900140703     C*
007000140703     C* eseguo l'estrazione
007100140703     C                   EXSR      Esegui
007200140702     C*
007300140703     C* invio via e-mail i dati estratti
007400140703     C                   EXSR      Invia
007500970526     C*
007600970526     C* FINE PGM
007700140702     C                   SETON                                        RT
007800140703     C*------------------------------------------------------------------------*
007900140703     C* ESEGUI - OPERAZIONI DI LANCIO PGM
008000140703     C*------------------------------------------------------------------------*
008100140703     C     ESEGUI        BEGSR
008200140703     C*
008300140703     C* chiamo
008400140703     C                   CALL      'TIS760R2'
008500140703     c                   parm                    pIn_DatDa
008600140703     c                   parm                    pIn_DatA
008700140703     c                   parm                    pIn_Ksc
008800140703     c                   parm                    pIn_Tip
008900140703     c                   parm                    pIn_Perc
009000140703     c                   parm                    pIn_Log
009100140703     c                   parm                    pIn_Modo
009200140703     c                   parm                    pOut_Esito
009300140703     C*
009400140703     C                   ENDSR
009500970526     C*------------------------------------------------------------------------*
009600140703     C* Invia - leggo i dati estratti, li raggruppo e preparo la e-mail
009700970526     C*------------------------------------------------------------------------*
009800140703     C     Invia         BEGSR
009900140702     C*
010000140702     C* leggo tutti i rcd a seconda dei parametri ricevuti dal lancio
010100140702     C                   EVAL      wSQL = 'select +
010200140702     C                                    lueksc, luetip, +
010300140702     C                                    sum(luecntlog), sum(luecnterr), +
010400140702     C                                    dec( +
010500140708     C                                    dec(sum(luecnterr) , 9 , 2)/ +
010600140708     C                                    dec(sum(luecntlog) , 9 , 2)*100 +
010700140702     C                                    , 3 , 0) +
010800140702     C                                    from WFLUE00F +
010900140702     C                                    group by lueksc, luetip +
011000140702     C                                    having sum(luecnterr) > 0 +
011100140702     C                                    and sum(luecntlog) >=' +
011200140702     C                                    %char(pIn_Log) +
011300140702     C                                    ' and dec( +
011400140708     C                                    dec(sum(luecnterr) , 9 , 2)/ +
011500140708     C                                    dec(sum(luecntlog) , 9 , 2)*100 +
011600140702     C                                    , 3 , 0) >=' +
011700140702     C                                    %char(pIn_Perc) +
011800140702     C                                    ' order by 5 desc , 4 desc , 1 , 2 '
011900140702     C*
012000140702     C/EXEC SQL
012100140702     C+ prepare S1 from :wSQL
012200140702     C/END-EXEC
012300140702     C*
012400140702     C*
012500140702     C/EXEC SQL
012600140702     C+ declare C1 cursor for S1
012700140702     C/END-EXEC
012800140702     C*
012900140702     C*
013000140702     C/EXEC SQL
013100140702     C+ open C1
013200140702     C/END-EXEC
013300140702     C*
013400140702     C                   DOW       SQLCod >= 0 and
013500140702     C                             SQLCod <> 100 and
013600140702     C                             wX < 10
013700140702     C*
013800140702     C/EXEC SQL
013900140702     C+ Fetch C1 into :DSFetch
014000140702     C/END-EXEC
014100140702     C*
014200140702     C                   SELECT
014300140702     C* se fine file, esco
014400140702     C                   WHEN      SQLCod = 100                                 *FINE FILE
014500140702     C                   LEAVE
014600140702     C* se errore, esco segnalando
014700140702     C                   WHEN      SQLCod < 0                                   *Errore
014800140702     C                   EVAL      pOut_Esito = 4
014900140702     C                   LEAVE
015000140702     C* se letto un rcd
015100140702     C                   WHEN      SQLCod <> 100 and SQLCod >= 0                *letto rcd
015200140702     C* lo aggiungo nella e-mail
015300140702     C                   EXSR      AggEmail
015400140702     C                   ENDSL
015500140702     C                   ENDDO
015600140702     C*
015700140702     C* se ho scritto almeno 1 errore
015800140702     C                   IF        wX > 0
015900140702     C* invio e-mail
016000140702     C                   EXSR      EmailInvia
016100140702     C                   ENDIF
016200140702     C*
016300140702     C/EXEC SQL
016400140702     C+ close C1
016500140702     C/END-EXEC
016600991214     C*
016700970526     C                   ENDSR
016800140702     C*------------------------------------------------------------------------*
016900140702     C* OpeIniz: operazioni iniziali
017000140702     C*------------------------------------------------------------------------*
017100140702     C     OpeIniz       BEGSR
017200140702     C*
017300140702     C                   EVAL      WX = 0
017400140703     C*
017500140703     C* se è stato passato un indirizzo email specifico, uso quello altrimenti uso i default
017600140703     C                   IF        pIn_Email <> *blank
017700140703     C                   EVAL      peml = pIn_Email
017800140703     C*
017900140703     C                   ELSE
018000140703     C*
018100140703     C* Reperisco il sistema AS/400 corrente
018200140703     C* e, a seconda di qual è, imposto il ricevente
018300140703     C                   callp     UBRTVNETA_Rtv(currSysNeta)
018400140703     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
018500140703     C                   EVAL      peml = 'cedalert@brt.it'
018600140703     C                   EVAL      pcceml = *blanks
018700140703     C                   else
018800140703     C                   EVAL      peml = 'cedvas@brt.it'
018900140703     C                   EVAL      pcceml = 'luciano.carini@brt.it'
019000140703     C                   endif
019100140702     C*
019200140703     C                   ENDIF
019300140703     C*
019400140924     C                   EVAL      pogg = 'BARTVAS - LOG UPLOAD IN ERRORE'
019500140702     C                   EVAL      pmsg =
019600140702     C                             'Nel periodo: ' +
019700140703     C                             %subst(%editc(pIn_DatDa:'X'):7:2) + '/' +
019800140703     C                             %subst(%editc(pIn_DatDa:'X'):5:2) + '/' +
019900140703     C                             %subst(%editc(pIn_DatDa:'X'):1:4) +
020000140703     C                             ' - ' +
020100140703     C                             %subst(%editc(pIn_DatA:'X'):7:2) +  '/' +
020200140703     C                             %subst(%editc(pIn_DatA:'X'):5:2) +  '/' +
020300140703     C                             %subst(%editc(pIn_DatA:'X'):1:4) +
020400140702     C                             ':/N' +
020500140703     C                             'per il cliente: '
020600140703     C                   IF        pIn_Ksc = *blank
020700140703     C                   EVAL      pMsg = %trimr(pMsg) + ' tutti'
020800140703     C                   ELSE
020900140703     C                   EVAL      pMsg = %trimr(pMsg) + ' ' +
021000140703     C                             pIn_Ksc
021100140703     C                   ENDIF
021200140703     C                   EVAL      pMsg = %trimr(pMsg) +
021300140703     C                             '  e per il tipo file: '
021400140703     C                   IF        pIn_Tip = *blank
021500140703     C                   EVAL      pMsg = %trimr(pMsg) + ' tutti '
021600140703     C                   ELSE
021700140703     C                   EVAL      pMsg = %trimr(pMsg) + ' ' +
021800140703     C                             pIn_Tip
021900140703     C                   ENDIF
022000140703     C                   EVAL      pMsg = %trimr(pMsg) +
022100140703     C                             ' :/N' +
022200140702     C                             'con i seguenti sbarramenti: +
022300140702     C                             % errore = ' + %char(pIn_Perc) +
022400140702     C                             '  nr. minimo file = ' + %char(pIn_Log) +
022500140703     C                             ' :/N' +
022600140703     C                             'sono stati trovati errori significativi +
022700140702     C                             sui seguenti clienti/tipi file:' +
022800140702     C                             ':/N' +
022900140702     C                             '(ogni e-mail contiene al max 10 errori)' +
023000140702     C                             ':/N'
023100140702     C*
023200140702     C                   ENDSR
023300140702     C*------------------------------------------------------------------------*
023400140702     C* AggEmail: aggiunge rcd all'e-mail
023500140702     C*------------------------------------------------------------------------*
023600140702     C     AggEmail      BEGSR
023700140702     C*
023800140702     C                   EVAL      pmsg = %trimr(pmsg) +
023900140702     C                             ':/N' +
024000140702     C                             'Cliente: ' +
024100140702     C                             KSC +
024200140702     C                             '  Tipo file: ' +
024300140702     C                             TIP +
024400140702     C                             '  Nr.file: ' +
024500140702     C                             %editc(LOG:'4') +
024600140702     C                             '  di cui errati: ' +
024700140702     C                             %editc(ERR:'4') +
024800140703     C                             '  ( ' +
024900140703     C                             %editc(%dec(PERC:3:0):'4') +
025000140703     C                             '% )'
025100140702     C*
025200140702     C                   EVAL      wX = wX + 1
025300140702     C*
025400140702     C                   ENDSR
025500140702     C*------------------------------------------------------------------------*
025600140702     C* EmailInvia - Invio e-mail di conferma forzatura
025700140702     C*------------------------------------------------------------------------*
025800140702     C     EmailInvia    BEGSR
025900140702     C*
026000140702     C* invio e-mail
026100140702     C                   call      'TIS701C1'
026200140702     C                   parm                    peml            253
026300140702     C                   parm                    pcceml          253
026400140702     C                   parm                    pogg             44
026500140702     C                   parm                    pmsg           5000
026600140702     C                   parm                    pesito            1
026700140702     C*
026800140702     C                   ENDSR
