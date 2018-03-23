000100050920      /TITLE Upload: gestione iter da file univoci a membri univoci in TIVIN00R.
000200170622     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300990907
000400170329     Ftivlrb01i uf a e           k disk    prefix(K_)
000500170329     F                                     rename(tivlrb000:tivlrb001)
000600170329     Ftivlrb02i uf a e           k disk
000700170802     FtivlrMMl  uf   e           k disk    infds(tivlrinf)
000800170802
000900170802     D tivlrinf        ds
001000170802     D  tivlrnrri            397    400i 0
001100990907
001200010524
001300000710     D* variabili di work
001400170329     D  DSVLR3       e ds                  inz
001500010921     D  esito          s              1    inz('0')
001600060103     D  libreria       s             10
001700170329     D  wCnt           s             15  0 inz
001800170329     D  wCntProc       s              2  0 inz
001900170329     D  wCntProcA      s              2    inz
002000170329     D  wProcBalan     s              2    inz
002100170802     D  wDivid         s                   like(divid)
002200010525
002300010525
002400010525     C*-----------------------------------------------------------------------------
002500170329     C*
002600170329     C* Iscrizione Processi in "balancing"
002700170329     C* (eseguito solamente 1 volta ad ogni  avvio del sottosistema)
002800170329     C                   if        not *in10
002900170329     C                   seton                                        10
003000170329     C*
003100170329     C* Reperisco il contatore più basso
003200170329     C     *loval        setll     tivlrb02i
003300170329     C                   read(n)   tivlrb02i
003400170329     C                   if        not %eof(tivlrb02i)
003500170329     C                   eval      wCnt = vlrbCOUNT
003600170329     C                   else
003700170329     C                   eval      wCnt = 1
003800170329     C                   endif
003900170329     C*
004000170329     C* Iscrizione Processi in "balancing"
004100170329     C                   z-add     *zeros        wCntProc
004200170329     C*
004300170329     C* Ciclo da 1 a 60 escludenzo alcunni specifici Processi da NON bilanciare assolutamente
004400170329     C                   dow       wCntProc < 60
004500170329     C                   add       1             wCntProc
004600170329     C                   if        wCntProc = 04 OR
004700170329     C                             wCntProc = 05 OR
004800170621     C                             wCntProc = 12 OR
004900170329     C                             wCntProc = 17 OR
005000170329     C                             wCntProc = 18 OR
005100170331     C                             wCntProc = 30 OR
005200170329     C                             wCntProc = 35 OR
005300170329     C                             wCntProc = 67
005400170329     C                   else
005500170329     C                   movel     wCntProc      wCntProcA
005600170329     C     wCntProcA     chain(n)  tivlrb01i
005700170329     C                   if        not %found(tivlrb01i)
005800170329     C                   eval      K_vlrbIDP   = wCntProcA
005900170329     C                   eval      K_vlrbCOUNT = wCnt
006000170329     C                   write(e)  tivlrb001
006100170329     C                   endif
006200170329     C                   endif
006300170329     C                   enddo
006400170329     C*
006500170329     C                   endif
006600010525     C*
006700010525     C* Leggo tutta la vista logica
006800100624     C                   setoff                                       rt
006900101006     C     *start        setll     tivlrMMl
007000101006     C                   read      tivlrMMl                               rt
007100010525     C*
007200100624     C                   dow       not *inrt
007300991105     C*
007400991111     C* Gestione ENDJOB.
007500991111     C                   shtdn                                        lr
007600010525     C                   if        not *inlr
007700170802     C*
007800170802     C* Gestisco elaborazione parallela
007900170802     C                   exsr      exeElapar
008000170802     C                   if        not *in40
008100170802     C                   unlock(e) tivlrMMl
008200170802     C                   else
008300050920     C*
008400050920     C* Elaboro il record corrente
008500050920     C                   call(e)   'TIS782C1'
008600050920     C                   parm                    libreria
008700050920     C                   parm                    vlrhdl
008800010921     C                   parm      '0'           esito
008900050920     C*
009000050920     C* Verifico esito e se tutto ok => aggiorno stato sul record d log x innescare l'iter standard
009100050920     C                   if        not %error and esito = '1'
009200170329     C*
009300170329     C* Se trattasi di transazione mono-file e non ri-esecuzione
009400170329     C* e non transazione appartente ai Processi esclusi => eseguo bilanciamento
009500170329     C                   eval      DSVLR3 = vlrFL3
009600170329     C                   if        vlrFL2 = '01' AND §VLR31 = *blanks AND
009700170329     C                             §VLR345 <> '04' AND
009800170329     C                             §VLR345 <> '05' AND
009900170622     C                             §VLR345 <> '12' AND
010000170329     C                             §VLR345 <> '17' AND
010100170329     C                             §VLR345 <> '18' AND
010200170331     C                             §VLR345 <> '30' AND
010300170329     C                             §VLR345 <> '35' AND
010400170329     C                             §VLR345 <> '67'
010500170329     C                   exsr      exeBalance
010600170329     C                   if        wProcBalan <> *blanks
010700170329     C                   eval      §VLR345 = wProcBalan
010800170329     C                   eval      vlrFL3 = DSVLR3
010900170329     C                   endif
011000170329     C                   endif
011100170329     C*
011200050920     C                   eval      vlrflg = '0'
011300050920     C                   update    tivlr000
011400050920     C                   endif
011500050920     C*
011600170802     C                   endif
011700050920     C                   endif
011800050920     C*
011900101006     C                   read      tivlrMMl                               rt
012000010525     C                   enddo
012100170329     C*
012200170329     C* In uscita disalloco file "bilanciamento"
012300170329     C                   exsr      disAlloca
012400100624     C*
012500100624     C                   return
012600010525     C*
012700010525     C*-----------------------------------------------------------------------------
012800170329
012900170329
013000170329
013100170329     C*
013200170329     C     exeBalance    begsr
013300170329     C*
013400170329     C                   clear                   wProcBalan
013500170329     C*
013600170329     C* Reperisco l'ID Processo con il carico totale più basso
013700170329     C     *loval        setll     tivlrb02i
013800170329     C                   read      tivlrb02i
013900170329     C*
014000170329     C                   if        not %eof(tivlrb02i)
014100170329     C*
014200170329     C* Se il progressivo più basso è > 999.000.000.000.000
014300170329     C* Azzero tutti i contatori di processo
014400170329     C                   if        vlrbCOUNT > 999000000000000
014500170329     C*
014600170329     C                   dow       not *in55
014700170329     C                   eval      vlrbCOUNT = 1
014800170329     C                   update    tivlrb000
014900170329     C                   read      tivlrb02i                              55
015000170329     C                   enddo
015100170329     C*
015200170329     C                   else
015300170329     C*
015400170329     C* Salvo il Processo con minor carico complessivo
015500170329     C                   eval      wProcBalan = vlrbIDP
015600170329     C*
015700170329     C* Incremento con step 1 il contatore del Processo con meno carico complessivo
015800170329     C                   eval      vlrbCOUNT = vlrbCOUNT + 1
015900170329     C                   update    tivlrb000
016000170329     C                   endif
016100170329     C*
016200170329     C                   endif
016300170329     C*
016400170329     C                   endsr
016500170329
016600170329
016700170329
016800170329     C*
016900170329     C     disAlloca     begsr
017000170329     C*
017100170329     C                   unlock(e) tivlrb01i
017200170329     C                   unlock(e) tivlrb02i
017300170329     C*
017400170329     C                   endsr
017500170802
017600170802
017700170802
017800170802       //-------------------------------------------------------------*
017900170802       // Gestione elaborazione parallela                             *
018000170802       //-------------------------------------------------------------*
018100170802       Begsr exeElapar;
018200170802
018300170802           *in40 = *off;                  // record KO
018400170802           if divis > *zeros AND divid > *zeros;
018500170802              wDivid = divid - 1;
018600170802              if %rem(tivlrnrri:divis) = wDivid;
018700170802                 *in40 = *on;             // record OK
018800170802              endif;
018900170802           else;
019000170802              *in40 = *on;                // record OK
019100170802           endif;
019200170802
019300170802       endsr;
019400170802
019500050920
019600050920
019700000714      /TITLE Routine di *init PGM
019800000714     C*
019900000714     C     *inzsr        begsr
020000060103     C*
020100060103     C     *entry        plist
020200060103     C                   parm                    libreria
020300170802     C                   PARM                    divis             2 0
020400170802     C                   PARM                    divid             2 0
020500000714     C*
020600000714     C                   endsr
020700000714
