000100970526     H*------------------------------------------------------------------------*
000200160204     H* Pulizia WFMAF050F
000300970526     H*------------------------------------------------------------------------*
000400140702     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500140703     h dftactgrp(*no) bnddir('UBBNDDIR')
000600140702     h alwnull(*inputonly)
000700140703
000800970526     F*------------------------------------------------------------------------*
000900970526     F* DATA BASE
001000970526     F*------------------------------------------------------------------------*
001100970526     D*------------------------------------------------------------------------*
001200970526     D* SCHIERE
001300970526     D*------------------------------------------------------------------------*
001400970526     D*------------------------------------------------------------------------*
001500970526     D* INPUT
001600970526     D*------------------------------------------------------------------------*
001700970526     D*------------
001800970526     D* ARCHITETTURA
001900970526     D*------------
002000970526     D KPJBA         E DS
002100160205       // - DS per TNTBE00f cod='AOP' key='LOG-GG'
002200160205     d DAOP001       e ds
002300160205       // - DS interna per i parametri di schedulazione
002400160205     d Parm_Input      ds
002500160205     d  pIn_LogGG                     3s 0
002600160205     d  pIn_Email                   253
002700120928
002800121003     d Status         sds
002900121003     d  SDSpgm           *proc
003000121003     d  SDSprm           *parms
003100121003     d  SDSuser              254    263
003200121003
003300160204     d pIn_Opz         s              1a
003400160204       // tipo lancio: 'C'=LR   *blank=RT
003500160204     d pIn_TLa         s              1a
003600160204     d pIn_COD         s              3a
003700160204     d pIn_KEY         s             15a
003800160204     d pIn_Data        s              8s 0
003900160204       // esito chiamata: 0=OK   1=ERR INPUT   2=KO
004000160204     d pOut_ERR        s              1s 0
004100160204     d pOut_DS         s             10a
004200160204     d pOut_Len        s              3s 0
004300160204     d pOut_Dati       s            256a
004400160204     D*------------------
004500160204     d WX              s              3  0
004600160204     D* Stringa SQL da eseguire
004700160204     d wSQL            s           2048    inz  varying
004800160204     D currSysNeta     s              8A   inz('*NULL')
004900160205     d wLogGG          s                   like(§AOPLOGGG)
005000140703     D*------------------
005100140703     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
005200140703     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
005300140627
005400970526     C*------------------------------------------------------------------------*
005500000303     C* MAIN LINES
005600970526     C*------------------------------------------------------------------------*
005700140630     C     *entry        PLIST
005800140630     C                   PARM                    KPJBA
005900970526     C*
006000140702     C* operazioni iniziali
006100140702     C                   EXSR      OpeIniz
006200140703     C*
006300160204     C* eseguo la pulizia
006400140703     C                   EXSR      Esegui
006500970526     C*
006600970526     C* FINE PGM
006700140702     C                   SETON                                        RT
006800140703     C*------------------------------------------------------------------------*
006900140703     C* ESEGUI - OPERAZIONI DI LANCIO PGM
007000140703     C*------------------------------------------------------------------------*
007100140703     C     ESEGUI        BEGSR
007200140703     C*
007300160205     C* se mi è stato passato il nr. gg pulizia uso quello
007400160205     C                   if        pIn_LogGG > 0
007500160205     C                   eval      wLogGG = pIn_LogGG
007600160205     C*
007700160205     C* altrimenti lo reperisco da tab.AOP chiave LOG-GG
007800160205     C                   else
007900160205     C*
008000160204     c                   eval      pIn_Opz = *blank
008100160204     c                   eval      pIn_TLa = *blank
008200160204     c                   eval      pIn_Cod = 'AOP'
008300160208     c                   eval      pIn_Key = 'LOG-GG'
008400160204     c                   eval      pIn_Data= 0
008500160204     c                   clear                   pOut_ERR
008600160204     c                   clear                   pOut_DS
008700160204     c                   clear                   pOut_Len
008800160204     c                   clear                   pOut_Dati
008900160204     c*
009000160204     C* chiamo il driver per reperire i giorni di mantenimento log
009100160204     C                   CALLB     'UBTBE01R'
009200160204     c                   parm                    pIn_Opz
009300160204     c                   parm                    pIn_TLa
009400160204     c                   parm                    pIn_COD
009500160204     c                   parm                    pIn_KEY
009600160204     c                   parm                    pIn_Data
009700160204     c                   parm                    pOut_ERR
009800160204     c                   parm                    pOut_DS
009900160204     c                   parm                    pOut_Len
010000160204     c                   parm                    pOut_Dati
010100160205     C*
010200160205     C                   if        pOut_Err > 0
010300160205     C                   EVAL      pmsg =
010400160205     C                             'Il giorno: ' +
010500160205     C                             %char(%date()) +
010600160205     C                             ' si è richiamato il driver UBTBE01R per +
010700160205     C                             ottenere i giorni di mantenimento log sul +
010800160205     C                             file WFMAF050F, ma si è ricevuto un errore +
010900160205     C                             di tipo: ' +
011000160205     C                             %editc(pOut_Err:'X') +
011100160205     C                             ' (0=OK  1=ERR INPUT  2=KO).'
011200160205     C* invio via e-mail se ho un errore
011300160205     C                   EXSR      Invia
011400160208     C* esco dalla routine per non eseguire l'SQL
011500160208     C                   leavesr
011600160204     C*
011700160205     C                   else
011800160205     C*
011900160205     C* reperisco i dati
012000160205     C                   SELECT
012100160205     C                   WHEN                    pOut_DS = 'DAOP001'
012200160205     C                   EVAL                    %subst(DAOP001:1:pOut_Len) =
012300160205     C                                           %subst(pOut_Dati:1:pOut_Len)
012400160205     C                   eval      wLogGG = §AOPLogGG
012500160205     C                   ENDSL
012600160205     C*
012700160205     C                   ENDIF
012800160208     C*
012900160208     C                   endif
013000160205     C*
013100160204     C* preparo l'SQL di pulizia
013200160204     C                   EVAL      wSQL = 'delete from WFMAF050F where +
013300160204     C                                     AF05DTEXER < +
013400160204     C                                     timestamp(date(days +
013500160204     C                                     (current_timestamp)-' +
013600160205     C                                     %editc(wLOGGG:'X') +
013700160204     C                                    ') , curtime())'
013800160204     C*        select current_timestamp
013900160205     C*        , timestamp(date(days(current_timestamp)-nnn) , curtime())
014000160204     C*        from sysibm/sysdummy1
014100160205     C*
014200160205     C/EXEC SQL
014300160205     C+ execute immediate :wSQL
014400160205     C/END-EXEC
014500160205     C*
014600140703     C*
014700140703     C                   ENDSR
014800970526     C*------------------------------------------------------------------------*
014900160205     C* Invia - preparo la e-mail
015000970526     C*------------------------------------------------------------------------*
015100140703     C     Invia         BEGSR
015200160205     C*
015300140702     C* invio e-mail
015400140702     C                   EXSR      EmailInvia
015500140702     C*
015600970526     C                   ENDSR
015700140702     C*------------------------------------------------------------------------*
015800140702     C* OpeIniz: operazioni iniziali
015900140702     C*------------------------------------------------------------------------*
016000140702     C     OpeIniz       BEGSR
016100140702     C*
016200140702     C                   EVAL      WX = 0
016300160205     C                   EVAL      Parm_Input = kpjbu
016400140703     C*
016500140703     C* se è stato passato un indirizzo email specifico, uso quello altrimenti uso i default
016600140703     C                   IF        pIn_Email <> *blank
016700140703     C                   EVAL      peml = pIn_Email
016800140703     C*
016900140703     C                   ELSE
017000140703     C*
017100140703     C* Reperisco il sistema AS/400 corrente
017200140703     C* e, a seconda di qual è, imposto il ricevente
017300140703     C                   callp     UBRTVNETA_Rtv(currSysNeta)
017400140703     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
017500140703     C                   EVAL      peml = 'cedalert@brt.it'
017600140703     C                   EVAL      pcceml = *blanks
017700140703     C                   else
017800140703     C                   EVAL      peml = 'cedvas@brt.it'
017900140703     C                   EVAL      pcceml = 'luciano.carini@brt.it'
018000140703     C                   endif
018100140702     C*
018200140703     C                   ENDIF
018300140703     C*
018400160205     C                   EVAL      pogg = 'PULIZIA WFMAF050F - ERRORE +
018500160205     C                             DRIVER UBTBE01R'
018600140702     C*
018700140702     C                   ENDSR
018800140702     C*------------------------------------------------------------------------*
018900140702     C* AggEmail: aggiunge rcd all'e-mail
019000140702     C*------------------------------------------------------------------------*
019100140702     C     AggEmail      BEGSR
019200140702     C*
019300140702     C*
019400140702     C                   ENDSR
019500140702     C*------------------------------------------------------------------------*
019600140702     C* EmailInvia - Invio e-mail di conferma forzatura
019700140702     C*------------------------------------------------------------------------*
019800140702     C     EmailInvia    BEGSR
019900140702     C*
020000140702     C* invio e-mail
020100140702     C                   call      'TIS701C1'
020200140702     C                   parm                    peml            253
020300140702     C                   parm                    pcceml          253
020400140702     C                   parm                    pogg             44
020500140702     C                   parm                    pmsg           5000
020600140702     C                   parm                    pesito            1
020700140702     C*
020800140702     C                   ENDSR
