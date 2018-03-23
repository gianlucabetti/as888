000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200121024     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400080924     FTIVGDTMP  uF   E             DISK
000500081007     Ftivawwwt  O    E             DISK
000600180206     FFIRAD01L  IF   E           K DISK
000700991027
000800080924     D fnvac00t      e ds
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200081007
001300180305     D* dati per il file di outout del cliente
001400180305     D SAP_Ord_NBR     s             10    inz
001500180305     D Event           s             20    inz
001600180305     D vuoto03         s             14    inz(*zeros)
001700180305     D vuoto04         s             14    inz(*zeros)
001800180305     D vuoto05         s             14    inz(*zeros)
001900180305     D End_Ist_Date    s             14    inz
002000180305     D CET_01          s              3    inz('CET')
002100180305     D CET_02          s              3    inz('CET')
002200180305     D HU              s             20    inz
002300021007
002400180305     D Intestazione    s            110    inz('SAPDN;EVENT;BEGIN_PLAN_DATE;+
002500180305     D                                     END_PLAN_DATE;BEGIN_IST_DATE;+
002600180305     D                                     END_IST_DATE;;timezone_plan;+
002700180305     D                                     timezone_act;handlingUnit')
002800180305
002900121024     D*------------------
003000121024     D* LINKING A DEFINIZIONI ESTERNE
003100121024     D*------------------
003200121024     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
003300121024     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
003400121024
003500121024
003600021007
003700991027     C                   EXSR      traduci
003800921023     C                   SETON                                        LR
003900991027
004000081007
004100081007
004200991027     C     traduci       BEGSR
004300180305     C*
004400180305     C* scrivo l'intestazione (obbligatoria e sempre uguale)
004500180305     C                   EVAL      VAWDTA = Intestazione
004600180305     C                   WRITE     TIVAW000
004700991027     C*
004800121017     C                   read      TIVGDTMP
004900121017     C                   dow       not %eof(TIVGDTMP)
005000080924     C*
005100121024     C                   setoff                                       31
005200121017     C                   eval      fnvac00t = vgdDTA
005300121024     C*
005400121024     C* Se richiesto nei parametri traduzione solo spedizione valide ai fini della spedizione =>
005500121024     C                   if        %subst(prmppt:1:1) = 'O'
005600121024     C*
005700121024     C* Inizializzo i campi d wrk
005800121024     C                   movel     *blanks       wEsito1           1
005900121024     C*
006000121024     C* Chiamata metodo GetLblTyp
006100121024     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
006200121024     C                                                vacAAS
006300121024     C                                               :vacLNP
006400121024     C                                               :vacNRS
006500121024     C                                               :vacNSP
006600121024     C                                               :pOutLblTyp
006700121024     C                                               :pOutAnnoBO
006800121024     C                                               :pOutLineaParBO
006900121024     C                                               :pOutSerieBO
007000121024     C                                               :pOutNumSpedBO
007100121024     C                                               :pOutDcmBO
007200121024     C                                               :pOutCcaBO
007300121024     C                                               :pOutRblBO))
007400121024     C*
007500121024     C* Se trattasi d bolla originale proseguo con le verifiche
007600121024     C                   if        pOutLblTyp <> 'O'
007700121024     C                   seton                                        31
007800121024     C                   endif
007900121024     C                   endif
008000180305     C*
008100180305     C                   reset                   SAP_Ord_NBR
008200180305     C                   reset                   Event
008300180305     C                   reset                   vuoto03
008400180305     C                   reset                   vuoto04
008500180305     C                   reset                   vuoto05
008600180305     C                   reset                   End_Ist_Date
008700180305     C                   reset                   CET_01
008800180305     C                   reset                   CET_02
008900180305     C                   reset                   HU
009000180306     C                   eval      SAP_Ord_Nbr = %trim(%subst(
009100180306     C                                            %editc(VACRMN:'X'):6:10))
009200180124     C*
009300180124     C* decido di cosa si tratta
009400180124     C                   select
009500180124     C*
009600180216     C* se il codice consegna anomala = 'C' = bolla in distinta (evento MIC - messa in consegna)
009700180206     C***                when      VACCCA = 'C'
009800180124     C* è la nostra messa in consegna della merce, cioè quando la mattina la filiale di destino
009900180124     C* mette a sistema in consegna la merce
010000180206     C***
010100180216     C*** adidas non la vuole
010200180216     C***
010300180124     C*
010400180216     C* se il codice consegna anomala = ' ' o '1' = Normale o prima Dirottata poi consegnata
010500180124     C                   when      (VACCCA = *blank or
010600180124     C                              VACCCA = '1'       ) and
010700180124     C*  ed è stata impostata la data consegna
010800180124     C                             VACDCM > 0            and
010900180124     C*  e questa è successiva a eventuale lasciato avviso
011000180124     C                             VACDCM >= VACDLA      and
011100180124     C*  e questa è successiva a eventuale giacenza
011200180124     C                             VACDCM >  VACDAG
011300180124     C* è la spedizione consegnata
011400180223     C* questo dato lo passo solo se richiesto a parametro
011500180223     C                   if        %subst(prmppt:2:1) = 'E'
011600180206     C                   eval      Event  = '008'
011700180305     C                   eval      End_Ist_Date = %trim(%editc(VACDCM:'X')) +
011800180305     C                                            %trim(%editc(VACHMC:'X')) +
011900180305     C                                            '00'
012000180223     C                   else
012100180223     C                   seton                                        31
012200180223     C                   endif
012300180124     C*
012400180124     C* se è stata impostata la data giacenza
012500180124     C                   when      VACDAG > 0            and
012600180124     C*  e questa è successiva o uguale a eventuale lasciato avviso
012700180124     C                             VACDAG >= VACDLA      and
012800180124     C*  e questa è successiva o uguale a eventuale consegna
012900180124     C                             VACDAG >= VACDCM
013000180206     C* è la spedizione in giacenza
013100180223     C* questo dato lo passo solo se richiesto a parametro
013200180223     C                   if        %subst(prmppt:2:1) = 'E'
013300180206     C                   eval      Event  = '016'
013400180305     C                   eval      End_Ist_Date = %trim(%editc(VACDAG:'X')) +
013500180305     C                                            '000000'
013600180223     C                   else
013700180223     C                   seton                                        31
013800180223     C                   endif
013900180124     C*
014000180124     C* se è stata impostata la data lasciato avviso
014100180206     C***                when      VACDLA > 0            and
014200180124     C*  e questa è successiva a eventuale giacenza
014300180206     C***                          VACDLA >  VACDAG      and
014400180124     C*  e questa è successiva a eventuale consegna
014500180206     C***                          VACDLA >  VACDCM
014600180124     C* non sono riuscito a consegnare e ho lasciato avviso
014700180206     C***
014800180216     C*** adidas non la vuole
014900180216     C***
015000180124     C*
015100180124     C* se il codice consegna anomala = '2' o '6' = ...
015200180206     C***                when      (VACCCA = '2'    or
015300180206     C***                           VACCCA = '6'       )
015400180124     C* è merce resa
015500180206     C***
015600180216     C*** adidas non la vuole
015700180216     C***
015800180206     C*
015900180206     C* negli altri casi passo indietro la data di consegna richiesta
016000180206     C                   other
016100180223     C* questo dato lo passo solo se richiesto a parametro
016200180223     C                   if        %subst(prmppt:2:1) = 'R'
016300180223     C                   eval      Event  = '007'
016400180305     C                   eval      End_Ist_Date = %trim(%editc(VACDCR:'X')) +
016500180305     C                                            %trim(%editc(VACHCR:'X')) +
016600180305     C                                            '00'
016700180223     C                   else
016800180223     C                   seton                                        31
016900180223     C                   endif
017000180223     C                   endsl
017100180124     C*
017200121017     C*
017300121024     C  N31              exsr      WRIOUT
017400080613     C*
017500121017     C                   delete    TIVGD000
017600121017     C                   read      TIVGDTMP
017700121017     C*
017800121017     C                   enddo
017900991027     C*
018000991027     C                   eval      wrkesito = '0'
018100991027     C*
018200910830     C                   ENDSR
018300090225
018400090225
018500090225
018600090225     C     WRIOUT        BEGSR
018700180124     C*
018800180206     C* devo scrivere tante righe quante HU trovo
018900180206     C                   eval      RADAAS =      VACAAS
019000180206     C                   eval      RADLNP =      VACLNP
019100180206     C                   eval      RADNRS =      VACNRS
019200180206     C                   eval      RADNSP =      VACNSP
019300180301     C***  k04rad01      chain     firad000
019400180222     C* se non c'è il dettaglio, scrivo lo stesso senza
019500180305      ***                if        not %found(firad01l)
019600180301     C     k04rad01      setll     firad000
019700180301     c                   if        not %equal(firad01l)
019800180305     C                   EVAL      VAWDTA =
019900180305     c                                      %trimr(SAP_Ord_NBR) + ';' +
020000180305     c                                      %trimr(Event) + ';' +
020100180305     c                                      %trimr(vuoto03) + ';' +
020200180305     c                                      %trimr(vuoto04) + ';' +
020300180305     c                                      %trimr(vuoto05) + ';' +
020400180305     c                                      %trimr(End_Ist_Date) + ';' + ';' +
020500180305     c                                      %trimr(CET_01) + ';' +
020600180305     c                                      %trimr(CET_02) + ';' +
020700180305     c                                      %trimr(HU)
020800180222     C                   WRITE     TIVAW000
020900180222
021000180222     C                   else
021100180222
021200180301     C****               dow       %found(firad01l)
021300180301     C     k04rad01      reade     firad000
021400180301     C                   dow       not %eof(firad01l)
021500180206     C*
021600180206     C                   EVAL      HU = RADUN1
021700180206     C*
021800180305     C                   EVAL      VAWDTA =
021900180305     c                                      %trimr(SAP_Ord_NBR) + ';' +
022000180305     c                                      %trimr(Event) + ';' +
022100180305     c                                      %trimr(vuoto03) + ';' +
022200180305     c                                      %trimr(vuoto04) + ';' +
022300180305     c                                      %trimr(vuoto05) + ';' +
022400180305     c                                      %trimr(End_Ist_Date) + ';' + ';' +
022500180305     c                                      %trimr(CET_01) + ';' +
022600180305     c                                      %trimr(CET_02) + ';' +
022700180305     c                                      %trimr(HU)
022800180124     C*
022900090225     C                   WRITE     TIVAW000
023000180206     C*
023100180206     C     k04rad01      reade     firad000
023200180206     C                   enddo
023300090225     C*
023400180222     C                   endif
023500180222     C*
023600090225     C                   ENDSR
023700991027
023800081007
023900081007
024000991027      /TITLE Operazioni iniziali.
024100991027     C     *inzsr        BEGSR
024200991027     C*
024300121024     C     *entry        PLIST
024400991027     C                   parm                    prmppt
024500991027     C     wrkesito      parm      wrkesito      prmesito
024600180206     C*
024700180206     C* klist
024800180206     C*
024900180206     C* FIRAD01L
025000180206     C     K04RAD01      KLIST
025100180206     C                   KFLD                    RADAAS
025200180206     C                   KFLD                    RADLNP
025300180206     C                   KFLD                    RADNRS
025400180206     C                   KFLD                    RADNSP
025500090225     C*
025600991027     C                   ENDSR
