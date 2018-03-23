000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200121024     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400180222     FFNVAC00T  uF   E             DISK
000500081007     Ftivawwwt  O    E             DISK
000600180206     FFIRAD01L  IF   E           K DISK
000700991027
000800180222     D*fnvac00t      e ds
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
002300180305
002400180305     D Intestazione    s            110    inz('SAPDN;EVENT;BEGIN_PLAN_DATE;+
002500180305     D                                     END_PLAN_DATE;BEGIN_IST_DATE;+
002600180305     D                                     END_IST_DATE;;timezone_plan;+
002700180305     D                                     timezone_act;handlingUnit')
002800021007
002900121024
003000121024     D*------------------
003100121024     D* LINKING A DEFINIZIONI ESTERNE
003200121024     D*------------------
003300121024     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
003400121024     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
003500121024
003600121024
003700021007
003800991027     C                   EXSR      traduci
003900921023     C                   SETON                                        LR
004000991027
004100081007
004200081007
004300991027     C     traduci       BEGSR
004400180305     C*
004500180305     C* scrivo l'intestazione (obbligatoria e sempre uguale)
004600180305     C                   EVAL      VAWDTA = Intestazione
004700180305     C                   WRITE     TIVAW000
004800991027     C*
004900180222     C                   read      FNVAC00T
005000180222     C                   dow       not %eof(FNVAC00T)
005100080924     C*
005200121024     C                   setoff                                       31
005300180222     C***                eval      fnvac00t = vgdDTA
005400121024     C*
005500121024     C* Se richiesto nei parametri traduzione solo spedizione valide ai fini della spedizione =>
005600121024     C                   if        %subst(prmppt:1:1) = 'O'
005700121024     C*
005800121024     C* Inizializzo i campi d wrk
005900121024     C                   movel     *blanks       wEsito1           1
006000121024     C*
006100121024     C* Chiamata metodo GetLblTyp
006200121024     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
006300121024     C                                                vacAAS
006400121024     C                                               :vacLNP
006500121024     C                                               :vacNRS
006600121024     C                                               :vacNSP
006700121024     C                                               :pOutLblTyp
006800121024     C                                               :pOutAnnoBO
006900121024     C                                               :pOutLineaParBO
007000121024     C                                               :pOutSerieBO
007100121024     C                                               :pOutNumSpedBO
007200121024     C                                               :pOutDcmBO
007300121024     C                                               :pOutCcaBO
007400121024     C                                               :pOutRblBO))
007500121024     C*
007600121024     C* Se trattasi d bolla originale proseguo con le verifiche
007700121024     C                   if        pOutLblTyp <> 'O'
007800121024     C                   seton                                        31
007900121024     C                   endif
008000121024     C                   endif
008100121024     C*
008200180305     C                   reset                   SAP_Ord_NBR
008300180305     C                   reset                   Event
008400180305     C                   reset                   vuoto03
008500180305     C                   reset                   vuoto04
008600180305     C                   reset                   vuoto05
008700180305     C                   reset                   End_Ist_Date
008800180305     C                   reset                   CET_01
008900180305     C                   reset                   CET_02
009000180305     C                   reset                   HU
009100180206     C                   eval      SAP_Ord_Nbr = %trim(%editc(VACRMN:'4'))
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
011700180216     C                   eval      End_Ist_Date = %trim(%editc(VACDCM:'4')) +
011800180216     C                                            %trim(%editc(VACHMC:'4'))
011900180223     C                   else
012000180223     C                   seton                                        31
012100180223     C                   endif
012200180124     C*
012300180124     C* se è stata impostata la data giacenza
012400180124     C                   when      VACDAG > 0            and
012500180124     C*  e questa è successiva o uguale a eventuale lasciato avviso
012600180124     C                             VACDAG >= VACDLA      and
012700180124     C*  e questa è successiva o uguale a eventuale consegna
012800180124     C                             VACDAG >= VACDCM
012900180206     C* è la spedizione in giacenza
013000180223     C* questo dato lo passo solo se richiesto a parametro
013100180223     C                   if        %subst(prmppt:2:1) = 'E'
013200180206     C                   eval      Event  = '016'
013300180216     C                   eval      End_Ist_Date = %trim(%editc(VACDAG:'4')) +
013400180216     C                                            '0000'
013500180223     C                   else
013600180223     C                   seton                                        31
013700180223     C                   endif
013800180124     C*
013900180124     C* se è stata impostata la data lasciato avviso
014000180206     C***                when      VACDLA > 0            and
014100180124     C*  e questa è successiva a eventuale giacenza
014200180206     C***                          VACDLA >  VACDAG      and
014300180124     C*  e questa è successiva a eventuale consegna
014400180206     C***                          VACDLA >  VACDCM
014500180124     C* non sono riuscito a consegnare e ho lasciato avviso
014600180206     C***
014700180216     C*** adidas non la vuole
014800180216     C***
014900180124     C*
015000180124     C* se il codice consegna anomala = '2' o '6' = ...
015100180206     C***                when      (VACCCA = '2'    or
015200180206     C***                           VACCCA = '6'       )
015300180124     C* è merce resa
015400180206     C***
015500180216     C*** adidas non la vuole
015600180216     C***
015700180206     C*
015800180206     C* negli altri casi passo indietro la data di consegna richiesta
015900180206     C                   other
016000180223     C* questo dato lo passo solo se richiesto a parametro
016100180223     C                   if        %subst(prmppt:2:1) = 'R'
016200180223     C                   eval      Event  = '007'
016300180206     C                   eval      End_Ist_Date = %trim(%editc(VACDCR:'4')) +
016400180206     C                                            %trim(%editc(VACHCR:'4'))
016500180223     C                   else
016600180223     C                   seton                                        31
016700180223     C                   endif
016800180124     C                   endsl
016900180124     C*
017000121017     C*
017100121024     C  N31              exsr      WRIOUT
017200080613     C*
017300180222     C***                delete    FNVAC000
017400180222     C                   read      FNVAC00T
017500121017     C*
017600121017     C                   enddo
017700991027     C*
017800991027     C                   eval      wrkesito = '0'
017900991027     C*
018000910830     C                   ENDSR
018100090225
018200090225
018300090225
018400090225     C     WRIOUT        BEGSR
018500180124     C*
018600180206     C* devo scrivere tante righe quante HU trovo
018700180206     C                   eval      RADAAS =      VACAAS
018800180206     C                   eval      RADLNP =      VACLNP
018900180206     C                   eval      RADNRS =      VACNRS
019000180206     C                   eval      RADNSP =      VACNSP
019100180305     C***  k04rad01      chain     firad000
019200180305     C* se non c'è il dettaglio, scrivo lo stesso senza
019300180305      ***                if        not %found(firad01l)
019400180305     C     k04rad01      setll     firad000
019500180305     c                   if        not %equal(firad01l)
019600180305     C                   EVAL      VAWDTA =
019700180305     c                                      %trimr(SAP_Ord_NBR) + ';' +
019800180305     c                                      %trimr(Event) + ';' +
019900180305     c                                      %trimr(vuoto03) + ';' +
020000180305     c                                      %trimr(vuoto04) + ';' +
020100180305     c                                      %trimr(vuoto05) + ';' +
020200180305     c                                      %trimr(End_Ist_Date) + ';' + ';' +
020300180305     c                                      %trimr(CET_01) + ';' +
020400180305     c                                      %trimr(CET_02) + ';' +
020500180305     c                                      %trimr(HU)
020600180222     C                   WRITE     TIVAW000
020700180222
020800180222     C                   else
020900180222
021000180305     C****               dow       %found(firad01l)
021100180305     C     k04rad01      reade     firad000
021200180305     C                   dow       not %eof(firad01l)
021300180206     C*
021400180206     C                   EVAL      HU = RADUN1
021500180206     C*
021600180305     C                   EVAL      VAWDTA =
021700180305     c                                      %trimr(SAP_Ord_NBR) + ';' +
021800180305     c                                      %trimr(Event) + ';' +
021900180305     c                                      %trimr(vuoto03) + ';' +
022000180305     c                                      %trimr(vuoto04) + ';' +
022100180305     c                                      %trimr(vuoto05) + ';' +
022200180305     c                                      %trimr(End_Ist_Date) + ';' + ';' +
022300180305     c                                      %trimr(CET_01) + ';' +
022400180305     c                                      %trimr(CET_02) + ';' +
022500180305     c                                      %trimr(HU)
022600180124     C*
022700090225     C                   WRITE     TIVAW000
022800180206     C*
022900180206     C     k04rad01      reade     firad000
023000180206     C                   enddo
023100090225     C*
023200180222     C                   endif
023300180222     C*
023400090225     C                   ENDSR
023500991027
023600081007
023700081007
023800991027      /TITLE Operazioni iniziali.
023900991027     C     *inzsr        BEGSR
024000991027     C*
024100121024     C     *entry        PLIST
024200991027     C                   parm                    prmppt
024300991027     C     wrkesito      parm      wrkesito      prmesito
024400180206     C*
024500180206     C* klist
024600180206     C*
024700180206     C* FIRAD01L
024800180206     C     K04RAD01      KLIST
024900180206     C                   KFLD                    RADAAS
025000180206     C                   KFLD                    RADLNP
025100180206     C                   KFLD                    RADNRS
025200180206     C                   KFLD                    RADNSP
025300090225     C*
025400991027     C                   ENDSR
