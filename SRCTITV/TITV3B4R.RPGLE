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
010000180307     C*
010100180309     C* se è richiesto l'appuntamento o la GDO
010200180307     C* (testo tutti e 2 i flag perché potrebbe il traduttore mette nel TC1 ma un operatore
010300180307     C*  potrebbe averlo aggiunto dopo e inserito nel secondo flag)
010400180307     C                   when      (VACTC1 = 'A' or VACTC2 = 'A' or
010500180307     C                              VACTC1 = 'S' or VACTC2 = 'S') and
010600180307     C* se il codice consegna anomala = 'C' = bolla in distinta (evento MIC - messa in consegna)
010700180307     C                             VACCCA = 'C' and
010800180307     C*  ed è impostata la data consegna merce ipotetica
010900180307     C                             VACDCM > 0
011000180307     C                   if        %subst(prmppt:2:1) = 'R'
011100180309     C                   eval      Event  = '007'
011200180309     C*  se è valorizzata la data consegna concordata passo quelle
011300180309     C                   if        VACDCR <> 0
011400180307     C                   eval      End_Ist_Date = %trim(%editc(VACDCR:'X')) +
011500180307     C                                            %trim(%editc(VACHCR:'X')) +
011600180307     C                                            '00'
011700180309     C*  altrimenti la data consegna merce ipotetica
011800180307     C                   else
011900180309     C                   eval      End_Ist_Date = %trim(%editc(VACDCM:'X')) +
012000180309     C                                            %trim(%editc(VACHMC:'X')) +
012100180309     C                                            '00'
012200180309     C                   endif
012300180309     C                   else
012400180307     C                   seton                                        31
012500180307     C                   endif
012600180124     C*
012700180216     C* se il codice consegna anomala = ' ' o '1' = Normale o prima Dirottata poi consegnata
012800180124     C                   when      (VACCCA = *blank or
012900180124     C                              VACCCA = '1'       ) and
013000180124     C*  ed è stata impostata la data consegna
013100180124     C                             VACDCM > 0            and
013200180124     C*  e questa è successiva a eventuale lasciato avviso
013300180124     C                             VACDCM >= VACDLA      and
013400180124     C*  e questa è successiva a eventuale giacenza
013500180124     C                             VACDCM >  VACDAG
013600180124     C* è la spedizione consegnata
013700180223     C* questo dato lo passo solo se richiesto a parametro
013800180223     C                   if        %subst(prmppt:2:1) = 'E'
013900180206     C                   eval      Event  = '008'
014000180305     C                   eval      End_Ist_Date = %trim(%editc(VACDCM:'X')) +
014100180305     C                                            %trim(%editc(VACHMC:'X')) +
014200180305     C                                            '00'
014300180223     C                   else
014400180223     C                   seton                                        31
014500180223     C                   endif
014600180124     C*
014700180124     C* se è stata impostata la data giacenza
014800180124     C                   when      VACDAG > 0            and
014900180124     C*  e questa è successiva o uguale a eventuale lasciato avviso
015000180124     C                             VACDAG >= VACDLA      and
015100180124     C*  e questa è successiva o uguale a eventuale consegna
015200180307     C                             VACDAG >  VACDCM
015300180206     C* è la spedizione in giacenza
015400180223     C* questo dato lo passo solo se richiesto a parametro
015500180223     C                   if        %subst(prmppt:2:1) = 'E'
015600180206     C                   eval      Event  = '016'
015700180305     C                   eval      End_Ist_Date = %trim(%editc(VACDAG:'X')) +
015800180305     C                                            '000000'
015900180223     C                   else
016000180223     C                   seton                                        31
016100180223     C                   endif
016200180124     C*
016300180124     C* se è stata impostata la data lasciato avviso
016400180206     C***                when      VACDLA > 0            and
016500180124     C*  e questa è successiva a eventuale giacenza
016600180206     C***                          VACDLA >  VACDAG      and
016700180124     C*  e questa è successiva a eventuale consegna
016800180206     C***                          VACDLA >  VACDCM
016900180124     C* non sono riuscito a consegnare e ho lasciato avviso
017000180206     C***
017100180216     C*** adidas non la vuole
017200180216     C***
017300180124     C*
017400180124     C* se il codice consegna anomala = '2' o '6' = ...
017500180206     C***                when      (VACCCA = '2'    or
017600180206     C***                           VACCCA = '6'       )
017700180124     C* è merce resa
017800180206     C***
017900180216     C*** adidas non la vuole
018000180216     C***
018100180206     C*
018200180307     C* negli altri casi NON passo indietro nulla
018300180206     C                   other
018400180307     C                   seton                                        31
018500180223     C                   endsl
018600180124     C*
018700121017     C*
018800121024     C  N31              exsr      WRIOUT
018900080613     C*
019000121017     C                   delete    TIVGD000
019100121017     C                   read      TIVGDTMP
019200121017     C*
019300121017     C                   enddo
019400991027     C*
019500991027     C                   eval      wrkesito = '0'
019600991027     C*
019700910830     C                   ENDSR
019800090225
019900090225
020000090225
020100090225     C     WRIOUT        BEGSR
020200180124     C*
020300180306     C* se evento NON 007
020400180206     C* devo scrivere tante righe quante HU trovo
020500180306     C* se evento 007
020600180306     C* devo scrivere una sola riga senza tenere conto delle HU
020700180206     C                   eval      RADAAS =      VACAAS
020800180206     C                   eval      RADLNP =      VACLNP
020900180206     C                   eval      RADNRS =      VACNRS
021000180206     C                   eval      RADNSP =      VACNSP
021100180222     C* se non c'è il dettaglio, scrivo lo stesso senza
021200180301     C     k04rad01      setll     firad000
021300180306
021400180301     c                   if        not %equal(firad01l)
021500180306     c                             or Event = '007'
021600180305     C                   EVAL      VAWDTA =
021700180305     c                                      %trimr(SAP_Ord_NBR) + ';' +
021800180305     c                                      %trimr(Event) + ';' +
021900180305     c                                      %trimr(vuoto03) + ';' +
022000180305     c                                      %trimr(vuoto04) + ';' +
022100180305     c                                      %trimr(vuoto05) + ';' +
022200180305     c                                      %trimr(End_Ist_Date) + ';' + ';' +
022300180306     c                                      ';' + ';'
022400180222     C                   WRITE     TIVAW000
022500180222
022600180222     C                   else
022700180222
022800180301     C     k04rad01      reade     firad000
022900180301     C                   dow       not %eof(firad01l)
023000180206     C*
023100180206     C                   EVAL      HU = RADUN1
023200180206     C*
023300180305     C                   EVAL      VAWDTA =
023400180305     c                                      %trimr(SAP_Ord_NBR) + ';' +
023500180305     c                                      %trimr(Event) + ';' +
023600180305     c                                      %trimr(vuoto03) + ';' +
023700180305     c                                      %trimr(vuoto04) + ';' +
023800180305     c                                      %trimr(vuoto05) + ';' +
023900180305     c                                      %trimr(End_Ist_Date) + ';' + ';' +
024000180305     c                                      %trimr(CET_01) + ';' +
024100180305     c                                      %trimr(CET_02) + ';' +
024200180305     c                                      %trimr(HU)
024300180124     C*
024400090225     C                   WRITE     TIVAW000
024500180206     C*
024600180206     C     k04rad01      reade     firad000
024700180206     C                   enddo
024800090225     C*
024900180222     C                   endif
025000180222     C*
025100090225     C                   ENDSR
025200991027
025300081007
025400081007
025500991027      /TITLE Operazioni iniziali.
025600991027     C     *inzsr        BEGSR
025700991027     C*
025800121024     C     *entry        PLIST
025900991027     C                   parm                    prmppt
026000991027     C     wrkesito      parm      wrkesito      prmesito
026100180206     C*
026200180206     C* klist
026300180206     C*
026400180206     C* FIRAD01L
026500180206     C     K04RAD01      KLIST
026600180206     C                   KFLD                    RADAAS
026700180206     C                   KFLD                    RADLNP
026800180206     C                   KFLD                    RADNRS
026900180206     C                   KFLD                    RADNSP
027000090225     C*
027100991027     C                   ENDSR
