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
001300180124     D* formato del file di outout del cliente
001400180206     D Adidas_rtn      ds
001500180222     D  SAP_Ord_NBR                  10    inz
001600180222     D  semicolon01                   1    inz(';')
001700180222     D  Event                        20    inz
001800180222     D  semicolon02                   1    inz(';')
001900180222     D  vuoto03                      14    inz(*zeros)
002000180222     D  semicolon03                   1    inz(';')
002100180222     D  vuoto04                      14    inz(*zeros)
002200180222     D  semicolon04                   1    inz(';')
002300180222     D  vuoto05                      14    inz(*zeros)
002400180222     D  semicolon05                   1    inz(';')
002500180222     D  End_Ist_Date                 14    inz
002600180222     D  semicolon06                   1    inz(';')
002700180222     D  semicolon07                   1    inz(';')
002800180222     D  CET_01                        3    inz('CET')
002900180222     D  semicolon08                   1    inz(';')
003000180222     D  CET_02                        3    inz('CET')
003100180222     D  semicolon09                   1    inz(';')
003200180222     D  HU                           20    inz
003300021007
003400121024
003500121024     D*------------------
003600121024     D* LINKING A DEFINIZIONI ESTERNE
003700121024     D*------------------
003800121024     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
003900121024     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
004000121024
004100121024
004200021007
004300991027     C                   EXSR      traduci
004400921023     C                   SETON                                        LR
004500991027
004600081007
004700081007
004800991027     C     traduci       BEGSR
004900991027     C*
005000180222     C                   read      FNVAC00T
005100180222     C                   dow       not %eof(FNVAC00T)
005200080924     C*
005300121024     C                   setoff                                       31
005400180222     C***                eval      fnvac00t = vgdDTA
005500121024     C*
005600121024     C* Se richiesto nei parametri traduzione solo spedizione valide ai fini della spedizione =>
005700121024     C                   if        %subst(prmppt:1:1) = 'O'
005800121024     C*
005900121024     C* Inizializzo i campi d wrk
006000121024     C                   movel     *blanks       wEsito1           1
006100121024     C*
006200121024     C* Chiamata metodo GetLblTyp
006300121024     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
006400121024     C                                                vacAAS
006500121024     C                                               :vacLNP
006600121024     C                                               :vacNRS
006700121024     C                                               :vacNSP
006800121024     C                                               :pOutLblTyp
006900121024     C                                               :pOutAnnoBO
007000121024     C                                               :pOutLineaParBO
007100121024     C                                               :pOutSerieBO
007200121024     C                                               :pOutNumSpedBO
007300121024     C                                               :pOutDcmBO
007400121024     C                                               :pOutCcaBO
007500121024     C                                               :pOutRblBO))
007600121024     C*
007700121024     C* Se trattasi d bolla originale proseguo con le verifiche
007800121024     C                   if        pOutLblTyp <> 'O'
007900121024     C                   seton                                        31
008000121024     C                   endif
008100121024     C                   endif
008200121024     C*
008300180206     C                   reset                   Adidas_rtn
008400180206     C                   eval      SAP_Ord_Nbr = %trim(%editc(VACRMN:'4'))
008500180124     C*
008600180124     C* decido di cosa si tratta
008700180124     C                   select
008800180124     C*
008900180216     C* se il codice consegna anomala = 'C' = bolla in distinta (evento MIC - messa in consegna)
009000180206     C***                when      VACCCA = 'C'
009100180124     C* è la nostra messa in consegna della merce, cioè quando la mattina la filiale di destino
009200180124     C* mette a sistema in consegna la merce
009300180206     C***
009400180216     C*** adidas non la vuole
009500180216     C***
009600180124     C*
009700180216     C* se il codice consegna anomala = ' ' o '1' = Normale o prima Dirottata poi consegnata
009800180124     C                   when      (VACCCA = *blank or
009900180124     C                              VACCCA = '1'       ) and
010000180124     C*  ed è stata impostata la data consegna
010100180124     C                             VACDCM > 0            and
010200180124     C*  e questa è successiva a eventuale lasciato avviso
010300180124     C                             VACDCM >= VACDLA      and
010400180124     C*  e questa è successiva a eventuale giacenza
010500180124     C                             VACDCM >  VACDAG
010600180124     C* è la spedizione consegnata
010700180223     C* questo dato lo passo solo se richiesto a parametro
010800180223     C                   if        %subst(prmppt:2:1) = 'E'
010900180206     C                   eval      Event  = '008'
011000180216     C                   eval      End_Ist_Date = %trim(%editc(VACDCM:'4')) +
011100180216     C                                            %trim(%editc(VACHMC:'4'))
011200180223     C                   else
011300180223     C                   seton                                        31
011400180223     C                   endif
011500180124     C*
011600180124     C* se è stata impostata la data giacenza
011700180124     C                   when      VACDAG > 0            and
011800180124     C*  e questa è successiva o uguale a eventuale lasciato avviso
011900180124     C                             VACDAG >= VACDLA      and
012000180124     C*  e questa è successiva o uguale a eventuale consegna
012100180124     C                             VACDAG >= VACDCM
012200180206     C* è la spedizione in giacenza
012300180223     C* questo dato lo passo solo se richiesto a parametro
012400180223     C                   if        %subst(prmppt:2:1) = 'E'
012500180206     C                   eval      Event  = '016'
012600180216     C                   eval      End_Ist_Date = %trim(%editc(VACDAG:'4')) +
012700180216     C                                            '0000'
012800180223     C                   else
012900180223     C                   seton                                        31
013000180223     C                   endif
013100180124     C*
013200180124     C* se è stata impostata la data lasciato avviso
013300180206     C***                when      VACDLA > 0            and
013400180124     C*  e questa è successiva a eventuale giacenza
013500180206     C***                          VACDLA >  VACDAG      and
013600180124     C*  e questa è successiva a eventuale consegna
013700180206     C***                          VACDLA >  VACDCM
013800180124     C* non sono riuscito a consegnare e ho lasciato avviso
013900180206     C***
014000180216     C*** adidas non la vuole
014100180216     C***
014200180124     C*
014300180124     C* se il codice consegna anomala = '2' o '6' = ...
014400180206     C***                when      (VACCCA = '2'    or
014500180206     C***                           VACCCA = '6'       )
014600180124     C* è merce resa
014700180206     C***
014800180216     C*** adidas non la vuole
014900180216     C***
015000180206     C*
015100180206     C* negli altri casi passo indietro la data di consegna richiesta
015200180206     C                   other
015300180223     C* questo dato lo passo solo se richiesto a parametro
015400180223     C                   if        %subst(prmppt:2:1) = 'R'
015500180223     C                   eval      Event  = '007'
015600180206     C                   eval      End_Ist_Date = %trim(%editc(VACDCR:'4')) +
015700180206     C                                            %trim(%editc(VACHCR:'4'))
015800180223     C                   else
015900180223     C                   seton                                        31
016000180223     C                   endif
016100180124     C                   endsl
016200180124     C*
016300121017     C*
016400121024     C  N31              exsr      WRIOUT
016500080613     C*
016600180222     C***                delete    FNVAC000
016700180222     C                   read      FNVAC00T
016800121017     C*
016900121017     C                   enddo
017000991027     C*
017100991027     C                   eval      wrkesito = '0'
017200991027     C*
017300910830     C                   ENDSR
017400090225
017500090225
017600090225
017700090225     C     WRIOUT        BEGSR
017800180124     C*
017900180206     C* devo scrivere tante righe quante HU trovo
018000180206     C                   eval      RADAAS =      VACAAS
018100180206     C                   eval      RADLNP =      VACLNP
018200180206     C                   eval      RADNRS =      VACNRS
018300180206     C                   eval      RADNSP =      VACNSP
018400180206     C     k04rad01      chain     firad000
018500180222     C* se non c'è il dettaglio, scrivo lo stesso senza
018600180222     C                   if        not %found(firad01l)
018700180222     C                   EVAL      VAWDTA = Adidas_rtn
018800180222     C                   WRITE     TIVAW000
018900180222
019000180222     C                   else
019100180222
019200180222     C                   dow       %found(firad01l)
019300180206     C*
019400180206     C                   EVAL      HU = RADUN1
019500180206     C*
019600180206     C                   EVAL      VAWDTA = Adidas_rtn
019700180124     C*
019800090225     C                   WRITE     TIVAW000
019900180206     C*
020000180206     C     k04rad01      reade     firad000
020100180206     C                   enddo
020200090225     C*
020300180222     C                   endif
020400180222     C*
020500090225     C                   ENDSR
020600991027
020700081007
020800081007
020900991027      /TITLE Operazioni iniziali.
021000991027     C     *inzsr        BEGSR
021100991027     C*
021200121024     C     *entry        PLIST
021300991027     C                   parm                    prmppt
021400991027     C     wrkesito      parm      wrkesito      prmesito
021500180206     C*
021600180206     C* klist
021700180206     C*
021800180206     C* FIRAD01L
021900180206     C     K04RAD01      KLIST
022000180206     C                   KFLD                    RADAAS
022100180206     C                   KFLD                    RADLNP
022200180206     C                   KFLD                    RADNRS
022300180206     C                   KFLD                    RADNSP
022400090225     C*
022500991027     C                   ENDSR
