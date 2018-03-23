000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200121024     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400080924     FTIVGDTMP  uF   E             DISK
000500081007     Ftivawwwt  O    E             DISK
000600991027
000700080924     D fnvac00t      e ds
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100081007
001200180124     D* formato del file di outout del cliente
001300180124     D TKINP00F        ds
001400180130     D  vuoto01                       3
001500180130     D  TIMHAW                       30
001600180130     D  TIHAWB                       30
001700180130     D  vuoto04                       3
001800180130     D  vuoto05                       3
001900180130     D  vuoto06                       5
002000180130     D  vuoto07                       8
002100180130     D  vuoto08                      30
002200180130     D  vuoto09                      30
002300180130     D  vuoto10                       8
002400180208     D  TISTEP                       15
002500180130     D  TISTDD                       40
002600180130     D  TIDTST                        8
002700180130     D  TIHHST                        6
002800180130     D  vuoto15                       8
002900180130     D  vuoto16                       6
003000180208     D  TISTFI                        1
003100180130     D  vuoto18                       1
003200021007
003300121024
003400121024     D*------------------
003500121024     D* LINKING A DEFINIZIONI ESTERNE
003600121024     D*------------------
003700121024     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
003800121024     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
003900121024
004000121024
004100021007
004200991027     C                   EXSR      traduci
004300921023     C                   SETON                                        LR
004400991027
004500081007
004600081007
004700991027     C     traduci       BEGSR
004800991027     C*
004900121017     C                   read      TIVGDTMP
005000121017     C                   dow       not %eof(TIVGDTMP)
005100080924     C*
005200121024     C                   setoff                                       31
005300121017     C                   eval      fnvac00t = vgdDTA
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
008200180124     C                   reset                   TKINP00F
008300180124     C                   eval      TIMHAW = %trim(%editc(VACRMN:'4'))
008400180124     C                   eval      TIHAWB = %trim(%editc(VACRMN:'4'))
008500180124     C*
008600180124     C* decido di cosa si tratta
008700180124     C                   select
008800180124     C*
008900180209     C* se il codice consegna anomala = 'C' = bolla in distinta (evento MIC - messa in consegna)
009000180124     C                   when      VACCCA = 'C'
009100180124     C* è la nostra messa in consegna della merce, cioè quando la mattina la filiale di destino
009200180124     C* mette a sistema in consegna la merce
009300180130     C                   eval      TISTDD = 'ARRIVO DEPOSITO VETTORE'
009400180208     C                   eval      TISTEP = '000000000000040'
009500180209     C                   eval      TIDTST =  %trim(%editc(VACDCM:'4'))
009600180209     C                   eval      TIHHST =  %trim(%editc(VACHMC:'4'))
009700180124     C*
009800180209     C* se il codice consegna anomala = ' ' o '1' = Normale o prima Dirottata poi consegnata
009900180124     C                   when      (VACCCA = *blank or
010000180124     C                              VACCCA = '1'       ) and
010100180124     C*  ed è stata impostata la data consegna
010200180124     C                             VACDCM > 0            and
010300180124     C*  e questa è successiva a eventuale lasciato avviso
010400180124     C                             VACDCM >= VACDLA      and
010500180124     C*  e questa è successiva a eventuale giacenza
010600180124     C                             VACDCM >  VACDAG
010700180124     C* è la spedizione consegnata
010800180124     C                   eval      TISTDD = 'CONSEGNA A CLIENTE'
010900180208     C                   eval      TISTEP = '000000000000060'
011000180209     C                   eval      TIDTST =  %trim(%editc(VACDCM:'4'))
011100180209     C                   eval      TIHHST =  %trim(%editc(VACHMC:'4'))
011200180208     C                   eval      TISTFI = 'Y'
011300180124     C*
011400180124     C* se è stata impostata la data giacenza
011500180124     C                   when      VACDAG > 0            and
011600180124     C*  e questa è successiva o uguale a eventuale lasciato avviso
011700180124     C                             VACDAG >= VACDLA      and
011800180124     C*  e questa è successiva o uguale a eventuale consegna
011900180209     C                             VACDAG >  VACDCM
012000180124     C* è la spedizione consegnata
012100180124     C                   eval      TISTDD = 'GIACENZA'
012200180209     C                   eval      TIDTST =  %trim(%editc(VACDAG:'4'))
012300180209     c                   eval      TIHHST =  '0000'
012400180124     C*
012500180124     C* se è stata impostata la data lasciato avviso
012600180124     C                   when      VACDLA > 0            and
012700180124     C*  e questa è successiva a eventuale giacenza
012800180124     C                             VACDLA >  VACDAG      and
012900180124     C*  e questa è successiva a eventuale consegna
013000180124     C                             VACDLA >  VACDCM
013100180124     C* non sono riuscito a consegnare e ho lasciato avviso
013200180124     C                   eval      TISTDD = 'LASCIATO AVVISO'
013300180209     C                   eval      TIDTST =  %trim(%editc(VACDLA:'4'))
013400180209     C                   eval      TIHHST =  '0000'
013500180124     C*
013600180209     C* se il codice consegna anomala = '2' o '6' = COSEGNA ANOMALA DI "RESO"
013700180209     C*                                             reso senza avaria, reso con avaria
013800180209     C* Teoricamente in caso di reso il dato viene restituito due volte :
013900180209     C* 1) data di creazione bolla di reso non di reso effettivo
014000180209     C* 2) reso effettivo
014100180124     C                   when      (VACCCA = '2'    or
014200180124     C                              VACCCA = '6'       )
014300180124     C* è merce resa
014400180124     C                   eval      TISTDD = 'MERCE RESA'
014500180209     C                   eval      TIDTST =  %trim(%editc(VACDCM:'4'))
014600180209     C                   eval      TIHHST =  %trim(%editc(VACHMC:'4'))
014700180124     C                   endsl
014800180124     C*
014900121017     C*
015000121024     C  N31              exsr      WRIOUT
015100080613     C*
015200121017     C                   delete    TIVGD000
015300121017     C                   read      TIVGDTMP
015400121017     C*
015500121017     C                   enddo
015600991027     C*
015700991027     C                   eval      wrkesito = '0'
015800991027     C*
015900910830     C                   ENDSR
016000090225
016100090225
016200090225
016300090225     C     WRIOUT        BEGSR
016400180124     C*
016500180124     C                   EVAL      VAWDTA = TKINP00F
016600180124     C*
016700090225     C                   WRITE     TIVAW000
016800090225     C*
016900090225     C                   ENDSR
017000991027
017100081007
017200081007
017300991027      /TITLE Operazioni iniziali.
017400991027     C     *inzsr        BEGSR
017500991027     C*
017600121024     C     *entry        PLIST
017700991027     C                   parm                    prmppt
017800991027     C     wrkesito      parm      wrkesito      prmesito
017900090225     C*
018000991027     C                   ENDSR
