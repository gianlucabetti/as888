000100170316     h dftactgrp(*no) actgrp(*caller) bnddir('TIS') decedit('0,')
000200060918      //********************************************************************************************
000300060918      //
000400060918      // Definizione variabili.
000500060918      //
000600060918      //********************************************************************************************
000601170327
000602170407     Fazorg01l  IF   E           k DISK
000603170407     Ftabel00f  IF   E           k DISK
000709170321
000724170327     d ICCM            s              7s 0
000725170327     d idata           s              8s 0
000726170327     d Icbo            s              2a
000727170327     d Itsp            s              1a
000728170327     d Ilnp            s              3s 0
000729170327     d Ilna            s              3s 0
000730170327     d Itfa            s              3s 0
000731170327     d icad            s              9a
000732170327     d ilod            s             35a
000733170327     d INzd            s              3a
000734170327     d Iffd            s              1a
000736170327     d igc1            s              2a
000737170327     d igc2            s              2a
000738170327     d Itcr            s              1a
000739170327     d Idcr            s              8s 0
000741131009     d Ilang           s              2a
000742170316     D oMSG            S            256A
001000061017     D RTNesito        S             10I 0
001100061017     D RTNopcode       S             10
001200061017     D RTNstatus       S             10I 0
002602170316
002603170328     d dataiso         s               d   datfmt(*iso)
002604170327     d wtfp            s              3s 0
002605170330     d wdcr            s              8s 0
002606170327
002607170322     d tnsd99ds      e ds
002608170407     d tisi95ds      e ds
002609170327     d FNLV55DS      e ds
002610170327     d ds3a          e ds
002611170407     d og143         e ds
002612170316
002700060918      //********************************************************************************************
002800060918      //
002900060918      // Definizione parametri procedura.
003000060918      //
003001170316     D rtvMsgLang      PR          3512A
003002170316     D  msgId                         7A   CONST
003003170316     D  pilinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
003004170316     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
003005170316     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
003006170316     D                                     VARYING
003007170316     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
003008170327     D                                     VARYING
003009170316     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
003010170316     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
003100060918      //********************************************************************************************
003101170328
003103170328
003200060918     C     *ENTRY        PLIST
003300131009     C                   PARM                    ICCM
003301170327     c                   parm                    idata
003303170327     C                   PARM                    Icbo
003305170327     C                   PARM                    Itsp
003307170327     C                   PARM                    Ilnp
003308170327     C                   PARM                    Ilna
003309170327     C                   PARM                    Itfa
003310170327     c                   parm                    icad
003311170327     c                   parm                    ilod
003312131009     C                   PARM                    INzd
003313170327     C                   PARM                    Iffd
003314170406     c****               parm                    ifin
003315170327     c                   parm                    igc1
003316170327     c                   parm                    igc2
003317170327     C                   PARM                    Itcr
003318170327     C                   PARM                    Idcr
003320131009     C                   PARM                    Ilang
003321170316     C                   PARM                    oMSG
003500061017     C                   PARM                    rtnesito
003600061017     C                   PARM                    rtnopcode
003700061017     C                   PARM                    rtnstatus
003800061025     c*
004200060918      /FREE
004300060918
004400060918       //*******************************************************************************************
004500060918       //
004600060918       // Main.
004700060918       //
004800060918       //*******************************************************************************************
004900061017       CLEAR rtnesito;
005000061017       CLEAR rtnopcode;
005100061017       CLEAR rtnstatus;
005101170316
005102170316       CLEAR Omsg     ;
005103170316
005200061026       *INLR = *ON;
005300080605
005701131009     c                   if        ilang=*blanks
005702131009     c                   eval      ilang='it'
005703131009     c                   endif
005704170328     c                   if        idata=0
005705170328             dataiso=%date();
005706170328             idata =%dec(dataiso);
005707170328     c                   endif
005708170330     c* se spedizione in Fermo deposito errore --> data consegna richiesta non ammessa
005709170330     c                   if        iffd<>*blanks
005710170330                         eval      omsg = rtvMsgLang('TIS0866':ilang);
005711170330     c                   eval      RTNesito  = -1
005712170330     c                   endif
005713170330
005714170406     c* se richiesto "Dopo Il" va sempre bene quindi vado a fine
005715170330     c                   if        itcr='D'
005716170330     c                   return
005717170330     c                   endif
005718170406     c* se lnp o lna vuoti o tipo servizio vuoti--> errore parametri
005719170406     c                   if        ilnp=0 or ilna=0 or itsp=*blanks
005720170406     c                   eval      RTNesito  = -1
005721170406     c                   eval      RTNopcode = 'ERR PARM'
005722170406     c                   return
005723170406     c                   endif
011901170316
011902170322     C                   CLEAR                   tnsd99ds
011903170322     C*
011907170322     C                   eval      D98TSP = itsp
011908170327     c                   if        itsp ='D'
011909170327     c                   eval      d98tsp ='C'
011910170327     c                   endif
011911170322     C                   eval      D98CAD = icad
011912170322     C                   eval      D98LOD = ilod
011913170322     C                   eval      D98NZD = inzd
011914170406     C****               eval      D98fin = ifin
011915170407     c* il flag inoltro lo devo calcolare
011916170407     C                   CLEAR                   Tisi95DS
011917170407     C                   MOVEL     ilod          I95LOC
011918170407     C                   MOVEL     icad          I95CAP
011919170407     C                   MOVEL     inzd          I95NAR
011920170407     C                   MOVEL     '3'           I95TCN
011921170407     C                   MOVEL     idata         I95DAT
011922170407     c* recupero network della lna
011923170407     c                   clear                   og143
011924170407     c     ilna          chain     azorg01l
011925170407     c                   if        %found(azorg01l)
011926170407     c                   eval      og143=orgde3
011927170407     c                   endif
011928170407     c                   if        §ogntw='FED'
011929170407     c                   movel     'S'           i95fi1
011930170407     c                   endif
011931170407     c                   call      'TISI95R'
011932170407     c                   parm                    tisi95ds
011933170407     c                   if        o95err=*blanks
011934170407     C                   eval      D98fin = o95iso
011935170407     c                   endif
011936170322     C*
011937170322     C* TERMINAL DI PARTENZA
011938170327     c                   clear                   wtfp
011939170327     c                   exsr      rtvtfp
011940170327     C                   eval      D98TFP = wtfp
011941170322     C*
011942170323     C                   eval      D98tfa = itfa
011943170323     C                   eval      D98lna = ilna
011944170323     C                   eval      D98lnp = ilnp
011945170323     c                   eval      d98dsp = idata
011946170323     C                   MOVE      idata         D98MGS
011947170323     c* a differenza di ls01r passo anche quando ' P' o ' M' fare poi una prova con Bari
011948170323     c                   eval      d98gc1 = igc1
011949170323     c                   eval      d98gc2 = igc2
011950170327     c* dal codice bolla determino il tipo bolla
011951170327          clear ds3a;
011952170327          chain (1:'3A':icbo) tabel00f     ;
011953170327          if %found(tabel00f);
011954170327             eval ds3a = tbluni    ;
011955170327          endif;
011956170327     C                   eval      D98TBL = §3atb1
011957170323     C                   eval      D98KSC = iccm
011958170327     C                   eval      d98ffd = iffd
011962170322     C*
011963170322     C                   CALL      'TNSD99R'
011964170322     C                   PARM                    tnsd99ds
011967170330     c                   select
011968170330     c                   when      d98dtctd>*zeros
011969170330     c                   move      d98dtctd      wdcr
011970170330     c                   when      d98dee>*zeros
011971170330     c                   eval      wdcr=d98dee
011972170330     c                   other
011974170330     c                   eval      wdcr=d98dce
011975170330     c                   endsl
011976170330     c                   if        wdcr = 0
011977170330     c                   eval      RTNesito  = -1
011978170330     c                   eval      RTNopcode = 'ERR PARM'
011979170330     c                   else
011980170601     c                   if        idcr < wdcr or (idcr=wdcr AND itcr='P')
011981170327     c* Errore:  Consegna richiesta MINORE della data consegna teorica.
011982170327                         eval      omsg = rtvMsgLang('TIS0860':ilang);
011983170327     c                   eval      RTNesito  = -1
011984170327     c                   endif
011985170330     c                   endif
012700061017
013400060918
013500060918       RETURN;
013600060918
013700170327      /TITLE Reperisco il terminal di partenza
013800170327     C     rtvTFP        BEGSR
013900170327     C*
014000170327     C                   clear                   fnlv55ds
014100170327     C                   movel     'P'           d55tpt
014200170327     C                   z-add     iLNP          d55lin
014400170327     C                   z-add     iDATA         d55drf
014500170327     C                   call      'FNLV55R'
014600170327     C                   PARM                    fnlv55ds
014700170327     C*
014800170327     C                   if        d55err = *blanks
014900170327     C                   z-add     d55tfp        wTFP
015000170327     C                   else
015400170327     C                   eval      RTNesito  = -1
015401170327     C                   eval      RTNopcode = 'ERR TFP'
015700170327     C                   return
015800170327     C                   endif
015900170327     C*
016000170327     C                   ENDSR
