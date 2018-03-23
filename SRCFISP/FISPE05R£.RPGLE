000100170316     h dftactgrp(*no) actgrp(*caller) bnddir('TIS') decedit('0,')
000200060918      //********************************************************************************************
000300060918      //
000400060918      // Definizione variabili.
000500060918      //
000600060918      //********************************************************************************************
000703131009
000704131009     d ICCM            s              7s 0
000705131009     d Ilna            s              3s 0
000706131009     d INzd            s              3a
000707170315     d Icbo            s              2a
000708170315     d Itsp            s              1a
000710170315     d INzm            s              3a
000711170315     d Ictr            s              3a
000712170315     d Itic            s              2a
000713170315     d Iimp            s             13s 3
000714170315     d Idiv            s              3a
000715131009     d Ilang           s              2a
000716170316     D oMSG            S            256A
000717131009
001000061017     D RTNesito        S             10I 0
001100061017     D RTNopcode       S             10
001200061017     D RTNstatus       S             10I 0
001201170316
002601170316     d wnum            s             14
002602170316
002603170316     d trul21ds      e ds
002604170316
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
003008170316     D                                     VARYING
003009170316     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
003010170316     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
003100060918      //********************************************************************************************
003200060918     C     *ENTRY        PLIST
003300131009     C                   PARM                    ICCM
003301131009     C                   PARM                    Ilna
003302131009     C                   PARM                    INzd
003303170316     c* da icbo a ictr: sono campi che il trul21r riceve ma non utilizza
003305170316     C                   PARM                    Icbo
003306170315     C                   PARM                    Itsp
003308170315     C                   PARM                    Inzm
003309170315     C                   PARM                    Ictr
003311170315     C                   PARM                    Itic
003312170315     C                   PARM                    Iimp
003313170315     C                   PARM                    Idiv
003314131009     C                   PARM                    Ilang
003315170316     C                   PARM                    oMSG
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
005500060918
005600060918      /END-FREE
005700061025
005701131009     c                   if        ilang=*blanks
005702131009     c                   eval      ilang='it'
005703131009     c                   endif
011901170316
011902170316     c                   clear                   trul21ds
011903170316     c                   eval      I21TLA='L'
011904170316     c                   eval      I21CBO=icbo
011905170316     c                   eval      I21TSP=itsp
011907170316     c                   eval      I21NZM=inzm
011908170316     c                   eval      I21LNA=ilna
011909170316     c                   eval      I21NZD=inzd
011910170316     c                   eval      I21KSC=iccm
011911170316     c                   eval      I21CTR=ictr
011912170316     c                   eval      I21TIC=itic
011913170316     c                   eval      I21IMP=iimp
011914170316     c                   eval      I21DIV=idiv
011915170316     c                   eval      I21PGM='FNLS01R'
011916170316     c                   call      'TRUL21R'
011917170316     c                   parm                    TRUL21ds
011918170316     c                   select
011919170316     C* IMPORTO MAGGIORE DEL LIMITE MAX
011920170316     c                   when      o21fx2<>*blanks
011921170316     c                   evalr     wnum = %editc(o21lx2:'4')
011922170316                         eval      omsg = rtvMsgLang('TIS0862':ilang:wnum);
011923170316     c                   eval      RTNesito  = -1
011924170317     c****               eval      RTNopcode = 'ERR MAX'
011925170317     c****               eval      RTNstatus = -1
011926170316
011927170316     C* IMPORTO INFERIORE AL LIMITE MINIMO
012000170316     c                   when      o21fmn<>*blanks
012001170316     c                   evalr     wnum = %editc(o21lmn:'4')
012002170316                         eval      omsg = rtvMsgLang('TIS0863':ilang:wnum);
012003170316     c                   eval      RTNesito  = -1
012004170317     c***                eval      RTNopcode = 'ERR MIN'
012005170317     c***                eval      RTNstatus = -1
012006170316     c                   endsl
012032170316
012600060918      /FREE
012700061017
013400060918
013500060918       RETURN;
013600060918
013700060918      /END-FREE
