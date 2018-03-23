000100170316     h dftactgrp(*no) actgrp(*caller) bnddir('TIS') decedit('0,')
000200060918      //********************************************************************************************
000300060918      //
000400060918      // Definizione variabili.
000500060918      //
000600060918      //********************************************************************************************
000703131009
000704170324     Ftntam01l  IF   E           k DISK
000705131009     d ICCM            s              7s 0
000706170317     d Idat            s              8s 0
000707170317     d INtw            s              3a
000708170317     d Ipkg            s              7s 1
000709170315     d Itsp            s              1a
000715131009     d Ilang           s              2a
000716170317     d oCtrPr          s              3a
000717170324     d oCtr            ds            45
000718170324     d oCtr_                          3    dim(15) overlay(oCtr)
000719170510     d*oDes            ds           750
000720170510     d*oDes_                         50    dim(15) overlay(oDes)
000721170324
000722170324     d oPgt            ds            45
000724170324     d oPgt_                          3    dim(15) overlay(oPgt)
000725131009
001000061017     D RTNesito        S             10I 0
001100061017     D RTNopcode       S             10
001200061017     D RTNstatus       S             10I 0
001201170316
002602170320     d dataiso         s               d   datfmt(*iso)
002603170510     d ric_ctr         s             10i 0 inz
002604170518     d idx             s              3  0 inz
002606170316
002607170510     D DsCtr           DS                  occurs(015) inz
002608170517     D  DSctr_cod                     3a
002609170510     D  DSctr_deco                   50A
002610170317     D* DS PER FNLV59R - CARICAMENTO E CONTROLLO TARIFFE DI 1 CLIENTE
002611170317     D fnlv59ds      E DS
002612170317     D* DS PER TRUL27R1 - Ricerca network bolla
002613170317     D trul27ds1     E DS
002614170316
002615170316       // -?Default per codice capoconto in anagrafica clienti?
002616170316     d c_Kci           c                   const(0151)
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
003301170317     C                   PARM                    Idat
003308170317     C                   PARM                    Intw
003309170317     C                   PARM                    Ipkg
003310170317     C                   PARM                    Itsp
003314131009     C                   PARM                    Ilang
003315170317     c                   parm                    oCtrPr
003316170510     c***                parm                    oCtr
003317170510     c***                parm                    oDes
003500061017     C                   PARM                    rtnesito
003600061017     C                   PARM                    rtnopcode
003700061017     C                   PARM                    rtnstatus
003800061025     c*
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
005102170317       CLEAR oCtrPr   ;
005103170317       CLEAR oCtr     ;
005104170510       //CLEAR oDes     ;
005105170510       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
005106170316
005200061026       *INLR = *ON;
005300080605
005700061025
005701131009     c                   if        ilang=*blanks
005702131009     c                   eval      ilang='it'
005703131009     c                   endif
005704170320     c                   if        idat=0
005705170320             dataiso=%date();
005706170320             idat =%dec(dataiso);
005707170320     c                   endif
011901170316
011902170317     C* Recupero network tariffa da passare a pgm di ricerca tariffe
011903170317     c                   clear                   trul27ds1
011904170317     c                   eval      i27ntw=intw
011910170320     c                   eval      I27CLI=iccm
011913170320     c                   eval      I27PKG=ipkg
011914170320
011915170317     c                   call      'TRUL27R1'
011916170317     c                   parm                    trul27ds1
011917170317
011918170316     c                   clear                   fnlv59ds
011919170316     c                   eval      ILV59KCI  = c_kci
011920170320     c                   eval      ILV59KSC  = iCcm
011921170320     c                   eval      ILV59DSP  = iDat
011922170320     c                   eval      ILV59tSP  = iTsp
011923170320     c                   eval      ILV59FIE  = o27fie
011924170320     c                   select
011925170320     c                   when      iNtw='DPD'
011926170320     c                   eval      ILV59TA3  = 'D'
011927170320     c                   when      iNtw='EEX'
011928170320     c                   eval      ILV59TA3  = 'E'
011929170320     c                   when      iNtw='FED'
011930170320     c                   eval      ILV59TA3  = 'F'
011931170320     c                   other
011932170320     c                   eval      ILV59TA3  = 'I'
011933170320     c                   endsl
011934170316     c                   eval      ILV59TBO  = 'P'
011935170316     c                   eval      ILV59RIC  = 'S'
011936170622     c                   eval      ILV59ta5  = 'W'
011937170316     c                   call      'FNLV59R'
011938170316     c                   parm                    fnlv59ds
011939170320     c                   eval      oCtrPr=olv59ctr
011940170320     c                   eval      oCtr  =olv59ctt
011941170510     c                   eval      opgt=olv59pgt
011942170510     c* carico la ds multipla per restituire questi dati come record set
011943170510     c                   z-add     1             ric_ctr
011944170518     c* nella prima ricorrenza metto sempre quella da proporre
011945170518     c     ric_ctr       occur     dsCtr
011946170518     c                   move      oCtrPr        DsCtr_cod
011947170518       chain (iccm:%int(oCtrPr):olv59prg) tntam01l;
011948170518       IF %FOUND(TNTAM01L);
011949170518          DsCtr_deco=tamdcv;
011950170518       ENDIF;
011952170518       idx=1    ;
011953170518     c                   dow       idx<=%elem(oCtr_) and
011954170518     c                             oCtr_(idx)<>*blanks
011955170518     c* escludo l'elemento se corrisponde al codice tariffa da proporre quindi
011956170518     c* già caricato nella prima ricorrenza
011957170518     c                   if        oCtr_(idx)<>oCtrPr
011958170518       ric_ctr+=1  ;
011959170510     c     ric_ctr       occur     dsCtr
011960170518     c                   move      oCtr_(idx)    DsCtr_cod
011961170518       chain (iccm:%int(octr_(idx)):%int(opgt_(idx))) tntam01l;
011962170510       IF %FOUND(TNTAM01L);
011963170510          DsCtr_deco=tamdcv;
011964170510       ENDIF;
011966170518     c                   endif
011967170518       idx+=1  ;
011968170510     c
011969170510     c                   enddo
011971170510     c
011972170510     C/EXEC SQL
011973170510     C+ SET RESULT SETS FOR RETURN TO CLIENT
011974170510     c+                 array :DsCtr  for :RIC_ctr rows
011975170510     C/END-EXEC
011976170510      /FREE
011977170510
011978170510       IF sqlCode < 0;
011979170510         RTNstatus = sqlCode;
011980170510         RTNopcode = 'SQLCODE';
011981170510         RTNesito  = -1;
011982170510         RETURN;
011983170510       ENDIF;
012032170316
013400060918
013500060918       RETURN;
013600060918
