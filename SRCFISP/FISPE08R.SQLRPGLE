000100170316     h dftactgrp(*no) actgrp(*caller) bnddir('TIS') decedit('0,')
000200060918      //********************************************************************************************
000300060918      //
000400060918      // Definizione variabili.
000500060918      //
000600060918      //********************************************************************************************
000700131009
000800170324     Ftntam01l  IF   E           k DISK
000900131009     d ICCM            s              7s 0
001000170317     d Idat            s              8s 0
001100170317     d INtw            s              3a
001200170317     d Ipkg            s              7s 1
001300170315     d Itsp            s              1a
001400131009     d Ilang           s              2a
001500170317     d oCtrPr          s              3a
001600170324     d oCtr            ds            45
001700170324     d oCtr_                          3    dim(15) overlay(oCtr)
001800170510     d*oDes            ds           750
001900170510     d*oDes_                         50    dim(15) overlay(oDes)
002000170324
002100170324     d oPgt            ds            45
002200170324     d oPgt_                          3    dim(15) overlay(oPgt)
002300131009
002400061017     D RTNesito        S             10I 0
002500061017     D RTNopcode       S             10
002600061017     D RTNstatus       S             10I 0
002700170316
002800170320     d dataiso         s               d   datfmt(*iso)
002900170510     d ric_ctr         s             10i 0 inz
003000170518     d idx             s              3  0 inz
003100170316
003200170510     D DsCtr           DS                  occurs(015) inz
003300170517     D  DSctr_cod                     3a
003400170510     D  DSctr_deco                   50A
003500170317     D* DS PER FNLV59R - CARICAMENTO E CONTROLLO TARIFFE DI 1 CLIENTE
003600170317     D fnlv59ds      E DS
003700170317     D* DS PER TRUL27R1 - Ricerca network bolla
003800170317     D trul27ds1     E DS
003900170316
004000170316       // -?Default per codice capoconto in anagrafica clienti?
004100170316     d c_Kci           c                   const(0151)
004200060918      //********************************************************************************************
004300060918      //
004400060918      // Definizione parametri procedura.
004500060918      //
004600170316     D rtvMsgLang      PR          3512A
004700170316     D  msgId                         7A   CONST
004800170316     D  pilinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
004900170316     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
005000170316     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
005100170316     D                                     VARYING
005200170316     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
005300170316     D                                     VARYING
005400170316     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
005500170316     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
005600060918      //********************************************************************************************
005700060918     C     *ENTRY        PLIST
005800131009     C                   PARM                    ICCM
005900170317     C                   PARM                    Idat
006000170317     C                   PARM                    Intw
006100170317     C                   PARM                    Ipkg
006200170317     C                   PARM                    Itsp
006300131009     C                   PARM                    Ilang
006400170317     c                   parm                    oCtrPr
006500170510     c***                parm                    oCtr
006600170510     c***                parm                    oDes
006700061017     C                   PARM                    rtnesito
006800061017     C                   PARM                    rtnopcode
006900061017     C                   PARM                    rtnstatus
007000061025     c*
007100060918
007200060918       //*******************************************************************************************
007300060918       //
007400060918       // Main.
007500060918       //
007600060918       //*******************************************************************************************
007700061017       CLEAR rtnesito;
007800061017       CLEAR rtnopcode;
007900061017       CLEAR rtnstatus;
008000170316
008100170317       CLEAR oCtrPr   ;
008200170317       CLEAR oCtr     ;
008300170510       //CLEAR oDes     ;
008400170510       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
008500170316
008600061026       *INLR = *ON;
008700080605
008800061025
008900131009     c                   if        ilang=*blanks
009000131009     c                   eval      ilang='it'
009100131009     c                   endif
009200170320     c                   if        idat=0
009300170320             dataiso=%date();
009400170320             idat =%dec(dataiso);
009500170320     c                   endif
009600170316
009700170317     C* Recupero network tariffa da passare a pgm di ricerca tariffe
009800170317     c                   clear                   trul27ds1
009900170317     c                   eval      i27ntw=intw
010000170320     c                   eval      I27CLI=iccm
010100170320     c                   eval      I27PKG=ipkg
010200170320
010300170317     c                   call      'TRUL27R1'
010400170317     c                   parm                    trul27ds1
010500170317
010600170316     c                   clear                   fnlv59ds
010700170316     c                   eval      ILV59KCI  = c_kci
010800170320     c                   eval      ILV59KSC  = iCcm
010900170320     c                   eval      ILV59DSP  = iDat
011000170320     c                   eval      ILV59tSP  = iTsp
011100170320     c                   eval      ILV59FIE  = o27fie
011200170320     c                   select
011300170320     c                   when      iNtw='DPD'
011400170320     c                   eval      ILV59TA3  = 'D'
011500170320     c                   when      iNtw='EEX'
011600170320     c                   eval      ILV59TA3  = 'E'
011700170320     c                   when      iNtw='FED'
011800170320     c                   eval      ILV59TA3  = 'F'
011900170320     c                   other
012000170320     c                   eval      ILV59TA3  = 'I'
012100170320     c                   endsl
012200170316     c                   eval      ILV59TBO  = 'P'
012300170316     c                   eval      ILV59RIC  = 'S'
012400170627     c                   eval      ILV59ta5  = *blanks
012500170316     c                   call      'FNLV59R'
012600170316     c                   parm                    fnlv59ds
012700170626     c*
012800170626     c* se non esistono tariffe valide per il cliente => no ritorno elenco
012900170626     c                   if        OLV59TKS = *blanks
013000170626     c                   else
013100170320     c                   eval      oCtrPr=olv59ctr
013200170320     c                   eval      oCtr  =olv59ctt
013300170510     c                   eval      opgt=olv59pgt
013400170510     c* carico la ds multipla per restituire questi dati come record set
013500170510     c                   z-add     1             ric_ctr
013600170518     c* nella prima ricorrenza metto sempre quella da proporre
013700170518     c     ric_ctr       occur     dsCtr
013800170518     c                   move      oCtrPr        DsCtr_cod
013900170518       chain (iccm:%int(oCtrPr):olv59prg) tntam01l;
014000170518       IF %FOUND(TNTAM01L);
014100170518          DsCtr_deco=tamdcv;
014200170518       ENDIF;
014300170518       idx=1    ;
014400170518     c                   dow       idx<=%elem(oCtr_) and
014500170518     c                             oCtr_(idx)<>*blanks
014600170518     c* escludo l'elemento se corrisponde al codice tariffa da proporre quindi
014700170518     c* già caricato nella prima ricorrenza
014800170518     c                   if        oCtr_(idx)<>oCtrPr
014900170518       ric_ctr+=1  ;
015000170510     c     ric_ctr       occur     dsCtr
015100170518     c                   move      oCtr_(idx)    DsCtr_cod
015200170518       chain (iccm:%int(octr_(idx)):%int(opgt_(idx))) tntam01l;
015300170510       IF %FOUND(TNTAM01L);
015400170510          DsCtr_deco=tamdcv;
015500170510       ENDIF;
015600170518     c                   endif
015700170518       idx+=1  ;
015800170510     c
015900170510     c                   enddo
016000170626     c                   endif
016100170510     c
016200170510     C/EXEC SQL
016300170510     C+ SET RESULT SETS FOR RETURN TO CLIENT
016400170510     c+                 array :DsCtr  for :RIC_ctr rows
016500170510     C/END-EXEC
016600170510      /FREE
016700170510
016800170510       IF sqlCode < 0;
016900170510         RTNstatus = sqlCode;
017000170510         RTNopcode = 'SQLCODE';
017100170510         RTNesito  = -1;
017200170510         RETURN;
017300170510       ENDIF;
017400170316
017500060918
017600060918       RETURN;
017700060918
