000100161130     H DECEDIT('0,') DATEDIT(*ymd)
000200161130     H OPTION( *SrcStmt : *NoDebugIO)
000300161130     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000400161130
000500110922     D GetErrno        PR              *   EXTPROC('__errno')
000600161130
000700161130     D/COPY GAITRASRC/SRCPROTOPI,UBSOCKET2
000800161130     D/COPY GAITRASRC/SRCPROTOPR,UBSOCKET2
000900161130
001000161130
001100161130     D*******
001200161130     D* PASSAGGIO PARAMETRI
001300161130     D*******
001400161130     D iURL            S           1024A
001500161130     D iServerName     S            255A
001600161130     D iPortNumber     S             10I 0
001700161130     D oResponse       S           1024A
001800161130
001900161130
002000161130     D*******
002100161130     D* VARIABILI DI WRK
002200161130     D*******
002300161130     D $erro           S             10I 0 INZ
002400161130     D errno           S             10I 0 BASED(errnoPtr)
002500161130     D $ii             S             10I 0 INZ(1)
002600161130     D $jj             S             10I 0 INZ
002700161130     D $Sep            S              4A   INZ(x'0D250D25')
002800161130     D $SepL           S             10I 0 INZ(%size($Sep))
002900161130
003000161130
003100040129     C     *entry        plist
003200161130     C                   parm                    iURL
003300161130     C                   parm                    iServerName
003400161130     C                   parm                    iPortNumber
003500161130     C                   parm                    oResponse
003600161130     C*
003700161130     C* Inizializzazione parametri di output e variabili di wrk
003800161130     C                   eval      *inlr = *on
003900161130     C                   clear                   $erro
004000161130     C                   clear                   oResponse
004100161130     C*
004200161130     C* Creo il Descrittore Socket
004300161130     C                   if        UBSOCKET2_Create('TCP'
004400161130     C                                             :pOutSktDescr) < 0
004500161130     C                   eval      $erro = 1
004600040129     C                   exsr      $finsr
004700040129     C                   endif
004800161130     C*
004900161130     C                   if        UBSOCKET2_Connect(pOutSktDescr
005000161130     C                                              :iServerName
005100161130     C                                              :iPortNumber) < 0
005200110922     C                   eval      errnoPtr = getErrno()
005300110922     C                   dump(a)
005400161130     C                   eval      $erro = 2
005500040129     C                   exsr      $finsr
005600040129     C                   endif
005700161130     C*
005800161130     C* Compongo il comando da inviare al server di JDOC e sottomettto il comando via Socket
005900161130     C                   eval      pInSktMsg = 'GET '+%trim(iURL)
006000161130     C                                        +' HTTP/1.0'
006100161130     C                                        + x'0d25'
006200161130     C                                        + x'0d25'
006300161130     C                   clear                   pInMsgCvtTbl
006400161130     C                   if        UBSOCKET2_Send(pOutSktDescr
006500161130     C                                           :pInSktMsg
006600161130     C                                           :pInMsgCvtTbl) < 0
006700161130     C                   eval      errnoPtr = getErrno()
006800161130     C                   dump(a)
006900161130     C                   eval      $erro = 3
007000161130     C                   exsr      $finsr
007100161130     C                   endif
007200161130     C*
007300161130     C* Ricevo la risposata del comando appena inviato
007400161130     C                   clear                   pInMsgCvtTbl
007500161130     c                   dow       1 = 1
007600161130     C                   if        UBSOCKET2_Receive(pOutSktDescr
007700161130     C                                              :%size(pInSktMsg)
007800161130     C                                              :'0'
007900161130     C                                              :pInMsgCvtTbl
008000161130     C                                              :pOutRespLen
008100161130     C                                              :pOutRespTxt
008200161130     C                                              :pOutRespFull) = 0
008300161130     C                   eval      oResponse = %subst(pOutRespTxt:1:pOutRespLen)
008400161130     C*
008500161130     C* Se tutto OK => isolo il nome del immagine servita da JDOC
008600161130     C                   dow       1=1
008700161130     C                   eval      $jj = %scan($Sep:oResponse:$ii)
008800161130     C                   if        $jj > *zeros
008900161130     C                   eval      $ii = $jj + $SepL
009000161130     C                   else
009100161130     C                   leave
009200161130     C                   endif
009300161130     C                   enddo
009400161130     C                   eval      oResponse = %subst(oResponse:$ii)
009500161130     C*
009600161130     C                   else
009700161130     C                   eval      errnoPtr = getErrno()
009800161130     C                   dump(a)
009900161130     C                   eval      $erro = 4
010000161130     C                   exsr      $finsr
010100161130     C                   endif
010200161130     C                   if        pOutRespFull > *zeros OR
010300161130     C                             pOutRespLen  = *zeros
010400161130     C                   leave
010500161130     C                   endif
010600161130     C                   enddo
010700161130     C*
010800161130     C* Chiudo il Descrittore Socket
010900161130     C                   callp     UBSOCKET2_Garbage(pOutSktDescr)
011000161130     C*
011100040129     C                   exsr      $finsr
011200161130
011300161130
011400161130
011500040129     C     $finSr        begsr
011600161130     C*
011700161130     C                   if        $erro > *zeros
011800161130     C                   eval      oResponse = 'ERRORE_CONNECT'
011900040204     C                   endif
012000040129     C                   return
012100161130     C*
012200040129     C                   endsr
012300161130
