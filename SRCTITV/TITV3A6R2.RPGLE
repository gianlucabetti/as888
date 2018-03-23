000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200131004     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400110103     FTISTB03L  UF   E           K DISK    commit
000500161129
000600161129
000700041015     D*-----------
000800041015     D* VARIABILI D WRK
000900041015     D*-----------
001000031103     D prmppi          S             50
001100030924     D prmesito        S              1
001200030924     D wrkesito        S                   like(prmesito)
001300170912     D wObj            s           1000    inz varying
001400170912     D wMsg            s           1000    inz varying
001500171004     D wAmbiente       s                   inz like(iAmbiente)
001600131004
001700170912
001800170912     D iAmbiente       s              1a   inz
001900170912     D iSAPnumDOC      s             10a   inz
002000170912     D iStatus         s              4a   inz
002100170912     D iTipoStato      s              2a   inz
002200170912     D iStatusDate     s              8a   inz
002300170912     D iStatusTime     s              6a   inz
002400170912     D oReturnCode     s             10a   inz
002500170912     D oReturnDescr    s             80a   inz
002600170912
002700131004
002800031103     D*-----------
002900031103     D* VARIABILI RIFERITE AL DATA-BASE
003000031103     D*-----------
003100170912     D wKstbKSU        S                   like(stbKSU)
003200170912     D wKstbFTR        S                   like(stbFTR)
003300030924
003400110705
003500031103
003600031103     D*-----------
003700031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
003800031103     D*-----------
003900031103     D DSPPI           DS
004000170912     D  w§PPIENV                      1
004100170912     D  w§PPITIP                      2
004200080618     D  w§PPIKSU                      7  0
004300170912     D  w§PPIVMSPROC                       like(iVMSB2CProc)
004400170912     D  w§PPIVMSKSU                        like(iVMSB2CKSU)
004500170912
004600141118
004700141118
004800141118     D*------------------
004900141118     D* LINKING A DEFINIZIONI ESTERNE
005000141118     D*------------------
005100170912     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
005200170912     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
005300170912     D/COPY GAITRASRC/SRCPROTOPR,UBVMSB2CR
005400170912     D/COPY GAITRASRC/SRCPROTOPI,UBVMSB2CR
005500170912     D/COPY GAITRASRC/SRCPROTOPI,UBVMSDDER
005600171024     D/COPY GAITRASRC/SRCPROTOPI,TIS713R
005700131004
005800030924
005900030325
006000170912       // EXEC SQL SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD;
006100170912
006200170912     C* MAIN/
006300140308     C*
006400170912     C* Verifica presenza mappatura VMS richiesta per traduzione
006500170912     C                   SETOFF                                       40
006600170912     C                   EXSR      chkVMS
006700170912     C*
006800170912     C* Se presente mappatura procedo
006900170912     C                   IF        not *in40
007000041015     C*
007100041015     C* Esegue traduzione
007200170912     C                   EXSR      traduci
007300161129     C*
007400170912     C* Diversamente traduzioe completamente fallita
007500161129     C                   ELSE
007600161129     C                   EVAL      wrkesito = '2'
007700170912     C*
007800170912     C* Invio email alert
007900170912     C                   EVAL      wObj =
008000170912     C                             'BARTVAS - Problema Mappatura TIVMS00F'
008100170912     C                   EVAL      wMsg =
008200170912     C                             'Problema Mappatura TIVMS00F'         +
008300170912     C                                                                    ':/N'+
008400170912     C                                                                    ':/N'+
008500170912     C                             'Verificare la mappatura: '           +':/N'+
008600170912     C                             'PROC: ' + w§PPIVMSPROC               +':/N'+
008700170912     C                             'KSU : ' + w§PPIVMSKSU                +':/N'+
008800170912     C                                                                    ':/N'+
008900170912     C                             'Causale mancante: ' + stbCOS         +':/N'+
009000170912     C                             ' '                                   +':/N'
009100170912     C                   EXSR      sndAlert
009200161129     C                   ENDIF
009300110705     C*
009400921023     C                   SETON                                        LR
009500161129
009600161129
009700161129
009800170912     C     chkVMS        BEGSR
009900161129     C*
010000161129     C* Reperisco la versione di mappatura stati correntemente valida
010100161129     C                   call      'UBVMSDDER'
010200161129     C                   parm      *blanks       iVMSDDEOpz
010300161129     C                   parm      w§PPIVMSPROC  iVMSDDEProc
010400161129     C                   parm      w§PPIVMSKSU   iVMSDDEKSU
010500161129     C                   parm      datcor8       iVMSDDEDtRif
010600161129     C                   parm                    oVMSDDEDDE
010700161129     C                   parm                    oVMSDDEEsito
010800161129     C*
010900161129     C* Verifico se esito no OK
011000161129     C                   if        oVMSDDEEsito <> '1'
011100161129     C                   seton                                        40
011200161129     C                   endif
011300161129     C*
011400161129     C                   ENDSR
011500131004
011600030924
011700030924
011800991027     C     traduci       BEGSR
011900171004     C*
012000171024     C                   eval      wAmbiente = w§PPIENV
012100171024     C                   select
012200171024     C                   when      wAmbiente = *blanks
012300171024     C                   exsr      CarTab
012400171024     C                   endsl
012500031103     C*
012600031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
012700031103     C* di traduzione (trattasi d "post-traduzione")
012800031104     C                   EVAL      wKstbKSU = w§PPIKSU
012900031103     C                   EVAL      wKstbFTR = *blanks
013000110103     C     KEYstb03P     SETLL     tistb03l
013100110103     C                   IF        %equal(tistb03l)
013200110103     C     KEYstb03P     READE     tistb03l
013300991027     C*
013400031103     C* Ciclo x tuttii record del cliente da trasmettere
013500110103     C                   DOW       not %eof(tistb03l)
013600080618     C*
013700170912     C                   SETON                                        50
013800141118     C*
013900141118     C* Solo bolle originali e valide ai fini della spedizione
014000170912     C                   MOVEL     *blanks       wEsito1           1
014100170912     C                   EVAL      wEsito1 = %char(UBLBLSPE_GetLblTyp(
014200141118     C                                                stbAAS
014300141118     C                                               :stbLNP
014400141118     C                                               :stbNRS
014500141118     C                                               :stbNSP
014600141118     C                                               :pOutLblTyp
014700141118     C                                               :pOutAnnoBO
014800141118     C                                               :pOutLineaParBO
014900141118     C                                               :pOutSerieBO
015000141118     C                                               :pOutNumSpedBO
015100141118     C                                               :pOutDcmBO
015200141118     C                                               :pOutCcaBO
015300141118     C                                               :pOutRblBO))
015400141118     C*
015500171004     C                   IF        (pOutLblTyp='O' AND pOutRblBO ='N') OR
015600171004     C                             wAmbiente = 'T'
015700031103     C*
015800031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
015900031103     C* deve restituire al cliente nel file tradotto
016000031103     C                   IF        stbFTR = *blanks                             * ridondante
016100110103     C*
016200110103     C* Gestisco solamente gli stati standard
016300171002     C                   IF        (stbTIS= '1' OR
016400171002     C                             (stbTIS= '9' AND stbPRS='1') OR
016500171002     C                             (stbTIS= '2' AND stbPRS='5')) AND
016600130808     C                             (%subst(stbFOP:1:1)= 'O'  OR
016700130808     C                             (%subst(stbFOP:1:1)= 'F' AND stbSTS= '9'))
016800131004     C*
016900131004     C* Per "sicurezza" non invio stati senza data
017000131004     C                   IF        stbDAS > *zeros
017100110103     C*
017200110103     C                   SETOFF                                       50
017300170912     C*
017400170912                 // Effettuo mappatura tra causale BRT e stato CLIENTE
017500170912                 UBVMSB2CR_S (  *blanks
017600170912                              : *zeros
017700170912                              : *blanks
017800170912                              : w§PPIVMSPROC
017900170912                              : *blanks
018000170912                              : stbCOS
018100170912                              : oVMSB2CEsito
018200170912                              : oVMSB2CCauTip
018300170912                              : oVMSB2CCauTT
018400170912                              : oVMSB2CStsCli
018500170912                              : oVMSB2CStsOpz
018600170912                              : oVMSB2CFasOp
018700170912                              : oVMSB2CDDE
018800170912                              : oVMSB2CDesc
018900170912                             );
019000170912
019100170912               // Se mappatura riuscita e stato CLIENTE da ritornare
019200170912               if oVMSB2CEsito   = '1' AND
019300170912                  oVMSB2CStsCli <> 'NONINVIARE' AND
019400170912                  oVMSB2CStsCli <> *blanks;
019500170912                  *in50 = *on;
019600170912               endif;
019700170912     C*
019800170912     C* Se mappatura OK => procedo con invio status al cliente
019900170912     C   50              EXSR      sndStato
020000110103     C*
020100131010     C                   ENDIF
020200131004     C                   ENDIF
020300110103     C                   ENDIF
020400141118     C*
020500170912     C                   ENDIF
020600031103     C*
020700031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
020800031103     C                   EVAL      stbFTR = 'T'
020900031103     C                   UPDATE    tistb000
021000991027     C*
021100110103     C     KEYstb03P     READE     tistb03l
021200030325     C                   ENDDO
021300031103     C                   ENDIF
021400991027     C*
021500170913     C                   EVAL      wrkesito = '1'
021600991027     C*
021700910830     C                   ENDSR
021800031103
021900031103
022000031103
022100170912     C     sndStato      BEGSR
022200170912     C*
022300171004     C                   eval      iAmbiente   = wAmbiente
022400170912     C                   eval      iSAPnumDOC  = %trim(stbRMA)
022500170912     C                   eval      iStatus     = %trim(oVMSB2CStsCli)
022600170912     C                   eval      iTipoStato  = 'SP'
022700170912     C                   eval      iStatusDate = %editc(stbDAS:'X')
022800170912     C                   eval      iStatusTime = %editc(stbORS:'X')+'00'
022900170912     C                   call      'TITV3A6C3'
023000170912     C                   parm                    iAmbiente
023100170912     C                   parm                    iSAPnumDOC
023200170912     C                   parm                    iStatus
023300170912     C                   parm                    iTipoStato
023400170912     C                   parm                    iStatusDate
023500170912     C                   parm                    iStatusTime
023600170912     C                   parm                    oReturnCode
023700170912     C                   parm                    oReturnDescr
023800170912     C*
023900170912     C*  Se esito invocazione servizio non OK
024000170912     C                   if         %trim(oReturnCode) = '0'
024100170912     C                   else
024200170912     C*
024300170912     C* Invio email alert
024400170912     C                   eval      wObj =
024500170912     C                             'BARTVAS - Problema invio stati a Telecom'
024600170912     C                   eval      wMsg =
024700170912     C                             'ATTENZIONE problemi nel invio stati a '    +
024800170912     C                             'Telecom via Web Service.'            +':/N'+
024900170912     C                             'Verificare connettività verso endpoint '   +
025000170912     C                             'remoto Telecom'
025100170912     C                   EXSR      sndAlert
025200170912     C                   endif
025300031103     C*
025400031103     C                   ENDSR
025500171024
025600171024
025700171024
025800171024     C*
025900171024     C     CarTab        begsr
026000171024     C*
026100171024     C* Reperimento Ambiente COLLAUDO(TEST) / ESERCIZIO(PROD) da utilizzare
026200171024     C                   eval      iS713TBECOD = 'VPR'
026300171024     C                   eval      iS713TBEKE1 = '*VAS-CLI'
026400171024     C                   eval      iS713TBEKE2 = 'TELECOM'
026500171024     C                   eval      iS713TBELIN = *blanks
026600171024     C                   eval      iS713TBESIF = *blanks
026700171024     C                   call      'TIS713R'
026800171024     C                   parm                    iS713TBECOD
026900171024     C                   parm                    iS713TBEKE1
027000171024     C                   parm                    iS713TBEKE2
027100171024     C                   parm                    iS713TBELIN
027200171024     C                   parm                    iS713TBESIF
027300171024     C                   parm                    oS713TBEUNI
027400171024     C                   parm                    oS713ESITO
027500171024     C*
027600171024     C                   if        oS713ESITO = '1'
027700171024     C                   eval      wAmbiente = %trim(oS713TBEUNI)
027800171024     C                   endif
027900171024     C*
028000171024     C                   endsr
028100171024     C*
028200161129
028300161129
028400161129
028500161129     C*--------------------------------------------------------------------------------------------*
028600161129     C* /TITLE Compongo il testo e spedisco una e-mail di alert
028700161129     C*--------------------------------------------------------------------------------------------*
028800161129     C     sndAlert      BEGSR
028900161129     C*
029000161129     C* Inizializzo variabili
029100161129     C                   movel     *blanks       wrkEml          253
029200161129     C                   movel     *blanks       wrkCcEml        253
029300161129     C                   movel     *blanks       wrkEmlMsg      5000
029400161129     C                   movel     *blanks       wrkEmlOgg        44
029500161129     C*
029600161129     C* Valorizzo i campi della e-m@ail
029700170912     C                   eval      wrkEml    = 'cedalert@brt.it'
029800170912     C                   eval      wrkEmlOgg = wObj
029900170912     C                   EVAL      wrkEmlMsg = wMsg
030000161129     C*
030100161129     C                   call(e)   'TIS701C1'
030200161129     C                   parm                    wrkEml
030300161129     C                   parm                    wrkCcEml
030400161129     C                   parm                    wrkEmlOgg
030500161129     C                   parm                    wrkEmlMsg
030600161129     C                   parm                    wrkEsito          1
030700161129     C*
030800161129     C                   ENDSR
030900031103
031000031103
031100991027
031200991027      /TITLE Operazioni iniziali.
031300991027     C     *inzsr        BEGSR
031400991027     C*
031500991027     C     *ENTRY        PLIST
031600031103     C                   parm                    prmppi
031700991027     C     wrkesito      parm      wrkesito      prmesito
031800031103     C*
031900031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
032000031103     C                   EVAL      DSPPI = prmppi
032100131004     C*
032200131004     C* Reperisco i valori variabili ma a livello di transazione corrente
032300131004     C* (tipicamente data e ora relativ al file generato)
032400141107     C*
032500141107     C* CALCOLA LA DATA CORRENTE
032600141107     C                   z-add     *zeros        datcor8           8 0          *DATA CORRENTE AA/M/
032700141107     C                   eval      datcor8 = %dec(%date() : *ISO)
032800030709     C*
032900030709     C* Definizione chiavi
033000030924     C*
033100110103     C* Chiave su TISTB03L - Parziale
033200110103     C     KEYstb03P     KLIST
033300031104     C                   KFLD                    wKstbKSU
033400031103     C                   KFLD                    wKstbFTR
033500991027     C*
033600991027     C                   ENDSR
