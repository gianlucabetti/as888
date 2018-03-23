000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200131004     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400171003     FTISTB03L  UF   E           K DISK
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
001500131004
001600170912
001700170919     D iKSU            s              8a
001800170919     D iTIP            s              2a
001900170919     D iTKNUM          s             10a
002000170919     D iVBELN          s             10a
002100170919     D iTRACKN         s             35a
002200170919     D iTRKSTAT        s              1a
002300170919     D iTRKSTATD       s              4a
002400170919     D iTRACKTSTMP     s             15s 0
002500170919     D iTRACKLOC       s             50a
002600170919     D iTRACKTEXT      s             80a
002700170919     D oReturnCode     s              1s 0
002800170912
002900131004
003000031103     D*-----------
003100031103     D* VARIABILI RIFERITE AL DATA-BASE
003200031103     D*-----------
003300170912     D wKstbKSU        S                   like(stbKSU)
003400170912     D wKstbFTR        S                   like(stbFTR)
003500110705
003600031103
003700031103     D*-----------
003800031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
003900031103     D*-----------
004000031103     D DSPPI           DS
004100170919     D  w§PPITIP                      2
004200170919     D  w§PPIKSU                      7  0
004300170912     D  w§PPIVMSPROC                       like(iVMSB2CProc)
004400170912     D  w§PPIVMSKSU                        like(iVMSB2CKSU)
004500170912
004600170919
004700170919     D*-----------
004800170919     D* RIDEFINIZIONE CAUSALE VMS CLIENTE
004900170919     D*-----------
005000170919     D DS_VMS          DS                  qualified
005100170919     D  CLISTS                        5
005200170919     D  CLIRSN                        5
005300141118
005400141118
005500141118     D*------------------
005600141118     D* LINKING A DEFINIZIONI ESTERNE
005700141118     D*------------------
005800170912     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
005900170912     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
006000170912     D/COPY GAITRASRC/SRCPROTOPR,UBVMSB2CR
006100170912     D/COPY GAITRASRC/SRCPROTOPI,UBVMSB2CR
006200170912     D/COPY GAITRASRC/SRCPROTOPI,UBVMSDDER
006300131004
006400030924
006500030325
006600170912       // EXEC SQL SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD;
006700170912
006800170912     C* MAIN/
006900171003     C*
007000171003     C* Avvio monitoring
007100171003     C                   monitor
007200140308     C*
007300170912     C* Verifica presenza mappatura VMS richiesta per traduzione
007400170912     C                   SETOFF                                       40
007500170912     C                   EXSR      chkVMS
007600170912     C*
007700170912     C* Se presente mappatura procedo
007800170912     C                   IF        not *in40
007900041015     C*
008000041015     C* Esegue traduzione
008100170912     C                   EXSR      traduci
008200161129     C*
008300170912     C* Diversamente traduzioe completamente fallita
008400161129     C                   ELSE
008500161129     C                   EVAL      wrkesito = '2'
008600170912     C*
008700170912     C* Invio email alert
008800170912     C                   EVAL      wObj =
008900170912     C                             'BARTVAS - Problema Mappatura TIVMS00F'
009000170912     C                   EVAL      wMsg =
009100170912     C                             'Problema Mappatura TIVMS00F'         +
009200170912     C                                                                    ':/N'+
009300170912     C                                                                    ':/N'+
009400170912     C                             'Verificare la mappatura: '           +':/N'+
009500170912     C                             'PROC: ' + w§PPIVMSPROC               +':/N'+
009600171002     C                             'KSU:  ' + w§PPIVMSKSU                +':/N'+
009700170912     C                             ' '                                   +':/N'
009800170912     C                   EXSR      sndAlert
009900161129     C                   ENDIF
010000171003     C*
010100171003     C* Gestisco errore monitoring
010200171003     C                   on-error
010300171003     C                   eval      wrkesito = '2'
010400171003     C                   dump(A)
010500171003     C*
010600171003     C* Stoppo il monitoring
010700171003     C                   endmon
010800110705     C*
010900921023     C                   SETON                                        LR
011000161129
011100161129
011200161129
011300170912     C     chkVMS        BEGSR
011400161129     C*
011500161129     C* Reperisco la versione di mappatura stati correntemente valida
011600161129     C                   call      'UBVMSDDER'
011700161129     C                   parm      *blanks       iVMSDDEOpz
011800161129     C                   parm      w§PPIVMSPROC  iVMSDDEProc
011900161129     C                   parm      w§PPIVMSKSU   iVMSDDEKSU
012000170919     C                   parm      datcor        iVMSDDEDtRif
012100161129     C                   parm                    oVMSDDEDDE
012200161129     C                   parm                    oVMSDDEEsito
012300161129     C*
012400161129     C* Verifico se esito no OK
012500161129     C                   if        oVMSDDEEsito <> '1'
012600170919     C                   seton                                        40
012700161129     C                   endif
012800161129     C*
012900161129     C                   ENDSR
013000131004
013100030924
013200030924
013300991027     C     traduci       BEGSR
013400031103     C*
013500031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
013600031103     C* di traduzione (trattasi d "post-traduzione")
013700031104     C                   EVAL      wKstbKSU = w§PPIKSU
013800031103     C                   EVAL      wKstbFTR = *blanks
013900110103     C     KEYstb03P     SETLL     tistb03l
014000110103     C                   IF        %equal(tistb03l)
014100110103     C     KEYstb03P     READE     tistb03l
014200991027     C*
014300031103     C* Ciclo x tuttii record del cliente da trasmettere
014400110103     C                   DOW       not %eof(tistb03l)
014500080618     C*
014600170912     C                   SETON                                        50
014700141118     C*
014800141118     C* Solo bolle originali e valide ai fini della spedizione
014900170912     C                   MOVEL     *blanks       wEsito1           1
015000170912     C                   EVAL      wEsito1 = %char(UBLBLSPE_GetLblTyp(
015100141118     C                                                stbAAS
015200141118     C                                               :stbLNP
015300141118     C                                               :stbNRS
015400141118     C                                               :stbNSP
015500141118     C                                               :pOutLblTyp
015600141118     C                                               :pOutAnnoBO
015700141118     C                                               :pOutLineaParBO
015800141118     C                                               :pOutSerieBO
015900141118     C                                               :pOutNumSpedBO
016000141118     C                                               :pOutDcmBO
016100141118     C                                               :pOutCcaBO
016200141118     C                                               :pOutRblBO))
016300141118     C*
016400170912     C                   IF        pOutLblTyp='O' AND pOutRblBO ='N'
016500031103     C*
016600031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
016700031103     C* deve restituire al cliente nel file tradotto
016800031103     C                   IF        stbFTR = *blanks                             * ridondante
016900110103     C*
017000110103     C* Gestisco solamente gli stati standard
017100130808     C                   IF        (stbTIS= '1' OR
017200171003     C                             (stbTIS= '9' AND stbPRS= '1'  ) OR
017300171003     C                             (stbTIS= '9' AND stbPRS= '4'  )) AND
017400130808     C                             (%subst(stbFOP:1:1)= 'O'  OR
017500130808     C                             (%subst(stbFOP:1:1)= 'F' AND stbSTS= '9'))
017600131004     C*
017700131004     C* Per "sicurezza" non invio stati senza data
017800131004     C                   IF        stbDAS > *zeros
017900110103     C*
018000110103     C                   SETOFF                                       50
018100170912     C*
018200170912                 // Effettuo mappatura tra causale BRT e stato CLIENTE
018300170912                 UBVMSB2CR_S (  *blanks
018400170912                              : *zeros
018500170912                              : *blanks
018600170912                              : w§PPIVMSPROC
018700170912                              : *blanks
018800170912                              : stbCOS
018900170912                              : oVMSB2CEsito
019000170912                              : oVMSB2CCauTip
019100170912                              : oVMSB2CCauTT
019200170912                              : oVMSB2CStsCli
019300170912                              : oVMSB2CStsOpz
019400170912                              : oVMSB2CFasOp
019500170912                              : oVMSB2CDDE
019600170912                              : oVMSB2CDesc
019700170912                             );
019800170912
019900170912               // Se mappatura riuscita e stato CLIENTE da ritornare
020000170912               if oVMSB2CEsito   = '1' AND
020100170912                  oVMSB2CStsCli <> 'NONINVIARE' AND
020200170912                  oVMSB2CStsCli <> *blanks;
020300170912                  *in50 = *on;
020400170912               endif;
020500170912     C*
020600170912     C* Se mappatura OK => procedo con invio status al cliente
020700170912     C   50              EXSR      sndStato
020800110103     C*
020900131010     C                   ENDIF
021000131004     C                   ENDIF
021100110103     C                   ENDIF
021200141118     C*
021300170912     C                   ENDIF
021400031103     C*
021500031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
021600031103     C                   EVAL      stbFTR = 'T'
021700171003     C                   UPDATE    tistb000
021800991027     C*
021900110103     C     KEYstb03P     READE     tistb03l
022000030325     C                   ENDDO
022100031103     C                   ENDIF
022200991027     C*
022300170913     C                   EVAL      wrkesito = '1'
022400991027     C*
022500910830     C                   ENDSR
022600031103
022700031103
022800170919
022900171002     C     sndStato      BEGSR
023000170919     C*
023100170919     C                   eval      DS_VMS = oVMSB2CStsCli
023200170919     C*
023300170919     C                   eval      iKSU        = '0'+%editc(w§PPIKSU:'X')
023400170919     C                   eval      iTIP        = w§PPITIP
023500170919     C                   eval      iTKNUM      = %trim(stbRMA)
023600170919     C                   eval      iVBELN      = %editc(stbRMN:'X')
023700170919     C                   eval      iTRACKN     = %editc(stbLNP:'X')+
023800170919     C                                           %editc(stbNRS:'X')+
023900170919     C                                           %editc(stbNSP:'X')
024000170919     C                   eval      iTRKSTAT    = DS_VMS.CLISTS
024100170919     C                   eval      iTRKSTATD   = DS_VMS.CLIRSN
024200170919     C                   eval      iTRACKTSTMP = STBDAS*1000000+STBORS*100
024300171002     C                   eval      iTRACKLOC   = *blanks
024400170919     C                   eval      iTRACKTEXT  = %trim(oVMSB2CDesc)
024500170919     C*
024600170919     C                   call      'TITV3A8R3'
024700170919     C                   parm                    iKSU
024800170919     C                   parm                    iTIP
024900170919     C                   parm                    iTKNUM
025000170919     C                   parm                    iVBELN
025100170919     C                   parm                    iTRACKN
025200170919     C                   parm                    iTRKSTAT
025300170919     C                   parm                    iTRKSTATD
025400170919     C                   parm                    iTRACKTSTMP
025500170919     C                   parm                    iTRACKLOC
025600170919     C                   parm                    iTRACKTEXT
025700170919     C                   parm                    oReturnCode
025800031103     C*
025900031103     C                   ENDSR
026000161129
026100161129
026200161129
026300161129     C*--------------------------------------------------------------------------------------------*
026400161129     C* /TITLE Compongo il testo e spedisco una e-mail di alert
026500161129     C*--------------------------------------------------------------------------------------------*
026600161129     C     sndAlert      BEGSR
026700161129     C*
026800161129     C* Inizializzo variabili
026900161129     C                   movel     *blanks       wrkEml          253
027000161129     C                   movel     *blanks       wrkCcEml        253
027100161129     C                   movel     *blanks       wrkEmlMsg      5000
027200161129     C                   movel     *blanks       wrkEmlOgg        44
027300161129     C*
027400161129     C* Valorizzo i campi della e-m@ail
027500170912     C                   eval      wrkEml    = 'cedalert@brt.it'
027600170912     C                   eval      wrkEmlOgg = wObj
027700170912     C                   EVAL      wrkEmlMsg = wMsg
027800161129     C*
027900161129     C                   call(e)   'TIS701C1'
028000161129     C                   parm                    wrkEml
028100161129     C                   parm                    wrkCcEml
028200161129     C                   parm                    wrkEmlOgg
028300161129     C                   parm                    wrkEmlMsg
028400161129     C                   parm                    wrkEsito          1
028500161129     C*
028600161129     C                   ENDSR
028700031103
028800031103
028900991027
029000991027      /TITLE Operazioni iniziali.
029100991027     C     *inzsr        BEGSR
029200991027     C*
029300991027     C     *ENTRY        PLIST
029400031103     C                   parm                    prmppi
029500991027     C     wrkesito      parm      wrkesito      prmesito
029600031103     C*
029700031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
029800031103     C                   EVAL      DSPPI = prmppi
029900131004     C*
030000131004     C* Reperisco i valori variabili ma a livello di transazione corrente
030100131004     C* (tipicamente data e ora relativ al file generato)
030200141107     C*
030300141107     C* CALCOLA LA DATA CORRENTE
030400170919     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
030500170919     C                   eval      datcor = %dec(%date() : *ISO)
030600030709     C*
030700030709     C* Definizione chiavi
030800030924     C*
030900110103     C* Chiave su TISTB03L - Parziale
031000110103     C     KEYstb03P     KLIST
031100031104     C                   KFLD                    wKstbKSU
031200031103     C                   KFLD                    wKstbFTR
031300991027     C*
031400991027     C                   ENDSR
